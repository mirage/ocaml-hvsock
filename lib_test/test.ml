let src =
  let src = Logs.Src.create "test" ~doc:"hvsock test" in
  Logs.Src.set_level src (Some Logs.Info);
  src

module Log = (val Logs.src_log src : Logs.LOG)

module TcpSocket = struct
  type 'a io = 'a
  type t = {
    mutable s: Unix.file_descr option;
    i: int;
  }

  let next_id =
    let next = ref 0 in
    fun () ->
      let result = !next in
      incr next;
      result

  let create () =
    let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt s Unix.SO_REUSEADDR true;
    let i = next_id () in
    { s = Some s; i }

  type sockaddr = int

  let with_fd fn_name fn_arg t f = match t with
    | { s = None } -> raise (Unix.Unix_error(Unix.EBADF, fn_name, fn_arg))
    | { s = Some s } -> f s

  let bind t port =
    with_fd "bind" "" t (fun s -> Unix.bind s (Unix.ADDR_INET (Unix.inet_addr_any, port)))

  let getsockname t =
    with_fd "getsockname" "" t
      (fun s -> match Unix.getsockname s with
        | Unix.ADDR_INET(_, port) -> port
        | _ -> assert false
      )

  let listen t backlog = with_fd "listen" "" t (fun s -> Unix.listen s backlog)

  let accept t =
    with_fd "accept" "" t
      (fun s ->
        let fd, sa = Unix.accept s in
        match sa with
        | Unix.ADDR_INET(_, port) ->
          let i = next_id () in
          { s = Some fd; i }, port
        | _ -> assert false
      )

  let connect t port =
    with_fd "connect" "" t
      (fun s ->
        Unix.connect s (Unix.ADDR_INET(Unix.inet_addr_of_string "127.0.0.1", port))
      )

  let close t = match t with
    | { s = None } ->
      Log.info (fun f -> f "double close");
      ()
    | { s = Some s } ->
      t.s <- None;
      Log.debug (fun f -> f "close %d" t.i);
      Unix.close s

  type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  external stub_ba_recv: Unix.file_descr -> buffer -> int -> int -> int = "stub_hvsock_ba_recv"
  external stub_ba_send: Unix.file_descr -> buffer -> int -> int -> int = "stub_hvsock_ba_send"

  let read t b =
    assert (b.Cstruct.len <> 0);
    with_fd "recv" "" t
      (fun s ->
        try
          Log.debug (fun f -> f "%d: read ..." t.i);
          let r = stub_ba_recv s b.Cstruct.buffer b.Cstruct.off b.Cstruct.len in
          Log.debug (fun f -> f "%d: read ... = %d" t.i r);
          r
        with
        | Unix.Unix_error(Unix.EPIPE, _, _) ->
          Log.debug (fun f -> f "%d: read ... EPIPE" t.i);
          0
        | Unix.Unix_error(Unix.ECONNRESET, _, _) ->
          Log.debug (fun f -> f "%d  read ... ECONNRESET" t.i);
          0
      )

  let write t b =
    assert (b.Cstruct.len <> 0);
    with_fd "send" "" t
      (fun s ->
        try
          Log.debug (fun f -> f "%d write ..." t.i);
          let r = stub_ba_send s b.Cstruct.buffer b.Cstruct.off b.Cstruct.len in
          Log.debug (fun f -> f "%d write ... = %d" t.i r);
          r
        with
        | Unix.Unix_error(Unix.EPIPE, _, _) ->
          Log.debug (fun f -> f "%d write ... EPIPE" t.i);
          0
      )
end

open Lwt.Infix

module Make(Time: V1_LWT.TIME)(Main: Lwt_hvsock_s.MAIN) = struct
  module Socket = Lwt_blocking_socket.Make(Time)(Main)(TcpSocket)

  type server = {
    s: Socket.t;
    port: int;
  }

  let create_server f =
    let tcp = TcpSocket.create () in
    TcpSocket.bind tcp 0;
    let port = TcpSocket.getsockname tcp in
    let s = Socket.of_file_descr tcp in
    Socket.listen s 1;
    let rec loop () =
      Socket.accept s
      >>= fun (client, _) ->
      let _ =
        Lwt.finalize
          (fun () -> f client)
          (fun () ->
            Socket.close client) in
      loop () in
    let _ = loop () in
    { s; port }

  let stop_server { s } =
    Socket.close s

  let with_server server_f next =
    let s = create_server server_f in
    Lwt.finalize
      (fun () -> next s.port)
      (fun () -> stop_server s)

  let rec really op s remaining =
    if Cstruct.len remaining = 0
    then Lwt.return_unit
    else begin
      op s remaining
      >>= fun n ->
      if n = 0 then begin
        Log.err (fun f -> f "IO operation returned 0 when we needed %d bytes" (Cstruct.len remaining));
        Lwt.fail End_of_file
      end
      else really op s (Cstruct.shift remaining n)
    end
  let really_read = really Socket.read
  let really_write = really Socket.write

  let echo s =
    let rec loop total =
      (* Let's allocate a lot to stress the system a bit *)
      let buffer = Cstruct.create 128 in
      Socket.read s buffer
      >>= function
      | 0 ->
        Lwt.return_unit
      | n ->
        let tosend = Cstruct.sub buffer 0 n in
        Lwt.catch
          (fun () ->
            really_write s tosend
          ) (fun e ->
            Log.err (fun f -> f "echo server caught %s" (Printexc.to_string e));
            Lwt.fail e
          )
        >>= fun () ->
        loop (total + n) in
    loop 0

  let rec with_client_one_attempt port f =
    let s = Socket.create () in
    Lwt.finalize
      (fun () ->
        Socket.connect s port
        >>= fun () ->
        f s
      ) (fun () -> Socket.close s)

  let rec with_client port f =
    Lwt.catch
      (fun () -> with_client_one_attempt port f)
      (function
        | Unix.Unix_error(Unix.ECONNREFUSED, _, _)
        | Unix.Unix_error(Unix.ECONNRESET, _, _) ->
          (* We've exceeded the server listen backlog *)
          Time.sleep 1.
          >>= fun () ->
          with_client port f
        | e -> Lwt.fail e
      )

  let nice_pattern length =
    let buf = Cstruct.create length in
    let message = "This is a nice readable pattern which looks absolutely nothing like an OCaml heap header" in
    for i = 0 to length - 1 do
      Cstruct.set_char buf i message.[i mod (String.length message)]
    done;
    buf

  let zeroes length =
    let buf = Cstruct.create length in
    Cstruct.memset buf 0;
    buf

  let rec range start finish = if start = finish then [] else start :: (range (start + 1) finish)

  let echo_test ~buffer_length ~iterations ~clients () =
    with_server echo
      (fun port ->
        let rec loop remaining =
          Log.debug (fun f -> f "Starting %d concurrent connections" remaining);
          if remaining = 0
          then Lwt.return_unit
          else begin
            let clients = List.map (fun i ->
              with_client port
                (fun s ->
                  let rec loop = function
                    | 0 -> Lwt.return_unit
                    | n ->
                    Gc.compact ();
                      let buf = nice_pattern buffer_length in
                      let buf' = zeroes buffer_length in
                      let background_reader_t = really_read s buf' in
                      really_write s buf
                      >>= fun () ->
                      background_reader_t
                      >>= fun () ->
                      if Cstruct.compare buf buf' <> 0 then failwith "buffers don't match";
                      loop (n - 1) in
                  Lwt.catch
                    (fun () ->
                      loop iterations
                      >>= fun () ->
                      Lwt.return 1
                    ) (fun e ->
                      Log.info (fun f -> f "client loop caught %s" (Printexc.to_string e));
                      Lwt.return 0
                    )
                )
            ) (range 0 remaining) in
            Lwt_list.fold_left_s
              (fun acc s ->
                s >>= fun result ->
                Lwt.return (acc + result)
              ) 0 clients
            >>= fun total ->
            loop (remaining - total)
          end in
        loop clients
      )
  let one_echo_test () = echo_test ~buffer_length:100 ~iterations:1 ~clients:1 ()
  let many_echo_test () = echo_test ~buffer_length:100 ~iterations:1000 ~clients:1 ()
  let concurrent_one_echo_test () = echo_test ~buffer_length:100 ~iterations:1 ~clients:100 ()
  let concurrent_many_echo_test () = echo_test ~buffer_length:100 ~iterations:100 ~clients:100 ()

  let suite = [
    "one echo request response", one_echo_test;
    "many echo request responses", many_echo_test;
    "concurrent one echo request responses", concurrent_one_echo_test;
    "concurrent many echo request responses", concurrent_many_echo_test;
  ]
end

module Lwt_unix_time = struct
  type 'a io = 'a Lwt.t
  let sleep = Lwt_unix.sleep
end
module Lwt_unix_main = struct
  let run_in_main = Lwt_preemptive.run_in_main
end

module Lwt_unix_tests = Make(Lwt_unix_time)(Lwt_unix_main)

let test_lwt_unix = List.map (fun (descr, thread) ->
  descr, `Quick, (fun () -> Lwt_main.run (thread ()))
) Lwt_unix_tests.suite


let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  (try Sys.set_signal Sys.sigpipe Sys.Signal_ignore with _ -> ());
  Alcotest.run "hvsock" [
    "Lwt_unix tests", test_lwt_unix;
  ]
