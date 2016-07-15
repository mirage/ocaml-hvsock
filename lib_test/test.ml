module TcpSocket = struct
  type 'a io = 'a
  type t = Unix.file_descr

  let create () =
    let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt s Unix.SO_REUSEADDR true;
    s

  type sockaddr = int

  let bind t port = Unix.bind t (Unix.ADDR_INET (Unix.inet_addr_any, port))

  let getsockname t = match Unix.getsockname t with
    | Unix.ADDR_INET(_, port) -> port
    | _ -> assert false

  let listen t backlog = Unix.listen t backlog

  let accept t =
    let fd, sa = Unix.accept t in
    match sa with
    | Unix.ADDR_INET(_, port) -> fd, port
    | _ -> assert false

  let connect t port = Unix.connect t (Unix.ADDR_INET(Unix.inet_addr_of_string "127.0.0.1", port))

  let close t = Unix.close t

  type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  external stub_ba_recv: Unix.file_descr -> buffer -> int -> int -> int = "stub_hvsock_ba_recv"
  external stub_ba_send: Unix.file_descr -> buffer -> int -> int -> int = "stub_hvsock_ba_send"

  let read t b = stub_ba_recv t b.Cstruct.buffer b.Cstruct.off b.Cstruct.len
  let write t b = stub_ba_send t b.Cstruct.buffer b.Cstruct.off b.Cstruct.len

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
    Socket.listen s 32;
    let rec loop () =
      Socket.accept s
      >>= fun (client, _) ->
      let _ =
        Lwt.finalize
          (fun () -> f client)
          (fun () -> Socket.close client) in
      loop () in
    let _ = loop () in
    { s; port }

  let stop_server { s } = Socket.close s

  let with_server server_f next =
    let s = create_server server_f in
    Lwt.finalize
      (fun () -> next s.port)
      (fun () -> stop_server s)

  let rec really op s remaining =
    if Cstruct.len remaining = 0
    then Lwt.return_unit
    else
      op s remaining
      >>= fun n ->
      really op s (Cstruct.shift remaining n)

  let really_read = really Socket.read
  let really_write = really Socket.write

  let echo s =
    let rec loop () =
      (* Let's allocate a lot to stress the system a bit *)
      let buffer = Cstruct.create 128 in
      Socket.read s buffer
      >>= function
      | 0 -> Lwt.return_unit
      | n ->
        let tosend = Cstruct.sub buffer 0 n in
        really_write s tosend
        >>= fun () ->
        loop () in
    loop ()

  let with_client port f =
    let s = Socket.create () in
    Lwt.finalize
      (fun () ->
        Socket.connect s port
        >>= fun () ->
        f s
      ) (fun () -> Socket.close s)

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

  let one_echo_test () =
    with_server echo
      (fun port ->
        with_client port
          (fun s ->
            let length = 100 in
            let buf = nice_pattern length in
            let buf' = zeroes length in
            let background_reader_t = really_read s buf' in
            really_write s buf
            >>= fun () ->
            background_reader_t
            >>= fun () ->
            if Cstruct.compare buf buf' <> 0 then failwith "buffers don't match";
            Lwt.return ()
          )
      )

end

module Lwt_unix_time = struct
  type 'a io = 'a Lwt.t
  let sleep = Lwt_unix.sleep
end
module Lwt_unix_main = struct
  let run_in_main = Lwt_preemptive.run_in_main
end

module Lwt_unix_tests = Make(Lwt_unix_time)(Lwt_unix_main)

let test_something_case () = ()

let test_something = [
  "something", `Quick, test_something_case;
]

let test_lwt_unix = [
  "one echo request response", `Quick, (fun () -> Lwt_main.run (Lwt_unix_tests.one_echo_test ()));
]

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Alcotest.run "hvsock" [
    "something", test_something;
    "Lwt_unix tests", test_lwt_unix;
  ]
