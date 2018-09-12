(*
 * Copyright (C) 2015 David Scott <dave.scott@unikernel.com>
 * Copyright (C) 2016 Docker Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Lwt

let sigint_t, sigint_u = Lwt.task ()

open Cmdliner

let default_serviceid =
  Printf.sprintf "%08x-FACB-11E6-BD58-64006A7986D3" 0x5653 (* matches virtsock/cmd/sock_stress/vsock.go *)

let buffer_size = 4096

module Time = struct
  type 'a io = 'a Lwt.t
  let sleep_ns ns = Lwt_unix.sleep (Duration.to_f ns)
end
module Hv = Flow_lwt_hvsock.Make(Time)(Lwt_hvsock_detach)

let rec connect vmid serviceid =
  let fd = Hv.Hvsock.create () in
  Lwt.catch
    (fun () ->
      Hv.Hvsock.connect fd { Hvsock.vmid; serviceid }
      >>= fun () ->
      let flow = Hv.connect fd in
      Lwt.return flow
    ) (fun e ->
      Printf.fprintf stderr "connect raised %s: sleep 1s and retrying\n%!" (Printexc.to_string e);
      Hv.Hvsock.close fd
      >>= fun () ->
      Lwt_unix.sleep 1.
      >>= fun () ->
      connect vmid serviceid
    )

let send_receive_verify flow =
  let reader_sha = Sha256.init () in
  let rec reader n =
    Printf.fprintf stderr "about to read\n%!";
    Hv.read flow
    >>= function
    | Ok `Eof ->
      Printf.fprintf stderr "Reader read total %d bytes\n%!" n;
      Lwt.return n
    | Ok (`Data buf) ->
      Printf.fprintf stderr "read(%d)\n%!" (Cstruct.len buf);
      let s = Cstruct.to_string buf in
      Sha256.update_string reader_sha s;
      reader (n + (Cstruct.len buf))
    | Error _ ->
      failwith "Flow read error" in
  let writer_sha = Sha256.init () in
  let writer n =
    let rec loop remaining =
      if remaining = 0 then begin
        (* FIXME: this really should be close *)
        Hv.shutdown_write flow
        >>= fun () ->
        Printf.fprintf stderr "Writer wrote total %d bytes\n%!" n;
        Lwt.return ()
      end else begin
        let this_time = min buffer_size remaining in
        let buf = Cstruct.create this_time in
        for i = 0 to Cstruct.len buf - 1 do
          Cstruct.set_uint8 buf i (Random.int 255)
        done;
        let s = Cstruct.to_string buf in
        Printf.fprintf stderr "about to write\n%!";
        Hv.write flow buf
        >>= function
        | Ok () ->
          Printf.fprintf stderr "write(%d)\n%!" n;
          Sha256.update_string writer_sha s;
          loop (remaining - this_time)
        | Error _ ->
          Printf.fprintf stderr "write failed\n%!";
          failwith "Flow write error"
      end in
    loop n in
  (* let n = Random.int (1024 * 1024) in *)
  let n_written = 1024 in
  Printf.fprintf stderr "starting threads\n%!";
  let writer_t = writer n_written in
  let reader_t = reader 0 in
  Lwt.join [ (reader_t >>= fun _ -> Lwt.return ()); writer_t ]
  >>= fun () ->
  reader_t
  >>= fun n_read ->
  let read_sha = Sha256.(to_hex @@ finalize reader_sha) in
  let write_sha = Sha256.(to_hex @@ finalize writer_sha) in
  Printf.printf "reader = %s\nwriter = %s\n" read_sha write_sha;
  if read_sha <> write_sha
  then failwith (Printf.sprintf "Checksum does not match. Written %d (%s), read %d (%s)" n_written write_sha n_read read_sha);
  Lwt.return_unit

let one vmid =
  connect vmid default_serviceid
  >>= fun flow ->
  Printf.fprintf stderr "Connected\n%!";
  send_receive_verify flow
  >>= fun () ->
  Printf.fprintf stderr "Closing\n%!";
  Hv.close flow

let client vmid p =
  let rec threads n =
    if n = 0 then [] else (one vmid) :: (threads (n-1)) in
  Lwt.join (threads p)

let main c p =
  match c with
  | None ->
    Printf.fprintf stderr "Please provide a -c hvsock://<vmid> argument\n";
    exit 1
  | Some uri ->
    let u = Uri.of_string uri in
    begin match Uri.scheme u, Uri.host u with
    | Some "hvsock", Some vmid ->
      Lwt_main.run (client (Hvsock.Id vmid) p);
      `Ok ()
    | _, _ ->
      Printf.fprintf stderr "Please provide a -c hvsock://<vmid> argument\n";
      exit 1
    end

(* Note we try to keep the command-line compatible with the Go
   virtsock/cmd/sock_stress *)

let c =
  Arg.(value & opt (some string) None & info ~docv:"CLIENT" ~doc:"Run as a client" [ "c" ])

let p =
  Arg.(value & opt int 1 & info ~docv:"PARALLEL" ~doc:"Threads to run in parallel" [ "p" ])

let cmd =
  let doc = "Test AF_HVSOCK connections" in
  let man = [
    `S "DESCRIPTION";
    `P "Establish a connection to an echo server via a Hyper-V socket, send random data, receive a response and check the data is the same. ";
    `S "EXAMPLES";
    `P "To connect to a service in a remote partition:";
    `P "sock_stress -c hvsock://<vmid>";
  ] in
  Term.(const main $ c $ p),
  Term.info "sock_stress" ~version:"%0.1" ~doc ~exits:Term.default_exits ~man

let () =
let (_: Lwt_unix.signal_handler_id) = Lwt_unix.on_signal Sys.sigint
  (fun (_: int) ->
    Lwt.wakeup_later sigint_u ();
  ) in
  Term.exit @@ Term.eval cmd
