(*
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

open Lwt.Infix

(* Wraps a BLOCKING_SOCKET implementation, offloading blocking operations to
   OCaml threads and synchronising with the Lwt world using run_in_main. *)

module Make(Time: V1_LWT.TIME)(Main: Lwt_hvsock_s.MAIN)(Socket: Hvsock_s.BLOCKING_SOCKET) = struct

  module Background = Lwt_background_threads.Make(Main)

  type t = {
    fd: Socket.t;
    read: (Cstruct.t, int) Background.fn;
    write: (Cstruct.t, int) Background.fn;
  }

  type sockaddr = Socket.sockaddr

  let of_file_descr fd =
    let read = Background.make_fn (Socket.read fd) in
    let write = Background.make_fn (Socket.write fd) in
    { fd; read; write }

  let create () = of_file_descr (Socket.create ())

  let read t buf = Background.apply t.read buf
  let write t buf = Background.apply t.write buf

  let close t =
    Socket.close t.fd;
    Background.shutdown t.read
    >>= fun () ->
    Background.shutdown t.write
    >>= fun () ->
    Lwt.return ()

  let bind t addr = Socket.bind t.fd addr

  let listen t n = Socket.listen t.fd n

  let accept t =
    Background.detach Socket.accept t.fd
    >>= fun (y, addr) ->
    Lwt.return (of_file_descr y, addr)

  let connect t addr =
    let connect_t =
      Background.detach (Socket.connect t.fd) addr
      >>= fun () ->
      Lwt.return true in
    let timeout_t =
      Time.sleep 1.
      >>= fun () ->
      Lwt.return false in
    Lwt.choose [ connect_t; timeout_t ]
    >>= fun ok ->
    if not ok then begin
      Socket.close t.fd;
      Lwt.fail (Unix.Unix_error(Unix.ECONNREFUSED, "connect", ""))
    end else Lwt.return_unit

  let read t buf = Background.apply t.read buf

  let write t buf = Background.apply t.write buf
end
