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

open Hvsock
open Lwt.Infix

(* Workarounds:
   1. select() is not implemented so we can't use regular non-blocking I/O
      i.e. we must use first class threads. Note that Lwt_preemptive calls
      can block if the thread pool fills up. We create our own threads per
      connection to avoid this.
   2. connect() blocks forever instead of failing with ECONNREFUSED if the
      server is down when the client calls connect. We declare a 1s timeout
      and raise ECONNREFUSED ourselves.
*)

module type MAIN = sig
  val run_in_main: (unit -> 'a Lwt.t) -> 'a
end

module type HVSOCK = sig
  type t
  val create: unit -> t
  val bind: t -> sockaddr -> unit
  val listen: t -> int -> unit
  val accept: t -> (t * sockaddr) Lwt.t
  val connect: t -> sockaddr -> unit Lwt.t
  val read: t -> Cstruct.t -> int Lwt.t
  val write: t -> Cstruct.t -> int Lwt.t
  val close: t -> unit Lwt.t
end

type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external stub_ba_recv: Unix.file_descr -> buffer -> int -> int -> int = "stub_hvsock_ba_recv"
external stub_ba_send: Unix.file_descr -> buffer -> int -> int -> int = "stub_hvsock_ba_send"

let cstruct_read fd b = stub_ba_recv fd b.Cstruct.buffer b.Cstruct.off b.Cstruct.len
let cstruct_write fd b = stub_ba_send fd b.Cstruct.buffer b.Cstruct.off b.Cstruct.len

type ('a, 'b) r =
  | Ok of 'a
  | Error of 'b

type result = (int, exn) r

type request = {
  file_descr: Unix.file_descr;
  buf: Cstruct.t;
  result: int Lwt.u;
}

module Make(Time: V1_LWT.TIME)(Main: MAIN) = struct
type t = {
  mutable fd: Unix.file_descr option;
  push_read_request: request option -> unit;
  push_write_request: request option -> unit;
}

let rec handle_requests blocking_op requests =
  match Main.run_in_main (fun () ->
    Lwt.catch
      (fun () -> Lwt_stream.next requests >>= fun x -> Lwt.return (Some x))
      (fun _ -> Lwt.return None)
    ) with
  | None -> ()
  | Some r ->
    let result =
      try
        Ok (blocking_op r.file_descr r.buf)
      with
      | e -> Error e in
    Main.run_in_main (fun () ->
      match result with
      | Ok x -> Lwt.wakeup_later r.result x; Lwt.return_unit
      | Error e -> Lwt.wakeup_later_exn r.result e; Lwt.return_unit
    );
    handle_requests blocking_op requests

let make fd =
  let read_requests, push_read_request = Lwt_stream.create () in
  let write_requests, push_write_request = Lwt_stream.create () in
  let _reader = Thread.create (handle_requests cstruct_read) read_requests in
  let _writer = Thread.create (handle_requests cstruct_write) write_requests in
  { fd = Some fd; push_read_request; push_write_request; }

let create () = make (create ())

let detach f x =
  let stream, push = Lwt_stream.create () in
  let return x = Main.run_in_main (fun () ->
    push (Some x);
    Lwt.return_unit
  ) in
  let _thread = Thread.create (fun () ->
    try
      return (`Ok (f x))
    with e ->
      return (`Error e)
  ) () in
  Lwt_stream.next stream
  >>= function
  | `Ok x -> Lwt.return x
  | `Error e -> Lwt.fail e

let close t = match t with
  | { fd = None } -> Lwt.return ()
  | { fd = Some x } ->
    t.fd <- None;
    t.push_read_request None;
    t.push_write_request None;
    detach Unix.close x

let bind t addr = match t with
  | { fd = None } -> raise (Unix.Unix_error(Unix.EBADF, "bind", ""))
  | { fd = Some x } -> bind x addr

let listen t n = match t with
  | { fd = None } -> raise (Unix.Unix_error(Unix.EBADF, "bind", ""))
  | { fd = Some x } -> Unix.listen x n

let accept = function
  | { fd = None } -> Lwt.fail (Unix.Unix_error(Unix.EBADF, "accept", ""))
  | { fd = Some x } ->
    detach accept x
    >>= fun (y, addr) ->
    Lwt.return (make y, addr)

let connect t addr = match t with
  | { fd = None } -> Lwt.fail (Unix.Unix_error(Unix.EBADF, "connect", ""))
  | { fd = Some x } ->
    (* If the server isn't listening then connect blocks forever.
       Declare a timeout and a failed connect results in a closed fd
       and an ECONNREFUSED *)
    let connect_t =
      detach (connect x) addr
      >>= fun () ->
      Lwt.return true in
    let timeout_t =
      Time.sleep_ns (Duration.of_sec 1)
      >>= fun () ->
      Lwt.return false in
    Lwt.choose [ connect_t; timeout_t ]
    >>= fun ok ->
    if not ok then begin
      close t
      >>= fun () ->
      Lwt.fail (Unix.Unix_error(Unix.ECONNREFUSED, "connect", ""))
    end else Lwt.return_unit

let read t buf = match t with
  | { fd = None } -> Lwt.fail (Unix.Unix_error(Unix.EBADF, "read", ""))
  | { fd = Some file_descr; push_read_request } ->
    let t, result = Lwt.task () in
    let request = { file_descr; buf; result } in
    push_read_request (Some request);
    t

let write t buf = match t with
  | { fd = None } -> Lwt.fail (Unix.Unix_error(Unix.EBADF, "write", ""))
  | { fd = Some file_descr; push_write_request } ->
    let t, result = Lwt.task () in
    let request = { file_descr; buf; result } in
    push_write_request (Some request);
    t
end
