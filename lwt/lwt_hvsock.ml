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
      i.e. we must use first class threads. Note that Lwt_Fn calls
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
  val connect: ?timeout_ms:int -> t -> sockaddr -> unit Lwt.t
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

type op = {
  file_descr: Unix.file_descr;
  buf: Cstruct.t;
}

type ('request, 'response) fn = {
  request: 'request;
  response: 'response Lwt.u;
}

module type FN = sig
  type ('request, 'response) t

  val create: ('request -> 'response) -> ('request, 'response) t
  val destroy: ('request, 'response) t -> unit

  val fn: ('request, 'response) t -> 'request -> 'response Lwt.t
end

module Run_with_detach = struct
  type ('request, 'response) t = 'request -> 'response
  let create f = f
  let destroy _ = ()
  let fn = Lwt_preemptive.detach
end

module Run_in_thread(Main: MAIN) = struct
  type ('request, 'response) t = {
    call: ('request, 'response) fn option -> unit;
  }

  let rec handle_requests blocking_op calls =
    match Main.run_in_main (fun () ->
      Lwt.catch
        (fun () -> Lwt_stream.next calls >>= fun x -> Lwt.return (Some x))
        (fun _ -> Lwt.return None)
      ) with
    | None -> ()
    | Some r ->
      let response =
        try
          Ok (blocking_op r.request)
        with
        | e -> Error e in
      Main.run_in_main (fun () ->
        match response with
        | Ok x -> Lwt.wakeup_later r.response x; Lwt.return_unit
        | Error e -> Lwt.wakeup_later_exn r.response e; Lwt.return_unit
      );
      handle_requests blocking_op calls

  let create blocking_op =
    let calls, call = Lwt_stream.create () in
    let _th = Thread.create (handle_requests blocking_op) calls in
    { call }

  let fn t request =
    let thread, response = Lwt.task () in
    let call = { request; response } in
    t.call (Some call);
    thread

  let destroy t = t.call None
end

module Make(Time: V1_LWT.TIME)(Main: MAIN) = struct
module Fn = Run_in_thread(Main)

type t = {
  mutable fd: Unix.file_descr option;
  read: (op, int) Fn.t;
  write: (op, int) Fn.t;
}

let make fd =
  let read = Fn.create (fun op -> cstruct_read op.file_descr op.buf) in
  let write = Fn.create (fun op -> cstruct_write op.file_descr op.buf) in
  { fd = Some fd; read; write; }

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
    Fn.destroy t.read;
    Fn.destroy t.write;
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

let connect ?timeout_ms t addr = match t with
  | { fd = None } -> Lwt.fail (Unix.Unix_error(Unix.EBADF, "connect", ""))
  | { fd = Some x } ->
    detach (connect ?timeout_ms x) addr

let read t buf = match t with
  | { fd = None } -> Lwt.fail (Unix.Unix_error(Unix.EBADF, "read", ""))
  | { fd = Some file_descr; read } ->
    Fn.fn read { file_descr; buf }

let write t buf = match t with
  | { fd = None } -> Lwt.fail (Unix.Unix_error(Unix.EBADF, "write", ""))
  | { fd = Some file_descr; write } ->
    Fn.fn write { file_descr; buf }
end
