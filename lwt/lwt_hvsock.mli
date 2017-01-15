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

module type MAIN = sig
  val run_in_main: (unit -> 'a Lwt.t) -> 'a
  (** Run the given closure in the main thread *)
end

module type HVSOCK = sig
  type t
  (** A Hyper-V socket *)

  val create: unit -> t
  (** [create ()] creates an unbound AF_HVSOCK socket *)

  val bind: t -> sockaddr -> unit
  (** [bind t sockaddr] binds [socket] to [sockaddr] *)

  val listen: t -> int -> unit
  (** [listen t queue] *)

  val accept: t -> (t * sockaddr) Lwt.t
  (** [accept t] accepts a single connection *)

  val connect: ?timeout_ms:int -> t -> sockaddr -> unit Lwt.t
  (** [connect ?timeout_ms t sockaddr] connects to a remote partition *)

  val read: t -> Cstruct.t -> int Lwt.t
  (** [read t buf] reads as many bytes as available into [buf] returning
      the number of bytes read. *)

  val write: t -> Cstruct.t -> int Lwt.t
  (** [write t buf] writes as many bytes from [buf] to [t] as will currently
      fit inside [t]'s internal buffer, and return the number of bytes
      written *)

  val close: t -> unit Lwt.t
  (** [close t] closes a socket *)
end

module Make(Time: Mirage_time_lwt.S)(Main: MAIN): HVSOCK
(** Create an HVSOCK implementation given the ability to sleep and the ability
    to run code in the main Lwt thread *)
