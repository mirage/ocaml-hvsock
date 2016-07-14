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

module type SOCKET = sig
  (** A simplified representation of BSD sockets *)

  type 'a io
  (** A value which requires blocking for I/O before resulting in ['a] has
      a type ['a io] *)

  type t
  (** A BSD socket *)

  val create: unit -> t
  (** [create ()] creates an unbound socket *)

  type sockaddr
  (** An address for a connected socket *)

  val bind: t -> sockaddr -> unit
  (** [bind socket sockaddr] binds [socket] to [sockaddr] *)

  val listen: t -> int -> unit
  (** [listen socket backlog] sets the socket into the listening state, with a
      connection backlog of [backlog] *)

  val accept: t -> (t * sockaddr) io
  (** [accept fd] accepts a single connection *)

  val connect: t -> sockaddr -> unit io
  (** [connect fd sockaddr] connects to a remote partition. Note this
      has been observed to block forever if the server is not running
      when this call is executed, even if the server starts up afterwards.
      The workaround seems to be to close the fd and try again. *)

  val close: t -> unit io
  (** [close t] closes a socket. *)

  val write: t -> Cstruct.t -> int io
  (** [write t buf] writes data from [buf] to [t], returning the number of bytes
      successfully written. *)

  val read: t -> Cstruct.t -> int io
  (** [read t buf] reads data into [buf] from [t], returning the number of bytes
      successfully read. *)
end

module type BLOCKING_SOCKET = sig
  (** A representation of BSD sockets which behaves like the Unix module:
      calls block without this being indicated in the type *)
  include SOCKET
    with type 'a io = 'a
end
