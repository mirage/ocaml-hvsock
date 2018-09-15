(*
 * Copyright (C) 2018 Docker Inc
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

type port = int32

type cid =
  | Any         (** Any address for binding *)
  | Hypervisor  (** Reserved for services built into the hypervisor *)
  | Host        (** Well-known address of the host *)
  | Id of int32 (** A specific VM id *)
(** A Context Identifier (CID) identifies the source or destination of a connection *)

val string_of_cid: cid -> string

val local: unit -> cid
(** [local ()] returns the CID of the local VM *)

type sockaddr = {
  cid: cid;
  port: port;
}
(** An AF_VSOCK socket address *)

val string_of_sockaddr: sockaddr -> string

val create: unit -> Unix.file_descr
(** [create ()] creates an unbound AF_VSOCK socket *)

val bind: Unix.file_descr -> sockaddr -> unit
(** [bind socket sockaddr] binds [socket] to [sockaddr] *)

val accept: Unix.file_descr -> Unix.file_descr * sockaddr
(** [accept fd] accepts a single connection *)

val connect: Unix.file_descr -> sockaddr -> unit
(** [connect fd sockaddr] connects to a remote context. *)
