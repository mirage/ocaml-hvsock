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

(** An interface for hypervisor sockets which hides as many of the differences
    between Linux and Windows as possible. *)

exception Unsupported_platform of string
(** An operation cannot be performed on this platform *)

type port =
  | Port of Af_vsock.port
  | Serviceid of Af_hyperv.serviceid
(** a port is where a service in a remote VM is bound *)

type peer =
  | Any
  | Host
  | CID of Af_vsock.cid
  | VMID of Af_hyperv.vmid
(** peer describes who we're talking to: either a remote VM or the host itself.
    Note there is no cross-platform way to describe a VM since a AF_VSOCK cid
    is completely different to an AF_HYPERV vmid. *)

type sockaddr = peer * port

include Af_common.S
  with type sockaddr := sockaddr
