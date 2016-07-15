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

type 'a io = 'a

type vmid =
  | Wildcard
  | Children
  | Loopback
  | Parent
  | Id of string

let string_of_vmid = function
  | Wildcard -> "*"
  | Children -> "children"
  | Loopback -> "loopback"
  | Parent -> "parent"
  | Id x -> x

type sockaddr = {
  vmid: vmid;
  serviceid: string;
}

type t = {
  mutable fd: Unix.file_descr option;
}

external get_wildcard: unit -> string = "stub_hvsock_wildcard"
let wildcard = get_wildcard ()

external get_children: unit -> string = "stub_hvsock_children"
let children = get_children ()

external get_loopback: unit -> string = "stub_hvsock_loopback"
let loopback = get_loopback ()

external get_parent: unit -> string = "stub_hvsock_parent"
let parent = get_parent ()

let string_of_vmid = function
  | Wildcard -> wildcard
  | Children -> children
  | Loopback -> loopback
  | Parent   -> parent
  | Id x     -> x

let vmid_of_string x =
  if x = wildcard then Wildcard
  else if x = children then Children
  else if x = loopback then Loopback
  else if x = parent then Parent
  else Id x

external do_socket: unit -> Unix.file_descr = "stub_hvsock_socket"

external do_bind: Unix.file_descr -> string -> string -> unit = "stub_hvsock_bind"

external do_accept: Unix.file_descr -> Unix.file_descr * string * string = "stub_hvsock_accept"

external do_connect: Unix.file_descr -> string -> string -> unit = "stub_hvsock_connect"

let create () = { fd = Some (do_socket ()) }

let with_fd fn_name fn_arg t f = match t with
  | { fd = None } -> raise (Unix.Unix_error(Unix.EBADF, fn_name, fn_arg))
  | { fd = Some fd } -> f fd

let bind t { vmid; serviceid } =
  let vmid' = string_of_vmid vmid in
  with_fd "bind" (vmid' ^ ":" ^ serviceid) t
    (fun fd ->
      do_bind fd vmid' serviceid
    )

let accept t =
  with_fd "accept" "" t
    (fun fd ->
      let c, vmid, serviceid = do_accept fd in
      let vmid = vmid_of_string vmid in
      { fd = Some fd }, { vmid; serviceid }
    )

let connect t { vmid; serviceid } =
  let vmid' = string_of_vmid vmid in
  with_fd "connect" (vmid' ^ ":" ^ serviceid) t
    (fun fd ->
      do_connect fd vmid' serviceid
    )

let close t = match t.fd with
  | None ->
    ()
  | Some fd ->
    t.fd <- None;
    Unix.close fd

let listen t backlog =
  with_fd "listen" (string_of_int backlog) t
    (fun fd ->
      Unix.listen fd backlog
    )

type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external stub_ba_recv: Unix.file_descr -> buffer -> int -> int -> int = "stub_hvsock_ba_recv"
external stub_ba_send: Unix.file_descr -> buffer -> int -> int -> int = "stub_hvsock_ba_send"

let read t b =
  with_fd "read" (string_of_int (Cstruct.len b)) t
    (fun fd ->
      stub_ba_recv fd b.Cstruct.buffer b.Cstruct.off b.Cstruct.len
    )

let write t b =
  with_fd "write" (string_of_int (Cstruct.len b)) t
    (fun fd ->
      stub_ba_send fd b.Cstruct.buffer b.Cstruct.off b.Cstruct.len
    )
