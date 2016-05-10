
type vmid =
  | Wildcard
  | Children
  | Loopback
  | Parent
  | Id of string

type sockaddr = {
  vmid: vmid;
  serviceid: string;
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

external do_connect: Unix.file_descr -> string -> string -> Unix.file_descr = "stub_hvsock_connect"

let create = do_socket

let bind fd { vmid; serviceid } = do_bind fd (string_of_vmid vmid) serviceid

let accept fd =
  let c, vmid, serviceid = do_accept fd in
  let vmid = vmid_of_string vmid in
  fd, { vmid; serviceid }

let connect fd { vmid; serviceid } = do_connect fd (string_of_vmid vmid) serviceid
