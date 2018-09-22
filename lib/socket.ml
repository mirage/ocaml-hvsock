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

type platform =
  | Windows
  | Linux
  | Unsupported of string

let finally f g =
  try
    let result = f () in
    g ();
    result
  with e ->
    g ();
    raise e

let startswith prefix line =
  let prefix_length = String.length prefix
  and line_length = String.length line in
  prefix_length <= line_length && (String.sub line 0 prefix_length = prefix)

let platform = match Sys.os_type with
  | "Win32" -> Windows
  | "Unix" ->
    begin
      try
        let f = open_in "/proc/version" in
        finally
          (fun () ->
            let line = input_line f in
            if startswith line "Linux"
            then Linux
            else Unsupported line
          ) (fun () -> close_in f)
      with _ ->
        Unsupported "Unknown Unix"
    end
  | x -> Unsupported x    

exception Unsupported_platform of string

let create () = match platform with
  | Windows -> Af_hyperv.create ()
  | Linux -> Af_vsock.create ()
  | Unsupported x -> raise (Unsupported_platform x)

type port =
  | Port of Af_vsock.port
  | Serviceid of Af_hyperv.serviceid

let serviceid_of_port = function
  | Port x -> Printf.sprintf "%08lx-FACB-11E6-BD58-64006A7986D3" x
  | Serviceid x -> x

let port_of_port = function
  | Port x -> x
  | Serviceid x ->
    try
        Scanf.sscanf x "%08lx-FACB-11E6-BD58-64006A7986D3" (fun x -> x)
    with _ ->
        raise (Unsupported_platform "Generic service IDs are only supported on Windows")

type peer =
  | Any
  | Host
  | CID of Af_vsock.cid
  | VMID of Af_hyperv.vmid

type sockaddr = peer * port
let vmid_of_peer = function
  | Any -> Af_hyperv.Wildcard
  | Host -> Af_hyperv.Parent
  | CID _ -> raise (Unsupported_platform "CIDs are only supported on Linux")
  | VMID x -> x

let cid_of_peer = function
  | Any -> Af_vsock.Any
  | Host -> Af_vsock.Host
  | CID x -> x
  | VMID _ -> raise (Unsupported_platform "VMIDs are not supported on Linux")

let bind fd (peer, port) = match platform with
  | Windows -> Af_hyperv.bind fd { Af_hyperv.vmid = vmid_of_peer peer; serviceid = serviceid_of_port port }
  | Linux   -> Af_vsock.bind fd { Af_vsock.cid = cid_of_peer peer; port = port_of_port port }
  | Unsupported x -> raise (Unsupported_platform x)

let accept fd = match platform with
  | Windows ->
    let fd', { Af_hyperv.vmid; serviceid } = Af_hyperv.accept fd in
    fd', (VMID vmid, Serviceid serviceid)
  | Linux ->
    let fd', { Af_vsock.cid; port } = Af_vsock.accept fd in
    fd', (CID cid, Port port)
  | Unsupported x -> raise (Unsupported_platform x)

let connect ?timeout_ms fd sockaddr = match platform, sockaddr with
  | Windows, (peer, port) ->
    let vmid = vmid_of_peer peer in
    let serviceid = serviceid_of_port port in
    Af_hyperv.connect ?timeout_ms fd { Af_hyperv.vmid; serviceid }
  | Linux, (peer, port) ->
    let cid = cid_of_peer peer in
    let port = port_of_port port in
    Af_vsock.connect ?timeout_ms fd { Af_vsock.cid; port }
  | Unsupported x, _ -> raise (Unsupported_platform x)