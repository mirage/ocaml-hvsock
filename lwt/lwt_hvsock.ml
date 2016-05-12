open Hvsock
open Lwt.Infix

(* Workarounds:
   1. select() is not implemented so we can't use regular non-blocking I/O
      i.e. we must use Lwt_preemptive
   2. connect() blocks forever instead of failing with ECONNREFUSED if the
      server is down when the client calls connect. We declare a 1s timeout
      and raise ECONNREFUSED ourselves.
*)

type t = {
  mutable fd: Unix.file_descr option
}

let create () = { fd = Some (create ()) }

let detach f x =
  let stream, push = Lwt_stream.create () in
  let return x = Lwt_preemptive.run_in_main (fun () ->
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
    detach Unix.close x

let bind t addr = match t with
  | { fd = None } -> raise (Unix.Unix_error(Unix.EBADF, "bind", ""))
  | { fd = Some x } -> bind x addr

let accept = function
  | { fd = None } -> Lwt.fail (Unix.Unix_error(Unix.EBADF, "accept", ""))
  | { fd = Some x } ->
    detach accept x
    >>= fun (y, addr) ->
    Lwt.return ({ fd = Some y }, addr)

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
      Lwt_unix.sleep 1.
      >>= fun () ->
      Lwt.return false in
    Lwt.choose [ connect_t; timeout_t ]
    >>= fun ok ->
    if not ok then begin
      close t
      >>= fun () ->
      Lwt.fail (Unix.Unix_error(Unix.ECONNREFUSED, "connect", ""))
    end else Lwt.return_unit

let read t buf off len = match t with
  | { fd = None } -> Lwt.fail (Unix.Unix_error(Unix.EBADF, "read", ""))
  | { fd = Some fd } -> detach (Unix.read fd buf off) len

let write t buf off len = match t with
  | { fd = None } -> Lwt.fail (Unix.Unix_error(Unix.EBADF, "write", ""))
  | { fd = Some fd } -> detach (Unix.write fd buf off) len
