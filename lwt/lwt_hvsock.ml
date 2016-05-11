open Hvsock

(* On Hyper-V select() is not implemented so we can't use regular non-blocking
   I/O *)

type t = Unix.file_descr

let create = create

let bind = bind

let accept t = Lwt_preemptive.detach accept t

let connect t addr = Lwt_preemptive.detach (connect t) addr

let read t buf off len = Lwt_preemptive.detach (Unix.read t buf off) len

let write t buf off len = Lwt_preemptive.detach (Unix.write t buf off) len

let close t = Lwt_preemptive.detach Unix.close t
