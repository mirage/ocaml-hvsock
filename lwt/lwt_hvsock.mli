
open Hvsock

val create: unit -> Lwt_unix.file_descr
(** [create ()] creates an unbound AF_HVSOCK socket *)

val bind: Lwt_unix.file_descr -> sockaddr -> unit
(** [bind socket sockaddr] binds [socket] to [sockaddr] *)

val accept: Lwt_unix.file_descr -> (Lwt_unix.file_descr * sockaddr) Lwt.t
(** [accept fd] accepts a single connection *)

val connect: Lwt_unix.file_descr -> sockaddr -> unit Lwt.t
(** [connect fd sockaddr] connects to a remote partition *)
