
open Hvsock

type t
(** A Hyper-V socket *)

val create: unit -> t
(** [create ()] creates an unbound AF_HVSOCK socket *)

val bind: t -> sockaddr -> unit
(** [bind t sockaddr] binds [socket] to [sockaddr] *)

val accept: t -> (t * sockaddr) Lwt.t
(** [accept t] accepts a single connection *)

val connect: t -> sockaddr -> unit Lwt.t
(** [connect t sockaddr] connects to a remote partition *)

val read: t -> bytes -> int -> int -> int Lwt.t
(** [read t buf offset len] reads up to [len] bytes from [t] into [buf]
    starting at offset [offset] *)

val write: t -> bytes -> int -> int -> int Lwt.t
(** [write t buf offset len] writes up to [len] bytes from [t] into [buf]
    starting at offset [offset] *)

val close: t -> unit Lwt.t
(** [close t] closes a socket *)
