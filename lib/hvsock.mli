
type vmid =
  | Wildcard      (** Any partition *)
  | Children      (** Any child partition *)
  | Loopback      (** The same partition *)
  | Parent        (** The parent partition *)
  | Id of string  (** A specific VM id *)

type sockaddr = {
  vmid: vmid;         (** identifies a partition *)
  serviceid: string;  (** identifies a service *)
}
(** An AF_HVSOCK socket address *)

val create: unit -> Unix.file_descr
(** [create ()] creates an unbound AF_HVSOCK socket *)

val bind: Unix.file_descr -> sockaddr -> unit
(** [bind socket sockaddr] binds [socket] to [sockaddr] *)

val accept: Unix.file_descr -> Unix.file_descr * sockaddr
(** [accept fd] accepts a single connection *)

val connect: Unix.file_descr -> sockaddr -> Unix.file_descr
(** [connect fd sockaddr] connects to a remote partition *)
