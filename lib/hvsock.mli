
type vmid =
  | Wildcard      (** Any partition *)
  | Children      (** Any child partition *)
  | Loopback      (** The same partition *)
  | Parent        (** The parent partition *)
  | Id of string  (** A specific VM id *)

val string_of_vmid: vmid -> string

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

val connect: Unix.file_descr -> sockaddr -> unit
(** [connect fd sockaddr] connects to a remote partition. Note this
    has been observed to block forever if the server is not running
    when this call is executed, even if the server starts up afterwards.
    The workaround seems to be to close the fd and try again. *)
