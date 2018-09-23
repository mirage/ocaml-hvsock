
module type S = sig
  (** A low-level socket interface, common to both Windows and Linux kernels *)

  type sockaddr
  (** A socket address *)

  val string_of_sockaddr: sockaddr -> string

  val create: unit -> Unix.file_descr
  (** [create ()] creates an unbound socket *)

  val bind: Unix.file_descr -> sockaddr -> unit
  (** [bind socket sockaddr] binds [socket] to [sockaddr] *)

  val accept: Unix.file_descr -> Unix.file_descr * sockaddr
  (** [accept fd] accepts a single connection *)

  val connect: ?timeout_ms:int -> Unix.file_descr -> sockaddr -> unit
  (** [connect ?timeout_ms fd sockaddr] connects to a remote socket.
      On Windows the raw connect call can block forever if the server is not
      running when the call is executed (even if the server starts up afterwards)
      there is a default timeout of 300ms. On timeout this will raise
      [Unix_error(Unix.ETIMEDOUT)] *)
end