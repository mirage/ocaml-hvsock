(*
 * Copyright (C) 2015 Docker Inc
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


(* On Windows 10 build 10586 larger maxMsgSize values work, but on
   newer builds it fails. It is unclear what the cause is... *)

let maxMsgSize = 4 * 1024

module Message = struct
  type t =
    | ShutdownRead
    | ShutdownWrite
    | Close
    | Data of int

  let sizeof = 4

  let marshal x =
    let results = Bytes.create sizeof in
    EndianString.LittleEndian.set_int32 results 0 (match x with
      | ShutdownRead  -> 0xdeadbeefl
      | ShutdownWrite -> 0xbeefdeadl
      | Close         -> 0xdeaddeadl
      | Data len      -> Int32.of_int len
    );
    results
  let unmarshal x =
    match EndianString.LittleEndian.get_int32 x 0 with
      | 0xdeadbeefl -> ShutdownRead
      | 0xbeefdeadl -> ShutdownWrite
      | 0xdeaddeadl -> Close
      | other       -> Data (Int32.to_int other)
end

open Lwt.Infix

type 'a io = 'a Lwt.t

type buffer = Cstruct.t

type error = Unix.error

let error_message = Unix.error_message

type flow = {
  fd: Lwt_hvsock.t;
  m: Lwt_mutex.t;
  read_buffer: Bytes.t;
  mutable leftover: (int * Bytes.t) option;
  mutable closed: bool;
  mutable read_closed: bool;
  mutable write_closed: bool;
}

let connect fd =
  let closed = false in
  let read_closed = false in
  let write_closed = false in
  let m = Lwt_mutex.create () in
  let read_buffer = Bytes.make maxMsgSize '\000' in
  let leftover = None in
  { fd; m; read_buffer; leftover; closed; read_closed; write_closed }

(* Write a whole string to the fd, without any encapsulation *)
let really_write fd buffer ofs len =
  let rec loop ofs len =
    if len = 0
    then Lwt.return (`Ok ())
    else
      Lwt_hvsock.write fd buffer ofs len
      >>= function
      | 0 -> Lwt.return (`Eof)
      | n ->
        loop (ofs + n) (len - n) in
  Lwt.catch
    (fun () ->
      loop ofs len
    ) (function
      | Unix.Unix_error(Unix.EPIPE, _, _) -> Lwt.return `Eof
      | e -> Lwt.fail e
    )

(* Read a whole string from the fd, without any encapsulation *)
let really_read fd buffer ofs len =
  let rec loop ofs len =
    if len = 0
    then Lwt.return (`Ok ())
    else
      Lwt_hvsock.read fd buffer ofs len
      >>= function
      | 0 -> Lwt.return (`Eof)
      | n ->
        loop (ofs + n) (len - n) in
  Lwt.catch
    (fun () ->
      loop ofs len
    ) (function
      | Unix.Unix_error(Unix.EPIPE, _, _) -> Lwt.return `Eof
      | e -> Lwt.fail e
    )

let shutdown_write flow =
  if flow.write_closed
  then Lwt.return ()
  else begin
    flow.write_closed <- true;
    Lwt_mutex.with_lock flow.m
      (fun () ->
        really_write flow.fd Message.(marshal ShutdownWrite) 0 Message.sizeof
        >>= function
        | `Eof -> Lwt.fail End_of_file
        | `Ok () -> Lwt.return ()
      )
  end

let shutdown_read flow =
  if flow.read_closed
  then Lwt.return ()
  else begin
    flow.read_closed <- true;
    Lwt_mutex.with_lock flow.m
      (fun () ->
        really_write flow.fd Message.(marshal ShutdownRead) 0 Message.sizeof
        >>= function
        | `Eof -> Lwt.fail End_of_file
        | `Ok () -> Lwt.return ()
      )
  end

let close flow =
  match flow.closed with
  | false ->
    flow.closed <- true;
    flow.read_closed <- true;
    flow.write_closed <- true;
    Lwt_mutex.with_lock flow.m
      (fun () ->
        really_write flow.fd Message.(marshal Close) 0 Message.sizeof
        >>= function
        | `Eof -> Lwt.return ()
        | `Ok () ->
          let header = Bytes.create Message.sizeof in
          let payload = Bytes.create maxMsgSize in
          let rec wait_for_close () =
            really_read flow.fd header 0 Message.sizeof
            >>= function
            | `Eof -> Lwt.return ()
            | `Ok () ->
              match Message.unmarshal header with
              | Message.Close ->
                Lwt.return ()
              | Message.ShutdownRead
              | Message.ShutdownWrite ->
                wait_for_close ()
              | Message.Data n ->
                really_read flow.fd payload 0 n
                >>= function
                | `Eof -> Lwt.return ()
                | `Ok () -> wait_for_close () in
          wait_for_close ()
      )
    >>= fun () ->
    Lwt_hvsock.close flow.fd
  | true ->
    Lwt.return ()

(* Write a whole buffer to the fd, in chunks according to the maximum message
   size *)
let write flow buf =
  if flow.closed || flow.write_closed then Lwt.return `Eof
  else
    let buffer = Cstruct.to_string buf in
    let rec loop ofs len =
      if len = 0
      then Lwt.return (`Ok ())
      else
        let this_batch = min len maxMsgSize in
        Lwt_mutex.with_lock flow.m
          (fun () ->
            really_write flow.fd Message.(marshal (Data this_batch)) 0 Message.sizeof
            >>= function
            | `Eof -> Lwt.return `Eof
            | `Ok () ->
              really_write flow.fd buffer ofs this_batch
          )
        >>= function
        | `Eof -> Lwt.return `Eof
        | `Ok () ->
          loop (ofs + this_batch) (len - this_batch) in
    loop 0 (Bytes.length buffer)

let read_next_chunk flow =
  if flow.closed || flow.read_closed then Lwt.return `Eof
  else
    let rec loop () =
      let header = Bytes.create Message.sizeof in
      really_read flow.fd header 0 Message.sizeof
      >>= function
      | `Eof -> Lwt.return `Eof
      | `Ok () ->
        match Message.unmarshal header with
        | Message.ShutdownWrite ->
          flow.read_closed <- true;
          Lwt.return `Eof
        | Message.Close ->
          close flow
          >>= fun () ->
          Lwt.return `Eof
        | Message.ShutdownRead ->
          flow.write_closed <- true;
          loop ()
        | Message.Data n ->
          let payload = Bytes.create n in
          really_read flow.fd payload 0 n
          >>= function
          | `Eof -> Lwt.return `Eof
          | `Ok () ->
            Lwt.return (`Ok payload) in
    loop ()

let read flow =
  match flow.leftover with
  | Some (consumed_last_time, payload) ->
    let to_consume = Bytes.length payload - consumed_last_time in
    let result = Cstruct.create to_consume in
    Cstruct.blit_from_string payload consumed_last_time result 0 to_consume;
    flow.leftover <- None;
    Lwt.return (`Ok result)
  | None ->
    Lwt_mutex.with_lock flow.m
      (fun () ->
        read_next_chunk flow
        >>= function
        | `Eof -> Lwt.return `Eof
        | `Ok payload ->
          let result = Cstruct.create (Bytes.length payload) in
          Cstruct.blit_from_string payload 0 result 0 (Bytes.length payload);
          Lwt.return (`Ok result)
      )

let rec read_into flow buf =
  if Cstruct.len buf = 0
  then Lwt.return (`Ok ())
  else match flow.leftover with
  | None ->
    begin Lwt_mutex.with_lock flow.m
      (fun () ->
        read_next_chunk flow
        >>= function
        | `Eof -> Lwt.return `Eof
        | `Ok payload ->
          let to_consume = min (Cstruct.len buf) (Bytes.length payload) in
          Cstruct.blit_from_string payload 0 buf 0 to_consume;
          let to_leave = Bytes.length payload - to_consume in
          if to_leave > 0 then flow.leftover <- Some (to_consume, payload);
          Lwt.return (`Ok (Cstruct.shift buf to_consume))
      ) >>= function
      | `Eof -> Lwt.return `Eof
      | `Ok buf -> read_into flow buf
    end
  | Some (consumed_last_time, payload) ->
    let to_consume = min (Cstruct.len buf) (Bytes.length payload - consumed_last_time) in
    Cstruct.blit_from_string payload consumed_last_time buf 0 to_consume;
    let to_leave = Bytes.length payload - consumed_last_time - to_consume in
    flow.leftover <- if to_leave = 0 then None else Some (consumed_last_time + to_consume, payload);
    read_into flow (Cstruct.shift buf to_consume)

let writev flow bufs =
  let rec loop = function
    | [] -> Lwt.return (`Ok ())
    | x :: xs ->
      write flow x
      >>= function
      | `Eof -> Lwt.return `Eof
      | `Ok () -> loop xs in
  loop bufs
