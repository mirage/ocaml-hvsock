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

let src =
  let src = Logs.Src.create "flow_lwt_hvsock_shutdown" ~doc:"AF_HYPERV framed messages" in
  Logs.Src.set_level src (Some Logs.Info);
  src

module Log = (val Logs.src_log src : Logs.LOG)

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

  let marshal x rest =
    Cstruct.LE.set_uint32 rest 0 (match x with
      | ShutdownRead  -> 0xdeadbeefl
      | ShutdownWrite -> 0xbeefdeadl
      | Close         -> 0xdeaddeadl
      | Data len      -> Int32.of_int len
    )
  let unmarshal x =
    match Cstruct.LE.get_uint32 x 0 with
      | 0xdeadbeefl -> ShutdownRead
      | 0xbeefdeadl -> ShutdownWrite
      | 0xdeaddeadl -> Close
      | other       -> Data (Int32.to_int other)
end

open Lwt.Infix

module Make(Time: Mirage_time_lwt.S)(Main: Lwt_hvsock.MAIN) = struct

module Hvsock = Lwt_hvsock.Make(Time)(Main)

type 'a io = 'a Lwt.t

type buffer = Cstruct.t

type error = [ `Unix of Unix.error ]
let pp_error = Bos.OS.U.pp_error
type write_error = [ Mirage_flow.write_error | error ]
let pp_write_error ppf = function
|#Mirage_flow.write_error as e -> Mirage_flow.pp_write_error ppf e
|#error as e -> pp_error ppf e

let error_message = Unix.error_message

type flow = {
  fd: Hvsock.t;
  rlock: Lwt_mutex.t;
  wlock: Lwt_mutex.t;
  read_header_buffer: Cstruct.t;
  write_header_buffer: Cstruct.t;
  read_buffer: Bytes.t;
  mutable leftover: Cstruct.t;
  mutable closed: bool;
  mutable read_closed: bool;
  mutable write_closed: bool;
}

let connect fd =
  let closed = false in
  let read_closed = false in
  let write_closed = false in
  let rlock = Lwt_mutex.create () in
  let wlock = Lwt_mutex.create () in
  let read_header_buffer = Cstruct.create Message.sizeof in
  let write_header_buffer = Cstruct.create Message.sizeof in
  let read_buffer = Bytes.make maxMsgSize '\000' in
  let leftover = Cstruct.create 0 in
  { fd; rlock; wlock; read_header_buffer; write_header_buffer;
    read_buffer; leftover; closed; read_closed; write_closed }

(* Write a whole buffer to the fd, without any encapsulation *)
let really_write fd buffer =
  let rec loop remaining =
    if Cstruct.len remaining = 0
    then Lwt.return (Ok ())
    else
      Hvsock.write fd remaining
      >>= function
      | 0 -> Lwt.return (Error `Closed)
      | n ->
        loop (Cstruct.shift remaining n) in
  Lwt.catch
    (fun () ->
      loop buffer
    ) (function
      (* ECONNRESET is common but other errors may be possible. Whatever the
         error we should treat it as Eof. *)
      | Unix.Unix_error(Unix.ECONNRESET, _, _) ->
        Lwt.return (Error `Closed)
      | e ->
        Log.err (fun f -> f "Hvsock.write: %s" (Printexc.to_string e));
        Lwt.return (Error `Closed)
    )

(* Read a whole buffer from the fd, without any encapsulation *)
let really_read fd buffer =
  let rec loop remaining =
    if Cstruct.len remaining = 0
    then Lwt.return (Ok `Done)
    else
      Hvsock.read fd remaining
      >>= function
      | 0 -> Lwt.return (Ok `Eof)
      | n ->
        loop (Cstruct.shift remaining n) in
  Lwt.catch
    (fun () ->
      loop buffer
    ) (function
      (* ECONNRESET is common but other errors may be possible. Whatever the
         error we should treat it as Eof. *)
      | Unix.Unix_error(Unix.ECONNRESET, _, _) ->
        Lwt.return (Ok `Eof)
      | e ->
        Log.err (fun f -> f "Hvsock.read: %s" (Printexc.to_string e));
        Lwt.return (Ok `Eof)
    )

let shutdown_write flow =
  if flow.write_closed
  then Lwt.return ()
  else begin
    flow.write_closed <- true;
    Lwt_mutex.with_lock flow.wlock
      (fun () ->
        Message.(marshal ShutdownWrite flow.write_header_buffer);
        Log.debug (fun f -> f "TX ShutdownWrite");
        really_write flow.fd flow.write_header_buffer
        >>= function
        | Ok () ->
          Log.err (fun f -> f "Hvsock.shutdown_write: got Eof");
          Lwt.return_unit
        | Error `Closed -> Lwt.return_unit
      )
  end

let shutdown_read flow =
  if flow.read_closed
  then Lwt.return ()
  else begin
    flow.read_closed <- true;
    Lwt_mutex.with_lock flow.wlock
      (fun () ->
        Message.(marshal ShutdownRead flow.write_header_buffer);
        Log.debug (fun f -> f "TX ShutdownRead");
        really_write flow.fd flow.write_header_buffer
        >>= function
        | Error `Closed ->
          Log.err (fun f -> f "Hvsock.shutdown_write: got Eof");
          Lwt.return_unit
        | Ok () -> Lwt.return_unit
      )
  end

let close flow =
  match flow.closed with
  | false ->
    flow.closed <- true;
    flow.read_closed <- true;
    flow.write_closed <- true;
    Lwt.finalize
      (fun () ->
        Lwt_mutex.with_lock flow.wlock
          (fun () ->
            Log.debug (fun f -> f "TX Close");
            Message.(marshal Close flow.write_header_buffer);
            really_write flow.fd flow.write_header_buffer
            >>= function
            | Error `Closed -> Lwt.return_unit
            | Ok () ->
              let header = Cstruct.create Message.sizeof in
              let payload = Cstruct.create maxMsgSize in
              let rec wait_for_close () =
                really_read flow.fd header
                >>= function
                | Error (`Unix m) ->
                   Log.err (fun f -> f "Read error %s" (Unix.error_message m));
                   Lwt.return_unit
                | Ok `Eof -> Lwt.return_unit
                | Ok `Done ->
                  match Message.unmarshal header with
                  | Message.Close ->
                    Log.debug (fun f -> f "RX Close");
                    Lwt.return_unit
                  | Message.ShutdownRead ->
                    Log.debug (fun f -> f "RX Close");
                    Lwt.return_unit
                  | Message.ShutdownWrite ->
                    Log.debug (fun f -> f "RX ShutdownWrite");
                    wait_for_close ()
                  | Message.Data n ->
                    Log.debug (fun f -> f "RX Data %d" n);
                    really_read flow.fd (Cstruct.sub payload 0 n)
                    >>= function
                    | Ok `Eof -> Lwt.return_unit
                    | Ok `Done -> wait_for_close ()
                    | Error (`Unix m) -> 
                      Log.err (fun f -> f "Read error %s" (Unix.error_message m));
                      Lwt.return_unit
              in
              wait_for_close ()
          )
      ) (fun () -> Hvsock.close flow.fd)
  | true -> Lwt.return_unit

(* Write a whole buffer to the fd, in chunks according to the maximum message
   size *)
let write flow buffer =
  if flow.closed || flow.write_closed then Lwt.return (Error `Closed)
  else
    let rec loop remaining =
      let len = Cstruct.len remaining in
      if len = 0
      then Lwt.return (Ok ())
      else
        let this_batch = min len maxMsgSize in
        Lwt_mutex.with_lock flow.wlock
          (fun () ->
            let to_send = Cstruct.sub remaining 0 this_batch in
            Log.debug (fun f -> f "TX Data %d (%s)" this_batch (String.escaped (Cstruct.to_string to_send)));
            Message.(marshal (Data this_batch) flow.write_header_buffer);
            really_write flow.fd flow.write_header_buffer
            >>= function
            | Error `Closed -> Lwt.return (Error `Closed)
            | Ok () -> really_write flow.fd to_send
          )
        >>= function
        | Error `Closed -> Lwt.return (Error `Closed)
        | Ok () -> loop (Cstruct.shift remaining this_batch) in
    loop buffer

let read_next_chunk flow =
  if flow.closed || flow.read_closed then Lwt.return (Ok `Eof)
  else
    let rec loop () =
      really_read flow.fd flow.read_header_buffer
      >>= function
      | Error (`Unix m) ->
         Log.err (fun f -> f "read_next_chunk error: %s" (Unix.error_message m));
         Lwt.return (Ok `Eof)
      | Ok `Eof -> Lwt.return (Ok `Eof)
      | Ok `Done ->
        match Message.unmarshal flow.read_header_buffer with
        | Message.ShutdownWrite ->
          Log.debug (fun f -> f "RX ShutdownWrite");
          flow.read_closed <- true;
          Lwt.return (Ok `Eof)
        | Message.Close ->
          Log.debug (fun f -> f "RX Close");
          close flow
          >>= fun () ->
          Lwt.return (Ok `Eof)
        | Message.ShutdownRead ->
          Log.debug (fun f -> f "RX ShutdownRead");
          flow.write_closed <- true;
          loop ()
        | Message.Data n ->
          Log.debug (fun f -> f "RX Data %d" n);
          let payload = Cstruct.create n in
          really_read flow.fd payload
          >>= function
          | Ok `Eof -> Lwt.return (Ok `Eof)
          | Ok `Done -> Lwt.return (Ok (`Data payload)) 
          | Error (`Unix m) ->
             Log.err (fun f -> f "read_next_chunk error, turning into eof: %s" (Unix.error_message m));
             Lwt.return (Ok `Eof)
    in
    loop ()

let read flow =
  if Cstruct.len flow.leftover = 0 then begin
    Lwt_mutex.with_lock flow.rlock
      (fun () ->
        read_next_chunk flow
      )
  end else begin
    let result = flow.leftover in
    flow.leftover <- Cstruct.create 0;
    Lwt.return (Ok (`Data result))
  end

let rec read_into flow buf =
  if Cstruct.len buf = 0
  then Lwt.return (Ok `Done)
  else begin
    if Cstruct.len flow.leftover = 0 then begin
      Lwt_mutex.with_lock flow.rlock
        (fun () ->
          read_next_chunk flow
          >>= function
          | Error (`Unix m) ->
             Log.err (fun f -> f "read_into error, turning into eof: %s" (Unix.error_message m));
             Lwt.return (Ok `Eof)
          | Ok `Eof -> Lwt.return (Ok `Eof)
          | Ok (`Data payload) ->
            let to_consume = min (Cstruct.len buf) (Cstruct.len payload) in
            Cstruct.blit payload 0 buf 0 to_consume;
            flow.leftover <- Cstruct.shift payload to_consume;
            Lwt.return (Ok (`Data (Cstruct.shift buf to_consume)))
        ) >>= function
        | Ok `Eof -> Lwt.return (Ok `Eof)
        | Ok (`Data buf) -> read_into flow buf
        | Error (`Unix m) ->
            Log.err (fun f -> f "read_into error, turning into eof: %s" (Unix.error_message m));
            Lwt.return (Ok `Eof)
    end else begin
      let to_consume = min (Cstruct.len buf) (Cstruct.len flow.leftover) in
      Cstruct.blit flow.leftover 0 buf 0 to_consume;
      flow.leftover <- Cstruct.shift flow.leftover to_consume;
      read_into flow (Cstruct.shift buf to_consume)
    end
  end

let writev flow bufs =
  let rec loop = function
    | [] -> Lwt.return (Ok ())
    | x :: xs ->
      write flow x
      >>= function
      | Error `Closed -> Lwt.return (Error `Closed)
      | Ok () -> loop xs in
  loop bufs
end
