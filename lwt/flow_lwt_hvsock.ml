(*
 * Copyright (C) 2015 David Scott <dave.scott@unikernel.com>
 * Copyright (C) 2016 Docker Inc
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
  let src = Logs.Src.create "flow_lwt_hvsock" ~doc:"AF_HYPERV flow" in
  Logs.Src.set_level src (Some Logs.Debug);
  src

module Log = (val Logs.src_log src : Logs.LOG)

open Lwt

module Make(Time: V1_LWT.TIME)(Main: Lwt_hvsock.MAIN) = struct

module Hvsock = Lwt_hvsock.Make(Time)(Main)

type 'a io = 'a Lwt.t

type buffer = Cstruct.t

type error = Unix.error

let error_message = Unix.error_message

type flow = {
  fd: Hvsock.t;
  read_buffer_size: int;
  mutable closed: bool;
}

let connect fd =
  let read_buffer_size = 4 * 1024 in
  let closed = false in
  { fd; read_buffer_size; closed }

let close t =
  match t.closed with
  | false ->
    t.closed <- true;
    Hvsock.close t.fd
  | true ->
    Lwt.return ()

let read flow =
  if flow.closed then return `Eof
  else
    let buffer = Bytes.make flow.read_buffer_size '\000' in
    Lwt.catch
      (fun () ->
        Hvsock.read flow.fd buffer 0 (Bytes.length buffer)
        >>= function
        | 0 ->
          return `Eof
        | n ->
          let result = Cstruct.create n in
          Cstruct.blit_from_string buffer 0 result 0 n;
          return (`Ok result)
      ) (fun e ->
        Log.err (fun f -> f "Hvsock.read raised %s: returning `Eof" (Printexc.to_string e));
        return `Eof
      )

let read_into flow buffer =
  if flow.closed then return `Eof
  else
    let bytes = Bytes.make (Cstruct.len buffer) '\000' in
    let rec loop ofs len =
      if len = 0
      then Lwt.return (`Ok ())
      else
        Hvsock.read flow.fd bytes ofs len
        >>= function
        | 0 ->
          Lwt.return `Eof
        | n ->
          Cstruct.blit_from_string bytes ofs buffer ofs n;
          loop (ofs + n) (len - n) in
    Lwt.catch
      (fun () ->
        loop 0 (Cstruct.len buffer)
      ) (fun e ->
        Log.err (fun f -> f "Hvsock.read raised %s: returning `Eof" (Printexc.to_string e));
        return `Eof
      )

let really_write fd buf =
  let buffer = Cstruct.to_string buf in
  let rec loop ofs =
    if ofs >= (Bytes.length buffer)
    then Lwt.return (`Ok ())
    else
      let remaining = Bytes.length buffer - ofs in
      Hvsock.write fd buffer ofs remaining
      >>= function
      | 0 -> Lwt.return (`Eof)
      | n ->
        loop (ofs + n) in
  Lwt.catch
    (fun () ->
      loop 0
    ) (function
      | Unix.Unix_error(Unix.EPIPE, _, _) -> return `Eof
      | e ->
        Log.err (fun f -> f "Hvsock.write raised %s: returning `Eof" (Printexc.to_string e));
        return `Eof
    )

let write flow buf =
  if flow.closed then return `Eof
  else really_write flow.fd buf

let writev flow bufs =
  let rec loop = function
    | [] -> return (`Ok ())
    | x :: xs ->
      if flow.closed then return `Eof
      else
        really_write flow.fd x
        >>= function
        | `Ok () -> loop xs
        | `Eof -> Lwt.return `Eof in
  loop bufs
end
