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

type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
external stub_ba_send: Unix.file_descr -> buffer -> int -> int -> int = "stub_hvsock_ba_send"
let cstruct_write fd b = stub_ba_send fd b.Cstruct.buffer b.Cstruct.off b.Cstruct.len

external stub_ba_recv: Unix.file_descr -> buffer -> int -> int -> int = "stub_hvsock_ba_recv"
let cstruct_read fd b = stub_ba_recv fd b.Cstruct.buffer b.Cstruct.off b.Cstruct.len

module Make(Time: V1_LWT.TIME)(Fn: Lwt_hvsock.FN) = struct

module Blocking_hvsock = Hvsock
module Hvsock = Lwt_hvsock.Make(Time)(Fn)

type 'a io = 'a Lwt.t

type buffer = Cstruct.t

type error = Unix.error

let error_message = Unix.error_message

type flow = {
  fd: Hvsock.t;
  read_buffers_max: int;
  read_max: int;
  mutable read_buffers: Cstruct.t list;
  mutable read_buffers_len: int;
  read_buffers_m: Mutex.t;
  read_buffers_c: Condition.t;
  mutable read_error: bool;
  mutable write_buffers: Cstruct.t list;
  mutable write_buffers_len: int;
  write_buffers_m: Mutex.t;
  write_buffers_c: Condition.t;
  write_buffers_max: int;
  write_max: int;
  mutable write_flushed: bool;
  mutable closed: bool;
  mutable write_error: bool;
}

let connect fd =
  let read_buffers_max = 65536 in
  let read_max = 8192 in
  let read_buffers = [] in
  let read_buffers_len = 0 in
  let read_buffers_m = Mutex.create () in
  let read_buffers_c = Condition.create () in
  let read_error = false in
  let write_buffers = [] in
  let write_buffers_len = 0 in
  let write_buffers_m = Mutex.create () in
  let write_buffers_c = Condition.create () in
  let write_buffers_max = 65536 in
  let write_max = 8192 in
  let write_flushed = false in
  let closed = false in
  let write_error = false in

  let t = { fd; read_buffers_max; read_max; read_buffers; read_buffers_len;
    read_buffers_m; read_buffers_c; read_error; write_buffers; write_buffers_len;
    write_buffers_m; write_buffers_c; closed; write_buffers_max; write_max; write_flushed;
    write_error } in

  let write_thread () =
    let fd = match Hvsock.to_fd fd with Some x -> x | None -> assert false in
    let get_buffers () =
      Mutex.lock write_buffers_m;
      while t.write_buffers = [] do
        Condition.wait write_buffers_c write_buffers_m
      done;
      let result = t.write_buffers in
      t.write_buffers <- [];
      t.write_buffers_len <- 0;
      Mutex.unlock write_buffers_m;
      Condition.broadcast write_buffers_c;
      List.rev result  in
    try
      while not t.closed do
        let buffer = get_buffers () |> Cstruct.concat in
        let rec loop remaining =
          if Cstruct.len remaining = 0 then () else begin
            let to_write = min t.write_max (Cstruct.len remaining) in
            let buf = Cstruct.sub remaining 0 to_write in
            let n = cstruct_write fd buf in
            loop @@ Cstruct.shift remaining n
          end in
        loop buffer
      done;
      t.write_flushed <- true;
      Condition.broadcast write_buffers_c
    with e ->
      Log.err (fun f -> f "Flow write_thread caught: %s" (Printexc.to_string e));
      t.write_error <- true;
      t.write_flushed <- true;
      Condition.broadcast write_buffers_c
    in
  let _ = Thread.create write_thread () in
  let read_thread () =
    let fd = match Hvsock.to_fd fd with Some x -> x | None -> assert false in
    let get_buffer () =
      Mutex.lock t.read_buffers_m;
      while t.read_buffers_len = t.read_buffers_max do
        Condition.wait t.read_buffers_c t.read_buffers_m
      done;
      let allowed = t.read_buffers_max - t.read_buffers_len in
      let buf = Cstruct.create allowed in
      Mutex.unlock t.read_buffers_m;
      buf in
    try
      while not t.closed do
        let buffer = get_buffer () in
        let rec loop remaining =
          if Cstruct.len remaining = 0 then () else begin
            let to_read = min t.read_max (Cstruct.len remaining) in
            let buf = Cstruct.sub remaining 0 to_read in
            let n = cstruct_read fd buf in
            let data = Cstruct.sub remaining 0 n in
            Mutex.lock t.read_buffers_m;
            t.read_buffers <- t.read_buffers @ [ data ];
            t.read_buffers_len <- t.read_buffers_len + (Cstruct.len data);
            Mutex.unlock t.read_buffers_m;
            Condition.broadcast t.read_buffers_c;
            loop @@ Cstruct.shift remaining n
          end in
        loop buffer
      done
    with e ->
      Log.err (fun f -> f "Flow read_thread caught: %s" (Printexc.to_string e));
      t.read_error <- true;
      Condition.broadcast read_buffers_c
    in
  let _ = Thread.create read_thread () in
  t

let detach f x =
  let fn = Fn.create f in
  Lwt.finalize
    (fun () -> Fn.fn fn x)
    (fun () -> Fn.destroy fn; Lwt.return_unit)

let wait_write_flush t =
  Log.info (fun f -> f "wait_write_flush");
  Mutex.lock t.write_buffers_m;
  while not t.write_flushed do
    Condition.wait t.write_buffers_c t.write_buffers_m
  done;
  Mutex.unlock t.write_buffers_m

let close t =
  Log.warn (fun f -> f "FLOW.close called");
  match t.closed with
  | false ->
    t.closed <- true;
    Condition.broadcast t.write_buffers_c;
    detach wait_write_flush t
    >>= fun () ->
    Hvsock.close t.fd
  | true ->
    Lwt.return ()

let wait_for_data flow n =
  Mutex.lock flow.read_buffers_m;
  while flow.read_buffers_len < n do
    Condition.wait flow.read_buffers_c flow.read_buffers_m;
  done;
  Mutex.unlock flow.read_buffers_m

let read flow =
  if flow.closed || flow.read_error then return `Eof
  else begin
    Mutex.lock flow.read_buffers_m;
    let take () =
      let result = List.hd flow.read_buffers in
      flow.read_buffers <- List.tl flow.read_buffers;
      flow.read_buffers_len <- flow.read_buffers_len - (Cstruct.len result);
      Condition.broadcast flow.read_buffers_c;
      result in
    if flow.read_buffers = [] then begin
      Mutex.unlock flow.read_buffers_m;
      detach (wait_for_data flow) 1
      >>= fun () ->
      (* Assume for now there's only one reader so no-one will steal the data *)
      Mutex.lock flow.read_buffers_m;
      let result = take () in
      Mutex.unlock flow.read_buffers_m;
      return (`Ok result)
    end else begin
      let result = take () in
      Mutex.unlock flow.read_buffers_m;
      return (`Ok result)
    end
  end

let read_into flow buffer =
  (* Can we drop this function altogether? *)
  Log.err (fun f -> f "read_into not implemented");
  failwith "not implemented read_into"

let wait_for_space flow n =
  Mutex.lock flow.write_buffers_m;
  while (flow.write_buffers_len + n) > flow.write_buffers_max do
    Condition.wait flow.write_buffers_c flow.write_buffers_m;
  done;
  Mutex.unlock flow.write_buffers_m

let writev flow bufs =
  if flow.closed || flow.write_error then return `Eof else begin
    let len = List.fold_left (+) 0 (List.map Cstruct.len bufs) in
    Mutex.lock flow.write_buffers_m;
    let put () =
      flow.write_buffers <- (List.rev bufs) @ flow.write_buffers;
      flow.write_buffers_len <- flow.write_buffers_len + len;
      Condition.broadcast flow.write_buffers_c in
    if flow.write_buffers_len + len > flow.write_buffers_max then begin
      Mutex.unlock flow.write_buffers_m;
      detach (wait_for_space flow) len
      >>= fun () ->
      (* Assume for now there's only one writer so no-one will steal the space *)
      Mutex.lock flow.write_buffers_m;
      put ();
      Mutex.unlock flow.write_buffers_m;
      Lwt.return (`Ok ())
    end else begin
      put ();
      Mutex.unlock flow.write_buffers_m;
      Lwt.return (`Ok ())
    end
  end

let write flow buf = writev flow [ buf ]
end
