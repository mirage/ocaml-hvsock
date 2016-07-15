(*
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

open Lwt.Infix

module Make(Main: Lwt_hvsock_s.MAIN) = struct
  let detach f x =
    let stream, push = Lwt_stream.create () in
    let return x = Main.run_in_main (fun () ->
      push (Some x);
      Lwt.return_unit
    ) in
    let _thread = Thread.create (fun () ->
      try
        return (Result.Ok (f x))
      with e ->
        return (Result.Error e)
    ) () in
    Lwt_stream.next stream
    >>= function
    | Result.Ok x -> Lwt.return x
    | Result.Error e -> Lwt.fail e

  type ('a, 'b) request = {
    input: 'a;
    output: 'b Lwt.u;
  }

  type ('a, 'b) fn = {
    push_request: ('a, 'b) request option -> unit;
    t: Thread.t;
  }

  let rec handle_requests processor requests =
    match Main.run_in_main (fun () ->
      Lwt.catch
        (fun () -> Lwt_stream.next requests >>= fun x -> Lwt.return (Some x))
        (fun _ -> Lwt.return None)
      ) with
    | None ->
Printf.fprintf stderr "None\n%!";
 ()
    | Some r ->
      let result =
        try
          Result.Ok (processor r.input)
        with
        | e -> Result.Error e in
      Main.run_in_main (fun () ->
        match result with
        | Result.Ok x -> Lwt.wakeup_later r.output x; Lwt.return_unit
        | Result.Error e -> Lwt.wakeup_later_exn r.output e; Lwt.return_unit
      );
      handle_requests processor requests

  let make_fn processor =
    let requests, push_request = Lwt_stream.create () in
    let t = Thread.create (handle_requests processor) requests in
    { push_request; t }

  let apply worker input =
    let t, output = Lwt.task () in
    let request = { input; output } in
    worker.push_request (Some request);
    t

  let shutdown worker =
    worker.push_request None;
    Lwt.return ()
end
