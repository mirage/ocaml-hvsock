open Hvsock

let create () =
  let fd = create () in
  Lwt_unix.of_unix_file_descr fd

let bind fd sockaddr =
  bind (Lwt_unix.unix_file_descr fd) sockaddr

(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_unix
 * Copyright (C) 2005-2008 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *                    2009 Jérémie Dimino
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later
 * version. See COPYING file for details.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *)

let accept fd =
  Lwt_unix.wrap_syscall Lwt_unix.Read fd
    (fun _ ->
      let (fd, addr) = accept (Lwt_unix.unix_file_descr fd) in
      (Lwt_unix.of_unix_file_descr ~blocking:false fd, addr)
    )

let connect ch addr =
    if Sys.win32 then
      (* [in_progress] tell wether connection has started but not
         terminated: *)
      let in_progress = ref false in
      Lwt_unix.wrap_syscall Lwt_unix.Write ch begin fun () ->
        if !in_progress then
          (* Nothing works without this test and i have no idea why... *)
          if Lwt_unix.writable ch then
            try
              connect (Lwt_unix.unix_file_descr ch) addr
            with
              | Unix.Unix_error (Unix.EISCONN, _, _) ->
                  (* This is the windows way of telling that the connection
                     has completed. *)
                  ()
          else
            raise Lwt_unix.Retry
        else
          try
            connect (Lwt_unix.unix_file_descr ch) addr
          with
            | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) ->
                in_progress := true;
                raise Lwt_unix.Retry
      end
    else
      (* [in_progress] tell wether connection has started but not
         terminated: *)
      let in_progress = ref false in
      Lwt_unix.wrap_syscall Lwt_unix.Write ch begin fun () ->
        if !in_progress then
          (* If the connection is in progress, [getsockopt_error] tells
             wether it succceed: *)
          match Unix.getsockopt_error (Lwt_unix.unix_file_descr ch) with
            | None ->
                (* The socket is connected *)
                ()
            | Some err ->
                (* An error happened: *)
                raise (Unix.Unix_error(err, "connect", ""))
        else
          try
            (* We should pass only one time here, unless the system call
               is interrupted by a signal: *)
            connect (Lwt_unix.unix_file_descr ch) addr
          with
            | Unix.Unix_error (Unix.EINPROGRESS, _, _) ->
                in_progress := true;
                raise Lwt_unix.Retry
      end
