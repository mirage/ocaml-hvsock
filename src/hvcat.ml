open Lwt

let sigint_t, sigint_u = Lwt.task ()

let proxy buffer_size (ic, oc) (stdin, stdout) =
  let a_buffer = Bytes.create buffer_size in
  let b_buffer = Bytes.create buffer_size in
  let rec proxy buffer a b =
    Lwt_io.read_into a buffer 0 buffer_size
    >>= function
    | 0 -> Lwt.fail End_of_file
    | n ->
      Lwt_io.write_from_exactly b buffer 0 n
      >>= fun () ->
      proxy buffer a b in
  let (a: unit Lwt.t) = proxy a_buffer stdin oc in
  let (b: unit Lwt.t) = proxy b_buffer ic stdout in
  Lwt.catch
    (fun () -> Lwt.pick [a; b])
    (function End_of_file -> Lwt.return ()
     | e -> Lwt.fail e)

open Cmdliner

let listen =
  let doc = "Act as a server rather than a client." in
  Arg.(value & flag & info [ "l"; "listen"] ~doc)

let vmid =
  Arg.(value & opt (some string) None & info ~docv:"VMID" ~doc:"Identifier of VM/partition" [ "vmid" ])

let serviceid =
  Arg.(value & pos 0 string "3049197C-9A4E-4FBF-9367-97F792F16994" & info ~docv:"SERVICEID" ~doc:"Identifier of service" [])

let echo =
  let doc = "Run a simple multithreaded echo server" in
  Arg.(value & flag & info ["echo"] ~doc)

let buffer_size = 4096

let rec client vmid serviceid =
  try
    let fd = Hvsock.create () in
    Hvsock.connect fd { Hvsock.vmid; serviceid };
    Printf.fprintf stderr "Connected\n%!";
    let msg = "hello\n" in
    Unix.write fd msg 0 (String.length msg);
    Printf.fprintf stderr "Reading 1 byte\n%!";
    let b = Bytes.create 6 in
    let n = Unix.read fd b 0 6 in
    Printf.fprintf stderr "Read %d byte (%s)\n%!" n b;
    Unix.close fd
  with
  | Unix.Unix_error(Unix.ENOENT, _, _) ->
    Printf.fprintf stderr "Server not found (ENOENT)\n"

let one_shot_server vmid serviceid =
  let s = Lwt_hvsock.create () in
  Lwt_hvsock.bind s { Hvsock.vmid; serviceid };
  Lwt_hvsock.accept s
  >>= fun (client, { Hvsock.vmid; serviceid }) ->
  Printf.fprintf stderr "Connection from %s:%s\n%!" (Hvsock.string_of_vmid vmid) serviceid;
  let ic = Lwt_io.of_fd ~close:Lwt.return ~mode:Lwt_io.input s in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.output s in
  proxy buffer_size (ic, oc) (Lwt_io.stdin, Lwt_io.stdout)

let echo_server vmid serviceid =
  let s = Lwt_hvsock.create () in
  Lwt_hvsock.bind s { Hvsock.vmid; serviceid };
  let rec loop () =
    Lwt_hvsock.accept s
    >>= fun (fd, { Hvsock.vmid; serviceid }) ->
    Printf.fprintf stderr "Connection from %s:%s\n%!" (Hvsock.string_of_vmid vmid) serviceid;
    Lwt.async (fun () ->
      let ic = Lwt_io.of_fd ~close:Lwt.return ~mode:Lwt_io.input fd in
      let oc = Lwt_io.of_fd ~close:Lwt.return ~mode:Lwt_io.output fd in
      proxy buffer_size (ic, oc) (ic, oc)
      >>= fun () ->
      Lwt_unix.close fd
      >>= fun () ->
      Printf.fprintf stderr "Disconnected\n%!";
      Lwt.return ()
    );
    loop () in
  loop ()

let main listen echo vmid serviceid =
  let vmid = match vmid with
    | None -> Hvsock.Wildcard
    | Some x -> Hvsock.Id x in
  Printf.fprintf stderr "listen=%b echo=%b vmid=%s serviceid=%s\n%!" listen echo (Hvsock.string_of_vmid vmid) serviceid;
  let t = match listen, echo with
    | true, false -> one_shot_server vmid serviceid
    | true, true -> echo_server vmid serviceid
    | false, _ -> client vmid serviceid; Lwt.return () in
  Lwt_main.run t

let cmd =
  let doc = "Establish Hyper-V socket connections" in
  let man = [
    `S "DESCRIPTION";
    `P "Establish a connection to a server via a Hyper-V socket and transfer data over stdin/stdout, in a similar way to 'nc'";
    `S "EXAMPLES";
    `P "To listen for an incoming connection from anywhere:";
    `P "hvcat -l 3049197C-9A4E-4FBF-9367-97F792F16994";
    `P "To connect to a service in a remote partition:";
    `P "hvcat <vmid> 3049197C-9A4E-4FBF-9367-97F792F16994";
  ] in
  Term.(pure main $ listen $ echo $ vmid $ serviceid),
  Term.info "hvcat" ~version:"0.1" ~doc ~man

let () =
let (_: Lwt_unix.signal_handler_id) = Lwt_unix.on_signal Sys.sigint
  (fun (_: int) ->
    Lwt.wakeup_later sigint_u ();
  ) in
  match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
