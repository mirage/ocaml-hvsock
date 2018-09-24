module In_memory_buffer =
  Hvsock_lwt.Buffering.Make (Hvsock_lwt_unix.Preemptive_detach) (In_memory)

let test_accept_connect () =
  let server = In_memory.create () in
  In_memory.bind server () ;
  In_memory.listen server ;
  let server_thread =
    Thread.create
      (fun () ->
        let sock, _ = In_memory.accept server in
        In_memory.close sock )
      ()
  in
  let client = In_memory.create () in
  In_memory.connect client () ;
  Thread.join server_thread ;
  In_memory.close server

let test_write_read () =
  let server = In_memory.create () in
  In_memory.bind server () ;
  In_memory.listen server ;
  let received = ref "" in
  let server_thread =
    Thread.create
      (fun () ->
        let sock, _ = In_memory.accept server in
        let buf = Cstruct.create 10 in
        let n = In_memory.read_into sock buf in
        (received := Cstruct.(to_string @@ sub buf 0 n)) ;
        In_memory.close sock )
      ()
  in
  let client = In_memory.create () in
  In_memory.connect client () ;
  let n = In_memory.writev client [Cstruct.of_string "hello\n"] in
  Alcotest.(check int) "writev" 6 n;
  In_memory.shutdown_write client ;
  Thread.join server_thread ;
  Alcotest.(check string) "read_into" "hello\n" !received;
  In_memory.close server ;
  In_memory.close client

let test_simulated_socket =
  [ ("accept connect", `Quick, test_accept_connect)
  ; ("write then read", `Quick, test_write_read) ]

let () =
  Logs.set_reporter (Logs_fmt.reporter ()) ;
  Alcotest.run "hvsock" [("socket", test_simulated_socket)]
