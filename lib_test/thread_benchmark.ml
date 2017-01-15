module Benchmark(Fn: Lwt_hvsock.FN) = struct

  let run () =
    let counter = ref 0 in
    let f = Fn.create (fun () -> incr counter) in
    let iterations = 1_000_000 in
    let start = Unix.gettimeofday () in
    Lwt_main.run begin
      let open Lwt.Infix in
      let rec loop = function
        | 0 -> Lwt.return_unit
        | n ->
          Fn.fn f ()
          >>= fun () ->
          loop (n - 1) in
      loop iterations
    end;
    Alcotest.(check int) "Iterations" iterations (!counter);
    let per_sec = float_of_int iterations /. (Unix.gettimeofday () -. start) in
    per_sec
end

module Run_in_thread = Benchmark(Lwt_hvsock.Run_in_thread(Lwt_preemptive))
module Run_with_detach = Benchmark(Lwt_hvsock.Run_with_detach)

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Printf.printf "Run_in_thread:   %.1f calls per second\n" (Run_in_thread.run ());
  Printf.printf "Run_with_detach: %.1f calls per second\n" (Run_with_detach.run ())
