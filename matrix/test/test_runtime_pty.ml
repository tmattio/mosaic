(* The runtime in a pseudo-terminal: signals, job control and handing the
   terminal to a child, with a real terminal driver. The program under test
   is runtime_child.ml. *)

open Windtrap

let child =
  Filename.concat (Filename.dirname Sys.executable_name) "runtime_child.exe"

type session = { pty : Pty.t; pid : int; output : Buffer.t }

let spawn () =
  let pty =
    Pty.spawn ~prog:child ~args:[]
      ~winsize:Pty.{ rows = 24; cols = 80; xpixel = 0; ypixel = 0 }
      ()
  in
  Pty.set_nonblock pty;
  { pty; pid = Option.get (Pty.pid pty); output = Buffer.create 4096 }

(* Reads what the program writes until [sub] appears after [from], at most
   [timeout] seconds. *)
let await ?(timeout = 5.) ?(from = 0) s sub =
  let deadline = Unix.gettimeofday () +. timeout in
  let buf = Bytes.create 4096 in
  let found () =
    let out = Buffer.contents s.output in
    let n = String.length sub in
    let rec go i =
      i + n <= String.length out && (String.sub out i n = sub || go (i + 1))
    in
    go from
  in
  while (not (found ())) && Unix.gettimeofday () < deadline do
    match Unix.select [ Pty.in_fd s.pty ] [] [] 0.05 with
    | [], _, _ -> ()
    | _ -> (
        match Pty.read s.pty buf 0 4096 with
        | n when n > 0 -> Buffer.add_subbytes s.output buf 0 n
        | _ -> ()
        | exception Unix.Unix_error _ -> ())
  done;
  if not (found ()) then
    failf "timed out waiting for %S in %S" sub (Buffer.contents s.output)

let send s bytes =
  ignore (Pty.write_string s.pty bytes 0 (String.length bytes) : int)

let mark s = Buffer.length s.output

let rec wait_exit ?(timeout = 5.) s =
  match Unix.waitpid [ Unix.WNOHANG ] s.pid with
  | 0, _ ->
      if timeout <= 0. then (
        Unix.kill s.pid Sys.sigkill;
        fail "the program did not end")
      else (
        Unix.sleepf 0.02;
        (* Keep draining what it writes. *)
        (try await ~timeout:0.01 s "\000\001" with _ -> ());
        wait_exit ~timeout:(timeout -. 0.03) s)
  | _, status -> status

let cooked s =
  let t = Unix.tcgetattr (Pty.file_descr s.pty) in
  t.c_icanon && t.c_echo && t.c_isig

let finish s = try Pty.close s with _ -> ()

let with_session f =
  let s = spawn () in
  Fun.protect ~finally:(fun () -> finish s.pty) (fun () -> f s)

(* A termination signal ends the run: the terminal is given back, then the
   signal is raised again, so the process ends with its status. *)
let test_sigterm () =
  with_session @@ fun s ->
  await s "READY";
  let from = mark s in
  Unix.kill s.pid Sys.sigterm;
  match wait_exit s with
  | WSIGNALED n ->
      equal ~msg:"ends by SIGTERM" int Sys.sigterm n;
      await ~from s "\027[?1049l";
      await ~from s "\027[?1004l";
      is_true ~msg:"cooked" (cooked s)
  | _ -> fail "expected the process to end by its signal"

(* A program in its own session is an orphaned process group, which a stop
   signal must not stop: the terminal is given back and taken again, and the
   run goes on. *)
let test_sigtstp_in_an_orphaned_group () =
  with_session @@ fun s ->
  await s "READY 1";
  let from = mark s in
  Unix.kill s.pid Sys.sigtstp;
  await ~from s "\027[?1049l";
  await ~from s "\027[?1049h";
  await ~from s "READY 2";
  send s "q";
  match wait_exit s with
  | WEXITED 0 -> is_true ~msg:"cooked after" (cooked s)
  | _ -> fail "expected a normal exit"

(* A SIGCONT the run did not cause: the modes are set again and the screen
   drawn whole. *)
let test_unsolicited_sigcont () =
  with_session @@ fun s ->
  await s "READY 1";
  let from = mark s in
  Unix.kill s.pid Sys.sigcont;
  await ~from s "\027[?1004h";
  await ~from s "READY";
  send s "q";
  ignore (wait_exit s : Unix.process_status)

(* A child given the terminal finds it cooked, on the primary screen, and
   no reply typed for it; the application's screen is drawn whole after. *)
let test_handoff () =
  with_session @@ fun s ->
  await s "READY 1";
  let from = mark s in
  send s "e";
  await ~from s "CHILD";
  let out = Buffer.contents s.output in
  let after = String.sub out from (String.length out - from) in
  let contains sub =
    let n = String.length sub in
    let rec go i =
      i + n <= String.length after && (String.sub after i n = sub || go (i + 1))
    in
    go 0
  in
  is_true ~msg:"the alternate screen is left first" (contains "\027[?1049l");
  is_true ~msg:"icanon" (contains " icanon");
  is_false ~msg:"not -icanon" (contains "-icanon");
  is_false ~msg:"not -echo " (contains "-echo ");
  is_false ~msg:"not -isig" (contains "-isig");
  await ~from s "READY 2";
  send s "q";
  match wait_exit s with WEXITED 0 -> () | _ -> fail "expected a normal exit"

(* SIGINT received while the terminal is given away was the child's: the
   runtime discards it. *)
let test_sigint_during_release () =
  with_session @@ fun s ->
  await s "READY 1";
  let from = mark s in
  send s "i";
  await ~from s "\027[?1049l";
  Unix.kill s.pid Sys.sigint;
  await ~from s "READY 2";
  send s "q";
  match wait_exit s with
  | WEXITED 0 -> ()
  | _ -> fail "SIGINT during the release ended the run"

(* abort() returns to no point where OCaml runs: the handler restores the
   terminal itself. *)
let test_sigabrt_restores_the_terminal () =
  with_session @@ fun s ->
  await s "READY 1";
  let from = mark s in
  send s "a";
  match wait_exit s with
  | WSIGNALED n ->
      equal ~msg:"ends by SIGABRT" int Sys.sigabrt n;
      await ~from s "\027[?1049l";
      is_true ~msg:"cooked" (cooked s)
  | _ -> fail "expected the process to end by SIGABRT"

(* Job control: the program runs as a job of its own process group under a
   shell (runtime_job.ml) in the same session, so a stop signal stops it. *)

let job =
  Filename.concat (Filename.dirname Sys.executable_name) "runtime_job.exe"

let spawn_job () =
  let pty =
    Pty.spawn ~prog:job ~args:[ child ]
      ~winsize:Pty.{ rows = 24; cols = 80; xpixel = 0; ypixel = 0 }
      ()
  in
  Pty.set_nonblock pty;
  { pty; pid = Option.get (Pty.pid pty); output = Buffer.create 4096 }

(* The job's process, which the shell reports. *)
let job_pid s =
  await s "JOB ";
  await s "READY 1";
  let out = Buffer.contents s.output in
  let i = Option.get (String.index_from_opt out 0 'J') in
  Scanf.sscanf (String.sub out i (String.length out - i)) "JOB %d" Fun.id

(* The state and the CPU ticks of a process, from /proc. *)
let stat pid =
  let line =
    In_channel.with_open_text
      (Printf.sprintf "/proc/%d/stat" pid)
      In_channel.input_all
  in
  let after =
    String.sub line
      (String.rindex line ')' + 2)
      (String.length line - String.rindex line ')' - 2)
  in
  let fields = String.split_on_char ' ' after in
  ( List.nth fields 0,
    int_of_string (List.nth fields 11) + int_of_string (List.nth fields 12) )

(* A stop in a job: while stopped the terminal is cooked and on the primary
   screen; continued in the foreground, the modes are set again and the
   screen drawn whole. *)
let test_sigtstp_stops_the_job () =
  let s = spawn_job () in
  Fun.protect ~finally:(fun () -> finish s.pty) @@ fun () ->
  let pid = job_pid s in
  let from = mark s in
  Unix.kill pid Sys.sigtstp;
  await ~from s "JOB-STOPPED";
  equal ~msg:"stopped" string "T" (fst (stat pid));
  await ~from s "\027[?1049l";
  is_true ~msg:"cooked while stopped" (cooked s);
  let from = mark s in
  send s "f\n";
  await ~from s "\027[?1049h";
  await ~from s "\027[?1004h";
  await ~from s "READY 2";
  send s "q";
  await s "JOB-EXIT 0"

(* Continued in the background, the job stops again when it sets the
   terminal (SIGTTOU), without spinning, until it is in the foreground. *)
let test_background_continue_stops_again () =
  let s = spawn_job () in
  Fun.protect ~finally:(fun () -> finish s.pty) @@ fun () ->
  let pid = job_pid s in
  let from = mark s in
  Unix.kill pid Sys.sigtstp;
  await ~from s "JOB-STOPPED";
  let from = mark s in
  send s "b\n";
  await ~from s "JOB-STOPPED";
  let state, ticks = stat pid in
  equal ~msg:"stopped again" string "T" state;
  Unix.sleepf 0.3;
  equal ~msg:"no CPU while stopped" int ticks (snd (stat pid));
  let from = mark s in
  send s "f\n";
  await ~from s "READY 2";
  send s "q";
  await s "JOB-EXIT 0"

let supported =
  Sys.os_type = "Unix" && Sys.file_exists child && Sys.file_exists job

let () =
  run "matrix.runtime.pty"
    [
      group "Signals"
        (if not supported then []
         else
           [
             test "SIGTERM ends with its status" test_sigterm;
             test "SIGTSTP in an orphaned group"
               test_sigtstp_in_an_orphaned_group;
             test "SIGTSTP stops the job" test_sigtstp_stops_the_job;
             test "continued in the background, it stops again"
               test_background_continue_stops_again;
             test "unsolicited SIGCONT" test_unsolicited_sigcont;
             test "SIGINT during a release is discarded"
               test_sigint_during_release;
             test "SIGABRT restores the terminal"
               test_sigabrt_restores_the_terminal;
           ]);
      group "Handoff"
        (if not supported then []
         else [ test "a child's terminal" test_handoff ]);
    ]
