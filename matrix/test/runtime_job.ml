(* A job-control shell for test_runtime_pty: it runs runtime_child.exe as a
   job of its own process group, in the foreground of the terminal, and
   reports on its standard output when the job stops or ends. While the job
   is stopped it reads a command: [f] continues it in the foreground, [b] in
   the background. *)

external setpgid : int -> int -> unit = "runtime_job_setpgid"
external tcsetpgrp : Unix.file_descr -> int -> unit = "runtime_job_tcsetpgrp"

let () =
  let child = Sys.argv.(1) in
  (* A shell that is not in the foreground still moves the terminal's
     foreground; it ignores SIGTTOU for that. *)
  ignore (Sys.signal Sys.sigttou Sys.Signal_ignore : Sys.signal_behavior);
  match Unix.fork () with
  | 0 ->
      setpgid 0 0;
      ignore (Sys.signal Sys.sigttou Sys.Signal_default : Sys.signal_behavior);
      Unix.execv child [| child |]
  | pid ->
      (try setpgid pid pid with Unix.Unix_error _ -> ());
      tcsetpgrp Unix.stdin pid;
      Printf.printf "JOB %d\r\n%!" pid;
      let rec wait () =
        match Unix.waitpid [ Unix.WUNTRACED ] pid with
        | _, WSTOPPED _ ->
            tcsetpgrp Unix.stdin (Unix.getpid ());
            Printf.printf "JOB-STOPPED\r\n%!";
            command ()
        | _, WEXITED n -> Printf.printf "JOB-EXIT %d\r\n%!" n
        | _, WSIGNALED n -> Printf.printf "JOB-SIGNALED %d\r\n%!" n
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
      and command () =
        match input_line stdin with
        | "f" ->
            tcsetpgrp Unix.stdin pid;
            Unix.kill (-pid) Sys.sigcont;
            wait ()
        | "b" ->
            Unix.kill (-pid) Sys.sigcont;
            wait ()
        | _ -> command ()
      in
      wait ()
