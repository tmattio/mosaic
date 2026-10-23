(* A job-control shell for test_runtime_pty: it runs runtime_child.exe as a
   job of its own process group, in the foreground of the terminal, and
   reports on its standard output when the job stops or ends. While the job
   is stopped it reads a command, a line that ends with it: [f] continues it
   in the foreground, [b] in the background. *)

external setpgid : int -> int -> unit = "runtime_job_setpgid"
external tcsetpgrp : Unix.file_descr -> int -> unit = "runtime_job_tcsetpgrp"

let () =
  let child = Sys.argv.(1) in
  (* A shell that is not in the foreground still moves the terminal's
     foreground; it ignores SIGTTOU for that. *)
  ignore (Sys.signal Sys.sigttou Sys.Signal_ignore : Sys.signal_behavior);
  match Unix.fork () with
  | 0 ->
      (* The job takes the terminal's foreground itself as well as the
         shell: whichever runs first, the job never reaches the terminal
         from the background, where setting it would stop the job. *)
      setpgid 0 0;
      tcsetpgrp Unix.stdin (Unix.getpid ());
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
        (* The command is the line's last character: a terminal's late
           answer to a query the job sent before it stopped is typing to
           whoever reads the terminal, and precedes it on the line. *)
        let line = input_line stdin in
        let last = String.length line - 1 in
        match if last < 0 then "" else String.sub line last 1 with
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
