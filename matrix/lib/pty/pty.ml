type t = { fd : Unix.file_descr; mutable pid : int option }
type winsize = { rows : int; cols : int; xpixel : int; ypixel : int }

(* External C functions *)
external open_pty_raw : unit -> Unix.file_descr * Unix.file_descr
  = "ocaml_pty_open"

external get_winsize_raw : Unix.file_descr -> winsize = "ocaml_pty_get_winsize"

external set_winsize_raw : Unix.file_descr -> winsize -> unit
  = "ocaml_pty_set_winsize_byte" "ocaml_pty_set_winsize"

external setsid_and_setctty : Unix.file_descr -> unit
  = "ocaml_pty_setsid_and_setctty"

external raise_fork_error : unit -> 'a = "ocaml_raise_fork_error"

let file_descr t = t.fd
let in_fd t = t.fd
let out_fd t = t.fd
let pid t = t.pid

let terminate t =
  match t.pid with
  | Some pid -> ( try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ())
  | None -> invalid_arg "Pty.terminate: no child process"

let kill t =
  match t.pid with
  | Some pid -> ( try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ())
  | None -> invalid_arg "Pty.kill: no child process"

let close ?(wait = true) t =
  (* Terminate and reap child process if spawned *)
  (match t.pid with
  | Some pid ->
      (* Try SIGTERM first for graceful shutdown *)
      (try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ());
      if wait then (
        (* Give process time to exit cleanly *)
        Unix.sleepf 0.1;
        (* Try to reap - if still running, force kill *)
        match Unix.waitpid [ WNOHANG ] pid with
        | 0, _ -> (
            (* Still running - send SIGKILL and wait *)
            (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
            try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ())
        | _, _ ->
            (* Already exited *)
            ())
  | None -> ());
  (* Mark as closed to prevent duplicate cleanup *)
  t.pid <- None;
  (* Close file descriptor *)
  try Unix.close t.fd with Unix.Unix_error _ -> ()

let get_winsize t = get_winsize_raw t.fd
let set_winsize t ws = set_winsize_raw t.fd ws

let resize t ~rows ~cols =
  let ws = { rows; cols; xpixel = 0; ypixel = 0 } in
  set_winsize t ws

let inherit_size ~src ~dst =
  let ws = get_winsize src in
  set_winsize dst ws

let open_pty ?winsize () =
  let master_fd, slave_fd = open_pty_raw () in
  let master = { fd = master_fd; pid = None } in
  let slave = { fd = slave_fd; pid = None } in
  (* Set initial window size if provided *)
  (match winsize with
  | Some ws -> (
      try set_winsize slave ws
      with e ->
        close master;
        close slave;
        raise e)
  | None -> ());
  (master, slave)

let spawn ?env ?cwd ?winsize ~prog ~args () =
  let pty_master, pty_slave = open_pty ?winsize () in
  let argv = Array.of_list (prog :: args) in

  match Unix.fork () with
  | -1 ->
      (* Fork failed *)
      close pty_master;
      close pty_slave;
      raise_fork_error ()
  | 0 -> (
      (* Child process *)
      Unix.close pty_master.fd;

      (* Create new session and set controlling TTY *)
      (try setsid_and_setctty pty_slave.fd
       with Unix.Unix_error _ ->
         Unix.close pty_slave.fd;
         exit 127);

      (* Redirect stdin, stdout, stderr to the slave pty *)
      Unix.dup2 pty_slave.fd Unix.stdin;
      Unix.dup2 pty_slave.fd Unix.stdout;
      Unix.dup2 pty_slave.fd Unix.stderr;

      (* Close original slave fd if not stdin/stdout/stderr *)
      if
        pty_slave.fd <> Unix.stdin
        && pty_slave.fd <> Unix.stdout
        && pty_slave.fd <> Unix.stderr
      then Unix.close pty_slave.fd;

      (* Change directory if requested *)
      (match cwd with
      | Some dir -> (
          try Unix.chdir dir
          with Unix.Unix_error (e, _, _) -> exit (Obj.magic e : int))
      | None -> ());

      (* Execute the program *)
      try
        match env with
        | None -> Unix.execvp prog argv
        | Some env_array -> Unix.execvpe prog argv env_array
      with Unix.Unix_error (e, _, _) -> exit (Obj.magic e : int))
  | pid ->
      (* Parent process *)
      close pty_slave;
      (* Store PID for cleanup on close *)
      pty_master.pid <- Some pid;
      pty_master

let with_pty ?winsize f =
  let master, slave = open_pty ?winsize () in
  Fun.protect
    ~finally:(fun () ->
      (try close master with Unix.Unix_error _ -> ());
      try close slave with Unix.Unix_error _ -> ())
    (fun () -> f master slave)

let with_spawn ?env ?cwd ?winsize ~prog ~args f =
  let pty = spawn ?env ?cwd ?winsize ~prog ~args () in
  Fun.protect
    ~finally:(fun () -> try close pty with Unix.Unix_error _ -> ())
    (fun () -> f pty)

(* I/O operations *)
let read t buf ofs len = Unix.read t.fd buf ofs len
let write t buf ofs len = Unix.write t.fd buf ofs len
let write_string t str ofs len = Unix.write_substring t.fd str ofs len

(* Non-blocking mode *)
let set_nonblock t = Unix.set_nonblock t.fd
let clear_nonblock t = Unix.clear_nonblock t.fd
