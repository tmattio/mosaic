type t = {
  fd : Unix.file_descr;
  mutable pid : int option; (* Store PID for spawned processes *)
}

type winsize = { rows : int; cols : int; x : int; y : int }

external open_pty_raw : unit -> Unix.file_descr * Unix.file_descr
  = "ocaml_pty_open"

external get_winsize_raw : Unix.file_descr -> winsize = "ocaml_pty_get_winsize"

external set_winsize_raw : Unix.file_descr -> winsize -> unit
  = "ocaml_pty_set_winsize_byte" "ocaml_pty_set_winsize"

external setsid_and_setctty : Unix.file_descr -> unit
  = "ocaml_pty_setsid_and_setctty"

external raise_fork_error : unit -> 'a = "ocaml_raise_fork_error"

let in_fd t = t.fd
let out_fd t = t.fd

let close t =
  (* Kill the child process if we spawned one *)
  (match t.pid with
  | Some pid -> (
      (try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ());
      (* Give it a moment to exit cleanly *)
      Unix.sleepf 0.1;
      try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ())
  | None -> ());
  Unix.close t.fd

let get_winsize t = get_winsize_raw t.fd
let set_winsize t ws = set_winsize_raw t.fd ws

let open_pty ?winsize () =
  let master_fd, slave_fd = open_pty_raw () in
  let master = { fd = master_fd; pid = None } in
  let slave = { fd = slave_fd; pid = None } in
  (* Set initial window size if provided *)
  (match winsize with
  | None -> ()
  | Some ws -> (
      try set_winsize slave ws (* Set winsize on slave, not master *)
      with e ->
        close master;
        close slave;
        raise e));
  (master, slave)

let inherit_size ~src ~dst =
  let ws = get_winsize src in
  set_winsize dst ws

let resize t ~cols ~rows =
  let ws = { rows; cols; x = 0; y = 0 } in
  set_winsize t ws

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
       with Unix.Unix_error (_e, _fn, _) ->
         Unix.close pty_slave.fd;
         exit 127);

      (* Redirect stdin, stdout, stderr to the slave pty *)
      Unix.dup2 pty_slave.fd Unix.stdin;
      Unix.dup2 pty_slave.fd Unix.stdout;
      Unix.dup2 pty_slave.fd Unix.stderr;

      (* The original pty_slave descriptor is no longer needed in the child
       if it has been successfully dup'ed to stdin/out/err. *)
      if
        pty_slave.fd <> Unix.stdin
        && pty_slave.fd <> Unix.stdout
        && pty_slave.fd <> Unix.stderr
      then Unix.close pty_slave.fd;

      (* Change directory if requested *)
      (match cwd with
      | None -> ()
      | Some dir -> (
          try Unix.chdir dir
          with Unix.Unix_error (e, _, _) -> exit (Obj.magic e : int)));

      (* Execute the new program *)
      try
        match env with
        | None -> Unix.execvp prog argv
        | Some env_array -> Unix.execvpe prog argv env_array
      with Unix.Unix_error (e, _, _) ->
        (* Extract the integer code from the Unix.error type and exit.
          This is a common, if slightly hacky, way to do this without
          a dedicated function in the Unix module itself. *)
        exit (Obj.magic e : int))
  | pid ->
      (* Parent process *)
      close pty_slave;
      (* Store the PID so we can kill the process on close *)
      pty_master.pid <- Some pid;
      pty_master

let with_pty ?winsize f =
  let master, slave = open_pty ?winsize () in
  Fun.protect
    ~finally:(fun () ->
      (* Close both, ignoring errors since one might already be closed *)
      (try close master with Unix.Unix_error _ -> ());
      try close slave with Unix.Unix_error _ -> ())
    (fun () -> f master slave)

(* I/O operations *)
let read t buf ofs len = Unix.read t.fd buf ofs len
let write t buf ofs len = Unix.write t.fd buf ofs len
let write_string t str ofs len = Unix.write_substring t.fd str ofs len

(* Set non-blocking mode on a PTY file descriptor *)
let set_nonblock t = Unix.set_nonblock t.fd
let clear_nonblock t = Unix.clear_nonblock t.fd
