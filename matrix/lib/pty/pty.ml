type t = {
  fd : Unix.file_descr;
  mutable pid : int option;
  (* [close] must be a true no-op the second time: the kernel can reallocate
     the fd number to anything else in the process, and a second [Unix.close]
     would then close that unrelated descriptor. *)
  mutable closed : bool;
}

type winsize = { rows : int; cols : int; xpixel : int; ypixel : int }

(* External C functions *)
external open_pty_raw : unit -> Unix.file_descr * Unix.file_descr
  = "ocaml_pty_open"

external get_winsize_raw : Unix.file_descr -> winsize = "ocaml_pty_get_winsize"

external set_winsize_raw : Unix.file_descr -> winsize -> unit
  = "ocaml_pty_set_winsize_byte" "ocaml_pty_set_winsize"

external setsid_and_setctty : Unix.file_descr -> unit
  = "ocaml_pty_setsid_and_setctty"

let file_descr t = t.fd
let in_fd t = t.fd
let out_fd t = t.fd
let pid t = t.pid

(* Every fd-consuming operation goes through this guard so a closed handle
   raises EBADF instead of touching a possibly-reused fd number. *)
let live_fd t op =
  if t.closed then raise (Unix.Unix_error (Unix.EBADF, op, ""));
  t.fd

let terminate t =
  match t.pid with
  | Some pid -> ( try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ())
  | None -> invalid_arg "Pty.terminate: no child process"

let kill t =
  match t.pid with
  | Some pid -> ( try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ())
  | None -> invalid_arg "Pty.kill: no child process"

(* Reap after SIGTERM. An immediate waitpid catches the common case — the
   child exited long ago and close must not stall the caller — then a short
   poll grants the SIGTERM grace before escalating to SIGKILL. *)
let reap pid =
  let reaped () =
    match Unix.waitpid [ WNOHANG ] pid with
    | 0, _ -> false
    | _ -> true
    | exception Unix.Unix_error _ -> true
  in
  let rec poll attempts =
    if reaped () then ()
    else if attempts <= 0 then (
      (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
      try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ())
    else (
      Unix.sleepf 0.01;
      poll (attempts - 1))
  in
  poll 10

let close ?(wait = true) t =
  if t.closed then ()
  else (
    t.closed <- true;
    (* Terminate and reap child process if spawned *)
    (match t.pid with
    | Some pid ->
        (* Try SIGTERM first for graceful shutdown *)
        (try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ());
        if wait then reap pid
    | None -> ());
    t.pid <- None;
    (* Close file descriptor *)
    try Unix.close t.fd with Unix.Unix_error _ -> ())

let get_winsize t = get_winsize_raw (live_fd t "get_winsize")
let set_winsize t ws = set_winsize_raw (live_fd t "set_winsize") ws

let resize t ~rows ~cols =
  let ws = { rows; cols; xpixel = 0; ypixel = 0 } in
  set_winsize t ws

let inherit_size ~src ~dst =
  let ws = get_winsize src in
  set_winsize dst ws

let open_pty ?winsize () =
  let master_fd, slave_fd = open_pty_raw () in
  let master = { fd = master_fd; pid = None; closed = false } in
  let slave = { fd = slave_fd; pid = None; closed = false } in
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
  (* [Unix.fork] never returns -1: on failure it raises [Unix_error]. Close
     both PTY ends before propagating so a failed fork does not leak them. *)
  let pid =
    try Unix.fork ()
    with exn ->
      close pty_master;
      close pty_slave;
      raise exn
  in
  match pid with
  | 0 -> (
      (* Child process. No exception may escape this branch: ordinary OCaml
         termination would run the parent's inherited [at_exit] handlers. *)
      try
        Unix.close pty_master.fd;
        setsid_and_setctty pty_slave.fd;
        Unix.dup2 pty_slave.fd Unix.stdin;
        Unix.dup2 pty_slave.fd Unix.stdout;
        Unix.dup2 pty_slave.fd Unix.stderr;
        if
          pty_slave.fd <> Unix.stdin
          && pty_slave.fd <> Unix.stdout
          && pty_slave.fd <> Unix.stderr
        then Unix.close pty_slave.fd;
        (match cwd with Some dir -> Unix.chdir dir | None -> ());
        match env with
        | None -> Unix.execvp prog argv
        | Some env_array -> Unix.execvpe prog argv env_array
      with _ -> Unix._exit 127)
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
let read t buf ofs len = Unix.read (live_fd t "read") buf ofs len
let write t buf ofs len = Unix.write (live_fd t "write") buf ofs len

let write_string t str ofs len =
  Unix.write_substring (live_fd t "write") str ofs len

(* Non-blocking mode *)
let set_nonblock t = Unix.set_nonblock (live_fd t "set_nonblock")
let clear_nonblock t = Unix.clear_nonblock (live_fd t "clear_nonblock")
