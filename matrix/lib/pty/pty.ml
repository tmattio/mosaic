(* The child of a {!spawn} handle. [Running pid] is a child the handle has
   not seen reaped: until it is reaped, its id names it and no other process.
   [Exited status] is a child the handle reaped while checking on it, whose
   status {!wait} returns. *)
type child = No_child | Running of int | Exited of Unix.process_status

type t = {
  fd : Unix.file_descr;
  mutable child : child;
  (* The slave of a {!spawn} PTY, held until [close] where [hold_slave]. *)
  slave : Unix.file_descr option;
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

external fork_exec :
  Unix.file_descr ->
  string array ->
  string array ->
  string array ->
  string option ->
  int = "ocaml_pty_fork_exec"

(* Why, and on which platforms, is in pty_stubs.c. *)
external hold_slave : unit -> bool = "ocaml_pty_hold_slave"

let hold_slave = hold_slave ()
let file_descr t = t.fd
let in_fd t = t.fd
let out_fd t = t.fd
let pid t = match t.child with Running pid -> Some pid | _ -> None

(* Every fd-consuming operation goes through this guard so a closed handle
   raises EBADF instead of touching a possibly-reused fd number. *)
let live_fd t op =
  if t.closed then raise (Unix.Unix_error (Unix.EBADF, op, ""));
  t.fd

(* Asks the system, without blocking, what became of a running child. One
   that has exited is reaped here and its status kept. ECHILD means the
   program reaped it with its own waitpid, and its id may now name another
   process, so the handle forgets it. *)
let rec check t =
  match t.child with
  | No_child | Exited _ -> ()
  | Running pid -> (
      match Unix.waitpid [ WNOHANG ] pid with
      | 0, _ -> ()
      | _, status -> t.child <- Exited status
      | exception Unix.Unix_error (EINTR, _, _) -> check t
      | exception Unix.Unix_error (ECHILD, _, _) -> t.child <- No_child)

(* Sends [signal] to the child if it is still running. *)
let signal t signal =
  check t;
  match t.child with
  | Running pid -> ( try Unix.kill pid signal with Unix.Unix_error _ -> ())
  | No_child | Exited _ -> ()

let terminate t =
  match t.child with
  | No_child -> invalid_arg "Pty.terminate: no child process"
  | Running _ | Exited _ -> signal t Sys.sigterm

let kill t =
  match t.child with
  | No_child -> invalid_arg "Pty.kill: no child process"
  | Running _ | Exited _ -> signal t Sys.sigkill

let rec wait t =
  match t.child with
  | No_child -> invalid_arg "Pty.wait: no child process"
  | Exited status ->
      t.child <- No_child;
      status
  | Running pid -> (
      match Unix.waitpid [] pid with
      | _, status ->
          t.child <- No_child;
          status
      | exception Unix.Unix_error (EINTR, _, _) -> wait t
      | exception (Unix.Unix_error (ECHILD, _, _) as e) ->
          t.child <- No_child;
          raise e)

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

let close_fd fd = try Unix.close fd with Unix.Unix_error _ -> ()

let close ?(wait = true) t =
  if t.closed then ()
  else (
    t.closed <- true;
    (* Hang up first: a child whose exit waits for its output to be read can
       then exit, so the reap below does not wait on it. *)
    close_fd t.fd;
    Option.iter close_fd t.slave;
    signal t Sys.sigterm;
    if wait then (
      (match t.child with Running pid -> reap pid | No_child | Exited _ -> ());
      t.child <- No_child))

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
  let master =
    { fd = master_fd; child = No_child; slave = None; closed = false }
  in
  let slave =
    { fd = slave_fd; child = No_child; slave = None; closed = false }
  in
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

(* Where [execvp] looks for [prog], in order: [prog] itself when it names a
   path, else [prog] in each directory of the parent's [PATH], where an empty
   directory is the current one. *)
let search_path prog =
  if String.contains prog '/' then [| prog |]
  else
    let path = Option.value (Sys.getenv_opt "PATH") ~default:"/bin:/usr/bin" in
    String.split_on_char ':' path
    |> List.map (fun dir -> if dir = "" then prog else Filename.concat dir prog)
    |> Array.of_list

let spawn ?env ?cwd ?winsize ~prog ~args () =
  let master, slave = open_pty ?winsize () in
  let env = match env with Some env -> env | None -> Unix.environment () in
  let argv = Array.of_list (prog :: args) in
  match fork_exec slave.fd (search_path prog) argv env cwd with
  | pid ->
      let slave =
        if hold_slave then Some slave.fd
        else (
          close slave;
          None)
      in
      { master with child = Running pid; slave }
  | exception e ->
      close master;
      close slave;
      raise e

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
