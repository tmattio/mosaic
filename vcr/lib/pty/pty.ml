type t = Unix.file_descr
type winsize = { rows : int; cols : int; x : int; y : int }

external open_pty_raw : unit -> t * t = "ocaml_pty_open"
external get_winsize : t -> winsize = "ocaml_pty_get_winsize"

external set_winsize : t -> winsize -> unit
  = "ocaml_pty_set_winsize_byte" "ocaml_pty_set_winsize"

external setsid_and_setctty : t -> unit = "ocaml_pty_setsid_and_setctty"
external raise_fork_error : unit -> 'a = "ocaml_raise_fork_error"

let to_file_descr t = t
let close t = Unix.close t

let open_pty ?winsize () =
  let master, slave = open_pty_raw () in
  (* Set initial window size if provided *)
  (match winsize with
  | None -> ()
  | Some ws -> (
      try set_winsize master ws
      with e ->
        close master;
        close slave;
        raise e));
  (master, slave)

let inherit_size ~src ~dst =
  let ws = get_winsize src in
  set_winsize dst ws

let spawn ~prog ~argv ?winsize ?env () =
  let pty_master, pty_slave = open_pty ?winsize () in

  match Unix.fork () with
  | -1 ->
      (* Fork failed *)
      Unix.close pty_master;
      Unix.close pty_slave;
      raise_fork_error ()
  | 0 -> (
      (* Child process *)
      Unix.close pty_master;

      (* Create new session and set controlling TTY *)
      (try setsid_and_setctty pty_slave
       with _ ->
         Unix.close pty_slave;
         exit 127);

      (* Redirect stdin, stdout, stderr to the slave pty *)
      Unix.dup2 pty_slave Unix.stdin;
      Unix.dup2 pty_slave Unix.stdout;
      Unix.dup2 pty_slave Unix.stderr;

      (* The original pty_slave descriptor is no longer needed in the child
       if it has been successfully dup'ed to stdin/out/err. *)
      if
        pty_slave <> Unix.stdin && pty_slave <> Unix.stdout
        && pty_slave <> Unix.stderr
      then Unix.close pty_slave;

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
      Unix.close pty_slave;
      (pty_master, pid)

let with_pty ?winsize f =
  let master, slave = open_pty ?winsize () in
  Fun.protect
    ~finally:(fun () ->
      (* Close both, ignoring errors since one might already be closed *)
      (try close master with _ -> ());
      try close slave with _ -> ())
    (fun () -> f master slave)

(* I/O operations *)
let read t buf ofs len = Unix.read t buf ofs len
let write t buf ofs len = Unix.write t buf ofs len
let write_string t str ofs len = Unix.write_substring t str ofs len
