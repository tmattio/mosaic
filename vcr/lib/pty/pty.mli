(** A library for using Unix pseudo-terminals.

    {b Note:} This library is currently supported on Linux and macOS. *)

type t
(** An abstract type representing a pseudo-terminal handle. *)

type winsize = { rows : int; cols : int; x : int; y : int }
(** A record representing the terminal window size. *)

val to_file_descr : t -> Unix.file_descr
(** [to_file_descr pty] returns the underlying Unix file descriptor for the pty.
*)

val close : t -> unit
(** [close pty] closes the pseudo-terminal. This is equivalent to
    [Unix.close (to_file_descr pty)] but provided for convenience. *)

val open_pty : ?winsize:winsize -> unit -> t * t
(** [open_pty ?winsize ()] opens a new pseudo-terminal, returning a pair of PTY
    handles for the master and slave ends, respectively.

    If [winsize] is provided, it sets the initial terminal size.

    @param winsize optional initial terminal size
    @raise Unix.Unix_error if the operation fails.
    @return (master, slave) *)

val get_winsize : t -> winsize
(** [get_winsize fd] returns the window size of the terminal associated with the
    file descriptor [fd].

    @raise Unix.Unix_error on failure. *)

val set_winsize : t -> winsize -> unit
(** [set_winsize fd ws] sets the window size of the terminal associated with the
    file descriptor [fd].

    @raise Unix.Unix_error on failure. *)

val inherit_size : src:t -> dst:t -> unit
(** [inherit_size ~src ~dst] reads the window size from the terminal [src] and
    applies it to the terminal [dst]. This is useful for propagating window size
    changes (e.g., from a SIGWINCH signal). *)

val spawn :
  prog:string ->
  argv:string array ->
  ?winsize:winsize ->
  ?env:string array ->
  unit ->
  t * int
(** [spawn ~prog ~argv ?winsize ?env ()] starts a new command in a
    pseudo-terminal. This function: 1. Creates a new pty/tty pair. 2. Forks a
    new process. 3. In the child, makes the tty its controlling terminal and
    connects its stdin, stdout, and stderr to it. 4. In the child, executes the
    command [prog] with arguments [argv]. 5. In the parent, closes the tty and
    returns the pty and the child's PID.

    If [winsize] is provided, it sets the initial terminal size. If [env] is
    provided, it sets the environment variables for the child process.

    @raise Unix.Unix_error on failure.

    @return (pty, pid) *)

val with_pty : ?winsize:winsize -> (t -> t -> 'a) -> 'a
(** [with_pty ?winsize f] opens a new pseudo-terminal pair, applies function [f]
    to the master and slave PTY handles, and ensures that both PTYs are closed
    after [f] completes (even if [f] raises an exception).

    If [winsize] is provided, it sets the initial terminal size.

    @param winsize optional initial terminal size
    @param f a function that receives the master and slave PTY handles
    @return the result of applying [f]
    @raise any exception raised by [f] (after closing the PTYs) *)

(** {2 Input/Output Operations} *)

val read : t -> bytes -> int -> int -> int
(** [read pty buf ofs len] reads up to [len] bytes from [pty], storing them in
    [buf] starting at position [ofs]. Returns the number of bytes actually read.

    This is equivalent to [Unix.read (to_file_descr pty) buf ofs len] but
    provided for convenience.

    @return the number of bytes read (0 indicates end-of-file)
    @raise Unix.Unix_error on error *)

val write : t -> bytes -> int -> int -> int
(** [write pty buf ofs len] writes up to [len] bytes from [buf] starting at
    position [ofs] to [pty]. Returns the number of bytes actually written.

    This is equivalent to [Unix.write (to_file_descr pty) buf ofs len] but
    provided for convenience.

    @return the number of bytes written
    @raise Unix.Unix_error on error *)

val write_string : t -> string -> int -> int -> int
(** [write_string pty str ofs len] writes up to [len] characters from [str]
    starting at position [ofs] to [pty]. Returns the number of bytes actually
    written.

    This is equivalent to [Unix.write_substring (to_file_descr pty) str ofs len]
    but provided for convenience.

    @return the number of bytes written
    @raise Unix.Unix_error on error *)

(** {2 Non-blocking I/O} *)

val set_nonblock : t -> unit
(** [set_nonblock pty] sets the file descriptor to non-blocking mode.

    This is equivalent to [Unix.set_nonblock (to_file_descr pty)] but provided
    for convenience. *)

val clear_nonblock : t -> unit
(** [clear_nonblock pty] clears the non-blocking mode on the file descriptor.

    This is equivalent to [Unix.clear_nonblock (to_file_descr pty)] but provided
    for convenience. *)
