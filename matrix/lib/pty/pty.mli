(** Unix pseudo-terminal (PTY) support for terminal emulation.

    This module provides cross-platform PTY operations for creating and managing
    pseudo-terminals, spawning processes within them, and controlling terminal
    properties. PTYs are bidirectional communication channels that emulate a
    terminal device, allowing programs to interact with processes as if they
    were running in a real terminal.

    {1 PTY Overview}

    A pseudo-terminal consists of two sides:
    - {b Master}: The controlling side, used by the terminal emulator to send
      input and receive output
    - {b Slave}: The controlled side, connected to a child process as its
      controlling terminal

    The master and slave communicate bidirectionally. Data written to the master
    appears on the slave's stdin, and data written to the slave's stdout/stderr
    appears on the master.

    {1 Basic Usage}

    Spawn a process and interact with it:
    {@ocaml[
      (* Spawn a shell in a PTY *)
      let pty = Pty.spawn ~prog:"/bin/bash" ~args:[] () in

      (* Send a command *)
      let cmd = "echo hello\n" in
      let _ = Pty.write_string pty cmd 0 (String.length cmd) in

      (* Read output from the shell *)
      let buf = Bytes.create 4096 in
      let n = Pty.read pty buf 0 (Bytes.length buf) in
      Printf.printf "Output: %s\n" (Bytes.sub_string buf 0 n);

      (* Clean up *)
      Pty.close pty
    ]}

    For automatic resource cleanup, use {!with_spawn}:
    {@ocaml[
      let output =
        Pty.with_spawn ~prog:"echo" ~args:[ "hello" ] (fun pty ->
            let buf = Bytes.create 1024 in
            let n = Pty.read pty buf 0 (Bytes.length buf) in
            Bytes.sub_string buf 0 n)
      in
      Printf.printf "Got: %s\n" output
    ]}

    {1 Platform Support}

    {2 POSIX (Linux, macOS)}

    Full support via POSIX PTY APIs (posix_openpt, grantpt, unlockpt):
    - All operations supported
    - File descriptors are true Unix file descriptors
    - [Unix.select] and [Unix.poll] work correctly
    - SIGWINCH delivered to child processes on resize

    {2 Windows (ConPTY)}

    Limited support via Windows Console Pseudo-terminal (ConPTY), available on
    Windows 10 version 1809 (October 2018) and later:
    - {!spawn} is not yet supported (requires CreateProcess API integration)
    - {!open_pty} creates a ConPTY with pipe handles for I/O
    - {!get_winsize} returns the last size set via {!set_winsize} or the initial
      size, as ConPTY provides no direct query API
    - {!set_winsize} works via [ResizePseudoConsole]
    - I/O operations use Windows named pipes wrapped as Unix file descriptors
    - Custom OCaml blocks ensure GC safety; handles cleaned up automatically

    {1 Process Management}

    PTYs created with {!spawn} automatically track the child process ID. The
    module ensures proper cleanup:
    - {!close} sends SIGTERM, waits briefly, then SIGKILL if needed, and calls
      waitpid to prevent zombies
    - {!terminate} and {!kill} send signals without closing the PTY
    - Always call {!close} to prevent resource leaks

    Process lifecycle example:
    {@ocaml[
      let pty = Pty.spawn ~prog:"cat" ~args:[] () in

      (* Get the child PID *)
      match Pty.pid pty with
      | Some pid -> Printf.printf "Child PID: %d\n" pid
      | None -> assert false

      (* Graceful shutdown *)
      Pty.terminate pty;  (* Send SIGTERM *)
      Unix.sleepf 0.1;    (* Wait for graceful exit *)
      Pty.close pty       (* Clean up and reap *)
    ]}

    {1 Async Integration}

    This library uses blocking I/O by default. For async/concurrent usage:

    {2 Using Unix.select}

    Enable non-blocking mode and use [Unix.select] for readiness notification:
    {@ocaml[
      let pty = Pty.spawn ~prog:"cat" ~args:[] () in
      Pty.set_nonblock pty;

      let rec read_loop () =
        let read_fds = [ Pty.in_fd pty ] in
        let ready, _, _ = Unix.select read_fds [] [] 0.1 in
        if ready <> [] then
          try
            let buf = Bytes.create 4096 in
            let n = Pty.read pty buf 0 (Bytes.length buf) in
            if n > 0 then (
              (* Process the output *)
              Printf.printf "%s" (Bytes.sub_string buf 0 n);
              read_loop ())
          with Unix.Unix_error (Unix.EAGAIN, _, _) -> read_loop ()
        else read_loop ()
      in
      read_loop ();
      Pty.close pty
    ]}

    {2 Using Lwt or Async}

    Wrap the file descriptor with [Lwt_unix.of_unix_file_descr] or equivalent:
    {@ocaml[
      (* With Lwt *)
      let lwt_fd = Lwt_unix.of_unix_file_descr (Pty.file_descr pty) in
      Lwt_unix.read lwt_fd buf 0 (Bytes.length buf)
    ]}

    Note: The [Unix.sleepf] call in {!close} is synchronous. For fully
    non-blocking cleanup, use [wait:false] and implement custom reaping.

    {1 Thread Safety}

    This module is {b not} thread-safe. The {!t} type contains mutable state
    (the [pid] field) that should not be accessed concurrently from multiple
    threads without external synchronization (e.g., a mutex).

    However, C stubs release the OCaml runtime lock during blocking operations
    (via [caml_enter_blocking_section]/[caml_leave_blocking_section]), allowing
    other OCaml threads to run while I/O is in progress.

    {1 Resource Management}

    {b Invariants:}
    - Each {!t} value owns a file descriptor that must be closed
    - {!spawn} PTYs also own a child process that must be reaped
    - {!close} is idempotent; calling it multiple times is safe
    - After {!close}, I/O operations raise [Unix.Unix_error (Unix.EBADF, _, _)]

    {b Best Practices:}
    - Always call {!close} or use {!with_pty}/{!with_spawn} for cleanup
    - Call {!close} with [wait:true] (default) to prevent zombie processes
    - If using [wait:false], manually call [Unix.waitpid] to reap the child
    - Read until EOF (return value 0) to detect child process exit *)

(** {1 Types} *)

type t
(** A handle to a pseudo-terminal.

    Represents either the master or slave side of a PTY pair. Created by
    {!open_pty} (which returns both master and slave) or {!spawn} (which returns
    the master and manages the slave internally).

    Each [t] value owns a file descriptor and possibly a child process (for
    spawned PTYs). After {!close}, the [t] value is invalidated; further I/O
    operations raise [Unix.Unix_error (EBADF, _, _)]. For handles created by
    {!spawn}, {!pid} returns [Some pid] until {!close} reaps the child and
    clears the PID; handles created via {!open_pty} or already closed always
    report [None]. *)

type winsize = {
  rows : int;  (** Number of rows (lines) *)
  cols : int;  (** Number of columns (characters per line) *)
  xpixel : int;  (** Width in pixels (often unused, set to 0) *)
  ypixel : int;  (** Height in pixels (often unused, set to 0) *)
}
(** Terminal window size information.

    Used to inform applications of the terminal dimensions. Applications like
    text editors and pagers use this to layout content appropriately. Provide
    non-negative values; the OS rejects negative dimensions with
    [Unix.Unix_error (Unix.EINVAL, _, _)]. *)

(** {1 PTY Creation} *)

val open_pty : ?winsize:winsize -> unit -> t * t
(** [open_pty ?winsize ()] creates a new pseudo-terminal pair.

    Returns [(master, slave)] where [master] is the controlling side used by the
    terminal emulator, and [slave] is the controlled side connected to a child
    process. Both handles must be closed with {!close} when done. The slave side
    should be connected to a child process's stdin/stdout/stderr via
    [Unix.dup2].

    Raises [Unix.Unix_error] if PTY creation fails (e.g., resource limits
    reached).

    @param winsize Initial terminal size. Defaults to system default if omitted.
    @return A pair [(master, slave)] of PTY handles *)

val spawn :
  ?env:string array ->
  ?cwd:string ->
  ?winsize:winsize ->
  prog:string ->
  args:string list ->
  unit ->
  t
(** [spawn ?env ?cwd ?winsize ~prog ~args ()] spawns a program in a new PTY.

    Creates a PTY pair, forks a child process, configures the child with the
    slave PTY as its controlling terminal, and executes the program. Returns the
    master PTY handle for communication with the child. The slave is
    automatically closed in the parent.

    The child process is configured with:
    - A new session (via [setsid])
    - The slave PTY as its controlling terminal (via [TIOCSCTTY])
    - [stdin], [stdout], and [stderr] redirected to the slave PTY
    - Optional custom working directory and environment

    @param env
      Environment variables for the child as an array of ["KEY=value"] strings.
      If omitted, inherits the parent's environment. Passed to [Unix.execvpe]
      when provided; otherwise [Unix.execvp] is used.
    @param cwd
      Working directory for the child. If omitted, inherits the parent's working
      directory. The child calls [Unix.chdir] before exec; if this fails, the
      child exits with the errno code.
    @param winsize Initial terminal size. If omitted, uses system default.
    @param prog
      Program to execute. If relative or a basename, searched in [PATH] (via
      [execvp]/[execvpe]). If absolute, executed directly.
    @param args
      Command-line arguments (excluding [argv[0]], which is set to [prog]).

    Raises [Unix.Unix_error] if PTY creation or fork fails. Exec failures in the
    child process are not reported as exceptions to the parent; the child exits
    with the errno value as its exit code (e.g., ENOENT=2, EACCES=13). Monitor
    via [Unix.waitpid] on {!pid}.

    @return Master PTY handle. The child's PID is available via {!pid}.

    {b Note:} The child process terminates if the master PTY is closed, as it
    receives [SIGHUP] when its controlling terminal disappears. *)

val with_pty : ?winsize:winsize -> (t -> t -> 'a) -> 'a
(** [with_pty ?winsize f] opens a PTY pair with automatic cleanup.

    Creates a master/slave PTY pair, applies [f master slave], and ensures both
    handles are closed via [Fun.protect] even if [f] raises an exception. Use
    this for custom process spawning or testing scenarios.

    Raises [Unix.Unix_error] if PTY creation fails or the initial size cannot be
    applied. Any exception raised by [f] is re-raised after both PTYs are
    closed.

    @param winsize Initial terminal size
    @param f Function receiving [master] then [slave] PTY handles
    @return Result of [f] *)

(** {1 PTY Operations} *)

val pid : t -> int option
(** [pid pty] returns the child process ID. Returns [Some pid] for PTYs created
    by {!spawn} until {!close} reaps the process and clears the PID. Returns
    [None] for PTYs created via {!open_pty} or after {!close}. *)

val close : ?wait:bool -> t -> unit
(** [close ?wait pty] closes the pseudo-terminal and releases resources.

    Closes the file descriptor. For PTYs created by {!spawn}, also terminates
    the child process via the following sequence:

    - Sends SIGTERM for graceful shutdown
    - If [wait] is [true] (default), sleeps 100ms, checks whether the process
      exited with [Unix.waitpid [Unix.WNOHANG] pid], and escalates to SIGKILL
      before waiting for completion if it is still running
    - Reaps the child with [Unix.waitpid] to prevent zombie processes

    @param wait
      Whether to wait for child termination. Defaults to [true]. Set to [false]
      for non-blocking close, but you must manually call [Unix.waitpid] on the
      PID to prevent zombie processes.

    This function is idempotent and safe to call multiple times. Subsequent
    calls are no-ops. After closing, I/O operations raise
    [Unix.Unix_error (Unix.EBADF, _, _)]. Errors from [Unix.kill] or
    [Unix.close] are ignored, so [close] itself never raises. *)

val terminate : t -> unit
(** [terminate pty] sends SIGTERM to the child process without closing the PTY.
    Only affects PTYs created by {!spawn}.

    Unix errors from [Unix.kill] are silently ignored (e.g., [ESRCH] if the
    process already exited).

    Raises [Invalid_argument] if [pty] has no associated child process (created
    via {!open_pty} or already closed). *)

val kill : t -> unit
(** [kill pty] sends SIGKILL to forcefully terminate the child process. Only
    affects PTYs created by {!spawn}. Unlike {!terminate}, this signal cannot be
    caught or ignored by the child.

    Unix errors from [Unix.kill] are silently ignored (e.g., [ESRCH] if the
    process already exited).

    Raises [Invalid_argument] if [pty] has no associated child process (created
    via {!open_pty} or already closed). *)

val file_descr : t -> Unix.file_descr
(** [file_descr pty] returns the underlying Unix file descriptor for use with
    [Unix.select], [Unix.poll], or async I/O libraries. The descriptor becomes
    invalid immediately after {!close}. *)

val in_fd : t -> Unix.file_descr
(** [in_fd pty] returns the file descriptor for reading from the PTY (i.e.,
    reading child process output). For PTY handles, this is the same as
    {!file_descr} and {!out_fd}, as PTYs use a single bidirectional file
    descriptor. The descriptor shares the same lifetime as {!file_descr}. *)

val out_fd : t -> Unix.file_descr
(** [out_fd pty] returns the file descriptor for writing to the PTY (i.e.,
    sending input to the child process). For PTY handles, this is the same as
    {!file_descr} and {!in_fd}, as PTYs use a single bidirectional file
    descriptor. The descriptor shares the same lifetime as {!file_descr}. *)

(** {1 Window Size Management} *)

val get_winsize : t -> winsize
(** [get_winsize pty] retrieves the current terminal window size.

    On POSIX systems, queries the PTY's current size. On Windows (ConPTY),
    returns the last size set via {!set_winsize} or the initial size, as ConPTY
    lacks a direct query API.

    Raises [Unix.Unix_error] if the ioctl fails (POSIX) or handle is invalid.

    @return The current window size *)

val set_winsize : t -> winsize -> unit
(** [set_winsize pty ws] sets the terminal window size.

    Updates the PTY's window size and triggers SIGWINCH in child processes that
    have this PTY as their controlling terminal. Applications listening for
    SIGWINCH will adapt to the new dimensions (e.g., text editors, shells).

    Raises [Unix.Unix_error] if the operation fails.

    @param ws
      The new window size. [xpixel] and [ypixel] are often unused; set to [0] if
      unknown. *)

val resize : t -> rows:int -> cols:int -> unit
(** [resize pty ~rows ~cols] resizes the terminal. Convenience wrapper for
    {!set_winsize} that sets [xpixel] and [ypixel] to [0].

    Raises [Unix.Unix_error] if the operation fails.

    @param rows Number of rows (lines)
    @param cols Number of columns (characters per line) *)

val inherit_size : src:t -> dst:t -> unit
(** [inherit_size ~src ~dst] copies the window size from [src] to [dst].

    Reads the size from [src] using {!get_winsize} and writes it to [dst] using
    {!set_winsize}. Useful for propagating terminal resize events between PTYs.

    Example of handling SIGWINCH to resize a spawned process:
    {@ocaml[
      let () =
        Sys.set_signal Sys.sigwinch
          (Sys.Signal_handle
             (fun _ -> Pty.inherit_size ~src:controlling_tty ~dst:child_pty))
    ]}

    Raises [Unix.Unix_error] if reading from [src] or writing to [dst] fails.

    @param src Source PTY to read size from
    @param dst Destination PTY to write size to *)

(** {1 I/O Operations} *)

val read : t -> bytes -> int -> int -> int
(** [read pty buf ofs len] reads up to [len] bytes from the PTY.

    Reads from the PTY and stores data in [buf] starting at offset [ofs].
    Returns the number of bytes actually read, which may be less than [len] if
    less data is available. Returns [0] on EOF (writing end closed, typically
    when child process exits).

    Blocks until data is available unless the PTY is in non-blocking mode (see
    {!set_nonblock}). In non-blocking mode, raises
    [Unix.Unix_error (EAGAIN, _, _)] if no data is ready.

    Raises [Unix.Unix_error] on error (e.g., [EAGAIN] in non-blocking mode,
    [EBADF] if PTY is closed).

    @return Number of bytes read ([0] indicates EOF) *)

val write : t -> bytes -> int -> int -> int
(** [write pty buf ofs len] writes up to [len] bytes to the PTY.

    Writes data from [buf] starting at offset [ofs] to the PTY. Returns the
    number of bytes actually written, which may be less than [len] if the PTY's
    internal buffer is full.

    Blocks until at least some data can be written unless the PTY is in
    non-blocking mode.

    Raises [Unix.Unix_error] on error (e.g., [EAGAIN] in non-blocking mode,
    [EPIPE] if child closed its end).

    @return Number of bytes actually written *)

val write_string : t -> string -> int -> int -> int
(** [write_string pty str ofs len] writes a substring to the PTY. Equivalent to
    {!write} but accepts a string instead of bytes.

    Raises [Unix.Unix_error] on error.

    @return Number of bytes actually written *)

(** {1 Non-blocking I/O} *)

val set_nonblock : t -> unit
(** [set_nonblock pty] enables non-blocking mode on the PTY.

    After calling this, {!read} and {!write} operations return immediately
    rather than blocking. If an operation would block, it raises
    [Unix.Unix_error (EAGAIN, _, _)] instead. Essential for event-driven I/O,
    integration with [Unix.select]/[Unix.poll], and async libraries.

    This setting persists until {!clear_nonblock} is called or the PTY is
    closed.

    Raises [Unix.Unix_error] if toggling non-blocking mode fails (e.g., the file
    descriptor has already been closed). *)

val clear_nonblock : t -> unit
(** [clear_nonblock pty] disables non-blocking mode, restoring default blocking
    behavior.

    Raises [Unix.Unix_error] if toggling blocking mode fails. *)

(** {1 Higher-Level Utilities} *)

val with_spawn :
  ?env:string array ->
  ?cwd:string ->
  ?winsize:winsize ->
  prog:string ->
  args:string list ->
  (t -> 'a) ->
  'a
(** [with_spawn ?env ?cwd ?winsize ~prog ~args f] spawns a process with
    automatic cleanup.

    Combines {!spawn} with resource management via [Fun.protect]. The PTY is
    automatically closed (and child process terminated and reaped) when [f]
    returns or raises an exception, ensuring no resource leaks or zombie
    processes. Relies on {!spawn}, so it is only available on platforms with
    POSIX-style [Unix.fork].

    Example running a command and capturing output:
    {@ocaml[
      let output =
        Pty.with_spawn ~prog:"/bin/echo" ~args:[ "hello" ] (fun pty ->
            let buf = Bytes.create 1024 in
            let n = Pty.read pty buf 0 (Bytes.length buf) in
            Bytes.sub_string buf 0 n)
      in
      assert (output = "hello\n")
    ]}

    @param env Environment variables for the child
    @param cwd Working directory for the child
    @param winsize Initial terminal size
    @param prog Program path (searched in PATH if relative)
    @param args Command-line arguments (excluding argv[0])

    Raises [Unix.Unix_error] if PTY creation or fork fails before [f] runs. Any
    exception raised by [f] is re-raised after PTY cleanup.

    @param f Function receiving the PTY handle
    @return Result of [f] *)
