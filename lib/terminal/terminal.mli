(** Terminal control and capabilities *)

type t
(** A terminal handle *)

type mode = [ `Raw | `Cooked | `Custom of Unix.terminal_io -> Unix.terminal_io ]
(** Terminal input mode. [`Raw] disables echo, canonical mode, and signals.
    [`Cooked] is the default mode. [`Custom] allows manual termios
    configuration. *)

val create : ?tty:bool -> Unix.file_descr -> Unix.file_descr -> t
(** [create ~tty input output] creates a terminal handle. [tty] indicates if
    this is a real TTY (default: autodetect) *)

val release : t -> unit
(** Release the terminal, restoring original settings *)

val save_state : t -> unit
(** Save current terminal state (termios and escape sequence state) *)

val restore_state : t -> unit
(** Restore saved terminal state *)

val set_mode : t -> mode -> unit
(** Set terminal input mode (affects termios settings) *)

(** {2 Terminal Features} *)

val enable_alternate_screen : t -> unit
(** Switch to alternate screen buffer *)

val disable_alternate_screen : t -> unit
(** Switch back to main screen buffer *)

val enable_mouse : t -> unit
(** Enable mouse tracking *)

val disable_mouse : t -> unit
(** Disable mouse tracking *)

val enable_bracketed_paste : t -> unit
(** Enable bracketed paste mode *)

val disable_bracketed_paste : t -> unit
(** Disable bracketed paste mode *)

val show_cursor : t -> unit
(** Show the cursor *)

val hide_cursor : t -> unit
(** Hide the cursor *)

val size : t -> int * int
(** Get terminal dimensions (width, height) *)

val is_tty : Unix.file_descr -> bool
(** Check if a file descriptor is a TTY *)

val input_fd : t -> Unix.file_descr
(** Get the input file descriptor *)

val output_fd : t -> Unix.file_descr
(** Get the output file descriptor *)

val write : t -> bytes -> int -> int -> unit
(** Write bytes to the terminal *)

val read_input :
  t -> timeout:float option -> [ `Input of bytes * int | `Timeout | `Eof ]
(** Read input with optional timeout in seconds *)

val flush : t -> unit
(** Flush output *)

(** {2 Signal Handling} *)

type sigwinch_handler = int * int -> unit

val set_sigwinch_handler : sigwinch_handler option -> unit
(** Set handler for terminal resize events *)

(** {2 Testing Support} *)

val create_from_strings : string -> t * (unit -> string)
(** Create a terminal from input string, returns terminal and function to get
    output. Useful for pure testing without Unix dependencies. *)
