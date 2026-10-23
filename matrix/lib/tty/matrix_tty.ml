(* The device *)

let is_tty fd = try Unix.isatty fd with Unix.Unix_error _ -> false

external is_controlling : Unix.file_descr -> bool = "matrix_tty_is_controlling"
external window : Unix.file_descr -> (int * int) option = "matrix_tty_window"
external now : unit -> int = "matrix_tty_now" [@@noalloc]

(* Settings *)

external get_iexten : Unix.file_descr -> bool = "matrix_tty_get_iexten"
external set_iexten : Unix.file_descr -> bool -> unit = "matrix_tty_set_iexten"

type settings = { termios : Unix.terminal_io; iexten : bool }

let raw fd =
  let termios = Unix.tcgetattr fd in
  let iexten = get_iexten fd in
  Unix.tcsetattr fd Unix.TCSANOW
    {
      termios with
      c_echo = false;
      c_icanon = false;
      c_isig = false;
      c_vmin = 1;
      c_vtime = 0;
      c_ixon = false;
      c_icrnl = false;
    };
  (* The record write above leaves IEXTEN as it was. *)
  set_iexten fd false;
  { termios; iexten }

let restore fd s =
  Unix.tcsetattr fd Unix.TCSADRAIN s.termios;
  set_iexten fd s.iexten

let flush_input fd =
  try Unix.tcflush fd Unix.TCIFLUSH with Unix.Unix_error _ -> ()

external enable_vt : Unix.file_descr -> unit = "matrix_tty_enable_vt"

let enable_vt fd = try enable_vt fd with Failure _ -> ()
