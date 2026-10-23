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

(* Signals *)

module Signal = struct
  let signals ~controlling =
    let ending =
      [ Sys.sigabrt; Sys.sigint; Sys.sigterm; Sys.sighup; Sys.sigquit ]
    in
    if controlling then Sys.sigwinch :: Sys.sigcont :: Sys.sigtstp :: ending
    else ending

  external take : int -> bool = "matrix_tty_take"
  external give_back : int -> unit = "matrix_tty_give_back"
  external ignored : int -> bool = "matrix_tty_ignored"
  external raise_default : int -> unit = "matrix_tty_raise"
  external stop_group : unit -> unit = "matrix_tty_stop_group"

  type holder = { id : int; wake_r : Unix.file_descr; wake_w : Unix.file_descr }

  external hold :
    bool -> Unix.file_descr option -> int * Unix.file_descr * Unix.file_descr
    = "matrix_tty_hold"

  external unhold : int -> unit = "matrix_tty_unhold"
  external received : int -> int -> bool = "matrix_tty_received"
  external terminating : int -> bool = "matrix_tty_terminating"

  external keep_settings : int -> Unix.file_descr -> unit
    = "matrix_tty_keep_settings"

  external forget_settings : int -> unit = "matrix_tty_forget_settings"
  external set_reset : int -> string -> unit = "matrix_tty_set_reset"
  external restore_all : unit -> unit = "matrix_tty_restore_all"

  let hold ~controlling ~output =
    let id, wake_r, wake_w = hold controlling output in
    { id; wake_r; wake_w }

  let release h = unhold h.id
  let wake_fd h = h.wake_r
  let byte = Bytes.make 1 'w'

  (* The pipe is non-blocking: a full one has a wake owed already. *)
  let wake h =
    try ignore (Unix.write h.wake_w byte 0 1 : int)
    with Unix.Unix_error _ -> ()

  let received h s = received h.id s
  let terminating h = terminating h.id
  let keep_settings h ~input = keep_settings h.id input
  let forget_settings h = forget_settings h.id
  let set_reset h bytes = set_reset h.id bytes

  (* [Unix.write] goes on past its first [write(2)] and, interrupted there,
     raises without saying how much went: each call here is one
     [write(2)], and the write goes on from where it stopped. *)
  let rec write h fd buf off len =
    if len > 0 then
      match Unix.single_write fd buf off len with
      | n -> write h fd buf (off + n) (len - n)
      | exception Unix.Unix_error (Unix.EAGAIN, _, _) ->
          (try ignore (Unix.select [] [ fd ] [] (-1.))
           with Unix.Unix_error (Unix.EINTR, _, _) -> ());
          write h fd buf off len
      | exception Unix.Unix_error (Unix.EINTR, _, _) ->
          if not (terminating h) then write h fd buf off len
end
