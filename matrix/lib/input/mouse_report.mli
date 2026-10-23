(** Mouse reports, which a terminal sends for the mouse while mouse tracking is
    on.

    A terminal with mouse tracking on sends a {e report} for each button press,
    release, wheel notch and, if asked, each motion, in one of three encodings
    (xterm's Control Sequences, "Mouse Tracking"):
    - SGR (mode 1006) sends [CSI < Cb ; Cx ; Cy M] for a press or motion, and
      the final [m] for a release;
    - URXVT (mode 1015) sends [CSI Cb ; Cx ; Cy M], with Cb offset by 32;
    - X10 and normal tracking send [CSI M] followed by three raw bytes, Cb, Cx
      and Cy, each offset by 32, which limits coordinates to 223.

    Cb is a mask. Its low two bits name a button (0 left, 1 middle, 2 right; 3
    is a release that does not say which, outside SGR), 4 is Shift, 8 Meta (Alt
    on a PC keyboard), 16 Control, 32 motion ("On button-motion events, xterm
    adds 32 to the event code"), 64 a wheel or the buttons 6 and 7, and 128 the
    buttons 8 to 11. "Past button 11, the encoding is ambiguous", so a report
    with both 64 and 128 names no button, and the decoder reads it as a
    {!Reply.Csi}.

    Each report carries its own meaning. Motion with a button held says which
    button in its own bits, and a release in SGR says which button was released,
    so the decoder keeps nothing between reports. A release lost outside the
    window therefore cannot turn later motions into drags.

    Coordinates are 1-based on the wire and 0-based here, so column 1 is
    [x = 0]. A coordinate below 1 is out of the terminal's range and is read as
    the first cell, so that a release is never lost and a drag never sticks. *)

(** The type for buttons. Wheel buttons are {!Scroll} reports. *)
type button = Left | Middle | Right | Extra of int  (** Buttons 8 to 11. *)

(** The type for what a report says happened. *)
type kind =
  | Press of button
  | Release of button option
      (** [None] is X10's and URXVT's release, which does not say which button
          was released. *)
  | Motion of button option
      (** The pointer moved to another cell, with the button the report says is
          held, or [None] when no button is. *)
  | Scroll of [ `Up | `Down | `Left | `Right ]
      (** One wheel notch. Buttons 4 and 5 are up and down; 6 and 7 are left and
          right, as X numbers the horizontal wheel. *)

type t = {
  kind : kind;
  x : int;  (** The column, from 0. *)
  y : int;  (** The row, from 0. *)
  shift : bool;
  alt : bool;  (** The report's Meta bit, which is Alt on a PC keyboard. *)
  ctrl : bool;
}
(** The type for mouse reports. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf m] formats [m] for debugging, as in [Press Left at 4,7 ctrl]. *)
