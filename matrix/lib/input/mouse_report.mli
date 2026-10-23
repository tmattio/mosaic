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

(** {1:encoding Encoding}

    A terminal reports the events its {e tracking} mode asks for, in its
    {e format}, and the program sets both (xterm's Control Sequences, "Mouse
    Tracking"):
    - X10 tracking (mode 9) reports the presses of the left, middle and right
      buttons, without modifiers;
    - normal tracking (mode 1000) also reports releases, wheel notches and the
      buttons 8 to 11, with Shift, Meta and Control;
    - button-event tracking (mode 1002) also reports motion while a button is
      held, and any-event tracking (mode 1003) every motion.

    The format is SGR (mode 1006), URXVT (mode 1015) or, with neither, X10's raw
    bytes, which carry columns and rows up to 223. A release in SGR says which
    button was released, and in the others it does not. *)

type encoding = {
  tracking : [ `X10 | `Normal | `Button | `Any ];
  format : [ `X10 | `Sgr | `Urxvt ];
}
(** The type for the mouse reporting a program has set. The formats are the ones
    {!Decoder} reads. Two others are left out. Mode 1005 writes X10's
    coordinates as UTF-8, and the decoder reads an X10 report as three bytes, so
    it reads such a report right only for a position below column and row 95,
    where the two are the same bytes. Mode 1016 writes SGR's report with the
    position in pixels, which the decoder cannot tell from SGR's and reads as
    cells. A Matrix run sets neither mode. *)

val encode : encoding -> t -> string
(** [encode e m] is the report a terminal in [e] sends for [m], and [""] when
    [e] reports no such event or, in X10's format, cannot carry its position.
    {!Decoder} reads it back as [m], less what the format cannot carry: the
    button of a release outside SGR, and the modifiers in X10 tracking.

    Raises [Invalid_argument] if [m]'s position is negative or its button is
    [Extra n] with [n] outside 8..11. *)
