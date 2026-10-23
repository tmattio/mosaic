(** Terminal input, read into keystrokes, mouse reports and replies.

    A terminal's input is one byte stream. It carries what the user does at the
    keyboard and the mouse, the reports the program enabled (focus, bracketed
    paste, resizes, color scheme changes), and the terminal's replies to the
    program's queries. {!Decoder} reads it into one ordered stream of events. A
    decoder has no clock, and the caller says when the terminal has been silent.

    A keystroke has one value whatever the terminal's encoding ({!Keystroke}). A
    mouse report carries its own meaning, so nothing is kept between reports
    ({!Mouse_report}). A reply answers one of the program's queries, and the
    decoder tells it from keys by its form and by the replies the program is
    owed ({!Reply}, {!Decoder.section-owed}).

    This decodes the [n] bytes read into [buf] and prints each event:
    {[
    let d = Matrix_input.Decoder.create () in
    Matrix_input.Decoder.feed d buf ~off:0 ~len:n (fun event ->
        Format.printf "%a@." Matrix_input.Decoder.pp_event event)
    ]} *)

module Keystroke = Keystroke
(** Keystrokes, each with one value whatever the terminal's encoding. *)

module Mouse_report = Mouse_report
(** Mouse reports, which a terminal sends while mouse tracking is on. *)

module Reply = Reply
(** Terminal replies to the program's queries. *)

module Decoder = Decoder
(** Decoders, which read the bytes a terminal sends into events. *)
