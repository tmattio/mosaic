(** Terminal input.

    A terminal's input is one byte stream: what the user does at the keyboard
    and the mouse, the reports the program enabled (focus, bracketed paste,
    resizes, color scheme changes), and the terminal's replies to the program's
    queries. {!Decoder} reads it into one ordered stream of events, with no
    clock: the caller says when the terminal has been silent.

    A keystroke has one value whatever the terminal's encoding ({!Keystroke}), a
    mouse report carries its own meaning ({!Mouse_report}), and a reply is never
    a key ({!Reply}).

    {[
    let d = Matrix_input.Decoder.create () in
    Matrix_input.Decoder.feed d buf ~off:0 ~len:n (fun event ->
        Format.printf "%a@." Matrix_input.Decoder.pp_event event)
    ]}

    {b Warning.} A decoder is not thread-safe; use one per input source. *)

module Keystroke = Keystroke
(** Keystrokes. See {!Keystroke}. *)

module Mouse_report = Mouse_report
(** Mouse reports. See {!Mouse_report}. *)

module Reply = Reply
(** Terminal replies. See {!Reply}. *)

module Decoder = Decoder
(** Decoding terminal input. See {!Decoder}. *)
