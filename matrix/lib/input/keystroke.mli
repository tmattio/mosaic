(** Keystrokes, each with one value whatever the terminal's encoding.

    A {e chord} is a key pressed while Ctrl, Alt, Super, Hyper or Meta is held.
    A terminal reports a key in one of several {e encodings}, and each carries
    its own set of facts:
    - {e legacy bytes} send the text a key types, a C0 control for Ctrl with a
      letter, and an ESC before either for Alt;
    - xterm's control sequences encode keys that type nothing, such as [CSI A]
      or [CSI 1 ; 5 A], whose second parameter is 1 plus a mask of modifiers,
      and SS3 forms such as [ESC O P] (xterm's Control Sequences, "PC-Style
      Function Keys");
    - xterm's modifyOtherKeys, [CSI 27 ; m ; code ~], encodes the chords that
      legacy bytes cannot tell apart;
    - the kitty keyboard protocol sends
      [CSI code:shifted:base ; m:event ; text u].

    A keystroke here has the one value that every encoding carrying the same
    facts agrees on. When an encoding cannot carry a fact, the value leaves it
    out, and the sections below state each such loss where it occurs. A
    {e binding} is a program's test of whether a keystroke is a given key or
    chord.

    This binding matches Ctrl+s whatever the encoding and the state of the
    locks:
    {[
    let is_save (k : Keystroke.t) =
      k.key = Keystroke.Char (Uchar.of_char 's')
      && k.mods.ctrl
      && not (k.mods.alt || k.mods.super || k.mods.hyper || k.mods.meta)
    ]}

    {1:chars Characters}

    [Char c] is a key that types a character, and [c] is the character typed,
    with Shift and the keyboard layout applied. Shift+a is [Char 'A'], and
    Shift+4 on a US layout is [Char '$']. Legacy bytes and modifyOtherKeys carry
    that character. The kitty protocol carries it in the shifted key it reports
    with its "report alternate keys" enhancement, and in the text it reports
    with a key that types one character when it sends every key as a sequence
    ("report associated text"). Shift is then consumed, in the sense that
    [shift] is set on a [Char] only when Shift did not change the character, as
    for Shift+Space. A binding for Ctrl+Shift+g therefore matches [Char 'G']
    with [ctrl].

    The kitty protocol without that enhancement carries less. Its key code is
    "always the lower-case (or more technically, un-shifted) version of the key"
    (kitty keyboard protocol, "Key codes"), so Ctrl+Shift+1 arrives as
    [Char '1'] with [ctrl] and [shift], where every other encoding gives
    [Char '!'] with [ctrl]. The value is then that key with [shift], since the
    bytes do not carry the character typed.

    {1:locks Locks}

    Lock keys never change a chord's key. A character typed with Caps Lock on
    arrives as text with the lock already applied, so the [Char] is what was
    typed. Caps Lock with r is [Char 'R'], from the byte R or from the kitty
    protocol's [CSI 114 ; 65 ; 82 u], whose text is R. In a chord the lock
    changes nothing. Ctrl+s is [Char 's'] with [ctrl] whatever Caps Lock's
    state, as xterm sends it (0x13) and as the kitty protocol does
    ([CSI 115 ; 69 u]), and [caps_lock] says the lock is on. The kitty protocol
    reports the lock bits on chords and legacy bytes never do, so a binding must
    ignore [caps_lock] and [num_lock]. Legacy bytes lose this fact under Alt.
    With Caps Lock on, Alt+a arrives as ESC A, [Char 'A'] with [alt], and
    Alt+Shift+a as ESC a, because the terminal applies the lock to the character
    before it adds the ESC.

    {1:controls Controls}

    Legacy bytes send Ctrl with a key as a C0 control (kitty keyboard protocol,
    "Legacy ctrl mapping of ASCII keys"). 0x01..0x1A are Ctrl+a..z, 0x00 is
    Ctrl+Space, and 0x1C..0x1F are Ctrl+\\, Ctrl+\], Ctrl+^ and Ctrl+_. Four
    controls are named keys, because legacy bytes cannot tell them from a chord.
    0x09 is [Tab] (also Ctrl+i), 0x0D [Enter] (Ctrl+m), 0x1B [Escape] (Ctrl+\[),
    and 0x08 and 0x7F are [Backspace] (0x08 is also Ctrl+h, and Ctrl+Backspace
    in some terminals). 0x0A is Ctrl+j. Enter sends 0x0D, but a terminal device
    that translates carriage returns on input (the termios flag ICRNL, set
    unless the terminal is in raw mode) delivers Enter as 0x0A, and Enter then
    arrives as Ctrl+j. Legacy bytes also lose Shift under Ctrl (Ctrl+Shift+a is
    Ctrl+a), and some chords alias a control (Ctrl+2 is Ctrl+Space). Under the
    kitty protocol's disambiguation all of these are distinct.

    {1:keypad The keypad}

    [keypad] is set when the encoding says the key is on the numeric keypad, as
    the kitty protocol's keypad codes and xterm's application keypad (the SS3
    forms of xterm's "PC-Style Function Keys") do. Legacy bytes send a keypad
    key as its main-keyboard equivalent ("All keypad keys are reported as their
    equivalent non-keypad keys", kitty keyboard protocol, "Legacy functional
    keys"), so the fact is absent there, and keypad 8 without Num Lock is [Up]
    without [keypad]. A binding ignores [keypad] unless it means the keypad.
    [Begin], the keypad's 5 without Num Lock, exists only on the keypad and
    always has [keypad]. *)

(** {1:mods Modifiers} *)

type mods = {
  shift : bool;
  alt : bool;
      (** Alt, or Option on a Mac. It is the kitty protocol's alt bit, xterm's
          Alt, and an ESC before a key. *)
  ctrl : bool;
  super : bool;
      (** The Windows or Command key. xterm's table names the bit worth 8 Meta;
          the kitty protocol, which reuses the same parameter, names it Super,
          and this is that bit. *)
  hyper : bool;
  meta : bool;
      (** A Meta key distinct from Alt, which only the kitty protocol reports
          (its bit worth 32). *)
  caps_lock : bool;  (** Caps Lock is on, whether or not its key is held. *)
  num_lock : bool;  (** Num Lock is on, whether or not its key is held. *)
}
(** The type for modifiers. The kitty protocol and xterm encode them as one
    parameter, 1 plus the sum of shift 1, alt 2, ctrl 4, super 8, hyper 16, meta
    32, caps_lock 64 and num_lock 128 (kitty keyboard protocol, "Modifiers"). *)

val no_mods : mods
(** [no_mods] is the modifiers with every field [false]. *)

(** {1:keys Keys} *)

(** The type for media keys (kitty keyboard protocol, "Functional key
    definitions"). *)
type media =
  | Play
  | Pause
  | Play_pause
  | Reverse
  | Stop
  | Fast_forward
  | Rewind
  | Track_next
  | Track_previous
  | Record
  | Volume_down
  | Volume_up
  | Mute

(** The type for modifier keys pressed or released on their own, which the kitty
    protocol reports with its "report all keys as escape codes" enhancement. *)
type modifier =
  | Left_shift
  | Left_ctrl
  | Left_alt
  | Left_super
  | Left_hyper
  | Left_meta
  | Right_shift
  | Right_ctrl
  | Right_alt
  | Right_super
  | Right_hyper
  | Right_meta
  | Level3_shift  (** ISO level 3 shift, AltGr on many layouts. *)
  | Level5_shift  (** ISO level 5 shift. *)

(** The type for keys. *)
type key =
  | Char of Uchar.t  (** A key that types a character ({!section-chars}). *)
  | Enter
  | Tab
  | Backspace
  | Escape
  | Up
  | Down
  | Left
  | Right
  | Home
  | End
  | Page_up
  | Page_down
  | Insert
  | Delete
  | Begin  (** The keypad's 5 without Num Lock. *)
  | F of int
      (** A function key: F1..F20 from xterm's forms, F1..F35 from the kitty
          protocol. *)
  | Print_screen
  | Pause
  | Menu
  | Scroll_lock
  | Caps_lock
  | Num_lock
  | Media of media
  | Modifier of modifier
  | Unidentified of int
      (** A code in the kitty protocol's functional range, 57344..63743, that
          names none of the keys above. *)

(** {1:strokes Keystrokes} *)

type release = {
  key : key;
  mods : mods;
      (** For a modifier key, the state after the release. Releasing Left Ctrl
          clears [ctrl] unless Right Ctrl is still held. *)
  base : Uchar.t option;
  keypad : bool;
}
(** The type for key releases. Only the kitty protocol reports them, with its
    "report event types" enhancement. A release types nothing, so it has no
    [text]. *)

type t = {
  key : key;
  mods : mods;
  text : string;
      (** What the key types, as UTF-8. It is the kitty protocol's associated
          text when the key reports one, else the character of a [Char]. It is
          [""] for a chord, which types nothing, for a control character, for a
          key other than a [Char] that reports no text, and when the encoding
          does not carry the character typed. *)
  base : Uchar.t option;
      (** The key at the same place on a US PC-101 layout, when the terminal
          reports it (the kitty protocol's base layout key). Ctrl+С on a
          Cyrillic layout has [base = Some 'c'], so a binding for Ctrl+c can
          match it. *)
  keypad : bool;
      (** The encoding says the key is on the keypad ({!section-keypad}). *)
  repeat : bool;
      (** The terminal reports an auto-repeat (the kitty protocol's event
          types). The other encodings send a repeat as a press, with [repeat]
          [false]. *)
}
(** The type for key presses. *)

val is_interrupt : t -> bool
(** [is_interrupt k] is [true] iff [k] is Ctrl+C, the key a terminal in cooked
    mode turns into SIGINT (its VINTR character): Ctrl with c, Shift allowed
    (the kitty protocol may report C), or with a key whose {!field-base} is c,
    and no other modifier. In raw mode the terminal sends it as this key, and
    the application decides what it means. *)

(** {1:fmt Formatting} *)

val pp_key : Format.formatter -> key -> unit
(** [pp_key ppf k] formats [k] for debugging, as in ['a'], [U+0001], [Enter] and
    [F 5]. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf k] formats [k] for debugging as its modifiers and key, such as
    [Ctrl+Shift+Up], followed by each other field that differs from its default.
*)

val pp_release : Format.formatter -> release -> unit
(** [pp_release ppf r] formats [r] as {!pp} formats a press. *)
