(** Keystrokes, with one value whatever the terminal's encoding.

    A terminal reports a key in one of several encodings, which carry different
    facts:
    - legacy bytes: the text a key types, a C0 control for Ctrl with a letter,
      and an ESC before either for Alt;
    - xterm's control sequences for keys that type nothing, such as [CSI A] or
      [CSI 1 ; 5 A], whose second parameter is 1 plus a mask of modifiers, and
      SS3 forms such as [ESC O P] (xterm's Control Sequences, "PC-Style Function
      Keys");
    - xterm's modifyOtherKeys, [CSI 27 ; m ; code ~], for the chords legacy
      bytes cannot tell apart;
    - the kitty keyboard protocol, [CSI code:shifted:base ; m:event ; text u].

    A keystroke has here the one value that every encoding carrying the same
    facts agrees on. A fact an encoding cannot carry is absent from the value,
    never guessed. Each loss is stated below where it occurs.

    {1:chars Characters}

    [Char c] is a key that types a character, and [c] is the character typed,
    with Shift and the keyboard layout applied: Shift+a is [Char 'A'] and
    Shift+4 on a US layout is [Char '$']. Legacy bytes and modifyOtherKeys carry
    that character, and so does the kitty protocol, in the shifted key it
    reports with its "report alternate keys" enhancement, and in the text it
    reports with a key that types one character when it sends every key as a
    sequence ("report associated text"). Shift is then consumed: [shift] is set
    on a [Char] only when Shift did not change the character, as for
    Shift+Space. A binding for Ctrl+Shift+g matches [Char 'G'] with [ctrl],
    never [Char 'g'] with [shift].

    The kitty protocol without that enhancement carries less. Its key code is
    "always the lower-case (or more technically, un-shifted) version of the key"
    (kitty keyboard protocol, "Key codes"), so Ctrl+Shift+1 arrives as
    [Char '1'] with [ctrl] and [shift], where every other encoding gives
    [Char '!'] with [ctrl]. The value is then that key with [shift]: the
    character typed is not in the bytes.

    {1:locks Locks}

    Lock keys never change a chord's key. A character typed with Caps Lock on
    arrives as text with the lock already applied, so the [Char] is what was
    typed: Caps Lock with r is [Char 'R'], from the byte R or from the kitty
    protocol's [CSI 114 ; 65 ; 82 u], whose text is R. A chord is another
    matter: Ctrl+s is [Char 's'] with [ctrl] whatever Caps Lock's state, as
    xterm sends it (0x13) and as the kitty protocol does ([CSI 115 ; 69 u]), and
    [caps_lock] says the lock is on. The kitty protocol reports the lock bits on
    chords and legacy bytes never do, so a binding ignores [caps_lock] and
    [num_lock]. Legacy bytes lose this fact under Alt: with Caps Lock on, Alt+a
    arrives as ESC A, [Char 'A'] with [alt], and Alt+Shift+a as ESC a, because
    the terminal applies the lock to the character before it adds the ESC.

    {1:controls Controls}

    Legacy bytes send Ctrl with a key as a C0 control (kitty keyboard protocol,
    "Legacy ctrl mapping of ASCII keys"): 0x01..0x1A are Ctrl+a..z, 0x00 is
    Ctrl+Space, and 0x1C..0x1F are Ctrl+\\, Ctrl+\], Ctrl+^ and Ctrl+_. Four
    controls are named keys, because legacy bytes cannot tell them from a chord:
    0x09 is [Tab] (also Ctrl+i), 0x0D [Enter] (Ctrl+m), 0x1B [Escape] (Ctrl+\[),
    and 0x08 and 0x7F are [Backspace] (0x08 is also Ctrl+h, and Ctrl+Backspace
    in some terminals). 0x0A is Ctrl+j. Enter sends 0x0D, but a terminal device
    that translates carriage returns on input (the termios flag ICRNL, set
    unless the terminal is in raw mode) delivers Enter as 0x0A, so Enter then
    arrives as Ctrl+j. Legacy bytes also lose Shift under Ctrl (Ctrl+Shift+a is
    Ctrl+a), and some chords alias a control (Ctrl+2 is Ctrl+Space). Under the
    kitty protocol's disambiguation all of these are distinct.

    {1:keypad The keypad}

    [keypad] is set when the encoding says the key is on the numeric keypad: the
    kitty protocol's keypad codes and xterm's application keypad (the SS3 forms
    of xterm's "PC-Style Function Keys"). Legacy bytes send a keypad key as its
    main-keyboard equivalent ("All keypad keys are reported as their equivalent
    non-keypad keys", kitty keyboard protocol, "Legacy functional keys"), so the
    fact is absent there: keypad 8 without Num Lock is [Up] without [keypad]. A
    binding ignores [keypad] unless it means the keypad. [Begin], the keypad's 5
    without Num Lock, exists only on the keypad and always has [keypad]. *)

(** {1:mods Modifiers} *)

type mods = {
  shift : bool;
  alt : bool;
      (** Alt, or Option on a Mac: the kitty protocol's alt bit, xterm's Alt, an
          ESC before a key, and the mouse's Meta bit. *)
  ctrl : bool;
  super : bool;
      (** The Windows or Command key. xterm's table names the bit worth 8 Meta;
          the kitty protocol, which reuses the same parameter, names it Super,
          and this is that bit. *)
  hyper : bool;
  meta : bool;
      (** A Meta key distinct from Alt, which only the kitty protocol reports
          (its bit worth 32). *)
  caps_lock : bool;  (** Caps Lock is on. A state, not a key held. *)
  num_lock : bool;  (** Num Lock is on. A state, not a key held. *)
}
(** The type for modifiers. The kitty protocol and xterm encode them as one
    parameter, 1 plus the sum of shift 1, alt 2, ctrl 4, super 8, hyper 16, meta
    32, caps_lock 64 and num_lock 128 (kitty keyboard protocol, "Modifiers"). *)

val no_mods : mods
(** [no_mods] has every modifier [false]. *)

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
  | Char of Uchar.t  (** A key that types a character: see {!section-chars}. *)
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
      (** A code from the kitty protocol's functional range, 57344..63743, that
          names no key yet. *)

(** {1:strokes Keystrokes} *)

type release = {
  key : key;
  mods : mods;
      (** For a modifier key, the state after the release: releasing Left Ctrl
          clears [ctrl] unless Right Ctrl is still held. *)
  base : Uchar.t option;
  keypad : bool;
}
(** The type for key releases. Only the kitty protocol reports them, with its
    "report event types" enhancement, and a release types nothing. *)

type t = {
  key : key;
  mods : mods;
  text : string;
      (** What the key types, as UTF-8: the kitty protocol's associated text
          when it reports one, else the character of a [Char]. It is [""] for a
          chord (Ctrl, Alt, Super, Hyper or Meta held), which types nothing, for
          any key other than a [Char], for a control character, and when the
          encoding does not carry the character typed. *)
  base : Uchar.t option;
      (** The key at the same place on a US PC-101 layout, when the terminal
          reports it (the kitty protocol's base layout key). Ctrl+С on a
          Cyrillic layout has [base = Some 'c'], so a binding for Ctrl+c can
          match it. *)
  keypad : bool;
      (** The encoding says the key is on the keypad: see {!section-keypad}. *)
  repeat : bool;
      (** The terminal reports an auto-repeat (the kitty protocol's event
          types). Other encodings send a repeat as a press. *)
}
(** The type for key presses. *)

(** {1:fmt Formatting} *)

val pp_key : Format.formatter -> key -> unit
(** [pp_key] formats a key: ['a'], [U+0001], [Enter], [F 5]. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a press as its modifiers and key, [Ctrl+Shift+Up], followed by
    the facts that are not defaults. *)

val pp_release : Format.formatter -> release -> unit
(** [pp_release] formats a release like {!pp}. *)
