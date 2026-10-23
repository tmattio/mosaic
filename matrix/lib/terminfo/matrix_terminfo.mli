(** Terminal descriptions from the terminfo database.

    A terminal description says what a kind of terminal, named by [TERM], can do
    and which bytes do it: booleans ([am]: the cursor wraps at the right margin;
    [xenl]: and a line feed right after that wrap is ignored), numbers
    ([colors]) and strings ([cup], the bytes that move the cursor, with the row
    and column as parameters). terminfo(5) lists the standard ones; user_caps(5)
    the user-defined ones that programs agree on, such as [RGB] (direct colour),
    [Smulx] (styled underlines) and [BE] (bracketed paste).

    The database is compiled: each description is a file, which {!of_string}
    reads, in either of term(5)'s formats, the legacy one (magic number 0432)
    and ncurses 6.1's with 32-bit numbers (01036), with the extended section
    that holds user-defined capabilities and their names. Finding the file for a
    name is the system's affair: [Tty.terminfo] does it.

    A description is the database's statement about a kind of terminal, not the
    terminal's own: it may be missing (a new terminal over ssh), and it often
    understates what the terminal does (no [RGB] for terminals that show direct
    colour). What the terminal answers to a query is better evidence, where it
    answers.

    {1:descriptions Descriptions} *)

type t
(** The type for a terminal description. *)

val of_string : string -> (t, string) result
(** [of_string bytes] is the description compiled in [bytes], the contents of a
    file of the database, or an error that says why [bytes] is not one. A
    capability absent from the description and one it cancels are alike: absent.
    Booleans, numbers and strings past the standard ones the compiled format
    orders (term.h's) are ignored, as a library older than the database ignores
    them. *)

val names : t -> string list
(** [names t] is the names [t] describes, the first being the primary one; the
    last of several is a description in words, such as
    ["xterm with 256 colors"]. *)

(** {1:capabilities Capabilities}

    A capability is named by its short name, terminfo(5)'s "Capname" ([cup], not
    [cursor_address]), or by its user-defined name ([RGB]). A name has one type,
    so it is read with the function of its type. *)

val flag : t -> string -> bool
(** [flag t name] is [true] iff [t] has the boolean capability [name]. *)

val number : t -> string -> int option
(** [number t name] is the numeric capability [name] of [t], if it has it. *)

val string : t -> string -> string option
(** [string t name] is the string capability [name] of [t], if it has it, as
    stored: control characters in their interpreted form, parameters ({!expand})
    and delays ({!pieces}) not yet applied. *)

val flags : t -> string list
(** [flags t] is the boolean capabilities [t] has, the standard ones first, in
    the compiled order, then the user-defined ones. *)

val numbers : t -> (string * int) list
(** [numbers t] is [t]'s numeric capabilities, ordered as {!flags}. *)

val strings : t -> (string * string) list
(** [strings t] is [t]'s string capabilities, ordered as {!flags}. *)

(** {1:parameters Parameters}

    A string capability that takes parameters says how to write them, in a small
    stack language of [%] codes (terminfo(5), "Parameterized Strings"). xterm's
    [cup] is

    {v \E[%i%p1%d;%p2%dH v}

    It adds one to the row and the column ([%i]), then writes each in decimal.
*)

type param =
  | Int of int
  | String of string  (** A parameter: a number, or a string for [%s]. *)

type vars
(** The type for the static variables [A] to [Z], which a terminal's strings may
    share: one sets a variable that another reads (the Wyse 350's [sgr] and
    [setf] do). They hold the state of one terminal, and keep their values from
    one {!expand} to the next. *)

val vars : unit -> vars
(** [vars ()] is a set of static variables, all zero. *)

val expand : ?vars:vars -> string -> param list -> string
(** [expand ~vars s params] is [s] with its [%] codes evaluated on [params], as
    tparm(3x) evaluates them. The first parameter is [%p1], and a parameter not
    given is [0]. The codes are printf(3)'s conversions [%d], [%o], [%x], [%X]
    and [%s] with their flags, width and precision ([%:-5d]), [%c], [%p1] to
    [%p9], the variables [%P] and [%g], the constants [%'c'] and [%{nn}], [%l],
    the arithmetic, bitwise and logical operators, [%i], and the conditional
    [%? c %t then %e else %;] with its else-ifs. The static variables are
    [vars], or fresh ones when it is not given. The dynamic variables, [a] to
    [z], start at zero in each expansion. Delays stay in the result ({!pieces}).

    Where terminfo(5) leaves a case open, or ncurses' tparm departs from it,
    [expand] does what tparm does, since those are the bytes every curses
    program sends for the same description.
    - Numbers are C ints. Arithmetic wraps at 32 bits, [%/] and [%m] by zero
      give [0], and [%o], [%x] and [%X] print a negative number as unsigned.
    - A missing stack operand is [0], or [""] for [%s] and [%l]. A string popped
      as a number is [0], and a number popped as a string is [""].
    - The flags [#] and space are read anywhere before the conversion, and [-]
      only after a [:]. [+] is the addition operator in every position. A width
      or precision over 10000, or a second [.], drops the flags, the width and
      the precision.
    - [%c] of [0] writes the byte 0200, which a C string can hold. Of another
      value it writes the low byte.
    - [%i] adds one to the first two numeric parameters once, however often it
      is written.
    - [%'c'] and [%{nn}] skip the character after the constant, whatever it is,
      where terminfo(5) puts the closing quote or brace.
    - An unknown code writes nothing, and a [%] that ends [s] is dropped.
    - A string with no [%p] code, as termcap wrote them ([\E\[%d;%dH]), has its
      parameters pushed for it, at most two, the first on top. [%i] then also
      sets the stack's two lowest slots, from the bottom, to the incremented
      first and second parameters, so [\E\[%i%d;%dR] on [4] and [9] is
      [\E\[10;5R]. *)

(** {1:padding Delays} *)

(** The type for the parts of a string to send: bytes, and delays. *)
type piece =
  | Text of string
  | Delay of { ms : float; per_line : bool; mandatory : bool }
      (** [$<ms>] in a string: a terminal too slow to keep up needs [ms]
          milliseconds before what follows, times the number of lines the
          operation affects if [per_line] ([*]). Padding is advisory on a
          terminal with flow control ([xon]), unless [mandatory] ([/]), as the
          visible bell's pause between reversing the screen and back. *)

val pieces : string -> piece list
(** [pieces s] is [s] cut at its delays, as tputs(3x) reads them: [$<] digits,
    at most one decimal, then [*] and [/], each at most once, then [>]; anything
    else is text. Writing [s] as it is would show a delay's bytes. *)
