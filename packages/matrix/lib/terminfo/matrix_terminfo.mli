(** Terminal descriptions from the terminfo database.

    A {e terminal description} says what a kind of terminal can do and which
    bytes do it. The kind is named by a {e terminal type}, such as the value of
    [TERM]. A description is a set of {e capabilities}, each a boolean, a number
    or a string. The boolean [am] says the cursor wraps at the right margin, and
    [xenl] that a line feed right after that wrap is ignored. The number
    [colors] counts the colors, and the string [cup] holds the bytes that move
    the cursor, with the row and the column as parameters. terminfo(5) lists the
    standard capabilities, and user_caps(5) the user-defined ones that programs
    agree on, such as [RGB] (direct color), [Smulx] (styled underlines) and [BE]
    (bracketed paste).

    The database is compiled. Each description is a file in one of term(5)'s
    formats, the legacy one (magic number 0432) or the one of ncurses 6.1 with
    32-bit numbers (01036). Either may end with the extended section, which
    holds the user-defined capabilities and their names. {!of_string} reads such
    a file. Finding the file for a terminal type is left to [Tty.terminfo], so
    that this library links no unix.

    A description is the database's statement about a kind of terminal. It may
    be missing, as for a new terminal reached over ssh, and it often leaves out
    what the terminal does, such as direct color. Where a terminal answers a
    query, its answer is better evidence.

    A program reads a description with {!of_string}, reads its capabilities by
    name ({!section-capabilities}), evaluates a string's parameters with
    {!expand}, and cuts the result at its delays with {!pieces} before it writes
    it. Every function is pure but {!expand}, which changes the static variables
    it is given ({!type-vars}).

    This function gives the bytes that move the cursor to [row] and [col], from
    [0], on a terminal that [t] describes. For xterm's description, [row = 4]
    and [col = 9] give ["\027\[5;10H"]:
    {[
    let cursor_to t ~row ~col =
      Option.map
        (fun cup -> Matrix_terminfo.(expand cup [ Int row; Int col ]))
        (Matrix_terminfo.string t "cup")
    ]} *)

(** {1:descriptions Descriptions} *)

type t
(** The type for terminal descriptions. *)

val of_string : string -> (t, string) result
(** [of_string bytes] is the description compiled in [bytes], the contents of a
    file of the database, or [Error why], where [why] says why [bytes] is not
    one. A capability that the description cancels reads as absent. The
    booleans, numbers and strings past the standard ones that the compiled
    format orders (term.h's) are ignored, as a library older than the database
    ignores them. *)

val names : t -> string list
(** [names t] is the terminal types [t] describes, the primary one first. The
    last of several names is a description in words, such as
    ["xterm with 256 colors"]. *)

(** {1:capabilities Capabilities}

    A capability is named by its short name, terminfo(5)'s "Capname", such as
    [cup] for [cursor_address], or by its user-defined name, such as [RGB]. A
    name has one type, and is read with the function of that type. *)

val flag : t -> string -> bool
(** [flag t name] is [true] iff [t] has the boolean capability [name]. *)

val number : t -> string -> int option
(** [number t name] is the numeric capability [name] of [t], if [t] has it. *)

val string : t -> string -> string option
(** [string t name] is the string capability [name] of [t], if [t] has it, as
    the database stores it. Control characters are in their interpreted form,
    and its parameters ({!expand}) and delays ({!pieces}) are not yet applied.
*)

val flags : t -> string list
(** [flags t] is the boolean capabilities [t] has, the standard ones first in
    the compiled order, then the user-defined ones. *)

val numbers : t -> (string * int) list
(** [numbers t] is [t]'s numeric capabilities, ordered as {!flags}. *)

val strings : t -> (string * string) list
(** [strings t] is [t]'s string capabilities, ordered as {!flags}. *)

(** {1:parameters Parameters}

    A string capability that takes parameters says how to write them in a small
    stack language of [%] codes (terminfo(5), "Parameterized Strings"). xterm's
    [cup] is

    {v \E[%i%p1%d;%p2%dH v}

    which adds one to the row and the column ([%i]), then writes each in
    decimal. The codes are printf(3)'s conversions [%d], [%o], [%x], [%X] and
    [%s] with their flags, width and precision ([%:-5d]), [%c], [%%], the
    parameters [%p1] to [%p9], the variables [%P] and [%g], the constants [%'c']
    and [%{nn}], [%l], the arithmetic, bitwise and logical operators, [%i], and
    the conditional [%? c %t then %e else %;] with its else-ifs. The
    {e static variables} [A] to [Z] keep their values from one expansion to the
    next ({!type-vars}). The {e dynamic variables} [a] to [z] start at [0] in
    each expansion.

    Where terminfo(5) leaves a case open, or ncurses' tparm(3x) departs from it,
    {!expand} does what tparm does, since those are the bytes every curses
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
    - An unknown code writes nothing, and a [%] that ends the string is dropped.
    - A string with no [%p] code, as termcap wrote them ([\E\[%d;%dH]), has its
      parameters pushed for it, at most two, the first on top. [%i] then also
      sets the stack's two lowest slots, from the bottom, to the incremented
      first and second parameters, so [\E\[%i%d;%dR] on [4] and [9] is
      [\E\[10;5R]. *)

type param =
  | Int of int
  | String of string  (** A parameter, a number or a string for [%s]. *)

type vars
(** The type for sets of static variables. A terminal's strings may share them,
    one setting a variable that another reads (the Wyse 350's [sgr] and [setf]
    do), so a set holds the state of one terminal. *)

val vars : unit -> vars
(** [vars ()] is a set of static variables, all [0]. *)

val expand : ?vars:vars -> string -> param list -> string
(** [expand ~vars s params] is [s] with its [%] codes evaluated on [params], as
    the {{!section-parameters}language} says. The first parameter is [%p1], a
    parameter not given is [0], and parameters past the ninth are ignored. The
    static variables are [vars], which [expand] changes, or a fresh set when
    [vars] is not given. The delays of [s] stay in the result ({!pieces}). *)

(** {1:padding Delays} *)

(** The type for the parts of a string to send, bytes and delays. *)
type piece =
  | Text of string
  | Delay of { ms : float; per_line : bool; mandatory : bool }
      (** [$<ms>] in a string. A terminal too slow to keep up needs [ms]
          milliseconds before what follows, times the number of lines the
          operation affects if [per_line] ([*]). The delay is advisory on a
          terminal with flow control ([xon]), unless [mandatory] ([/]), as the
          pause of the visible bell between reversing the screen and back is. *)

val pieces : string -> piece list
(** [pieces s] is [s] cut at its delays, as tputs(3x) reads them. A delay is
    [$<], digits with at most one decimal, then [*] and [/], each at most once,
    then [>]. Anything else is text. A delay written to the terminal as it is
    shows as text. *)
