(** Text attributes, such as bold and underline, and sets of them.

    A {e flag} ({!type-flag}) is one attribute a terminal can give the
    characters it shows. One SGR code sets it and another clears it. A {e set}
    ({!type-t}) holds any number of flags. Some flags share the code that clears
    them. SGR 22 clears bold and dim, 24 both underlines, and 54 framed and
    encircled, so clearing one flag of a pair clears the other too, and a
    program that keeps the other sets it again, as {!Style.emit} does.

    Terminals differ in the flags they show. Bold, underline and inverse show in
    nearly every terminal, and blink, framed and encircled in few. A terminal
    ignores a flag it does not show.

    A set packs into the low {!bits} bits of an integer ({!pack}), the form a
    grid of cells stores. *)

(** {1:flags Flags} *)

(** The type for flags. *)
type flag =
  | Bold  (** Bold or increased intensity, SGR 1. *)
  | Dim  (** Decreased intensity, SGR 2. *)
  | Italic  (** Italic, SGR 3. *)
  | Underline  (** Single underline, SGR 4. *)
  | Double_underline  (** Double underline, SGR 21. *)
  | Blink  (** Blinking, SGR 5. *)
  | Inverse  (** Foreground and background swapped, SGR 7. *)
  | Hidden  (** Concealed, SGR 8. *)
  | Strikethrough  (** Crossed out, SGR 9. *)
  | Overline  (** Overlined, SGR 53. *)
  | Framed  (** Framed, SGR 51. *)
  | Encircled  (** Encircled, SGR 52. *)

val flag_to_sgr_code : flag -> int
(** [flag_to_sgr_code f] is the SGR code that sets [f]. *)

val flag_to_sgr_disable_code : flag -> int
(** [flag_to_sgr_disable_code f] is the SGR code that clears [f], which pairs of
    flags share. *)

val flag_to_string : flag -> string
(** [flag_to_string f] is the constructor name of [f], such as ["Bold"]. *)

(** {1:sets Sets} *)

type t
(** The type for sets of flags. *)

(** {2:predefined Predefined sets}

    [bold] to [encircled] are the sets of one flag each. *)

val empty : t
(** [empty] is the set without flags. *)

val bold : t
val dim : t
val italic : t
val underline : t
val double_underline : t
val blink : t
val inverse : t
val hidden : t
val strikethrough : t
val overline : t
val framed : t
val encircled : t

(** {2:predicates Predicates} *)

val is_empty : t -> bool
(** [is_empty s] is [true] iff [s] has no flag. *)

val mem : flag -> t -> bool
(** [mem f s] is [true] iff [f] is in [s]. *)

(** {2:ops Set operations} *)

val add : flag -> t -> t
(** [add f s] is [s] with [f]. *)

val remove : flag -> t -> t
(** [remove f s] is [s] without [f]. *)

val toggle : flag -> t -> t
(** [toggle f s] is [s] with [f] if [s] lacks it, and [s] without [f] otherwise.
*)

val union : t -> t -> t
(** [union a b] is the flags in [a] or [b]. *)

val intersect : t -> t -> t
(** [intersect a b] is the flags in both [a] and [b]. *)

val diff : t -> t -> t
(** [diff a b] is the flags in [a] and not in [b]. *)

val cardinal : t -> int
(** [cardinal s] is the number of flags in [s]. *)

val with_flag : flag -> bool -> t -> t
(** [with_flag f b s] is [add f s] if [b] is [true], and [remove f s] otherwise.
*)

(** {1:converting Converting} *)

val of_list : flag list -> t
(** [of_list fs] is the set of the flags in [fs]. *)

val to_list : t -> flag list
(** [to_list s] is the flags of [s], in an order that is the same for every set.
*)

val combine :
  ?bold:bool ->
  ?dim:bool ->
  ?italic:bool ->
  ?underline:bool ->
  ?double_underline:bool ->
  ?blink:bool ->
  ?inverse:bool ->
  ?hidden:bool ->
  ?strikethrough:bool ->
  ?overline:bool ->
  ?framed:bool ->
  ?encircled:bool ->
  unit ->
  t
(** [combine ?bold ?dim ... ()] is the set of the flags whose argument is
    [true]. Each argument defaults to [false]. *)

(** {1:sgr SGR codes} *)

val to_sgr_codes : t -> int list
(** [to_sgr_codes s] is the codes that set the flags of [s], in the order of
    {!to_list}. *)

val iter_sgr_codes : (int -> unit) -> t -> unit
(** [iter_sgr_codes f s] calls [f] on each code of {!to_sgr_codes}[ s], in
    order, and builds no list. *)

val iter_sgr_disable_codes : (int -> unit) -> t -> unit
(** [iter_sgr_disable_codes f s] calls [f] on each code that clears a flag of
    [s], in increasing order. A code that clears two flags of [s] is passed
    once. *)

val fold_sgr_codes : (int -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold_sgr_codes f s init] folds [f] over the codes of {!to_sgr_codes}[ s],
    in order, from [init]. *)

(** {1:iteration Iteration} *)

val fold : (flag -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold f s init] folds [f] over the flags of [s], in the order of {!to_list},
    from [init]. *)

val iter : (flag -> unit) -> t -> unit
(** [iter f s] calls [f] on each flag of [s], in the order of {!to_list}. *)

(** {1:predicates_cmp Comparisons} *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] have the same flags. *)

val compare : t -> t -> int
(** [compare a b] is a total order on sets, compatible with {!equal}. *)

(** {1:encoding Packing} *)

val pack : t -> int
(** [pack s] is [s] as an integer below [1 lsl bits], one bit per flag. The bit
    of each flag stays the same from one release to the next. *)

val unpack : int -> t
(** [unpack n] is the set whose flags are the bits of [n] below {!bits}. It is
    the inverse of {!pack}. *)

val bits : int
(** [bits] is the number of low bits {!pack} uses. A store of packed sets may
    keep other facts in the bits above. *)

(** {1:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf s] formats [s] as a list of flag names, such as ["[Bold, Italic]"].
*)
