(** Text attribute flags for ANSI terminal styling.

    Provides an efficient bit-flag representation for text attributes like bold,
    italic, underline, etc. Attributes are stored as a compact integer bitmask,
    enabling fast set operations and minimal memory usage.

    {1 Attribute Flags}

    Text attributes modify how terminal text is displayed. Common attributes
    include:

    - {b Bold}: Increased weight/brightness (SGR 1)
    - {b Dim}: Decreased brightness (SGR 2)
    - {b Italic}: Slanted text (SGR 3)
    - {b Underline}: Single underline (SGR 4)
    - {b Double_underline}: Double underline (SGR 21)
    - {b Blink}: Blinking text (SGR 5) - rarely supported
    - {b Inverse}: Swap foreground/background (SGR 7)
    - {b Hidden}: Invisible text (SGR 8)
    - {b Strikethrough}: Line through text (SGR 9)
    - {b Overline}: Line above text (SGR 53)
    - {b Framed}: Framed text (SGR 51) - rarely supported
    - {b Encircled}: Encircled text (SGR 52) - rarely supported

    Terminal support varies. Bold, underline, and inverse are widely supported.
    Blink, framed, and encircled have limited support.

    {1 Usage}

    Create attribute sets using predefined constants or combinators:
    {[
      let attrs1 = Attr.(union bold italic) in
      let attrs2 = Attr.combine ~bold:true ~underline:true () in
      let attrs3 = Attr.of_list [Attr.Bold; Attr.Underline]
    ]}

    Test and modify attributes:
    {[
      let has_bold = Attr.mem Attr.Bold attrs1 in
      let with_italic = Attr.add Attr.Italic attrs2 in
      let without_bold = Attr.remove Attr.Bold attrs1
    ]}

    Set operations:
    {[
      let combined = Attr.union attrs1 attrs2 in
      let common = Attr.intersect attrs1 attrs2 in
      let diff = Attr.diff attrs1 attrs2
    ]}

    {1 Allocation Behavior}

    Most operations are zero-allocation:
    - All set operations ([add], [remove], [union], [intersect], [diff], etc.)
    - Iteration with [iter], [fold], [iter_sgr_codes], [fold_sgr_codes]
    - Lookup functions ([mem], [is_empty], [cardinal])
    - Printing with [pp]

    Functions that allocate are explicitly noted in their documentation:
    - [to_list] - allocates a list
    - [to_sgr_codes] - allocates a list (use [iter_sgr_codes] instead)
    - [of_list] - traverses caller-provided list only

    {1 Contracts}

    - The representation is a 12-bit mask.
    - Idempotent setters ensure accidental duplication never changes results.
    - Encodings follow the ANSI SGR standard; unsupported attributes simply have
      no visible effect when rendered. *)

type flag =
  | Bold
  | Dim
  | Italic
  | Underline
  | Double_underline
  | Blink
  | Inverse
  | Hidden
  | Strikethrough
  | Overline
  | Framed
  | Encircled

val flag_to_sgr_code : flag -> int
(** [flag_to_sgr_code flag] returns the SGR code to enable [flag].

    Zero-allocation. Use with {!iter} for efficient SGR sequence generation. *)

val flag_to_sgr_disable_code : flag -> int
(** [flag_to_sgr_disable_code flag] returns the SGR code to disable [flag].

    Note: Some flags share disable codes:
    - Bold and Dim both use 22 (normal intensity)
    - Underline and Double_underline both use 24
    - Framed and Encircled both use 54

    Zero-allocation. *)

val flag_to_string : flag -> string
(** [flag_to_string flag] returns the string representation of [flag].

    Zero-allocation (returns a static string). *)

type t
(** Opaque bitmask of attribute flags.

    Internally represented as an integer with one bit per flag. Efficient for
    storage and set operations. *)

(** {1 Predefined Attribute Sets} *)

val empty : t
(** No attributes set. *)

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

(** {1 Set Operations} *)

val is_empty : t -> bool
(** [is_empty t] tests if no attributes are set. *)

val mem : flag -> t -> bool
(** [mem flag t] tests if [flag] is set in [t]. *)

val add : flag -> t -> t
(** [add flag t] adds [flag] to [t].

    Idempotent: adding an already-present flag has no effect. *)

val remove : flag -> t -> t
(** [remove flag t] removes [flag] from [t].

    Idempotent: removing an absent flag has no effect. *)

val toggle : flag -> t -> t
(** [toggle flag t] toggles [flag] in [t].

    Adds if absent, removes if present. *)

val union : t -> t -> t
(** [union a b] computes the union of [a] and [b].

    Contains all flags present in either set. *)

val intersect : t -> t -> t
(** [intersect a b] computes the intersection of [a] and [b].

    Contains only flags present in both sets. *)

val diff : t -> t -> t
(** [diff a b] computes the difference [a - b].

    Contains flags in [a] that are not in [b]. *)

val cardinal : t -> int
(** [cardinal t] counts the number of flags set in [t]. *)

(** {1 Construction and Conversion} *)

val of_list : flag list -> t
(** [of_list flags] creates an attribute set from a list.

    Duplicate flags are ignored. Order-independent. *)

val to_list : t -> flag list
(** [to_list t] converts to a list of flags.

    {b Allocates} a new list. Use {!iter} or {!fold} when allocation is
    undesirable. Order is deterministic but unspecified. *)

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
(** [combine ?bold ?dim ... ()] creates attributes from labeled arguments.

    Each parameter defaults to [false]. Only flags set to [true] are included.
    Rarely supported attributes like [framed] and [encircled] are included when
    explicitly enabled. *)

val with_flag : flag -> bool -> t -> t
(** [with_flag flag enabled t] conditionally adds or removes [flag].

    If [enabled] is [true], adds [flag]; otherwise removes it. *)

(** {1 ANSI Escape Sequence Generation} *)

val to_sgr_codes : t -> int list
(** [to_sgr_codes t] converts to SGR parameter codes for enabling attributes.

    {b Allocates} a new list. Use {!iter_sgr_codes} or {!fold_sgr_codes} when
    allocation is undesirable.

    Returns codes to enable all flags in [t]. Does not include reset codes.
    Order is deterministic but unspecified.

    Example: [{!bold} ∪ {!italic}] produces [[1; 3]]. *)

val iter_sgr_codes : (int -> unit) -> t -> unit
(** [iter_sgr_codes f t] calls [f code] for each SGR enable code in [t].

    Zero-allocation alternative to {!to_sgr_codes}. Order is unspecified. *)

val iter_sgr_disable_codes : (int -> unit) -> t -> unit
(** [iter_sgr_disable_codes f t] calls [f code] for each SGR disable code in
    [t].

    Automatically deduplicates shared codes (e.g., if both Bold and Dim are in
    [t], code 22 is emitted only once).

    Zero-allocation. Order is unspecified. *)

val fold_sgr_codes : (int -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold_sgr_codes f t init] folds over SGR codes for set flags.

    Zero-allocation alternative to {!to_sgr_codes}. Order is unspecified. *)

(** {1 Iteration and Folding} *)

val fold : (flag -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold f t init] folds over set flags.

    Applies [f] to each flag in [t] from left to right (unspecified order),
    threading an accumulator. *)

val iter : (flag -> unit) -> t -> unit
(** [iter f t] iterates over set flags.

    Applies [f] to each flag in [t]. Order is unspecified. *)

(** {1 Comparison and Encoding} *)

val equal : t -> t -> bool
(** [equal a b] tests if [a] and [b] contain exactly the same flags.

    More efficient than [compare a b = 0]. *)

val compare : t -> t -> int
(** [compare a b] compares attribute sets.

    Returns a value compatible with {!Stdlib.compare}: negative if [a < b], zero
    if equal, positive if [a > b]. Comparison is based on integer
    representation. *)

val pack : t -> int
(** [pack t] encodes to an integer for compact storage.

    Direct exposure of internal representation. Only use when storing attribute
    sets in custom tables; the value is stable across releases. *)

val unpack : int -> t
(** [unpack n] decodes from an integer.

    Inverse of {!pack}. *)

(** {1 Utilities} *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] prints a human-readable representation.

    Example output: ["[Bold, Italic]"]. Empty set prints as ["[]"]. *)
