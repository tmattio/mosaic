(** Unicode property lookups for grapheme segmentation and width calculation.

    Uses compact data structures generated from uucp. See
    [matrix/support/gen_unicode_data.ml] for the generator. *)

val grapheme_cluster_break : Uchar.t -> int
(** [grapheme_cluster_break u] returns the grapheme cluster break property of
    [u] as a byte index (0-17). See UAX #29. *)

val indic_conjunct_break : Uchar.t -> int
(** [indic_conjunct_break u] returns the indic conjunct break property of [u] as
    a byte index (0-3). Used for GB9c in UAX #29. *)

val is_extended_pictographic : Uchar.t -> bool
(** [is_extended_pictographic u] returns [true] if [u] has the
    Extended_Pictographic property. Used for GB11 in UAX #29. *)

val tty_width_hint : Uchar.t -> int
(** [tty_width_hint u] returns the suggested display width of [u]:
    - [-1] for control characters (C0, DEL, C1)
    - [0] for non-spacing marks and format characters
    - [1] for most characters
    - [2] for wide/fullwidth East Asian characters *)

val grapheme_props : Uchar.t -> int
(** [grapheme_props u] returns all grapheme properties in one lookup. Layout:
    bits 0-4 = gcb, bits 5-6 = incb, bit 7 = extpic. *)

val all_props : Uchar.t -> int
(** [all_props u] returns all properties including width in one lookup. Layout:
    bits 0-4 = gcb, bits 5-6 = incb, bit 7 = extpic, bits 8-9 = width_enc (0=-1,
    1=0, 2=1, 3=2). *)
