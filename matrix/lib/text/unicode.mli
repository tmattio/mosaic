(** The Unicode properties of scalars that segmentation and widths read.

    The properties come from tables that [matrix/support/gen_unicode_data.ml]
    generates from {{:https://github.com/ocaml/uucp}uucp}, in {!Unicode_data}. A
    lookup reads one table entry and allocates nothing. *)

(** {1:grapheme Grapheme properties} *)

val grapheme_cluster_break : Uchar.t -> int
(** [grapheme_cluster_break u] is the
    {{:https://www.unicode.org/reports/tr29/}UAX #29} Grapheme_Cluster_Break
    property of [u], as an index in \[0;17\] in uucp's order of its values. *)

val indic_conjunct_break : Uchar.t -> int
(** [indic_conjunct_break u] is the Indic_Conjunct_Break property of [u], as an
    index in \[0;3\], which rule GB9c of UAX #29 reads. *)

val is_extended_pictographic : Uchar.t -> bool
(** [is_extended_pictographic u] is [true] iff [u] has the Extended_Pictographic
    property, which rule GB11 of UAX #29 reads. *)

(** {1:width Width} *)

val tty_width_hint : Uchar.t -> int
(** [tty_width_hint u] is the width of [u] in a terminal that measures each
    scalar alone:
    - [-1] for control characters (C0, DEL, C1).
    - [0] for nonspacing marks and format characters, and for the Hangul
      conjoining vowels and trailing consonants (U+1160..U+11FF,
      U+D7B0..U+D7FF). A leading consonant before them composes them into one
      syllable (Unicode §3.12), as wide as the consonant, and the [wcwidth]s of
      Markus Kuhn, glibc and xterm give them 0.
    - [1] for most characters.
    - [2] for the characters whose East_Asian_Width is Wide (W) or Fullwidth (F)
      ({{:https://www.unicode.org/reports/tr11/}UAX #11}).

    The W characters include every character with the Emoji_Presentation
    property except the regional indicators. A character without it has a text
    presentation by default ({{:https://www.unicode.org/reports/tr51/}UTS #51}
    ED-7) and keeps its East_Asian_Width: U+2764 HEAVY BLACK HEART, U+26A0
    WARNING SIGN and U+1F000 MAHJONG TILE EAST WIND are [1]. Only U+FE0F after
    an emoji makes one [2] (see {!is_emoji}). *)

val is_emoji : Uchar.t -> bool
(** [is_emoji u] is [true] iff [u] has the Emoji property. U+FE0F gives such a
    character of width [1] its emoji presentation, of width [2]. *)

val is_emoji_modifier_base : Uchar.t -> bool
(** [is_emoji_modifier_base u] is [true] iff [u] has the Emoji_Modifier_Base
    property. An emoji modifier (U+1F3FB..U+1F3FF) after such a character makes
    an emoji modifier sequence, which has the emoji presentation, of width [2],
    whatever the base's own ({{:https://www.unicode.org/reports/tr51/}UTS #51}
    §2.7.1). U+261D WHITE UP POINTING INDEX is one of width [1]. *)

(** {1:combined Combined lookups} *)

val packed_props : Uchar.t -> int
(** [packed_props u] is the properties of [u] in one integer. Bits 0 to 4 are
    {!grapheme_cluster_break}, bits 5 and 6 {!indic_conjunct_break}, bit 7
    {!is_extended_pictographic}, bits 8 and 9 {!tty_width_hint} plus one, bit 10
    {!is_zero_in_grapheme}, bit 11 {!is_emoji}, bit 12
    {!is_emoji_modifier_base}, and bit 13 is set for a spacing mark
    (General_Category Mc). *)

val grapheme_props : Uchar.t -> int
(** [grapheme_props u] is the low 8 bits of {!packed_props}[ u], the properties
    that grapheme cluster boundaries read. *)

val is_zero_in_grapheme : Uchar.t -> bool
(** [is_zero_in_grapheme u] is [true] iff [u] adds nothing to the width of a
    cluster it continues under [`Unicode_wide]. *)
