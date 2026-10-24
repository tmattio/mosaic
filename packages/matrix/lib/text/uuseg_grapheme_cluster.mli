(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Grapheme cluster boundaries
    ({{:https://www.unicode.org/reports/tr29/}UAX #29}), checked one scalar at
    a time.

    This is uuseg 18.0.0's grapheme cluster segmenter reduced to its boundary
    checks. A segmenter holds what the rules need of the scalars before the
    next one. Its checks allocate nothing. *)

(** {1:segmenter Segmenter} *)

type t
(** The type for grapheme cluster segmenters. *)

val create : unit -> t
(** [create ()] is a new grapheme cluster segmenter. *)

val reset : t -> unit
(** [reset s] puts [s] at the start of a text. *)

(** {1:boundary Boundary checks} *)

val check_boundary : t -> Uchar.t -> bool
(** [check_boundary s u] is [true] iff a grapheme cluster boundary falls
    before [u], and adds [u] to the text [s] has seen. It is [true] for the
    first scalar of a text (GB1). *)

val is_boundary : t -> Uchar.t -> bool
(** [is_boundary s u] is {!check_boundary}[ s u] without updating [s]. *)

val may_continue : Uchar.t -> Uchar.t -> bool
(** [may_continue u v] is [false] iff a grapheme cluster boundary falls
    between [u] and [v] whatever precedes [u]. It is [true] when the boundary
    depends on what precedes [u] (GB9c, GB11, GB12, GB13), and when no
    boundary falls there (GB3, GB6 to GB9b). *)

val check_boundary_with_width : t -> Uchar.t -> int
(** [check_boundary_with_width s u] is {!check_boundary}[ s u] and properties
    of [u], read in the same table lookup, packed in one integer. Bits 0 and
    1 are [Unicode.tty_width_hint u] plus one, bit 2 is set iff a boundary
    falls before [u], and bit 3 iff [u] is a spacing mark (General_Category
    Mc). *)
