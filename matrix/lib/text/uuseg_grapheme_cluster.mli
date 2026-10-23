(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Grapheme cluster segmenter.

    Vendored from uuseg v18.0.0, stripped to its boundary-check core:
    the streaming [add] API is removed, and {!reset},
    {!check_boundary}, {!check_boundary_with_width} and {!is_boundary} are
    added for zero-allocation segmentation. *)

(** {1:segmenter Segmenter} *)

type t
(** The type for grapheme cluster segmenters. *)

val create : unit -> t
(** [create ()] is a new grapheme cluster segmenter. *)

val reset : t -> unit
(** [reset s] resets [s] to its initial state, ready to segment a
    new string. *)

(** {1:boundary Boundary checks} *)

val check_boundary : t -> Uchar.t -> bool
(** [check_boundary s u] is [true] if there is a grapheme cluster
    boundary before [u], and updates [s]. The first character
    always returns [true] (rule GB1). Zero allocation. *)

val is_boundary : t -> Uchar.t -> bool
(** [is_boundary s u] is {!check_boundary}[ s u] without updating [s]. *)

val may_continue : Uchar.t -> Uchar.t -> bool
(** [may_continue u v] is [false] iff there is a grapheme cluster
    boundary between [u] and [v] whatever precedes [u]. It is [true]
    when the boundary depends on what precedes [u] (GB9c, GB11, GB12,
    GB13) and when there is none (GB3, GB6 to GB9b). Zero allocation. *)

val check_boundary_with_width : t -> Uchar.t -> int
(** [check_boundary_with_width s u] is like {!check_boundary} but
    also extracts the display width from a single property-table
    lookup. The result is a packed integer: bit 3 = [u] is a spacing mark
    (General_Category Mc), bit 2 = is_boundary, bits 0–1 = width encoding
    (0 = -1, 1 = 0, 2 = 1, 3 = 2). *)
