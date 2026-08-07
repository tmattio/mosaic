(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Grapheme cluster segmenter.

    Vendored from uuseg v17.0.0, stripped to its boundary-check core:
    the streaming [add] API is removed, and {!reset},
    {!check_boundary}, and {!check_boundary_with_width} are added for
    zero-allocation segmentation. *)

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

val check_boundary_with_width : t -> Uchar.t -> int
(** [check_boundary_with_width s u] is like {!check_boundary} but
    also extracts the display width from a single property-table
    lookup. The result is a packed integer: bit 2 = is_boundary,
    bits 0–1 = width encoding (0 = -1, 1 = 0, 2 = 1, 3 = 2). *)
