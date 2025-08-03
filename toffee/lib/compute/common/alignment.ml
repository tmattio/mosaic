(*
  alignment.ml
  ---------------------------------------------------------------------------
  Generic CSS **alignment** helpers shared by Flexbox and CSS-Grid algorithms.
  This is a direct and faithful translation of the reference Rust code found
  in `alignment.rs`.

  ---------------------------------------------------------------------------
  SPDX-License-Identifier: MIT OR Apache-2.0
  ---------------------------------------------------------------------------
*)

open Style

(** Fallback alignment – see <https://www.w3.org/TR/css-align-3/> and the
    resolution of <https://github.com/w3c/csswg-drafts/issues/10154>. *)
let apply_alignment_fallback ~free_space ~num_items
    ~(alignment_mode : Style.Alignment.align_content) ~(is_safe : bool) :
    Style.Alignment.align_content =
  (* 1. Distributed keywords fall back when there is ≤ 1 item or no free space. *)
  let alignment_mode, is_safe =
    if num_items <= 1 || free_space <= 0. then
      match alignment_mode with
      | Style.Alignment.Stretch -> (Alignment.Flex_start, true)
      | Style.Alignment.Space_between -> (Alignment.Flex_start, true)
      | Style.Alignment.Space_around -> (Alignment.Center, true)
      | Style.Alignment.Space_evenly -> (Alignment.Center, true)
      | _ -> (alignment_mode, is_safe)
    else (alignment_mode, is_safe)
  in
  (* 2. Negative free-space forces the “safe” variants back to Start. *)
  let alignment_mode =
    if free_space <= 0. && is_safe then Alignment.Start else alignment_mode
  in
  alignment_mode

(** Generic offset computation shared by *align-content* and *justify-content*.
    The [gap] argument must be zero when used from CSS-Grid. *)
let compute_alignment_offset ~free_space ~num_items ~gap
    ~(alignment_mode : Style.Alignment.align_content) ~layout_is_flex_reversed
    ~is_first : float =
  if is_first then
    match alignment_mode with
    | Style.Alignment.Start -> 0.
    | Flex_start -> if layout_is_flex_reversed then free_space else 0.
    | End -> free_space
    | Flex_end -> if layout_is_flex_reversed then 0. else free_space
    | Center -> free_space /. 2.
    | Stretch -> 0.
    | Space_between -> 0.
    | Space_around ->
        if free_space >= 0. then free_space /. float_of_int num_items /. 2.
        else free_space /. 2.
    | Space_evenly ->
        if free_space >= 0. then free_space /. float_of_int (num_items + 1)
        else free_space /. 2.
  else
    let free_space = Float.max free_space 0. in
    gap
    +.
    match alignment_mode with
    | Style.Alignment.Start | Flex_start | End | Flex_end | Center | Stretch ->
        0.
    | Space_between -> free_space /. float_of_int (num_items - 1)
    | Space_around -> free_space /. float_of_int num_items
    | Space_evenly -> free_space /. float_of_int (num_items + 1)
