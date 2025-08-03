(*
  content_size.ml
  ---------------------------------------------------------------------------
  Shared **content‑size** helper used by every CSS layout algorithm.
  Direct translation of `content_size.rs`.

  ---------------------------------------------------------------------------
  SPDX-License-Identifier: MIT OR Apache-2.0
  ---------------------------------------------------------------------------
*)

open Style

(** How much width/height a given child contributes to its parentʼs *intrinsic
    content size*. Mirrors the logic of the Rust version. *)
let compute_content_size_contribution ~(location : float Geometry.point)
    ~(size : float Geometry.size) ~(content_size : float Geometry.size)
    ~(overflow : Style.overflow Geometry.point) : float Geometry.size =
  let open Geometry in
  let size_cs =
    {
      width =
        (match overflow.x with
        | Visible -> Float.max size.width content_size.width
        | _ -> size.width);
      height =
        (match overflow.y with
        | Visible -> Float.max size.height content_size.height
        | _ -> size.height);
    }
  in
  if size_cs.width > 0. && size_cs.height > 0. then
    {
      width = location.x +. size_cs.width;
      height = location.y +. size_cs.height;
    }
  else size_zero
