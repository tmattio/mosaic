(** Controls what happens when content overflows a container *)

type t =
  | Visible
      (** The automatic minimum size of this node as a flexbox/grid item should
          be based on the size of its content. Content that overflows this node
          should contribute to the scroll region of its parent. *)
  | Clip
      (** The automatic minimum size of this node as a flexbox/grid item should
          be based on the size of its content. Content that overflows this node
          should NOT contribute to the scroll region of its parent. *)
  | Hidden
      (** The automatic minimum size of this node as a flexbox/grid item should
          be 0. Content that overflows this node should NOT contribute to the
          scroll region of its parent. *)
  | Scroll
      (** The automatic minimum size of this node as a flexbox/grid item should
          be 0. Additionally, space should be reserved for a scrollbar. The
          amount of space reserved is controlled by the scrollbar_width
          property. Content that overflows this node should NOT contribute to
          the scroll region of its parent. *)

let default = Visible

let to_string = function
  | Visible -> "visible"
  | Clip -> "clip"
  | Hidden -> "hidden"
  | Scroll -> "scroll"

let is_container = function Hidden | Scroll -> true | Visible | Clip -> false

let to_automatic_min_size = function
  | Visible | Clip -> Dimension.auto
  | Hidden | Scroll -> Dimension.zero

let equal a b =
  match (a, b) with
  | Visible, Visible -> true
  | Clip, Clip -> true
  | Hidden, Hidden -> true
  | Scroll, Scroll -> true
  | _ -> false

let compare a b =
  let to_int = function
    | Visible -> 0
    | Clip -> 1
    | Hidden -> 2
    | Scroll -> 3
  in
  Int.compare (to_int a) (to_int b)

let pp fmt t = Format.pp_print_string fmt (to_string t)
