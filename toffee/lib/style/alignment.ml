module Align_items = struct
  type t =
    | Start (* Items are packed toward the start of the axis *)
    | End (* Items are packed toward the end of the axis *)
    | Flex_start
      (* Items are packed towards the flex-relative start of the axis.
         For flex containers with flex_direction RowReverse or ColumnReverse this is equivalent
         to End. In all other cases it is equivalent to Start. *)
    | Flex_end
      (* Items are packed towards the flex-relative end of the axis.
         For flex containers with flex_direction RowReverse or ColumnReverse this is equivalent
         to Start. In all other cases it is equivalent to End. *)
    | Center (* Items are packed along the center of the cross axis *)
    | Baseline (* Items are aligned such as their baselines align *)
    | Stretch (* Stretch to fill the container *)

  let default = Stretch

  let to_string (x : t) =
    match x with
    | Start -> "start"
    | End -> "end"
    | Flex_start -> "flex-start"
    | Flex_end -> "flex-end"
    | Center -> "center"
    | Baseline -> "baseline"
    | Stretch -> "stretch"
end

module Justify_items = struct
  type t = Align_items.t

  let default = Align_items.default
  let to_string = Align_items.to_string
end

module Align_self = struct
  type t = Align_items.t

  let default = Align_items.default
  let to_string = Align_items.to_string
end

module Justify_self = struct
  type t = Align_items.t

  let default = Align_items.default
  let to_string = Align_items.to_string
end

module Align_content = struct
  type t =
    | Start (* Items are packed toward the start of the axis *)
    | End (* Items are packed toward the end of the axis *)
    | Flex_start
      (* Items are packed towards the flex-relative start of the axis.
                 For flex containers with flex_direction RowReverse or ColumnReverse this is equivalent
                 to End. In all other cases it is equivalent to Start. *)
    | Flex_end
      (* Items are packed towards the flex-relative end of the axis.
               For flex containers with flex_direction RowReverse or ColumnReverse this is equivalent
               to Start. In all other cases it is equivalent to End. *)
    | Center (* Items are centered around the middle of the axis *)
    | Stretch (* Items are stretched to fill the container *)
    | Space_between
      (* The first and last items are aligned flush with the edges of the container (no gap)
                    The gap between items is distributed evenly. *)
    | Space_evenly
      (* The gap between the first and last items is exactly THE SAME as the gap between items.
                   The gaps are distributed evenly *)
    | Space_around
  (* The gap between the first and last items is exactly HALF the gap between items.
                   The gaps are distributed evenly in proportion to these ratios. *)

  let default = Stretch

  let to_string (x : t) =
    match x with
    | Start -> "start"
    | End -> "end"
    | Flex_start -> "flex-start"
    | Flex_end -> "flex-end"
    | Center -> "center"
    | Stretch -> "stretch"
    | Space_between -> "space-between"
    | Space_evenly -> "space-evenly"
    | Space_around -> "space-around"
end

module Justify_content = struct
  type t = Align_content.t

  let default = Align_content.Flex_start
  let to_string = Align_content.to_string
end
