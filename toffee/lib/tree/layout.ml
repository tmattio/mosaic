open Geometry

module Run_mode = struct
  type t = Perform_layout | Compute_size | Perform_hidden_layout
end

module Sizing_mode = struct
  type t = Content_size | Inherent_size
end

module Requested_axis = struct
  type t = Horizontal | Vertical | Both

  let of_absolute_axis = function
    | Geometry.Horizontal -> Horizontal
    | Geometry.Vertical -> Vertical

  let to_absolute_axis = function
    | Horizontal -> Ok Geometry.Horizontal
    | Vertical -> Ok Geometry.Vertical
    | Both -> Error ()
end

module Collapsible_margin_set = struct
  type t = { positive : float; negative : float }

  let zero = { positive = 0.0; negative = 0.0 }

  let of_margin margin =
    if margin >= 0.0 then { positive = margin; negative = 0.0 }
    else { positive = 0.0; negative = margin }

  let collapse_with_margin t margin =
    if margin >= 0.0 then { t with positive = Float.max t.positive margin }
    else { t with negative = Float.min t.negative margin }

  let collapse_with_set t other =
    {
      positive = Float.max t.positive other.positive;
      negative = Float.min t.negative other.negative;
    }

  let resolve t = t.positive +. t.negative
end

module Layout_input = struct
  type t = {
    run_mode : Run_mode.t;
    sizing_mode : Sizing_mode.t;
    axis : Requested_axis.t;
    known_dimensions : float option size;
    parent_size : float option size;
    available_space : Style.Available_space.t size;
    vertical_margins_are_collapsible : bool line;
  }

  let hidden =
    {
      run_mode = Run_mode.Perform_hidden_layout;
      sizing_mode = Sizing_mode.Inherent_size;
      axis = Requested_axis.Both;
      known_dimensions = { width = None; height = None };
      parent_size = { width = None; height = None };
      available_space =
        {
          width = Style.Available_space.Max_content;
          height = Style.Available_space.Max_content;
        };
      vertical_margins_are_collapsible = { start = false; end_ = false };
    }
end

module Layout_output = struct
  type t = {
    size : float size;
    first_baselines : float option point;
    top_margin : Collapsible_margin_set.t;
    bottom_margin : Collapsible_margin_set.t;
    margins_can_collapse_through : bool;
  }

  let hidden =
    {
      size = size_zero;
      first_baselines = { x = None; y = None };
      top_margin = Collapsible_margin_set.zero;
      bottom_margin = Collapsible_margin_set.zero;
      margins_can_collapse_through = false;
    }

  let default = hidden

  let of_sizes_and_baselines ~size ~content_size:_ ~first_baselines =
    {
      size;
      first_baselines;
      top_margin = Collapsible_margin_set.zero;
      bottom_margin = Collapsible_margin_set.zero;
      margins_can_collapse_through = false;
    }

  let of_sizes ~size ~content_size =
    of_sizes_and_baselines ~size ~content_size
      ~first_baselines:{ x = None; y = None }

  let of_outer_size size = of_sizes ~size ~content_size:size_zero
end

module Layout = struct
  type t = {
    order : int;
    location : float point;
    size : float size;
    content_size : float size;
    scrollbar_size : float size;
    border : float rect;
    padding : float rect;
    margin : float rect;
  }

  let empty =
    {
      order = 0;
      location = point_zero;
      size = size_zero;
      content_size = size_zero;
      scrollbar_size = size_zero;
      border = rect_zero;
      padding = rect_zero;
      margin = rect_zero;
    }

  let with_order order = { empty with order }

  let content_box_width t =
    t.size.width -. t.padding.left -. t.padding.right -. t.border.left
    -. t.border.right

  let content_box_height t =
    t.size.height -. t.padding.top -. t.padding.bottom -. t.border.top
    -. t.border.bottom

  let content_box_size t =
    { width = content_box_width t; height = content_box_height t }

  let content_box_x t = t.location.x +. t.border.left +. t.padding.left
  let content_box_y t = t.location.y +. t.border.top +. t.padding.top

  let scroll_width t =
    Float.max 0.0
      (t.content_size.width
      +. Float.min t.scrollbar_size.width t.size.width
      -. t.size.width +. t.border.right)

  let scroll_height t =
    Float.max 0.0
      (t.content_size.height
      +. Float.min t.scrollbar_size.height t.size.height
      -. t.size.height +. t.border.bottom)
end
