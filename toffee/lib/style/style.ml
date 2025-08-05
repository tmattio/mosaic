(** Style types for CSS layout *)

(* Type for calc resolver function *)
type calc_resolver = Compact_length.calc_resolver

(* Re-export dimension modules *)
module Compact_length = Compact_length
module Length_percentage = Length_percentage
module Length_percentage_auto = Length_percentage_auto
module Dimension = Dimension

(* Re-export layout control modules *)
module Display = Display
module Position = Position
module Overflow = Overflow
module Box_sizing = Box_sizing
module Box_generation_mode = Box_generation_mode
module Text_align = Text_align

(* Re-export flexbox modules *)
module Flex_direction = Flex_direction
module Flex_wrap = Flex_wrap

(* Re-export alignment modules *)
module Align_items = Alignment.Align_items
module Align_self = Alignment.Align_self
module Align_content = Alignment.Align_content
module Justify_items = Alignment.Justify_items
module Justify_self = Alignment.Justify_self
module Justify_content = Alignment.Justify_content

(* Re-export grid modules *)
module Grid = struct
  include Grid
  module Auto_flow = Grid_auto_flow
  module Placement = Grid_placement
  module Track_sizing_function = Track_sizing_function
  module Repetition_count = Repetition_count
  module Repetition = Grid_repetition
  module Template_component = Grid_template_component
  module Template_area = Grid_template_area
end

(* Type aliases for convenience *)
type length_percentage = Length_percentage.t
type length_percentage_auto = Length_percentage_auto.t
type dimension = Dimension.t
type display = Display.t
type position = Position.t
type overflow = Overflow.t
type box_sizing = Box_sizing.t
type box_generation_mode = Box_generation_mode.t
type text_align = Text_align.t
type flex_direction = Flex_direction.t
type flex_wrap = Flex_wrap.t
type align_items = Align_items.t
type align_content = Align_content.t
type justify_items = Justify_items.t
type align_self = Align_self.t
type justify_self = Justify_self.t
type justify_content = Justify_content.t
type grid_auto_flow = Grid.Auto_flow.t
type grid_placement = Grid.Placement.t
type track_sizing_function = Grid.Track_sizing_function.t
type repetition_count = Grid.Repetition_count.t
type grid_repetition = Grid.Repetition.t
type grid_template_component = Grid.Template_component.t
type grid_template_area = Grid.Template_area.t

(* Main Style type *)
type t = {
  (* Display properties *)
  display : display;
  item_is_table : bool;
  item_is_replaced : bool;
  box_sizing : box_sizing;
  (* Block layout properties *)
  text_align : Text_align.t;
  (* Overflow properties *)
  overflow : overflow Geometry.point;
  scrollbar_width : float;
  (* Position properties *)
  position : position;
  inset : length_percentage_auto Geometry.rect;
  (* Size properties *)
  size : dimension Geometry.size;
  min_size : dimension Geometry.size;
  max_size : dimension Geometry.size;
  aspect_ratio : float option;
  (* Spacing properties *)
  margin : length_percentage_auto Geometry.rect;
  padding : length_percentage Geometry.rect;
  border : length_percentage Geometry.rect;
  (* Alignment properties *)
  align_items : align_items option;
  align_self : align_self option;
  justify_items : justify_items option;
  justify_self : justify_self option;
  align_content : align_content option;
  justify_content : justify_content option;
  (* Gap properties *)
  gap : length_percentage Geometry.size;
  (* Flexbox properties *)
  flex_direction : flex_direction;
  flex_wrap : flex_wrap;
  flex_grow : float;
  flex_shrink : float;
  flex_basis : dimension;
  (* Grid container properties *)
  grid_template_rows : grid_template_component list;
  grid_template_columns : grid_template_component list;
  grid_auto_rows : track_sizing_function list;
  grid_auto_columns : track_sizing_function list;
  grid_auto_flow : grid_auto_flow;
  grid_template_areas : grid_template_area list;
  grid_template_column_names : string list list;
  grid_template_row_names : string list list;
  (* Grid item properties *)
  grid_row : grid_placement Geometry.line;
  grid_column : grid_placement Geometry.line;
}

(* Default style values *)
let default =
  {
    (* Display properties *)
    display = Display.default;
    item_is_table = false;
    item_is_replaced = false;
    box_sizing = Box_sizing.default;
    (* Block layout properties *)
    text_align = Text_align.Auto;
    (* Overflow properties *)
    overflow = { x = Overflow.default; y = Overflow.default };
    scrollbar_width = 0.0;
    (* Position properties *)
    position = Position.default;
    inset =
      Geometry.Rect.make ~left:Length_percentage_auto.auto
        ~right:Length_percentage_auto.auto ~top:Length_percentage_auto.auto
        ~bottom:Length_percentage_auto.auto;
    (* Size properties *)
    size = { width = Dimension.auto; height = Dimension.auto };
    min_size = { width = Dimension.auto; height = Dimension.auto };
    max_size = { width = Dimension.auto; height = Dimension.auto };
    aspect_ratio = None;
    (* Spacing properties *)
    margin =
      Geometry.Rect.make ~left:Length_percentage_auto.zero
        ~right:Length_percentage_auto.zero ~top:Length_percentage_auto.zero
        ~bottom:Length_percentage_auto.zero;
    padding =
      Geometry.Rect.make ~left:Length_percentage.zero
        ~right:Length_percentage.zero ~top:Length_percentage.zero
        ~bottom:Length_percentage.zero;
    border =
      Geometry.Rect.make ~left:Length_percentage.zero
        ~right:Length_percentage.zero ~top:Length_percentage.zero
        ~bottom:Length_percentage.zero;
    (* Alignment properties *)
    align_items = None;
    align_self = None;
    justify_items = None;
    justify_self = None;
    align_content = None;
    justify_content = None;
    (* Gap properties *)
    gap = { width = Length_percentage.zero; height = Length_percentage.zero };
    (* Flexbox properties *)
    flex_direction = Flex_direction.default;
    flex_wrap = Flex_wrap.default;
    flex_grow = 0.0;
    flex_shrink = 1.0;
    flex_basis = Dimension.auto;
    (* Grid container properties *)
    grid_template_rows = [];
    grid_template_columns = [];
    grid_auto_rows = [];
    grid_auto_columns = [];
    grid_auto_flow = Grid_auto_flow.default;
    grid_template_areas = [];
    grid_template_column_names = [];
    grid_template_row_names = [];
    (* Grid item properties *)
    grid_row = { start = Grid_placement.auto; end_ = Grid_placement.auto };
    grid_column = { start = Grid_placement.auto; end_ = Grid_placement.auto };
  }

(* Accessors *)
let display t = t.display
let item_is_table t = t.item_is_table
let item_is_replaced t = t.item_is_replaced

let box_generation_mode t =
  match t.display with
  | Display.None -> Box_generation_mode.None
  | _ -> Box_generation_mode.Normal

let set_box_generation_mode t mode =
  match mode with
  | Box_generation_mode.None -> { t with display = Display.None }
  | Box_generation_mode.Normal -> t (* Keep current display mode *)

let is_compressible_replaced t = t.item_is_replaced
let text_align t = t.text_align
let box_sizing t = t.box_sizing
let overflow t = t.overflow
let scrollbar_width t = t.scrollbar_width
let position t = t.position
let inset t = t.inset
let size t = t.size
let min_size t = t.min_size
let max_size t = t.max_size
let aspect_ratio t = t.aspect_ratio
let margin t = t.margin
let padding t = t.padding
let border t = t.border
let gap t = t.gap
let align_items t = t.align_items
let align_self t = t.align_self
let align_content t = t.align_content
let justify_items t = t.justify_items
let justify_self t = t.justify_self
let justify_content t = t.justify_content
let flex_direction t = t.flex_direction
let flex_wrap t = t.flex_wrap
let flex_grow t = t.flex_grow
let flex_shrink t = t.flex_shrink
let flex_basis t = t.flex_basis
let grid_template_rows t = t.grid_template_rows
let grid_template_columns t = t.grid_template_columns
let grid_auto_rows t = t.grid_auto_rows
let grid_auto_columns t = t.grid_auto_columns
let grid_auto_flow t = t.grid_auto_flow
let grid_template_areas t = t.grid_template_areas
let grid_template_column_names t = t.grid_template_column_names
let grid_template_row_names t = t.grid_template_row_names
let grid_row t = t.grid_row
let grid_column t = t.grid_column

(* Functional updates *)
let set_display t display = { t with display }
let set_position t position = { t with position }
let set_overflow t overflow = { t with overflow }
let set_scrollbar_width t scrollbar_width = { t with scrollbar_width }
let set_text_align t text_align = { t with text_align }
let set_inset t inset = { t with inset }
let set_size t size = { t with size }
let set_min_size t min_size = { t with min_size }
let set_max_size t max_size = { t with max_size }
let set_aspect_ratio t aspect_ratio = { t with aspect_ratio }
let set_margin t margin = { t with margin }
let set_padding t padding = { t with padding }
let set_border t border = { t with border }
let set_gap t gap = { t with gap }
let set_align_items t align_items = { t with align_items }
let set_align_self t align_self = { t with align_self }
let set_align_content t align_content = { t with align_content }
let set_justify_items t justify_items = { t with justify_items }
let set_justify_self t justify_self = { t with justify_self }
let set_justify_content t justify_content = { t with justify_content }
let set_flex_direction t flex_direction = { t with flex_direction }
let set_flex_wrap t flex_wrap = { t with flex_wrap }
let set_flex_grow t flex_grow = { t with flex_grow }
let set_flex_shrink t flex_shrink = { t with flex_shrink }
let set_flex_basis t flex_basis = { t with flex_basis }
let set_grid_template_rows t grid_template_rows = { t with grid_template_rows }

let set_grid_template_columns t grid_template_columns =
  { t with grid_template_columns }

let set_grid_auto_rows t grid_auto_rows = { t with grid_auto_rows }
let set_grid_auto_columns t grid_auto_columns = { t with grid_auto_columns }
let set_grid_auto_flow t grid_auto_flow = { t with grid_auto_flow }

let set_grid_template_areas t grid_template_areas =
  { t with grid_template_areas }

let set_grid_template_column_names t grid_template_column_names =
  { t with grid_template_column_names }

let set_grid_template_row_names t grid_template_row_names =
  { t with grid_template_row_names }

let set_grid_row t grid_row = { t with grid_row }
let set_grid_column t grid_column = { t with grid_column }

(* Smart constructor *)
let make ?display ?(box_sizing = default.box_sizing)
    ?(position = default.position) ?(overflow = default.overflow)
    ?(scrollbar_width = default.scrollbar_width)
    ?(text_align = default.text_align) ?(inset = default.inset)
    ?(size = default.size) ?(min_size = default.min_size)
    ?(max_size = default.max_size) ?aspect_ratio ?(margin = default.margin)
    ?(padding = default.padding) ?(border = default.border) ?(gap = default.gap)
    ?align_items ?align_self ?align_content ?justify_items ?justify_self
    ?justify_content ?(flex_direction = default.flex_direction)
    ?(flex_wrap = default.flex_wrap) ?(flex_grow = default.flex_grow)
    ?(flex_shrink = default.flex_shrink) ?(flex_basis = default.flex_basis)
    ?(grid_template_rows = default.grid_template_rows)
    ?(grid_template_columns = default.grid_template_columns)
    ?(grid_auto_rows = default.grid_auto_rows)
    ?(grid_auto_columns = default.grid_auto_columns)
    ?(grid_auto_flow = default.grid_auto_flow)
    ?(grid_template_areas = default.grid_template_areas)
    ?(grid_template_column_names = default.grid_template_column_names)
    ?(grid_template_row_names = default.grid_template_row_names)
    ?(grid_row = default.grid_row) ?(grid_column = default.grid_column) () =
  (* Infer display mode from properties if not explicitly set *)
  let display =
    match display with
    | Some d -> d
    | None ->
        (* Default to Flex to match Taffy's behavior when flexbox feature is enabled *)
        Display.Flex
  in
  {
    display;
    item_is_table = false;
    item_is_replaced = false;
    box_sizing;
    text_align;
    overflow;
    scrollbar_width;
    position;
    inset;
    size;
    min_size;
    max_size;
    aspect_ratio;
    margin;
    padding;
    border;
    gap;
    align_items;
    align_self;
    align_content;
    justify_items;
    justify_self;
    justify_content;
    flex_direction;
    flex_wrap;
    flex_grow;
    flex_shrink;
    flex_basis;
    grid_template_rows;
    grid_template_columns;
    grid_auto_rows;
    grid_auto_columns;
    grid_auto_flow;
    grid_template_areas;
    grid_template_column_names;
    grid_template_row_names;
    grid_row;
    grid_column;
  }

(* Common style patterns *)
let flex_row =
  { default with display = Display.Flex; flex_direction = Flex_direction.Row }

let flex_column =
  {
    default with
    display = Display.Flex;
    flex_direction = Flex_direction.Column;
  }

let absolute = { default with position = Position.Absolute }

let flex_1 =
  {
    default with
    display = Display.Flex;
    flex_grow = 1.0;
    flex_shrink = 1.0;
    flex_basis = Dimension.length 0.0;
  }
