(* style.ml *)

open Geometry

(* ─────────────────────────  Dimension  ────────────────────────── *)

module Length_percentage = struct
  type t =
    | Length of float  (** absolute length, e.g. px *)
    | Percent of float  (** 0.0 – 1.0 *)
end

module Length_percentage_auto = struct
  type t =
    | Length of float
    | Percent of float
    | Auto  (** same semantics as CSS "auto" *)
end

module Dimension = struct
  type t = Length of float | Percent of float | Auto

  let auto = Auto
  let length v = Length v
  let percent v = Percent v

  let maybe_resolve self context _calc =
    match self with
    | Auto -> None
    | Length value -> Some value
    | Percent value -> Option.map (fun dim -> dim *. value) context

  let resolve_or_zero self context calc =
    match maybe_resolve self context calc with
    | Some value -> value
    | None -> 0.0
end

(* ─────────────────────────  Available-space  ───────────────────── *)

module Available_space = struct
  type t = Definite of float | Min_content | Max_content

  let maybe_sub available_space value =
    match available_space with
    | Definite x -> Definite (x -. value)
    | Min_content -> Min_content
    | Max_content -> Max_content

  let map_definite_value available_space ~f =
    match available_space with
    | Definite x -> Definite (f x)
    | Min_content -> Min_content
    | Max_content -> Max_content

  let maybe_set available_space value =
    match value with Some v -> Definite v | None -> available_space

  let from value = Definite value

  let into_option = function
    | Definite v -> Some v
    | Min_content | Max_content -> None

  let compute_free_space available_space used_space =
    match available_space with
    | Max_content -> Float.infinity
    | Min_content -> 0.0
    | Definite available_space -> available_space -. used_space

  let is_definite = function Definite _ -> true | _ -> false
end

(* ─────────────────────────  Alignment  ─────────────────────────── *)

module Alignment = struct
  type align_items =
    | Start
    | End
    | Flex_start
    | Flex_end
    | Center
    | Baseline
    | Stretch

  type justify_items = align_items
  type align_self = align_items
  type justify_self = align_items

  type align_content =
    | Start
    | End
    | Flex_start
    | Flex_end
    | Center
    | Stretch
    | Space_between
    | Space_evenly
    | Space_around

  type justify_content = align_content
end

(* ─────────────────────────  Block  ─────────────────────────────── *)

module Block = struct
  type text_align = Auto | Legacy_left | Legacy_right | Legacy_center
end

(* ─────────────────────────  Flex  ──────────────────────────────── *)

module Flex = struct
  type flex_direction = Row | Column | Row_reverse | Column_reverse
  type flex_wrap = No_wrap | Wrap | Wrap_reverse
end

(* ─────────────────────────  Grid  ──────────────────────────────── *)

module Grid = struct
  type grid_line = int (* 1-based (CSS) *)
  type grid_placement = Auto | Line of grid_line | Span of int (* positive *)

  type min_track_sizing_function =
    | Length of float
    | Percent of float
    | Auto
    | Min_content
    | Max_content

  type max_track_sizing_function =
    | Length of float
    | Percent of float
    | Auto
    | Min_content
    | Max_content
    | Fit_content of Length_percentage.t
    | Fr of float

  type non_repeated_track_sizing_function = {
    min : min_track_sizing_function;
    max : max_track_sizing_function;
  }

  type grid_track_repetition = Auto_fill | Auto_fit | Count of int

  type track_sizing_function =
    | Single of non_repeated_track_sizing_function
    | Repeat of grid_track_repetition * non_repeated_track_sizing_function list

  type grid_auto_flow = Row | Column | Row_dense | Column_dense
end

(* ───────────────────────  Core enumerations  ───────────────────── *)

type display = Block | Flex | Grid | None
type box_generation_mode = Normal | None (* abstracted “display” *)
type position = Relative | Absolute
type box_sizing = Border_box | Content_box
type overflow = Visible | Clip | Hidden | Scroll

(* ─────────────────────────  Style record  ──────────────────────── *)

type style = {
  display : display;
  position : position;
  box_sizing : box_sizing;
  overflow : overflow point;
  scrollbar_width : float;
  inset : Length_percentage_auto.t rect;
  margin : Length_percentage_auto.t rect;
  padding : Length_percentage.t rect;
  border : Length_percentage.t rect;
  size : Dimension.t size;
  min_size : Dimension.t size;
  max_size : Dimension.t size;
  aspect_ratio : float option;
  gap : Length_percentage.t size;
  align_items : Alignment.align_items option;
  align_self : Alignment.align_self option;
  align_content : Alignment.align_content option;
  justify_items : Alignment.justify_items option;
  justify_self : Alignment.justify_self option;
  justify_content : Alignment.justify_content option;
  text_align : Block.text_align;
  item_is_table : bool;  (** legacy table flag *)
  item_is_replaced : bool;  (** replaced element flag *)
  (*  Flex  *)
  flex_direction : Flex.flex_direction;
  flex_wrap : Flex.flex_wrap;
  flex_grow : float;
  flex_shrink : float;
  flex_basis : Dimension.t;
  (*  Grid  *)
  grid_template_rows : Grid.track_sizing_function list;
  grid_template_columns : Grid.track_sizing_function list;
  grid_auto_rows : Grid.non_repeated_track_sizing_function list;
  grid_auto_columns : Grid.non_repeated_track_sizing_function list;
  grid_auto_flow : Grid.grid_auto_flow;
  grid_row : Grid.grid_placement line;
  grid_column : Grid.grid_placement line;
}

(* ─────────────────────────  Accessor functions  ───────────────────────────── *)

let margin style = style.margin
let padding style = style.padding
let border style = style.border
let size style = style.size
let min_size style = style.min_size
let max_size style = style.max_size
let aspect_ratio style = style.aspect_ratio
let overflow style = style.overflow
let scrollbar_width style = style.scrollbar_width
let box_sizing style = style.box_sizing
let position style = style.position
let inset style = style.inset
let is_block style = style.display = Block

(* ─────────────────────────  Helpers  ───────────────────────────── *)

(* Plain helpers that mimic taffy's style_helpers.rs *)

let length (v : float) : Length_percentage.t = Length_percentage.Length v
let percent (p : float) : Length_percentage.t = Length_percentage.Percent p

(* "Auto" and "zero" are intentionally polymorphic – they rely on type-
   directed usage exactly like taffy's const-generics helpers.  They're
   implemented with Obj.magic to stay totally generic; in real code you
   would want specialised helpers per type instead. *)
let auto : 'a = Obj.magic "auto"
let zero : 'a = Obj.magic 0
let fr (n : float) : Grid.max_track_sizing_function = Grid.Fr n

let minmax (mn : Grid.min_track_sizing_function)
    (mx : Grid.max_track_sizing_function) :
    Grid.non_repeated_track_sizing_function =
  { min = mn; max = mx }

let flex (n : float) : Grid.non_repeated_track_sizing_function =
  minmax (Grid.Length 0.) (fr n)

let line (i : int) : Grid.grid_placement = if i = 0 then Auto else Line i
let span (n : int) : Grid.grid_placement = Span n

let repeat (rep : Grid.grid_track_repetition)
    (tracks : Grid.non_repeated_track_sizing_function list) :
    Grid.track_sizing_function =
  Repeat (rep, tracks)

(* ─────────────────────────  Collapsible_margin_set  ──────────────────────────── *)

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

(* ─────────────────────────  Defaults  ──────────────────────────── *)

let default : style =
  let lp0 : Length_percentage.t = Length_percentage.Length 0.0
  and lpa0 : Length_percentage_auto.t =
    Length_percentage_auto.Length 0.0 (* zero but _auto-able_ type *)
  and auto_dim : Dimension.t = Dimension.Auto in
  {
    display = Flex;
    (* taffy’s compile-time default order *)
    position = Relative;
    box_sizing = Border_box;
    overflow = { x = Visible; y = Visible };
    scrollbar_width = 0.0;
    inset = { left = Auto; right = Auto; top = Auto; bottom = Auto };
    margin = { left = lpa0; right = lpa0; top = lpa0; bottom = lpa0 };
    padding = { left = lp0; right = lp0; top = lp0; bottom = lp0 };
    border = { left = lp0; right = lp0; top = lp0; bottom = lp0 };
    size = { width = auto_dim; height = auto_dim };
    min_size = { width = auto_dim; height = auto_dim };
    max_size = { width = auto_dim; height = auto_dim };
    aspect_ratio = None;
    gap = { width = lp0; height = lp0 };
    align_items = None;
    align_self = None;
    align_content = None;
    justify_items = None;
    justify_self = None;
    justify_content = None;
    text_align = Auto;
    item_is_table = false;
    item_is_replaced = false;
    flex_direction = Row;
    flex_wrap = No_wrap;
    flex_grow = 0.;
    flex_shrink = 1.;
    flex_basis = Dimension.Auto;
    grid_template_rows = [];
    grid_template_columns = [];
    grid_auto_rows = [];
    grid_auto_columns = [];
    grid_auto_flow = Row;
    grid_row = { start = Auto; end_ = Auto };
    grid_column = { start = Auto; end_ = Auto };
  }
