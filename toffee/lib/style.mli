(** style.mli ― OCaml interface for the Taffy-stylebox *)

open Geometry
(* provides [point], [size], [rect], [line] and [('a,'b) min_max] *)

module Length_percentage : sig
  type t =
    | Length of float  (** absolute length *)
    | Percent of float  (** percentage in the range [0.0, 1.0] *)
end

module Length_percentage_auto : sig
  type t = Length of float | Percent of float | Auto
end

module Dimension : sig
  type t = Length of float | Percent of float | Auto

  val auto : t
  val length : float -> t
  val percent : float -> t

  val maybe_resolve :
    t -> float option -> (int -> float -> float) -> float option

  val resolve_or_zero : t -> float option -> (int -> float -> float) -> float
end

module Available_space : sig
  type t =
    | Definite of float  (** a concrete number of CSS pixels *)
    | Min_content  (** indefinite → min-content constraint *)
    | Max_content  (** indefinite → max-content constraint *)

  val maybe_sub : t -> float -> t
  val map_definite_value : t -> f:(float -> float) -> t
  val maybe_set : t -> float option -> t
  val from : float -> t
  val into_option : t -> float option
  val compute_free_space : t -> float -> float
  val is_definite : t -> bool
end

module Alignment : sig
  (** Flex-/Grid-align values (`align-items`, `justify-items`, …) *)
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

  (** Space-distribution along a track (`align-content`, `justify-content`). *)
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

module Block : sig
  (** Emulates legacy `<center>`/`align=` behaviour. *)
  type text_align = Auto | Legacy_left | Legacy_right | Legacy_center
end

module Flex : sig
  type flex_direction = Row | Column | Row_reverse | Column_reverse
  type flex_wrap = No_wrap | Wrap | Wrap_reverse

  val is_reverse : flex_direction -> bool
end

module Grid : sig
  type grid_line = int
  (** 1-based CSS grid line index (negative = reverse lookup). *)

  (** Placement spec for `grid-row/column-(start|end)`. *)
  type grid_placement =
    | Auto
    | Line of grid_line
    | Span of int  (** strictly positive *)

  (* ─ Track-sizing functions ─ *)
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

type display = Block | Flex | Grid | None
type box_generation_mode = Normal | None  (** “normal” ≅ not display:none *)
type position = Relative | Absolute
type box_sizing = Border_box | Content_box
type overflow = Visible | Clip | Hidden | Scroll

val is_scroll_container : overflow -> bool

type style = {
  (* — core — *)
  display : display;
  position : position;
  box_sizing : box_sizing;
  overflow : overflow point;
  scrollbar_width : float;
  (* — offsets & spacing — *)
  inset : Length_percentage_auto.t rect;
  margin : Length_percentage_auto.t rect;  (** **default = zero** *)
  padding : Length_percentage.t rect;
  border : Length_percentage.t rect;
  (* — sizing — *)
  size : Dimension.t size;
  min_size : Dimension.t size;
  max_size : Dimension.t size;
  aspect_ratio : float option;  (** width ÷ height *)
  (* — container gaps / alignment — *)
  gap : Length_percentage.t size;
  align_items : Alignment.align_items option;
  align_self : Alignment.align_self option;
  align_content : Alignment.align_content option;
  justify_items : Alignment.justify_items option;
  justify_self : Alignment.justify_self option;
  justify_content : Alignment.justify_content option;
  (* — block layout — *)
  text_align : Block.text_align;
  (* — legacy flags — *)
  item_is_table : bool;
      (** true = treat child as display:table (block layout) *)
  item_is_replaced : bool;  (** image / video / form-control etc. *)
  (* — flexbox — *)
  flex_direction : Flex.flex_direction;
  flex_wrap : Flex.flex_wrap;
  flex_grow : float;
  flex_shrink : float;
  flex_basis : Dimension.t;
  (* — grid container — *)
  grid_template_rows : Grid.track_sizing_function list;
  grid_template_columns : Grid.track_sizing_function list;
  grid_auto_rows : Grid.non_repeated_track_sizing_function list;
  grid_auto_columns : Grid.non_repeated_track_sizing_function list;
  grid_auto_flow : Grid.grid_auto_flow;
  (* — grid item — *)
  grid_row : Grid.grid_placement line;
  grid_column : Grid.grid_placement line;
}

(* Accessor functions *)
val margin : style -> Length_percentage_auto.t rect
val padding : style -> Length_percentage.t rect
val border : style -> Length_percentage.t rect
val size : style -> Dimension.t size
val min_size : style -> Dimension.t size
val max_size : style -> Dimension.t size
val aspect_ratio : style -> float option
val overflow : style -> overflow point
val scrollbar_width : style -> float
val box_sizing : style -> box_sizing
val position : style -> position
val inset : style -> Length_percentage_auto.t rect
val is_block : style -> bool
val box_generation_mode : style -> box_generation_mode

val default : style
(** Spec-compliant default (matches `Style::DEFAULT` in Rust). Notably, **margin
    defaults to zero**, not `auto`. *)

(* — generic geometry helpers — *)
val length : float -> Length_percentage.t
val percent : float -> Length_percentage.t

val auto : 'a
(** polymorphic `auto` helper – use with care *)

val zero : 'a
(** polymorphic zero helper – use with care *)

(* — grid helpers (mirror taffy::style_helpers) — *)
val fr : float -> Grid.max_track_sizing_function
val flex : float -> Grid.non_repeated_track_sizing_function

val minmax :
  Grid.min_track_sizing_function ->
  Grid.max_track_sizing_function ->
  Grid.non_repeated_track_sizing_function

val line : int -> Grid.grid_placement
(** 1-based; 0 ⇒ `Auto` *)

val span : int -> Grid.grid_placement
(** positive span *)

val repeat :
  Grid.grid_track_repetition ->
  Grid.non_repeated_track_sizing_function list ->
  Grid.track_sizing_function

(** Collapsible margin set for block layout *)
module Collapsible_margin_set : sig
  type t = { positive : float; negative : float }

  val zero : t
  val of_margin : float -> t
  val collapse_with_margin : t -> float -> t
  val collapse_with_set : t -> t -> t
  val resolve : t -> float
end
