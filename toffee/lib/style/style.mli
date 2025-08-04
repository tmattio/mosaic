(** Style types for CSS layout *)

type calc_resolver = int -> float -> float
(** Type for calc resolver function *)

(** {1 Length types} *)

(** A unit of linear measurement, representing either a fixed length or a
    percentage. *)
module Length_percentage : sig
  type t
  (** The type representing a length or percentage *)

  val length : float -> t
  (** Create a length value in pixels *)

  val percent : float -> t
  (** Create a percentage value (0.0 to 1.0) *)

  val calc : int -> t
  (** Create a calc value with the given index *)

  val zero : t
  (** Zero length constant *)

  val is_length : t -> bool
  (** Check if the value is a length *)

  val is_percent : t -> bool
  (** Check if the value is a percentage *)

  val is_calc : t -> bool
  (** Check if the value is a calc expression *)

  val value : t -> float
  (** Extract the numeric value *)

  val resolve : t -> float -> float
  (** Resolve the value given a context size *)

  val resolve_with_calc : t -> float -> calc_resolver -> float
  (** Resolve the value given a context size and calc resolver *)

  val to_string : t -> string
  (** Convert to string representation *)

  val equal : t -> t -> bool
  (** Equality function *)

  val compare : t -> t -> int
  (** Comparison function for use in sets/maps *)

  val pp : Format.formatter -> t -> unit
  (** Pretty printer *)

  val uses_percentage : t -> bool
  (** Check if the value depends on the parent size *)

  val resolved_percentage_size : t -> float -> float option
  (** Resolve percentage values against the parent size, returning None for
      non-percentage values *)

  val resolved_percentage_size_with_calc :
    t -> float -> calc_resolver -> float option
  (** Resolve percentage values against the parent size with calc support *)

  val maybe_resolve : t -> float option -> calc_resolver -> float option
  (** Resolve the value given an optional context size. Returns None if context
      is None. *)

  val resolve_or_zero : t -> float option -> calc_resolver -> float
  (** Resolve the value given an optional context size. Returns 0.0 if context
      is None. *)
end

(** A unit of linear measurement, which can also be 'auto'. *)
module Length_percentage_auto : sig
  type t
  (** The type representing a length, percentage, or auto *)

  val length : float -> t
  (** Create a length value in pixels *)

  val percent : float -> t
  (** Create a percentage value (0.0 to 1.0) *)

  val auto : t
  (** The 'auto' value *)

  val calc : int -> t
  (** Create a calc value with the given index *)

  val zero : t
  (** Zero length constant *)

  val is_length : t -> bool
  (** Check if the value is a length *)

  val is_percent : t -> bool
  (** Check if the value is a percentage *)

  val is_auto : t -> bool
  (** Check if the value is 'auto' *)

  val is_calc : t -> bool
  (** Check if the value is a calc expression *)

  val value : t -> float
  (** Extract the numeric value. Raises exception for 'auto' *)

  val resolve_to_option : t -> float -> float option
  (** Resolve the value given a context size. Returns None for 'auto' *)

  val resolve_to_option_with_calc : t -> float -> calc_resolver -> float option
  (** Resolve the value given a context size and calc resolver. Returns None for
      'auto' *)

  val to_string : t -> string
  (** Convert to string representation *)

  val equal : t -> t -> bool
  (** Equality function *)

  val compare : t -> t -> int
  (** Comparison function for use in sets/maps *)

  val pp : Format.formatter -> t -> unit
  (** Pretty printer *)

  val uses_percentage : t -> bool
  (** Check if the value depends on the parent size *)

  val resolved_percentage_size : t -> float -> float option
  (** Resolve percentage values against the parent size, returning None for
      non-percentage values *)

  val resolved_percentage_size_with_calc :
    t -> float -> calc_resolver -> float option
  (** Resolve percentage values against the parent size with calc support *)

  val maybe_resolve : t -> float option -> calc_resolver -> float option
  (** Resolve the value given an optional context size. Returns None if context
      is None or value is 'auto'. *)

  val resolve_or_zero : t -> float option -> calc_resolver -> float
  (** Resolve the value given an optional context size. Returns 0.0 if context
      is None or value is 'auto'. *)
end

(** A unit of linear measurement for main sizing properties. *)
module Dimension : sig
  type t
  (** The type representing a dimension (length, percentage, or auto) *)

  val length : float -> t
  (** Create a length value in pixels *)

  val percent : float -> t
  (** Create a percentage value (0.0 to 1.0) *)

  val auto : t
  (** The 'auto' value *)

  val calc : int -> t
  (** Create a calc value with the given index *)

  val zero : t
  (** Zero dimension constant *)

  val is_length : t -> bool
  (** Check if the value is a length *)

  val is_percent : t -> bool
  (** Check if the value is a percentage *)

  val is_auto : t -> bool
  (** Check if the value is 'auto' *)

  val is_calc : t -> bool
  (** Check if the value is a calc expression *)

  val value : t -> float
  (** Extract the numeric value. Raises exception for 'auto' *)

  val to_option : t -> float option
  (** Get length value if this is a length, None otherwise *)

  val to_string : t -> string
  (** Convert to string representation *)

  val equal : t -> t -> bool
  (** Equality function *)

  val compare : t -> t -> int
  (** Comparison function for use in sets/maps *)

  val pp : Format.formatter -> t -> unit
  (** Pretty printer *)

  val is_zero : t -> bool
  (** Check if the value is 0px *)

  val uses_percentage : t -> bool
  (** Check if the value depends on the parent size *)

  val resolved_percentage_size : t -> float -> float option
  (** Resolve percentage values against the parent size, returning None for
      non-percentage values *)

  val resolved_percentage_size_with_calc :
    t -> float -> calc_resolver -> float option
  (** Resolve percentage values against the parent size with calc support *)

  val maybe_resolve : t -> float option -> calc_resolver -> float option
  (** Resolve the value given an optional context size. Returns None if context
      is None or value is 'auto'. *)

  val resolve_or_zero : t -> float option -> calc_resolver -> float
  (** Resolve the value given an optional context size. Returns 0.0 if context
      is None or value is 'auto'. *)
end

(** {1 Layout control types} *)

(** Defines how an element generates boxes and how its children are laid out *)
module Display : sig
  type t =
    | Block  (** The children will follow the block layout algorithm *)
    | Flex  (** The children will follow the flexbox layout algorithm *)
    | Grid  (** The children will follow the CSS grid layout algorithm *)
    | None  (** The node is hidden and does not generate any boxes *)

  val default : t
  (** Default display value *)

  val to_string : t -> string
  (** Convert to string representation *)

  val is_none : t -> bool
  (** Check if display is None *)

  val equal : t -> t -> bool
  (** Equality function *)

  val compare : t -> t -> int
  (** Comparison function for use in sets/maps *)

  val pp : Format.formatter -> t -> unit
  (** Pretty printer *)
end

(** Controls whether a node's position is determined by the normal flow of
    layout *)
module Position : sig
  type t =
    | Relative  (** The offset is computed relative to the final position *)
    | Absolute
        (** The node is taken out of normal flow and positioned relative to its
            parent *)

  val default : t
  (** Default position value *)

  val to_string : t -> string
  (** Convert to string representation *)

  val equal : t -> t -> bool
  (** Equality function *)

  val compare : t -> t -> int
  (** Comparison function for use in sets/maps *)

  val pp : Format.formatter -> t -> unit
  (** Pretty printer *)
end

(** Controls what happens when content overflows a container *)
module Overflow : sig
  type t =
    | Visible  (** Content that overflows is visible *)
    | Clip  (** Content that overflows is clipped but doesn't affect layout *)
    | Hidden  (** Content that overflows is hidden *)
    | Scroll  (** Content that overflows scrolls *)

  val default : t
  (** Default overflow value *)

  val to_string : t -> string
  (** Convert to string representation *)

  val is_container : t -> bool
  (** Returns true for overflow modes that contain their contents (Hidden,
      Scroll) *)

  val to_automatic_min_size : t -> Dimension.t
  (** Convert overflow mode to automatic minimum size for flexbox/grid items *)

  val equal : t -> t -> bool
  (** Equality function *)

  val compare : t -> t -> int
  (** Comparison function for use in sets/maps *)

  val pp : Format.formatter -> t -> unit
  (** Pretty printer *)
end

(** Defines which box sizing model to use *)
module Box_sizing : sig
  type t =
    | Border_box  (** Size includes padding and border *)
    | Content_box  (** Size excludes padding and border *)

  val default : t
  (** Default box sizing value *)

  val to_string : t -> string
  (** Convert to string representation *)

  val equal : t -> t -> bool
  (** Equality function *)

  val compare : t -> t -> int
  (** Comparison function for use in sets/maps *)

  val pp : Format.formatter -> t -> unit
  (** Pretty printer *)
end

(** Defines whether a box should be generated or not *)
module Box_generation_mode : sig
  type t =
    | Normal  (** The node generates a box in the regular way *)
    | None  (** The node and its descendants generate no boxes *)

  val default : t
  (** Default box generation mode *)

  val to_string : t -> string
  (** Convert to string representation *)

  val equal : t -> t -> bool
  (** Equality function *)

  val compare : t -> t -> int
  (** Comparison function for use in sets/maps *)

  val pp : Format.formatter -> t -> unit
  (** Pretty printer *)
end

(** Text alignment for block layout *)
module Text_align : sig
  type t =
    | Auto  (** No special legacy text align behaviour *)
    | Legacy_left  (** Corresponds to -webkit-left or -moz-left in browsers *)
    | Legacy_right
        (** Corresponds to -webkit-right or -moz-right in browsers *)
    | Legacy_center
        (** Corresponds to -webkit-center or -moz-center in browsers *)

  val default : t
  (** Default text alignment value *)

  val to_string : t -> string
  (** Convert to string representation *)

  val equal : t -> t -> bool
  (** Equality function *)

  val compare : t -> t -> int
  (** Comparison function for use in sets/maps *)

  val pp : Format.formatter -> t -> unit
  (** Pretty printer *)
end

(** {1 Flexbox types} *)

(** The direction of the flexbox layout main axis *)
module Flex_direction : sig
  type t =
    | Row  (** Main axis is horizontal, left to right *)
    | Column  (** Main axis is vertical, top to bottom *)
    | Row_reverse  (** Main axis is horizontal, right to left *)
    | Column_reverse  (** Main axis is vertical, bottom to top *)

  val default : t
  (** Default flex direction *)

  val is_row : t -> bool
  (** Check if the direction is row or row-reverse *)

  val is_column : t -> bool
  (** Check if the direction is column or column-reverse *)

  val is_reverse : t -> bool
  (** Check if the direction is reversed *)

  val main_axis : t -> Geometry.Absolute_axis.t
  (** Get the main axis direction *)

  val cross_axis : t -> Geometry.Absolute_axis.t
  (** Get the cross axis direction *)

  val to_string : t -> string
  (** Convert to string representation *)

  val equal : t -> t -> bool
  (** Equality function *)

  val compare : t -> t -> int
  (** Comparison function for use in sets/maps *)

  val pp : Format.formatter -> t -> unit
  (** Pretty printer *)
end

(** Controls whether flex items wrap onto multiple lines *)
module Flex_wrap : sig
  type t =
    | No_wrap  (** Items stay on a single line *)
    | Wrap  (** Items wrap to multiple lines as needed *)
    | Wrap_reverse  (** Items wrap to multiple lines in reverse direction *)

  val default : t
  (** Default flex wrap value *)

  val to_string : t -> string
  (** Convert to string representation *)

  val equal : t -> t -> bool
  (** Equality function *)

  val compare : t -> t -> int
  (** Comparison function for use in sets/maps *)

  val pp : Format.formatter -> t -> unit
  (** Pretty printer *)
end

(** {1 Alignment types} *)

(** Controls how child nodes are aligned - for Flexbox it controls alignment in
    the cross axis *)
module Align_items : sig
  type t =
    | Start  (** Items are packed toward the start of the axis *)
    | End  (** Items are packed toward the end of the axis *)
    | Flex_start  (** Items are packed towards the flex-relative start *)
    | Flex_end  (** Items are packed towards the flex-relative end *)
    | Center  (** Items are packed along the center *)
    | Baseline  (** Items are aligned by their baselines *)
    | Stretch  (** Items stretch to fill the container *)

  val default : t
  val to_string : t -> string
end

(** Controls how child nodes are aligned - for Grid it controls alignment in the
    inline axis *)
module Justify_items : sig
  type t = Align_items.t

  val default : t
  val to_string : t -> string
end

(** Controls alignment of an individual node - overrides the parent's AlignItems
*)
module Align_self : sig
  type t = Align_items.t

  val default : t
  val to_string : t -> string
end

(** Controls alignment of an individual node - overrides the parent's
    JustifyItems *)
module Justify_self : sig
  type t = Align_items.t

  val default : t
  val to_string : t -> string
end

(** Sets the distribution of space between and around content items *)
module Align_content : sig
  type t =
    | Start  (** Content is packed toward the start *)
    | End  (** Content is packed toward the end *)
    | Flex_start  (** Content is packed towards the flex-relative start *)
    | Flex_end  (** Content is packed towards the flex-relative end *)
    | Center  (** Content is centered *)
    | Stretch  (** Content is stretched *)
    | Space_between  (** First/last items flush with edges, even gaps between *)
    | Space_evenly  (** Even gaps between all items including edges *)
    | Space_around  (** Even gaps, half-size at edges *)

  val default : t
  val to_string : t -> string
end

(** Sets the distribution of space between and around content items - for
    Flexbox it controls alignment in the main axis *)
module Justify_content : sig
  type t = Align_content.t

  val default : t
  val to_string : t -> string
end

(** {1 Layout constraints} *)

(** {1 Grid types} *)

(** Grid-specific types and utilities for CSS Grid layout computation *)
module Grid : sig
  type origin_zero_line = int
  (** 0-indexed line coordinate used internally during grid layout *)

  (** Grid placement in origin-zero coordinates *)
  module Origin_zero_placement : sig
    type t = Auto | Line of origin_zero_line | Span of int
  end

  type track_counts = {
    negative_implicit : int;
        (** Number of implicit tracks before the explicit grid *)
    explicit : int;  (** Number of explicit tracks *)
    positive_implicit : int;
        (** Number of implicit tracks after the explicit grid *)
  }
  (** Track counts for a grid axis *)

  type grid_line = int
  (** Grid line coordinate (CSS 1-indexed, negative allowed) *)

  (** {1 Coordinate conversion} *)

  val grid_line_to_origin_zero_line : grid_line -> int -> origin_zero_line
  (** Convert CSS grid line to origin-zero line *)

  (** {1 Track counts} *)

  val make_track_counts :
    negative_implicit:int ->
    explicit:int ->
    positive_implicit:int ->
    track_counts
  (** Create track counts *)

  val total_track_count : track_counts -> int
  (** Get total track count *)

  val oz_line_to_track : origin_zero_line -> track_counts -> int option
  (** Convert origin-zero line to track index *)

  val oz_line_to_next_track : origin_zero_line -> int
  (** Get the next track index from an origin-zero line *)

  val oz_line_range_to_track_range :
    origin_zero_line -> origin_zero_line -> int * int
  (** Convert an origin-zero line range to track range *)

  (** Controls whether grid items are placed row-wise or column-wise *)
  module Auto_flow : sig
    type t = Grid_auto_flow.t =
      | Row  (** Fill rows first *)
      | Column  (** Fill columns first *)
      | Row_dense  (** Fill rows first, packing items *)
      | Column_dense  (** Fill columns first, packing items *)

    val default : t
    val is_dense : t -> bool
    val primary_axis : t -> Geometry.Abstract_axis.t
    val to_string : t -> string
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val pp : Format.formatter -> t -> unit
  end

  (** Grid placement specification for a single axis *)
  module Placement : sig
    type grid_line = int

    type t = Grid_placement.t =
      | Auto  (** Place item according to auto-placement algorithm *)
      | Line of grid_line  (** Place item at specified line index *)
      | Named_line of string * int  (** Place item at specified named line *)
      | Span of int  (** Item should span specified number of tracks *)
      | Named_span of string * int
          (** Item should span until nth line named <name> *)

    val auto : t
    val line : int -> t
    val span : int -> t
    val named_line : string -> int -> t
    val named_span : string -> int -> t
    val default : t
    val to_string : t -> string
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val pp : Format.formatter -> t -> unit
    val is_definite : t -> bool

    val into_origin_zero_placement_ignoring_named :
      t -> int -> Origin_zero_placement.t

    val into_origin_zero_placement : t -> int -> Origin_zero_placement.t

    (** Operations on Line<GridPlacement> *)
    module Line : sig
      val into_origin_zero_ignoring_named :
        t Geometry.Line.t -> int -> Origin_zero_placement.t Geometry.Line.t

      val into_origin_zero :
        t Geometry.Line.t -> int -> Origin_zero_placement.t Geometry.Line.t

      val is_definite : t Geometry.Line.t -> bool
    end
  end

  (** Track sizing function for grid tracks *)
  module Track_sizing_function : sig
    type t = Track_sizing_function.t

    val make : min:Length_percentage.t -> max:Length_percentage.t -> t
    val min : t -> Length_percentage.t
    val max : t -> Length_percentage.t
    val auto : t
    val min_content : t
    val max_content : t
    val zero : t
    val fr : float -> t
    val length : float -> t
    val percent : float -> t
    val from_length_percentage : Length_percentage.t -> t
    val fit_content : Length_percentage.t -> t
    val minmax : min:Length_percentage.t -> max:Length_percentage.t -> t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val to_string : t -> string
    val pp : Format.formatter -> t -> unit
    val has_fixed_component : t -> bool
    val min_sizing_function : t -> Length_percentage.t
    val max_sizing_function : t -> Length_percentage.t

    (** Min track sizing function helpers *)
    module Min : sig
      val is_intrinsic : t -> bool
      val is_min_or_max_content : t -> bool
      val is_fr : t -> bool
      val is_auto : t -> bool
      val is_min_content : t -> bool
      val is_max_content : t -> bool
      val definite_value : t -> float option -> float option

      val definite_value_with_calc :
        t -> float option -> calc_resolver -> float option

      val resolved_percentage_size : t -> float -> float option
      val uses_percentage : t -> bool
    end

    (** Max track sizing function helpers *)
    module Max : sig
      val is_intrinsic : t -> bool
      val is_max_content_alike : t -> bool
      val is_fr : t -> bool
      val is_auto : t -> bool
      val is_min_content : t -> bool
      val is_max_content : t -> bool
      val is_fit_content : t -> bool
      val is_max_or_fit_content : t -> bool
      val fr_value : t -> float
      val has_definite_value : t -> float option -> bool
      val definite_value : t -> float option -> float option

      val definite_value_with_calc :
        t -> float option -> calc_resolver -> float option

      val definite_limit : t -> float option -> float option

      val definite_limit_with_calc :
        t -> float option -> calc_resolver -> float option

      val resolved_percentage_size : t -> float -> float option
      val uses_percentage : t -> bool
    end
  end

  (** Repetition count for repeated grid tracks *)
  module Repetition_count : sig
    type t = Repetition_count.t = Count of int | Auto_fill | Auto_fit

    val count : int -> t
    val auto_fill : t
    val auto_fit : t
    val to_string : t -> string
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val pp : Format.formatter -> t -> unit
    val is_auto : t -> bool
    val of_string : string -> (t, string) result
  end

  (** Grid repetition pattern *)
  module Repetition : sig
    type t = Grid_repetition.t

    val make :
      count:Repetition_count.t ->
      tracks:Track_sizing_function.t list ->
      line_names:string list list ->
      t

    val count : t -> Repetition_count.t
    val tracks : t -> Track_sizing_function.t list
    val line_names : t -> string list list
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val is_auto_repetition : t -> bool
    val track_count : t -> int
  end

  (** Component in a grid-template-* definition *)
  module Template_component : sig
    type t = Grid_template_component.t =
      | Single of Track_sizing_function.t
      | Repeat of Repetition.t

    val single : Track_sizing_function.t -> t
    val repeat : Repetition.t -> t
    val auto : t
    val min_content : t
    val max_content : t
    val zero : t
    val fr : float -> t
    val length : float -> t
    val percent : float -> t
    val fit_content : Length_percentage.t -> t
    val minmax : min:Length_percentage.t -> max:Length_percentage.t -> t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val to_string : t -> string
    val pp : Format.formatter -> t -> unit
    val is_auto_repetition : t -> bool
  end

  (** Named area in CSS Grid *)
  module Template_area : sig
    type t = Grid_template_area.t

    val make :
      name:string ->
      row_start:int ->
      row_end:int ->
      column_start:int ->
      column_end:int ->
      t

    val name : t -> string
    val row_start : t -> int
    val row_end : t -> int
    val column_start : t -> int
    val column_end : t -> int
    val equal : t -> t -> bool
    val compare : t -> t -> int
  end
end

(** {1 Type aliases} *)

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

(** {1 Main Style type} *)

type t
(** The main style struct representing CSS style properties *)

val default : t
(** Default style values *)

(** {2 Constructor} *)

val make :
  ?display:display ->
  ?box_sizing:box_sizing ->
  ?position:position ->
  ?overflow:overflow Geometry.point ->
  ?scrollbar_width:float ->
  ?text_align:text_align ->
  ?inset:length_percentage_auto Geometry.rect ->
  ?size:dimension Geometry.size ->
  ?min_size:dimension Geometry.size ->
  ?max_size:dimension Geometry.size ->
  ?aspect_ratio:float ->
  ?margin:length_percentage_auto Geometry.rect ->
  ?padding:length_percentage Geometry.rect ->
  ?border:length_percentage Geometry.rect ->
  ?gap:length_percentage Geometry.size ->
  ?align_items:align_items ->
  ?align_self:align_self ->
  ?align_content:align_content ->
  ?justify_items:justify_items ->
  ?justify_self:justify_self ->
  ?justify_content:justify_content ->
  ?flex_direction:flex_direction ->
  ?flex_wrap:flex_wrap ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:grid_template_component list ->
  ?grid_template_columns:grid_template_component list ->
  ?grid_auto_rows:track_sizing_function list ->
  ?grid_auto_columns:track_sizing_function list ->
  ?grid_auto_flow:grid_auto_flow ->
  ?grid_template_areas:grid_template_area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:grid_placement Geometry.line ->
  ?grid_column:grid_placement Geometry.line ->
  unit ->
  t
(** Create a style with optional parameters, defaulting to [default] for
    unspecified fields *)

(** {2 Accessors} *)

val display : t -> display
val item_is_table : t -> bool
val box_generation_mode : t -> box_generation_mode
val set_box_generation_mode : t -> box_generation_mode -> t
val is_compressible_replaced : t -> bool
val text_align : t -> text_align
val item_is_replaced : t -> bool
val box_sizing : t -> box_sizing
val overflow : t -> overflow Geometry.point
val scrollbar_width : t -> float
val position : t -> position
val inset : t -> length_percentage_auto Geometry.rect
val size : t -> dimension Geometry.size
val min_size : t -> dimension Geometry.size
val max_size : t -> dimension Geometry.size
val aspect_ratio : t -> float option
val margin : t -> length_percentage_auto Geometry.rect
val padding : t -> length_percentage Geometry.rect
val border : t -> length_percentage Geometry.rect
val gap : t -> length_percentage Geometry.size
val align_items : t -> align_items option
val align_self : t -> align_self option
val align_content : t -> align_content option
val justify_items : t -> justify_items option
val justify_self : t -> justify_self option
val justify_content : t -> justify_content option
val flex_direction : t -> flex_direction
val flex_wrap : t -> flex_wrap
val flex_grow : t -> float
val flex_shrink : t -> float
val flex_basis : t -> dimension
val grid_template_rows : t -> grid_template_component list
val grid_template_columns : t -> grid_template_component list
val grid_auto_rows : t -> track_sizing_function list
val grid_auto_columns : t -> track_sizing_function list
val grid_auto_flow : t -> grid_auto_flow
val grid_template_areas : t -> grid_template_area list
val grid_template_column_names : t -> string list list
val grid_template_row_names : t -> string list list
val grid_row : t -> grid_placement Geometry.line
val grid_column : t -> grid_placement Geometry.line

(** {2 Functional updates} *)

val set_display : t -> display -> t
val set_position : t -> position -> t
val set_overflow : t -> overflow Geometry.point -> t
val set_scrollbar_width : t -> float -> t
val set_text_align : t -> text_align -> t
val set_inset : t -> length_percentage_auto Geometry.rect -> t
val set_size : t -> dimension Geometry.size -> t
val set_min_size : t -> dimension Geometry.size -> t
val set_max_size : t -> dimension Geometry.size -> t
val set_aspect_ratio : t -> float option -> t
val set_margin : t -> length_percentage_auto Geometry.rect -> t
val set_padding : t -> length_percentage Geometry.rect -> t
val set_border : t -> length_percentage Geometry.rect -> t
val set_gap : t -> length_percentage Geometry.size -> t
val set_align_items : t -> align_items option -> t
val set_align_self : t -> align_self option -> t
val set_align_content : t -> align_content option -> t
val set_justify_items : t -> justify_items option -> t
val set_justify_self : t -> justify_self option -> t
val set_justify_content : t -> justify_content option -> t
val set_flex_direction : t -> flex_direction -> t
val set_flex_wrap : t -> flex_wrap -> t
val set_flex_grow : t -> float -> t
val set_flex_shrink : t -> float -> t
val set_flex_basis : t -> dimension -> t
val set_grid_template_rows : t -> grid_template_component list -> t
val set_grid_template_columns : t -> grid_template_component list -> t
val set_grid_auto_rows : t -> track_sizing_function list -> t
val set_grid_auto_columns : t -> track_sizing_function list -> t
val set_grid_auto_flow : t -> grid_auto_flow -> t
val set_grid_template_areas : t -> grid_template_area list -> t
val set_grid_template_column_names : t -> string list list -> t
val set_grid_template_row_names : t -> string list list -> t
val set_grid_row : t -> grid_placement Geometry.line -> t
val set_grid_column : t -> grid_placement Geometry.line -> t

(** {2 Common style patterns} *)

val flex_row : t
(** Style with flex display and row direction *)

val flex_column : t
(** Style with flex display and column direction *)

val absolute : t
(** Style with absolute positioning *)

val flex_1 : t
(** Style with flex_grow=1, flex_shrink=1, flex_basis=0 *)
