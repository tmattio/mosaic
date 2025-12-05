(** Layout input constraints passed from parent to child during layout
    computation.

    Layout input encapsulates the sizing constraints, hints, and execution mode
    that govern how a child node computes its layout. Parent nodes construct
    layout inputs based on their own constraints, available space, and the
    child's position in the layout tree, then pass them down when invoking child
    layout algorithms.

    Key components include execution mode (full layout vs. size-only
    computation), sizing mode (whether to apply style constraints), requested
    axes, known dimensions for constraint solving, parent size for percentage
    resolution, available space for wrapping decisions, and margin
    collapsibility flags for CSS Block layout. *)

type t = {
  run_mode : Run_mode.t;
      (** Execution mode determining whether to compute full layout or size
          only. *)
  sizing_mode : Sizing_mode.t;
      (** Whether to apply inherent size styles (width, height, min/max
          constraints, aspect ratio) or measure content size only. *)
  axis : Requested_axis.t;
      (** Requested axis dimensions. [Horizontal], [Vertical], or [Both]. *)
  known_dimensions : float option Geometry.size;
      (** Fixed dimensions acting as hard constraints during layout.

          When [width] is [Some w], layout treats the width as fixed at [w] and
          computes the height accordingly. When both are [Some], this represents
          final layout with exact dimensions, and the node performs positioning
          without further size computation. *)
  parent_size : float option Geometry.size;
      (** Parent container dimensions for resolving percentage-based sizes.

          When a style specifies [width: 50%], this value provides the reference
          for computing the absolute pixel width. *)
  available_space : Available_space.t Geometry.size;
      (** Available space for layout, acting as a soft constraint.

          Used primarily for wrapping decisions in Flexbox and intrinsic sizing
          in Grid. Can be [Definite pixel_count], [Min_content], or
          [Max_content]. *)
  vertical_margins_are_collapsible : bool Geometry.line;
      (** Margin collapsibility for top and bottom margins.

          Specific to CSS Block layout's margin collapsing algorithm. Both
          fields are typically [false] for Flexbox and Grid layouts. *)
}

val make :
  run_mode:Run_mode.t ->
  sizing_mode:Sizing_mode.t ->
  axis:Requested_axis.t ->
  known_dimensions:float option Geometry.size ->
  parent_size:float option Geometry.size ->
  available_space:Available_space.t Geometry.size ->
  vertical_margins_are_collapsible:bool Geometry.line ->
  t
(** [make ~run_mode ~sizing_mode ~axis ~known_dimensions ~parent_size
     ~available_space ~vertical_margins_are_collapsible] creates a layout input.
*)

val run_mode : t -> Run_mode.t
(** [run_mode t] returns the execution mode. *)

val sizing_mode : t -> Sizing_mode.t
(** [sizing_mode t] returns the sizing mode. *)

val axis : t -> Requested_axis.t
(** [axis t] returns the requested axis. *)

val known_dimensions : t -> float option Geometry.size
(** [known_dimensions t] returns the known dimensions. *)

val parent_size : t -> float option Geometry.size
(** [parent_size t] returns the parent size. *)

val available_space : t -> Available_space.t Geometry.size
(** [available_space t] returns the available space. *)

val vertical_margins_are_collapsible : t -> bool Geometry.line
(** [vertical_margins_are_collapsible t] returns margin collapsibility flags. *)

val hidden : t
(** [hidden] is a layout input for nodes with [Display.None].

    Sets [run_mode] to [Perform_hidden_layout], which causes the node to receive
    a zero-sized layout without computing children. Other fields are set to
    defaults: [Inherent_size] sizing mode, [Both] axis, no known dimensions or
    parent size, [Max_content] available space, and non-collapsible margins. *)

val to_string : t -> string
(** [to_string t] returns a string representation. *)

val compare : t -> t -> int
(** [compare a b] compares layout inputs lexicographically.

    Compares fields in order: [run_mode], [sizing_mode], [axis],
    [known_dimensions], [parent_size], [available_space], and
    [vertical_margins_are_collapsible]. Returns a negative integer if [a < b],
    zero if [a = b], and a positive integer if [a > b]. *)

val equal : t -> t -> bool
(** [equal a b] tests equality of layout inputs. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] pretty-prints [t] to [fmt]. *)
