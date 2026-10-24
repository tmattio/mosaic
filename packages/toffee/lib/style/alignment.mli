(** CSS alignment properties for Flexbox and Grid layouts.

    This module provides types for controlling how child nodes align within
    their containers. Alignment properties control positioning along both the
    main/inline axis (justify) and cross/block axis (align).

    {1 Axis Behavior}

    In Flexbox:
    - [align-*] properties control cross-axis alignment
    - [justify-content] controls main-axis alignment

    In Grid:
    - [align-*] properties control block-axis alignment
    - [justify-*] properties control inline-axis alignment

    See
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Box_Alignment}MDN:
     CSS Box Alignment}. *)

(** Controls how child nodes are aligned.

    For Flexbox it controls alignment in the cross axis. For Grid it controls
    alignment in the block axis.

    See
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/align-items}MDN:
     align-items}. *)
module Align_items : sig
  type t =
    | Start  (** Pack items toward the start of the axis. *)
    | End  (** Pack items toward the end of the axis. *)
    | Flex_start
        (** Pack items toward the flex-relative start of the axis.

            For flex containers with [flex_direction] [Row_reverse] or
            [Column_reverse], this is equivalent to [End]. In all other cases it
            is equivalent to [Start]. *)
    | Flex_end
        (** Pack items toward the flex-relative end of the axis.

            For flex containers with [flex_direction] [Row_reverse] or
            [Column_reverse], this is equivalent to [Start]. In all other cases
            it is equivalent to [End]. *)
    | Center  (** Pack items along the center of the axis. *)
    | Baseline  (** Align items such that their baselines align. *)
    | Stretch  (** Stretch items to fill the container. *)

  val default : t
  (** [default] returns [Stretch]. *)

  val to_string : t -> string
  (** [to_string x] converts the alignment value to its CSS string
      representation. *)
end

(** Controls how child nodes are aligned.

    Does not apply to Flexbox and will be ignored if specified on a flex
    container. For Grid it controls alignment in the inline axis.

    See
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/justify-items}MDN:
     justify-items}. *)
module Justify_items : sig
  type t = Align_items.t

  val default : t
  (** [default] returns [Stretch]. *)

  val to_string : t -> string
  (** [to_string x] converts the alignment value to its CSS string
      representation. *)
end

(** Controls alignment of an individual child node.

    Overrides the parent node's [align_items] property. For Flexbox it controls
    alignment in the cross axis. For Grid it controls alignment in the block
    axis.

    See
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/align-self}MDN:
     align-self}. *)
module Align_self : sig
  type t = Align_items.t

  val default : t
  (** [default] returns [Stretch]. *)

  val to_string : t -> string
  (** [to_string x] converts the alignment value to its CSS string
      representation. *)
end

(** Controls alignment of an individual child node.

    Overrides the parent node's [justify_items] property. Does not apply to
    Flexbox and will be ignored if specified on a flex child. For Grid it
    controls alignment in the inline axis.

    See
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/justify-self}MDN:
     justify-self}. *)
module Justify_self : sig
  type t = Align_items.t

  val default : t
  (** [default] returns [Stretch]. *)

  val to_string : t -> string
  (** [to_string x] converts the alignment value to its CSS string
      representation. *)
end

(** Sets the distribution of space between and around content items.

    For Flexbox it controls alignment in the cross axis. For Grid it controls
    alignment in the block axis.

    See
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/align-content}MDN:
     align-content}. *)
module Align_content : sig
  type t =
    | Start  (** Pack items toward the start of the axis. *)
    | End  (** Pack items toward the end of the axis. *)
    | Flex_start
        (** Pack items toward the flex-relative start of the axis.

            For flex containers with [flex_direction] [Row_reverse] or
            [Column_reverse], this is equivalent to [End]. In all other cases it
            is equivalent to [Start]. *)
    | Flex_end
        (** Pack items toward the flex-relative end of the axis.

            For flex containers with [flex_direction] [Row_reverse] or
            [Column_reverse], this is equivalent to [Start]. In all other cases
            it is equivalent to [End]. *)
    | Center  (** Center items around the middle of the axis. *)
    | Stretch  (** Stretch items to fill the container. *)
    | Space_between
        (** Distribute items evenly with the first item flush with the start and
            the last item flush with the end.

            The gap between items is distributed evenly. No gap exists between
            the container edges and the first/last items. *)
    | Space_evenly
        (** Distribute items evenly with equal spacing between all items and
            container edges.

            The gap between the container edges and the first/last items is
            exactly the same as the gap between items. *)
    | Space_around
        (** Distribute items evenly with half-size gaps at the container edges.

            The gap between the container edges and the first/last items is
            exactly half the gap between items. *)

  val default : t
  (** [default] returns [Stretch]. *)

  val to_string : t -> string
  (** [to_string x] converts the alignment value to its CSS string
      representation. *)
end

(** Sets the distribution of space between and around content items.

    For Flexbox it controls alignment in the main axis. For Grid it controls
    alignment in the inline axis.

    See
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/justify-content}MDN:
     justify-content}. *)
module Justify_content : sig
  type t = Align_content.t

  val default : t
  (** [default] returns [Flex_start]. *)

  val to_string : t -> string
  (** [to_string x] converts the alignment value to its CSS string
      representation. *)
end
