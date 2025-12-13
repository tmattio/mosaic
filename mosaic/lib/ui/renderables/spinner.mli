(** Animated spinner indicator.

    Spinner displays a cycling animation from a sequence of frames, commonly
    used to indicate loading or processing states. Supports built-in spinner
    presets and custom frame sequences.

    {1 Overview}

    A spinner cycles through frames at a configurable interval. The animation
    runs via the renderer's frame callback, advancing one frame per interval.
    Spinners can be started, stopped, and reset programmatically.

    {1 Built-in Presets}

    Several common spinner styles are provided:

    - [Dots]: Classic braille dot pattern (default)
    - [Line]: Simple rotating line
    - [Circle]: Rotating circle quarters
    - [Bounce]: Bouncing ball
    - [Bar]: Growing/shrinking bar
    - [Arrow]: Rotating arrow

    Custom spinners can be created by providing a frame array directly. *)

type preset =
  | Dots
  | Line
  | Circle
  | Bounce
  | Bar
  | Arrow  (** Built-in spinner animation styles. *)

type t
(** Spinner state. *)

module Props : sig
  type t

  val make :
    ?preset:preset ->
    ?frames:string array ->
    ?interval:float ->
    ?autoplay:bool ->
    ?color:Ansi.Color.t ->
    ?background:Ansi.Color.t ->
    unit ->
    t
  (** [make ?preset ?frames ?interval ?autoplay ?color ?background ()]
      configures a spinner.

      - [preset]: Built-in animation style (default: [Dots])
      - [frames]: Custom frame sequence (overrides [preset] if provided)
      - [interval]: Seconds between frames (default: 0.08)
      - [autoplay]: Start animation on mount (default: true)
      - [color]: Foreground color (default: white)
      - [background]: Background color (default: transparent) *)

  val default : t

  val equal : t -> t -> bool
  (** [equal a b] reports structural equality between spinner props. *)
end

val apply_props : t -> Props.t -> unit
(** [apply_props spinner props] applies [props] to a mounted spinner using its
    setters. *)

val mount : ?props:Props.t -> Renderable.t -> t
(** [mount ?props node] configures [node] to render an animated spinner. *)

val node : t -> Renderable.t
(** [node t] returns the underlying renderable. *)

(** {1 Animation Control} *)

val start : t -> unit
(** [start t] begins or resumes the animation. *)

val stop : t -> unit
(** [stop t] pauses the animation at the current frame. *)

val reset : t -> unit
(** [reset t] stops the animation and resets to the first frame. *)

val is_running : t -> bool
(** [is_running t] returns [true] if the animation is active. *)

(** {1 Configuration} *)

val set_preset : t -> preset -> unit
(** [set_preset t preset] switches to a built-in animation style. *)

val set_frames : t -> string array -> unit
(** [set_frames t frames] sets a custom frame sequence. *)

val set_interval : t -> float -> unit
(** [set_interval t seconds] sets the frame duration. *)

val set_color : t -> Ansi.Color.t -> unit
(** [set_color t color] sets the foreground color. *)

val set_background : t -> Ansi.Color.t -> unit
(** [set_background t color] sets the background color. *)

(** {1 State} *)

val current_frame : t -> int
(** [current_frame t] returns the current frame index (0-based). *)

val frame_count : t -> int
(** [frame_count t] returns the total number of frames. *)
