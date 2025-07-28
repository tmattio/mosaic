module type S = sig
  type t
  (** The abstract type for a renderer's state. *)

  type config
  (** Renderer-specific configuration. *)

  val create : Vte.t -> config -> t
  (** [create vte config] creates a new renderer instance. *)

  val capture_frame : t -> unit
  (** [capture_frame renderer] captures the current state of the VTE as a single
      frame. *)

  val render : t -> string
  (** [render renderer] renders the captured frames and returns the output data
      as a string. *)
end
