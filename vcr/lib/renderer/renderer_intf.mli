type frame = {
  grid : Grid.t;
  cursor_row : int;
  cursor_col : int;
  cursor_visible : bool;
  delay_cs : int;  (** Delay in centiseconds (1/100th of a second) *)
  dirty_regions : Grid.dirty_region list;
      (** Regions that changed since last frame *)
  cursor_moved : bool;  (** Whether cursor position changed *)
}
(** A frame to render *)

module type S = sig
  type config
  (** Renderer-specific configuration. Should include terminal dimensions
      (rows/cols) to enable streaming encoding where applicable. *)

  type t

  val create : rows:int -> cols:int -> config -> t
  (** Create a new renderer *)

  val write_frame :
    t ->
    frame ->
    incremental:bool ->
    writer:(bytes -> int -> int -> unit) ->
    unit
  (** Write a frame.
      @param frame The frame to render
      @param incremental
        If true, this is an incremental update (allows optimizations)
      @param writer
        Function to write bytes (e.g., to file, channel, buffer). Called as
        [writer buf offset length]. *)

  val finalize : t -> writer:(bytes -> int -> int -> unit) -> unit
end
