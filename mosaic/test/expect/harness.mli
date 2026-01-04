(** Test harness for Mosaic TEA applications.

    Provides an isolated runtime for testing TEA apps without terminal I/O. The
    harness renders views into a virtual terminal emulator and allows controlled
    input injection for simulating user interactions. *)

type ('model, 'msg) t
(** Mutable test harness state, tracking model, subscriptions, and render state.
*)

(** {1 Creation} *)

val create :
  ?subscriptions:('model -> 'msg Mosaic.Sub.t) ->
  width:int ->
  height:int ->
  init:(unit -> 'model * 'msg Mosaic.Cmd.t) ->
  update:('msg -> 'model -> 'model * 'msg Mosaic.Cmd.t) ->
  view:('model -> 'msg Mosaic.t) ->
  unit ->
  ('model, 'msg) t
(** [create ?subscriptions ~width ~height ~init ~update ~view ()] creates a
    harness with the given app components.

    The harness initializes the model via [init], sets up subscriptions, and
    prepares a virtual terminal emulator of the specified dimensions. *)

(** {1 Rendering} *)

val render : ('model, 'msg) t -> delta:float -> unit
(** [render t ~delta] performs a full render cycle without returning output.

    Processes pending messages, reconciles the view, and updates the internal
    VTE buffer. Use {!snapshot} or {!snapshot_ansi} to capture the result. *)

val snapshot : ('model, 'msg) t -> string
(** [snapshot t] returns the current terminal display as plain text.

    Each row is on its own line. Trailing spaces are preserved. No ANSI codes.
    This is the VTE's visual representation of what a user would see. *)

val snapshot_ansi : ?reset:bool -> ('model, 'msg) t -> string
(** [snapshot_ansi ?reset t] returns the current frame with ANSI escape codes.

    @param reset Include a final reset sequence (default [true]). *)

val step : ('model, 'msg) t -> delta:float -> string
(** [step t ~delta] renders a frame and returns plain text output.

    Combines {!render} and {!snapshot} into a single call. *)

val step_ansi : ?reset:bool -> ('model, 'msg) t -> delta:float -> string
(** [step_ansi ?reset t ~delta] renders a frame and returns ANSI output.

    Combines {!render} and {!snapshot_ansi} into a single call. *)

(** {1 Time and Sizing} *)

val tick : ('model, 'msg) t -> delta:float -> unit
(** [tick t ~delta] triggers the tick subscription with the given delta.

    Only dispatches if the app has an active [Sub.on_tick] subscription. *)

val resize : ('model, 'msg) t -> width:int -> height:int -> unit
(** [resize t ~width ~height] changes the terminal dimensions.

    Also triggers the resize subscription if the app has one. *)

(** {1 Model Access} *)

val model : ('model, 'msg) t -> 'model
(** [model t] returns the current model state. *)

(** {1 Message Dispatch} *)

val send : ('model, 'msg) t -> 'msg -> unit
(** [send t msg] dispatches a message directly to the update function.

    This is useful for triggering state changes without going through
    subscriptions or simulating input. *)

(** {1 Input Simulation} *)

val send_key : ('model, 'msg) t -> Matrix.Input.Key.event -> unit
(** [send_key t key_event] simulates a keyboard event.

    Triggers all [Sub.on_key] subscriptions and the renderer's key handler. *)

val send_paste : ('model, 'msg) t -> string -> unit
(** [send_paste t text] simulates a paste event.

    Triggers all [Sub.on_paste] subscriptions and the renderer's paste handler.
*)

val handle_key : ('model, 'msg) t -> Mosaic_ui.Event.key -> unit
(** [handle_key t ev] handles a wrapped key event directly. *)

val handle_mouse : ('model, 'msg) t -> Mosaic_ui.Event.mouse -> unit
(** [handle_mouse t ev] handles a mouse event directly.

    Triggers all [Sub.on_mouse] subscriptions and the renderer's mouse handler.
*)

val handle_paste : ('model, 'msg) t -> Mosaic_ui.Event.paste -> unit
(** [handle_paste t ev] handles a wrapped paste event directly. *)

(** {1 Event Construction Helpers} *)

val key :
  ?modifier:Matrix.Input.Key.modifier ->
  ?event_type:Matrix.Input.Key.event_type ->
  ?associated_text:string ->
  ?shifted_key:Uchar.t ->
  ?base_key:Uchar.t ->
  Matrix.Input.Key.t ->
  Matrix.Input.Key.event
(** [key k] constructs a key event for the given key. Alias for
    {!Matrix.Input.key_event}. *)

val char :
  ?modifier:Matrix.Input.Key.modifier ->
  ?event_type:Matrix.Input.Key.event_type ->
  ?associated_text:string ->
  ?shifted_key:Uchar.t ->
  ?base_key:Uchar.t ->
  char ->
  Matrix.Input.Key.event
(** [char c] constructs a key event for an ASCII character. Alias for
    {!Matrix.Input.char_event}. *)

val press :
  ?modifier:Matrix.Input.Key.modifier ->
  ?associated_text:string ->
  ?shifted_key:Uchar.t ->
  ?base_key:Uchar.t ->
  Matrix.Input.Key.t ->
  Matrix.Input.Key.event
(** [press k] constructs a press key event. *)

val repeat :
  ?modifier:Matrix.Input.Key.modifier ->
  ?associated_text:string ->
  ?shifted_key:Uchar.t ->
  ?base_key:Uchar.t ->
  Matrix.Input.Key.t ->
  Matrix.Input.Key.event
(** [repeat k] constructs a repeat key event. *)

val release :
  ?modifier:Matrix.Input.Key.modifier ->
  ?associated_text:string ->
  ?shifted_key:Uchar.t ->
  ?base_key:Uchar.t ->
  Matrix.Input.Key.t ->
  Matrix.Input.Key.event
(** [release k] constructs a release key event. *)

val focus : ('model, 'msg) t -> unit
(** [focus t] simulates gaining focus. Currently a no-op. *)

val blur : ('model, 'msg) t -> unit
(** [blur t] simulates losing focus. Currently a no-op. *)
