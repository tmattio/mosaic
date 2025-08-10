(** Terminal user interface framework inspired by The Elm Architecture.

    Mosaic provides a declarative, functional approach to building interactive
    terminal applications in OCaml. Applications are structured around immutable
    models, pure update functions, and views that automatically render state
    changes.

    {1 Architecture Overview}

    Mosaic follows The Elm Architecture pattern with four key components:
    - Model: Immutable application state
    - Messages: Events that trigger state changes
    - Update: Pure function transforming (message, model) into (new_model,
      effects)
    - View: Pure function rendering model to UI elements

    All operations preserve functional purity. Models are immutable, updates
    return new states without side effects. Effects are represented as command
    values executed by the runtime. Views declaratively describe UI without
    manual rendering. *)

(** {1 Styling} *)

module Style = Ui.Style
(** @inline *)

(** {1 Building User Interfaces} *)

module Ui = Ui
(** @inline *)

(** {1 Commands (Effects)} *)

module Cmd = Engine.Cmd
(** @inline *)

(** {1 Input Events} *)

module Input = Input
(** @inline *)

(** {1 Subscriptions (Event Listeners)} *)

module Sub = Engine.Sub
(** @inline *)

module Event = Engine.Event
(** @inline *)

(** {1 Components} *)

module Component = Component
(** @inline *)

(** {1 Building Applications} *)

type ('model, 'msg) app = {
  init : unit -> 'model * 'msg Cmd.t;
  update : 'msg -> 'model -> 'model * 'msg Cmd.t;
  view : 'model -> Ui.element;
  subscriptions : 'model -> 'msg Sub.t;
}
(** [app] represents a complete Mosaic application following The Elm
    Architecture.

    The type parameters ['model] and ['msg] define the application's state type
    and message type respectively. The [init] function creates the initial state
    and startup commands. The [update] function handles messages to produce new
    states and side effects. The [view] function renders the current state as UI
    elements. The [subscriptions] function declares external event sources. *)

val app :
  init:(unit -> 'model * 'msg Cmd.t) ->
  update:('msg -> 'model -> 'model * 'msg Cmd.t) ->
  view:('model -> Ui.element) ->
  ?subscriptions:('model -> 'msg Sub.t) ->
  unit ->
  ('model, 'msg) app
(** [app ~init ~update ~view ?subscriptions ()] creates an application following
    The Elm Architecture.

    The [init] function produces the initial model and startup commands. The
    [update] function processes messages to transform the model and produce side
    effects. The [view] function renders the model as UI elements. The optional
    [subscriptions] function declares external event sources (defaults to
    [Sub.none]).

    Example: Creates a simple counter application.
    {[
      let app =
        Mosaic.app
          ~init:(fun () -> (0, Cmd.none))
          ~update:(fun msg count ->
            match msg with
            | `Increment -> (count + 1, Cmd.none)
            | `Decrement -> (count - 1, Cmd.none))
          ~view:(fun count -> Ui.text (Printf.sprintf "Count: %d" count))
          ()
    ]} *)

val run :
  ?terminal:Tty.t ->
  ?alt_screen:bool ->
  ?mouse:bool ->
  ?fps:int ->
  ?debug:bool ->
  ('model, 'msg) app ->
  unit
(** [run ?terminal ?alt_screen ?mouse ?fps ?debug app] executes the application
    within an Eio runtime context.

    The runtime manages the event loop, renders frames at the target FPS, and
    processes commands and subscriptions. The alternate screen buffer preserves
    terminal contents when the application exits. Mouse support enables cursor
    and click events.

    @param terminal
      Custom terminal instance for I/O (default: stdin/stdout terminal)
    @param alt_screen Whether to use alternate screen buffer (default: true)
    @param mouse Whether to enable mouse event capture (default: false)
    @param fps
      Target frames per second for rendering, clamped to reasonable values
      (default: 60)
    @param debug
      Whether to write debug logs to mosaic-debug.log (default: false)

    @raise Invalid_argument if called outside [Eio_main.run] context.

    Example: Runs application with mouse support at 30 FPS.
    {[
      Mosaic.run ~mouse:true ~fps:30 my_app
    ]} *)

val run_eio :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  ?terminal:Tty.t ->
  ?alt_screen:bool ->
  ?mouse:bool ->
  ?fps:int ->
  ?debug:bool ->
  ('model, 'msg) app ->
  unit
(** [run_eio ~sw ~env ?terminal ?alt_screen ?mouse ?fps ?debug app] executes the
    application with explicit Eio environment control.

    This lower-level function allows integration with existing Eio applications
    by accepting an explicit switch and environment. The switch manages resource
    cleanup on application exit. Parameters behave identically to [run].

    Example: Integrates Mosaic app within larger Eio application.
    {[
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      (* Other Eio operations... *)
      Mosaic.run_eio ~sw ~env my_app
    ]} *)
