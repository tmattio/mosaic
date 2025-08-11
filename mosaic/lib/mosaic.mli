(** Mosaic - A declarative terminal UI framework for OCaml

    Mosaic is a modern, React-inspired framework for building terminal user
    interfaces in OCaml. It provides a declarative API with hooks for state
    management, a flexible styling system, and both low-level UI primitives and
    high-level interactive components.

    {2 Quick Example}

    {[
      open Mosaic

      let counter () =
        let count, set_count, _ = use_state 0 in

        Tile.vbox ~gap:(`Cells 1)
          [
            Tile.text (Printf.sprintf "Count: %d" count);
            Tile.hbox ~gap:(`Cells 1)
              [
                Tile.button ~label:"+"
                  ~on_click:(fun () -> set_count (count + 1))
                  ();
                Tile.button ~label:"-"
                  ~on_click:(fun () -> set_count (count - 1))
                  ();
              ];
          ]

      let () = run counter
    ]} *)

(** {1 Core Modules} *)

module Ui = Ui
(** Terminal UI primitives and layout system *)

module Style = Ui.Style
(** Text styling (colors, attributes) *)

module Cmd = Engine.Cmd
(** Command messages for side effects *)

module Sub = Engine.Sub
(** Event subscriptions *)

module Input = Input
(** Keyboard and mouse input types *)

module Deps = Deps
(** Dependency tracking for hooks *)

module Tile = Tile
(** Interactive UI components module with widgets and event handling *)

(** {1 Supporting Types} *)

module Reducer_id : sig
  type ('s, 'a) t
  (** Unique identifier for a reducer, ensuring type safety *)

  val make : unit -> ('a, 'b) t
  (** Create a new unique reducer ID *)
end

(** {1 Hooks} *)

(** {2 State Management} *)

val use_state : 'a -> 'a * ('a -> unit) * (('a -> 'a) -> unit)
(** [use_state initial] creates a state hook with an initial value. Returns
    [(value, set_value, update_value)] where:
    - [value] is the current state
    - [set_value] replaces the state with a new value
    - [update_value] applies a function to update the state

    {[
      let count, set_count, update_count = use_state 0 in
      (* Use count as current value *)
      (* set_count 5 to set to 5 *)
      (* update_count (fun x -> x + 1) to increment *)
    ]} *)

val use_reducer :
  ?id:('s, 'a) Reducer_id.t -> ('s -> 'a -> 's) -> 's -> 's * ('a -> unit)
(** [use_reducer reducer initial] creates a reducer hook for complex state
    logic.

    {[
      type action = Increment | Decrement | Reset

      let reducer state = function
        | Increment -> state + 1
        | Decrement -> state - 1
        | Reset -> 0

      let count, dispatch = use_reducer reducer 0 in
      (* dispatch Increment to increment counter *)
    ]} *)

val use_reducer_latest :
  ?id:('s, 'a) Reducer_id.t -> ('s -> 'a -> 's) -> 's -> 's * ('a -> unit)
(** Like [use_reducer] but always uses the latest reducer function, useful when
    the reducer captures values from the component scope. *)

(** {2 Effects and Lifecycle} *)

val use_effect : ?deps:Deps.t -> (unit -> (unit -> unit) option) -> unit
(** [use_effect ?deps f] runs a side effect after render.
    - [f] is called after each render when dependencies change
    - [f] can return a cleanup function that runs before the next effect or
      unmount
    - [deps] controls when the effect re-runs (default: always)

    {[
      use_effect ~deps:(Deps.some count) (fun () ->
          Printf.printf "Count changed to %d\n" count;
          None (* No cleanup needed *))
    ]} *)

val use_memo : (unit -> 'a) -> deps:Deps.t -> 'a
(** [use_memo f ~deps] memoizes a computation, only re-computing when
    dependencies change.

    {[
      let expensive_value =
        use_memo
          (fun () ->
            (* Expensive computation *)
            compute_something items)
          ~deps:(Deps.some items)
    ]} *)

val use_callback : 'a -> deps:Deps.t -> 'a
(** [use_callback value ~deps] memoizes a value (often a function), returning
    the same instance unless dependencies change. Useful for optimization when
    passing callbacks to child components. *)

val use_ref : 'a -> 'a ref
(** [use_ref initial] creates a mutable reference that persists across renders.
    Unlike state, updating a ref doesn't trigger a re-render.

    {[
      let previous_count = use_ref None in
      use_effect (fun () ->
          previous_count := Some count;
          None)
    ]} *)

(** {2 Context} *)

type 'a context
(** Type-safe context for passing values down the component tree *)

val create_context : ?default:'a -> ?name:string -> unit -> 'a context
(** [create_context ?default ?name ()] creates a new context. The [name] is used
    for debugging purposes. *)

val provide : 'a context -> 'a -> (unit -> 'b) -> 'b
(** [provide context value f] provides a context value to all components in [f].

    {[
      let theme_context = create_context ~default:"light" () in

      let app () =
        provide theme_context "dark" (fun () ->
          (* All components here can access the "dark" theme *)
          child_component ()
        )
    ]} *)

val use_context : 'a context -> 'a
(** [use_context context] retrieves the current context value.

    {[
      let theme = use_context theme_context in
      (* Use theme value *)
    ]} *)

(** {2 Keys and Identity} *)

val use_key : prefix:string -> Ui.Attr.key
(** [use_key ~prefix] generates a stable, unique key for UI elements. The key
    persists across re-renders at the same call site.

    {[
      let button_key = use_key ~prefix:"btn" in
      Tile.with_key button_key (Tile.text "Button")
    ]} *)

(** {2 Input Handling} *)

val use_keyboard :
  ?ctrl:bool -> ?alt:bool -> ?shift:bool -> Input.key -> 'msg Cmd.t -> unit
(** [use_keyboard ?modifiers key cmd] registers a global keyboard shortcut.

    {[
      (* Quit on 'q' key press *)
      use_keyboard (Input.Char (Uchar.of_char 'q')) Cmd.quit;

      (* Save on Ctrl+S *)
      use_keyboard ~ctrl:true
        (Input.Char (Uchar.of_char 's'))
        (save_document ())
    ]} *)

val use_subscription : 'msg Sub.t -> unit
(** [use_subscription sub] subscribes to external events. The subscription is
    automatically managed across the component lifecycle. *)

val dispatch_cmd : 'msg Cmd.t -> unit
(** [dispatch_cmd cmd] dispatches a command for immediate execution. Commands
    can trigger side effects like quitting, focusing elements, etc. *)

(** {2 Animation} *)

val use_tick : (float -> unit) -> unit
(** [use_tick f] calls [f] with elapsed time on each animation frame. Useful for
    animations and real-time updates.

    {[
      use_tick (fun elapsed ->
          set_rotation (rotation +. (elapsed *. 45.0))
          (* 45 degrees per second *))
    ]} *)

val use_timer : every:float -> (unit -> unit) -> unit
(** [use_timer ~every f] calls [f] repeatedly at the specified interval (in
    seconds).

    {[
      use_timer ~every:1.0 (fun () -> set_seconds (seconds + 1))
    ]} *)

val use_scroll :
  ?initial:int ->
  ?min_offset:int ->
  ?max_offset:int ->
  ?momentum:bool ->
  ?friction:float ->
  ?impulse_scale:float ->
  ?inertia_threshold:float ->
  ?alpha:float ->
  ?max_dt:float ->
  unit ->
  int * (int -> unit) * (int -> unit)
(** [use_scroll ()] creates a scrolling state with optional momentum physics.

    Returns [(offset, scroll_by, set_offset)] where:
    - [offset] is the current scroll position
    - [scroll_by delta] scrolls by the given amount (positive = down)
    - [set_offset v] sets the scroll position directly

    Optional parameters:
    - [initial]: Initial scroll offset (default: 0)
    - [min_offset]: Minimum allowed offset (default: 0)
    - [max_offset]: Maximum allowed offset (default: max_int)
    - [momentum]: Enable momentum scrolling with inertia (default: false)
    - [friction]: Friction coefficient for momentum decay (default: 2.0)
    - [impulse_scale]: Scale factor for scroll impulses (default: 3.0)
    - [inertia_threshold]: Time after input before momentum kicks in (default:
      0.05s)
    - [alpha]: Smoothing factor for velocity calculation (default: 0.8)
    - [max_dt]: Maximum time delta for velocity calculation (default: 0.1)

    When momentum is disabled (default), scrolling is immediate and stops
    instantly. When momentum is enabled, scrolling has inertia and gradually
    decelerates with smooth velocity based on input speed. *)

(** {1 Event Subscriptions} *)

val on_click : Ui.Attr.key -> (unit -> unit) -> unit Sub.t
(** Subscribe to click events on a keyed element *)

val on_hover : Ui.Attr.key -> (bool -> unit) -> unit Sub.t
(** Subscribe to hover events (true when entering, false when leaving) *)

val on_drag :
  Ui.Attr.key -> (Engine.Input_router.drag_event -> unit) -> unit Sub.t
(** Subscribe to drag events with position and phase information *)

val on_focus : Ui.Attr.key -> (bool -> unit) -> unit Sub.t
(** Subscribe to focus events (true when gaining focus, false when losing) *)

val on_key : Ui.Attr.key -> (Input.key_event -> unit) -> unit Sub.t
(** Subscribe to keyboard events when element has focus *)

val on_scroll : Ui.Attr.key -> (int -> unit) -> unit Sub.t
(** Subscribe to scroll events on a keyed element. The callback receives the
    scroll delta (positive for down, negative for up). *)

(** {1 Running the Application} *)

val run :
  ?terminal:Engine.Tty_eio.t ->
  ?alt_screen:bool ->
  ?mouse:bool ->
  ?fps:int ->
  ?debug:string ->
  (unit -> Ui.element) ->
  unit
(** [run app] starts the Mosaic application.

    - [terminal]: TTY configuration (default: auto-detect)
    - [alt_screen]: Use alternate screen buffer (default: true)
    - [mouse]: Enable mouse support (default: false)
    - [fps]: Frame rate for animations (default: 60)
    - [debug]: Channel for debug output

    {[
      let app () =
        Tile.vbox
          [
            Tile.text "Hello, Mosaic!";
            Tile.button ~label:"Click me"
              ~on_click:(fun () -> print_endline "Clicked!")
              ();
          ]

      let () = run app
    ]} *)

val run_eio :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  ?terminal:Engine.Tty_eio.t ->
  ?alt_screen:bool ->
  ?mouse:bool ->
  ?fps:int ->
  ?debug:string ->
  (unit -> Ui.element) ->
  unit
(** [run_eio ~sw ~env app] runs the application with an existing Eio
    environment. Useful for integration with other Eio-based systems. *)
