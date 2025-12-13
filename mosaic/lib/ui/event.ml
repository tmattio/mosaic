module Key = struct
  type t = { data : Input.Key.event; mutable default_prevented : bool }

  let of_input data = { data; default_prevented = false }
  let data t = t.data
  let prevent_default t = t.default_prevented <- true
  let default_prevented t = t.default_prevented
end

module Paste = struct
  type t = { text : string; mutable default_prevented : bool }

  let of_text text = { text; default_prevented = false }
  let text t = t.text
  let prevent_default t = t.default_prevented <- true
  let default_prevented t = t.default_prevented
end

module Mouse = struct
  type scroll_direction = Input.Mouse.scroll_direction =
    | Scroll_up
    | Scroll_down
    | Scroll_left
    | Scroll_right

  type kind = Down | Up | Move | Drag | Drag_end | Drop | Over | Out | Scroll

  type meta = {
    mutable target_id : string option;
    mutable target_number : int option;
    mutable default_prevented : bool;
    mutable propagation_stopped : bool;
  }

  type base = { x : int; y : int; modifiers : Input.Key.modifier; meta : meta }

  type t =
    | Down of { base : base; button : Input.Mouse.button }
    | Up of {
        base : base;
        button : Input.Mouse.button;
        mutable is_selecting : bool;
      }
    | Move of { base : base }
    | Drag of {
        base : base;
        button : Input.Mouse.button;
        mutable is_selecting : bool;
      }
    | Drag_end of { base : base; button : Input.Mouse.button }
    | Drop of {
        base : base;
        button : Input.Mouse.button;
        mutable source_id : string option;
        mutable source_number : int option;
      }
    | Over of {
        base : base;
        mutable source_id : string option;
        mutable source_number : int option;
      }
    | Out of { base : base }
    | Scroll of { base : base; direction : scroll_direction; delta : int }

  let kind (event : t) : kind =
    match event with
    | Down _ -> Down
    | Up _ -> Up
    | Move _ -> Move
    | Drag _ -> Drag
    | Drag_end _ -> Drag_end
    | Drop _ -> Drop
    | Over _ -> Over
    | Out _ -> Out
    | Scroll _ -> Scroll

  let make_meta () =
    {
      target_id = None;
      target_number = None;
      default_prevented = false;
      propagation_stopped = false;
    }

  let make_base ~x ~y ~modifiers = { x; y; modifiers; meta = make_meta () }

  let base = function
    | Down e -> e.base
    | Up e -> e.base
    | Move e -> e.base
    | Drag e -> e.base
    | Drag_end e -> e.base
    | Drop e -> e.base
    | Over e -> e.base
    | Out e -> e.base
    | Scroll e -> e.base

  let meta event = (base event).meta

  (* Constructors *)

  let down ~x ~y ~button ~modifiers =
    let base = make_base ~x ~y ~modifiers in
    Down { base; button }

  let up ~x ~y ~button ~modifiers =
    let base = make_base ~x ~y ~modifiers in
    Up { base; button; is_selecting = false }

  let move ~x ~y ~modifiers =
    let base = make_base ~x ~y ~modifiers in
    Move { base }

  let drag ~x ~y ~button ~modifiers =
    let base = make_base ~x ~y ~modifiers in
    Drag { base; button; is_selecting = false }

  let drag_end ~x ~y ~button ~modifiers =
    let base = make_base ~x ~y ~modifiers in
    Drag_end { base; button }

  let drop ~x ~y ~button ~modifiers =
    let base = make_base ~x ~y ~modifiers in
    Drop { base; button; source_id = None; source_number = None }

  let over ~x ~y ~modifiers =
    let base = make_base ~x ~y ~modifiers in
    Over { base; source_id = None; source_number = None }

  let out ~x ~y ~modifiers =
    let base = make_base ~x ~y ~modifiers in
    Out { base }

  let scroll ~x ~y ~direction ~delta ~modifiers =
    let base = make_base ~x ~y ~modifiers in
    Scroll { base; direction; delta }

  (* Default + Propagation Control *)

  let stop_propagation event =
    let m = meta event in
    m.propagation_stopped <- true

  let propagation_stopped event = (meta event).propagation_stopped

  let prevent_default event =
    let m = meta event in
    m.default_prevented <- true

  let default_prevented event = (meta event).default_prevented

  (* Payload Accessors *)

  let x event = (base event).x
  let y event = (base event).y
  let modifiers event = (base event).modifiers

  let button = function
    | Down e -> Some e.button
    | Up e -> Some e.button
    | Drag e -> Some e.button
    | Drag_end e -> Some e.button
    | Drop e -> Some e.button
    | Move _ | Over _ | Out _ | Scroll _ -> None

  let scroll_delta = function
    | Scroll e -> Some (e.direction, e.delta)
    | Down _ | Up _ | Move _ | Drag _ | Drag_end _ | Drop _ | Over _ | Out _ ->
        None

  let is_selecting = function
    | Drag e -> e.is_selecting
    | Up e -> e.is_selecting
    | Down _ | Move _ | Drag_end _ | Drop _ | Over _ | Out _ | Scroll _ -> false

  (* Hit Testing Metadata *)

  let target_id event = (meta event).target_id
  let target_number event = (meta event).target_number

  let source_id = function
    | Over e -> e.source_id
    | Drop e -> e.source_id
    | Down _ | Up _ | Move _ | Drag _ | Drag_end _ | Out _ | Scroll _ -> None

  let source_number = function
    | Over e -> e.source_number
    | Drop e -> e.source_number
    | Down _ | Up _ | Move _ | Drag _ | Drag_end _ | Out _ | Scroll _ -> None

  module Internal = struct
    let set_target event ~id ~number =
      let m = meta event in
      m.target_id <- id;
      m.target_number <- number

    let set_source event ~id ~number =
      match event with
      | Drop e ->
          e.source_id <- id;
          e.source_number <- number
      | Over e ->
          e.source_id <- id;
          e.source_number <- number
      | Down _ | Up _ | Move _ | Drag _ | Drag_end _ | Out _ | Scroll _ -> ()

    let set_is_selecting event flag =
      match event with
      | Drag e -> e.is_selecting <- flag
      | Up e -> e.is_selecting <- flag
      | Down _ | Move _ | Drag_end _ | Drop _ | Over _ | Out _ | Scroll _ -> ()
  end
end

(* Convenience aliases *)
type key = Key.t
type paste = Paste.t
type mouse = Mouse.t
