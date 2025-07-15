module type T = sig
  type model
  type msg

  val init : unit -> model * msg Cmd.t
  val update : msg -> model -> model * msg Cmd.t
  val view : model -> Ui.element
  val subscriptions : model -> msg Sub.t
end

module Instance = struct
  type ('child_msg, 'parent_model, 'parent_msg) t = {
    update : 'child_msg -> 'parent_model -> 'parent_model * 'parent_msg Cmd.t;
    view : 'parent_model -> Ui.element;
    subscriptions : 'parent_model -> 'parent_msg Sub.t;
  }
end

let make (type child_model child_msg parent_model parent_msg)
    (module Child : T with type model = child_model and type msg = child_msg)
    ~(get : parent_model -> child_model)
    ~(set : child_model -> parent_model -> parent_model)
    ~(wrap : child_msg -> parent_msg) :
    (child_msg, parent_model, parent_msg) Instance.t =
  let update child_msg parent_model =
    let child_model = get parent_model in
    let new_child_model, child_cmd = Child.update child_msg child_model in
    let new_parent_model = set new_child_model parent_model in
    let parent_cmd = Cmd.map wrap child_cmd in
    (new_parent_model, parent_cmd)
  in

  let view parent_model =
    let child_model = get parent_model in
    Child.view child_model
  in

  let subscriptions parent_model =
    let child_model = get parent_model in
    let child_subs = Child.subscriptions child_model in
    Sub.map wrap child_subs
  in

  { Instance.update; view; subscriptions }
