module type S = sig
  type model
  type msg
  type outgoing

  val update : msg -> model -> model * msg Cmd.t * outgoing option
  val view : model -> Ui.element
  val subscriptions : model -> msg Sub.t
end

type ('child_msg, 'child_outgoing, 'parent_model, 'parent_msg) connected = {
  update :
    'child_msg ->
    'parent_model ->
    'parent_model * 'parent_msg Cmd.t * 'child_outgoing option;
  view : 'parent_model -> Ui.element;
  subscriptions : 'parent_model -> 'parent_msg Sub.t;
}

let connect (type child_model child_msg child_outgoing parent_model parent_msg)
    (module Child : S
      with type model = child_model
       and type msg = child_msg
       and type outgoing = child_outgoing) ~(get : parent_model -> child_model)
    ~(set : child_model -> parent_model -> parent_model)
    ~(wrap : child_msg -> parent_msg) :
    (child_msg, child_outgoing, parent_model, parent_msg) connected =
  let update_logic child_msg parent_model =
    let child_model = get parent_model in
    let new_child_model, child_cmd, outgoing_opt =
      Child.update child_msg child_model
    in
    let new_parent_model = set new_child_model parent_model in
    let parent_cmd = Cmd.map wrap child_cmd in
    (new_parent_model, parent_cmd, outgoing_opt)
  in

  let view_logic parent_model =
    let child_model = get parent_model in
    Child.view child_model
  in

  let subscriptions_logic parent_model =
    let child_model = get parent_model in
    Sub.map wrap (Child.subscriptions child_model)
  in

  {
    update = update_logic;
    view = view_logic;
    subscriptions = subscriptions_logic;
  }
