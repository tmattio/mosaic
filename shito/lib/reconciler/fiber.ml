module Make (Host : Host_config.S) = struct
  type tag =
    | HostNode of Host.primitive
    | TextNode
    | FragmentNode
    | SuspenseNode

  module Flag = struct
    type t = int

    let none = 0
    let placement = 1 lsl 0
    let update = 1 lsl 1
    let child_deletion = 1 lsl 2
    let layout = 1 lsl 3
    let passive = 1 lsl 4
    let ref_ = 1 lsl 5
    let ( + ) a b = a lor b
    let mem set flag = set land flag <> 0

    let pp fmt v =
      let xs = ref [] in
      let add name flag = if mem v flag then xs := name :: !xs in
      add "Placement" placement;
      add "Update" update;
      add "ChildDeletion" child_deletion;
      add "Layout" layout;
      add "Passive" passive;
      add "Ref" ref_;
      Format.fprintf fmt "{%s}" (String.concat "," (List.rev !xs))
  end

  type effect_ = {
    mutable create : unit -> (unit -> unit) option;
    mutable destroy : (unit -> unit) option;
  }

  type t = {
    mutable tag : tag;
    mutable key : Key.t;
    mutable element_type : Host.primitive option;
    mutable pending_props : Host.props option;
    mutable memo_props : Host.props option;
    mutable state_node : (Host.instance, Host.text_instance) Either.t option;
    mutable parent : t option;
    mutable child : t option;
    mutable sibling : t option;
    mutable alternate : t option;
    mutable flags : Flag.t;
    mutable subtree_flags : Flag.t;
    mutable layout_effects : effect_ list;
    mutable passive_effects : effect_ list;
  }

  let empty_effects = []

  let base ~tag ~key =
    {
      tag;
      key;
      element_type = None;
      pending_props = None;
      memo_props = None;
      state_node = None;
      parent = None;
      child = None;
      sibling = None;
      alternate = None;
      flags = Flag.none;
      subtree_flags = Flag.none;
      layout_effects = empty_effects;
      passive_effects = empty_effects;
    }

  let make_host ~key ~element_type ~props =
    let f = base ~tag:(HostNode element_type) ~key in
    f.element_type <- Some element_type;
    f.pending_props <- Some props;
    f.memo_props <- None;
    f

  let make_text ~key = base ~tag:TextNode ~key
  let make_fragment ~key = base ~tag:FragmentNode ~key
  let make_suspense ~key = base ~tag:SuspenseNode ~key
  let set_child f c = f.child <- c
  let set_sibling f s = f.sibling <- s
  let set_parent f p = f.parent <- p

  let set_alternate_pair ~current ~wip =
    current.alternate <- Some wip;
    wip.alternate <- Some current

  let reset_flags f =
    f.flags <- Flag.none;
    f.subtree_flags <- Flag.none

  let add_flag f fl = f.flags <- Flag.(f.flags + fl)

  let bubble_subtree_flags f =
    (* Combine children's flags into subtree_flags; keep f.flags intact. *)
    let rec _collect acc = function
      | None -> acc
      | Some c ->
          let acc = acc lor c.flags lor c.subtree_flags in
          _collect (match c.sibling with None -> acc | Some _ -> acc) c.sibling
    in
    let child_flags =
      match f.child with
      | None -> Flag.none
      | Some c ->
          let rec loop acc n =
            let acc = acc lor n.flags lor n.subtree_flags in
            match n.sibling with None -> acc | Some s -> loop acc s
          in
          loop Flag.none c
    in
    f.subtree_flags <- child_flags

  let set_state_instance f inst = f.state_node <- Some inst
  let get_state_instance f = f.state_node

  let push_layout_effect f eff =
    f.layout_effects <- eff :: f.layout_effects;
    add_flag f Flag.layout

  let push_passive_effect f eff =
    f.passive_effects <- eff :: f.passive_effects;
    add_flag f Flag.passive
end
