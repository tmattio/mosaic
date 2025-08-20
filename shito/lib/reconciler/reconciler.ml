module Host_config = Host_config
module Element = Element
module Either = Either

module Make (Host : Host_config.S) = struct
  module Fiber = Fiber.Make (Host)

  type element = (Host.primitive, Host.props) Element.t
  type host_child = (Host.instance, Host.text_instance) Either.t

  type parent_host =
    | PH_Container of Host.container
    | PH_Instance of Host.instance

  (* ---------- Reconciliation adapter for Elements ---------- *)

  module ElemNode = struct
    type t = element

    let key = Element.key

    let same_kind a b =
      match (Element.kind a, Element.kind b) with
      | `Primitive p1, `Primitive p2 -> Host.equal_primitive p1 p2
      | `Text, `Text
      | `Fragment, `Fragment
      | `Suspense, `Suspense
      | `Empty, `Empty ->
          true
      | _ -> false

    let kind_id = function
      | Element.Text _ -> 0
      | Element.Fragment _ -> 1
      | Element.Suspense _ -> 2
      | Element.Empty -> 3
      | Element.Primitive { element_type; _ } -> 100 + Hashtbl.hash element_type
  end

  module RC = Reconcile_children.Make (ElemNode)

  (* ---------- Host helpers ---------- *)

  let append_to_parent (p : parent_host) (c : host_child) =
    match p with
    | PH_Instance parent -> Host.append_child ~parent ~child:c
    | PH_Container container ->
        Host.append_child_to_container ~container ~child:c

  let insert_before_in_parent (p : parent_host) (c : host_child)
      ~(before : host_child) =
    match p with
    | PH_Instance parent ->
        Host.insert_before ~parent ~child:c ~before_child:before
    | PH_Container container ->
        Host.insert_before_in_container ~container ~child:c ~before_child:before

  let remove_from_parent (p : parent_host) (c : host_child) =
    match p with
    | PH_Instance parent -> Host.remove_child ~parent ~child:c
    | PH_Container container ->
        Host.remove_child_from_container ~container ~child:c

  (* ---------- Fiber utilities ---------- *)

  let fiber_children_to_array (f : Fiber.t) : Fiber.t array =
    let rec collect acc = function
      | None -> List.rev acc |> Array.of_list
      | Some c -> collect (c :: acc) c.Fiber.sibling
    in
    match f.Fiber.child with None -> [||] | Some c -> collect [] (Some c)

  let link_children_as_list (parent : Fiber.t) (arr : Fiber.t array) : unit =
    let n = Array.length arr in
    match n with
    | 0 -> Fiber.set_child parent None
    | _ ->
        Fiber.set_child parent (Some arr.(0));
        for i = 0 to n - 1 do
          Fiber.set_parent arr.(i) (Some parent);
          Fiber.set_sibling arr.(i)
            (if i = n - 1 then None else Some arr.(i + 1))
        done

  let fiber_host_root (f : Fiber.t) : host_child option =
    match (f.Fiber.tag, f.Fiber.state_node) with
    | Fiber.HostNode _, Some (Left inst) -> Some (Left inst)
    | Fiber.TextNode, Some (Right ti) -> Some (Right ti)
    | _ -> None

  let rec fiber_host_roots (f : Fiber.t) : host_child list =
    match fiber_host_root f with
    | Some h -> [ h ]
    | None ->
        let children = fiber_children_to_array f in
        Array.fold_left (fun acc ch -> acc @ fiber_host_roots ch) [] children

  (* ---------- Creation (detached, with host objects) ---------- *)

  let rec create_subtree ~(container : Host.container) (el : element) : Fiber.t
      =
    match el with
    | Element.Empty -> Fiber.make_fragment ~key:None
    | Element.Text s ->
        let f = Fiber.make_text ~key:None in
        let ti = Host.create_text_instance ~text:s ~container in
        Fiber.set_state_instance f (Right ti);
        f
    | Element.Primitive { element_type; key; props; children } ->
        let f = Fiber.make_host ~key ~element_type ~props in
        let inst =
          Host.create_instance ~primitive:element_type ~props ~container
        in
        Fiber.set_state_instance f (Left inst);
        f.Fiber.memo_props <- Some props;
        (* needed for commit_mount *)
        let kids = Array.map (create_subtree ~container) children in
        Array.iter
          (fun k ->
            List.iter
              (fun hc -> Host.append_initial_child ~parent:inst ~child:hc)
              (fiber_host_roots k))
          kids;
        Host.finalize_initial_children ~instance:inst ~props;
        link_children_as_list f kids;
        f
    | Element.Fragment { key; children } ->
        let f = Fiber.make_fragment ~key in
        link_children_as_list f (Array.map (create_subtree ~container) children);
        f
    | Element.Suspense { key; children; _ } ->
        let f = Fiber.make_suspense ~key in
        link_children_as_list f (Array.map (create_subtree ~container) children);
        f

  (* ---------- Deletion (cleanup + host detach) ---------- *)

  let rec detach_deleted_instances (f : Fiber.t) : unit =
    (match f.Fiber.state_node with
    | Some (Left inst) -> Host.detach_deleted_instance ~child:(Left inst)
    | Some (Right ti) -> Host.detach_deleted_instance ~child:(Right ti)
    | None -> ());
    let children = fiber_children_to_array f in
    Array.iter detach_deleted_instances children

  let remove_fiber_roots_from_parent (p : parent_host) (f : Fiber.t) : unit =
    List.iter (remove_from_parent p) (fiber_host_roots f)

  let delete_subtree_from_parent (p : parent_host) (f : Fiber.t) : unit =
    detach_deleted_instances f;
    remove_fiber_roots_from_parent p f

  (* ---------- Updates ---------- *)

  let update_primitive_props ~(instance : Host.instance)
      ~(old_props : Host.props) ~(new_props : Host.props) =
    match Host.prepare_update ~instance ~old_props ~new_props with
    | None -> ()
    | Some upd -> Host.commit_update ~instance ~update:upd

  let update_text ~(text_instance : Host.text_instance) ~(new_text : string) =
    Host.commit_text_update ~text_instance ~new_text

  (* ---------- Reconcile children under a given parent ---------- *)

  let element_shell_of_fiber (f : Fiber.t) : element =
    match (f.Fiber.tag, f.Fiber.element_type) with
    | Fiber.HostNode p, _ ->
        Element.Primitive
          {
            element_type = p;
            key = f.Fiber.key;
            props = Obj.magic ();
            children = [||];
          }
    | Fiber.TextNode, _ -> Element.Text ""
    | Fiber.FragmentNode, _ ->
        Element.Fragment { key = f.Fiber.key; children = [||] }
    | Fiber.SuspenseNode, _ ->
        Element.Suspense { key = f.Fiber.key; children = [||]; fallback = None }

  let rec patch_children ~(container : Host.container)
      ~(parent_host : parent_host) ~(parent_f : Fiber.t)
      ~(old_children : Fiber.t array) ~(new_children : element array) : unit =
    let plan =
      RC.diff
        ~old_nodes:(Array.map element_shell_of_fiber old_children)
        ~new_nodes:new_children
    in

    (* 1) Deletes *)
    List.iter
      (fun j -> delete_subtree_from_parent parent_host old_children.(j))
      plan.deletes;

    (* 2) Build new fiber array (reused or fresh), patching reused *)
    let new_len = Array.length new_children in
    let prepared : Fiber.t array =
      Array.make new_len (Fiber.make_fragment ~key:None)
    in

    let patch_reused (oldf : Fiber.t) (el : element) : Fiber.t =
      match (oldf.Fiber.tag, el) with
      | ( Fiber.HostNode old_prim,
          Element.Primitive { element_type; key; props; children } ) ->
          if not (Host.equal_primitive old_prim element_type) then
            create_subtree ~container el
          else (
            (match oldf.Fiber.state_node with
            | Some (Left inst) ->
                let old_props =
                  Option.value oldf.Fiber.memo_props ~default:props
                in
                update_primitive_props ~instance:inst ~old_props
                  ~new_props:props
            | _ -> ());
            oldf.Fiber.memo_props <- Some props;
            oldf.Fiber.key <- key;
            oldf.Fiber.element_type <- Some element_type;
            let host_parent =
              match oldf.Fiber.state_node with
              | Some (Left inst) -> PH_Instance inst
              | _ -> parent_host
            in
            let old_kids = fiber_children_to_array oldf in
            patch_children ~container ~parent_host:host_parent ~parent_f:oldf
              ~old_children:old_kids ~new_children:children;
            oldf)
      | Fiber.TextNode, Element.Text s ->
          (match oldf.Fiber.state_node with
          | Some (Right ti) -> update_text ~text_instance:ti ~new_text:s
          | _ -> ());
          oldf
      | Fiber.FragmentNode, Element.Fragment { key; children } ->
          oldf.Fiber.key <- key;
          let old_kids = fiber_children_to_array oldf in
          patch_children ~container ~parent_host ~parent_f:oldf
            ~old_children:old_kids ~new_children:children;
          oldf
      | Fiber.SuspenseNode, Element.Suspense { key; children; _ } ->
          oldf.Fiber.key <- key;
          let old_kids = fiber_children_to_array oldf in
          patch_children ~container ~parent_host ~parent_f:oldf
            ~old_children:old_kids ~new_children:children;
          oldf
      | _, _ -> create_subtree ~container el
    in

    for i = 0 to new_len - 1 do
      let el = new_children.(i) in
      prepared.(i) <-
        (match plan.reuse.(i) with
        | None -> create_subtree ~container el
        | Some j -> patch_reused old_children.(j) el)
    done;

    (* 3) Placements / moves *)
    let next_stable_host_sibling (idx : int) : host_child option =
      let rec find k =
        if k >= new_len then None
        else if plan.needs_placement.(k) then find (k + 1)
        else
          match fiber_host_root prepared.(k) with
          | None -> find (k + 1)
          | Some h -> Some h
      in
      find (idx + 1)
    in

    for i = 0 to new_len - 1 do
      (if plan.needs_placement.(i) then
         let roots = fiber_host_roots prepared.(i) in
         match next_stable_host_sibling i with
         | Some anchor ->
             List.iter
               (fun r -> insert_before_in_parent parent_host r ~before:anchor)
               roots
         | None -> List.iter (fun r -> append_to_parent parent_host r) roots);
      (* commit_mount only for brand-new nodes *)
      match plan.reuse.(i) with
      | None ->
          let rec commit_mount_subtree (n : Fiber.t) =
            (match (n.Fiber.state_node, n.Fiber.memo_props, n.Fiber.tag) with
            | Some (Left inst), Some props, Fiber.HostNode _ ->
                Host.commit_mount ~instance:inst ~props
            | _ -> ());
            let kids = fiber_children_to_array n in
            Array.iter commit_mount_subtree kids
          in
          commit_mount_subtree prepared.(i)
      | Some _ -> ()
    done;

    (* 4) Link prepared children under parent fiber *)
    link_children_as_list parent_f prepared

  (* ---------- Node patch at root or under a parent ---------- *)

  let patch_node ~(container : Host.container) ~(parent_host : parent_host)
      (old_f : Fiber.t option) (new_el : element option) : Fiber.t option =
    match (old_f, new_el) with
    | None, None -> None
    | Some f, None ->
        delete_subtree_from_parent parent_host f;
        None
    | None, Some el ->
        let f = create_subtree ~container el in
        List.iter (append_to_parent parent_host) (fiber_host_roots f);
        (* initial mount callbacks *)
        let rec commit_mount_subtree (n : Fiber.t) =
          (match (n.Fiber.state_node, n.Fiber.memo_props, n.Fiber.tag) with
          | Some (Left inst), Some props, Fiber.HostNode _ ->
              Host.commit_mount ~instance:inst ~props
          | _ -> ());
          let kids = fiber_children_to_array n in
          Array.iter commit_mount_subtree kids
        in
        commit_mount_subtree f;
        Some f
    | Some oldf, Some el -> (
        match (oldf.Fiber.tag, el) with
        | ( Fiber.HostNode old_prim,
            Element.Primitive { element_type; key; props; children } ) ->
            if Host.equal_primitive old_prim element_type then (
              (match oldf.Fiber.state_node with
              | Some (Left inst) ->
                  let old_props =
                    Option.value oldf.Fiber.memo_props ~default:props
                  in
                  update_primitive_props ~instance:inst ~old_props
                    ~new_props:props
              | _ -> ());
              oldf.Fiber.memo_props <- Some props;
              oldf.Fiber.key <- key;
              oldf.Fiber.element_type <- Some element_type;

              let old_kids = fiber_children_to_array oldf in
              patch_children ~container
                ~parent_host:
                  (match oldf.Fiber.state_node with
                  | Some (Left inst) -> PH_Instance inst
                  | _ -> parent_host)
                ~parent_f:oldf ~old_children:old_kids ~new_children:children;
              Some oldf)
            else (
              delete_subtree_from_parent parent_host oldf;
              let f = create_subtree ~container el in
              List.iter (append_to_parent parent_host) (fiber_host_roots f);
              let rec commit_mount_subtree (n : Fiber.t) =
                (match
                   (n.Fiber.state_node, n.Fiber.memo_props, n.Fiber.tag)
                 with
                | Some (Left inst), Some props, Fiber.HostNode _ ->
                    Host.commit_mount ~instance:inst ~props
                | _ -> ());
                let kids = fiber_children_to_array n in
                Array.iter commit_mount_subtree kids
              in
              commit_mount_subtree f;
              Some f)
        | Fiber.TextNode, Element.Text s ->
            (match oldf.Fiber.state_node with
            | Some (Right ti) -> update_text ~text_instance:ti ~new_text:s
            | _ -> ());
            Some oldf
        | Fiber.FragmentNode, Element.Fragment { key; children } ->
            oldf.Fiber.key <- key;
            let old_kids = fiber_children_to_array oldf in
            patch_children ~container ~parent_host ~parent_f:oldf
              ~old_children:old_kids ~new_children:children;
            Some oldf
        | Fiber.SuspenseNode, Element.Suspense { key; children; _ } ->
            oldf.Fiber.key <- key;
            let old_kids = fiber_children_to_array oldf in
            patch_children ~container ~parent_host ~parent_f:oldf
              ~old_children:old_kids ~new_children:children;
            Some oldf
        | _, _ ->
            delete_subtree_from_parent parent_host oldf;
            let f = create_subtree ~container el in
            List.iter (append_to_parent parent_host) (fiber_host_roots f);
            let rec commit_mount_subtree (n : Fiber.t) =
              (match (n.Fiber.state_node, n.Fiber.memo_props, n.Fiber.tag) with
              | Some (Left inst), Some props, Fiber.HostNode _ ->
                  Host.commit_mount ~instance:inst ~props
              | _ -> ());
              let kids = fiber_children_to_array n in
              Array.iter commit_mount_subtree kids
            in
            commit_mount_subtree f;
            Some f)

  (* ---------- Public Root API ---------- *)

  type root = { container : Host.container; mutable tree : Fiber.t option }

  let container r = r.container

  let mount ~container el =
    Host.prepare_for_commit ~container;
    let tree =
      patch_node ~container ~parent_host:(PH_Container container) None (Some el)
    in
    Host.reset_after_commit ~container;
    { container; tree }

  let update r el =
    Host.prepare_for_commit ~container:r.container;
    r.tree <-
      patch_node ~container:r.container ~parent_host:(PH_Container r.container)
        r.tree (Some el);
    Host.reset_after_commit ~container:r.container

  let unmount r =
    Host.prepare_for_commit ~container:r.container;
    (match r.tree with
    | None -> ()
    | Some f ->
        delete_subtree_from_parent (PH_Container r.container) f;
        r.tree <- None);
    Host.reset_after_commit ~container:r.container
end
