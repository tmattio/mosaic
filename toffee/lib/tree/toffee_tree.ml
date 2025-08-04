open Node
open Layout

module Error = struct
  type t =
    | Child_index_out_of_bounds of {
        parent : Node_id.t;
        child_index : int;
        child_count : int;
      }
    | Invalid_parent_node of Node_id.t
    | Invalid_child_node of Node_id.t
    | Invalid_input_node of Node_id.t

  let to_string = function
    | Child_index_out_of_bounds { parent; child_index; child_count } ->
        Printf.sprintf
          "Index (is %d) should be < child_count (%d) for parent node %s"
          child_index child_count (Node_id.show parent)
    | Invalid_parent_node parent ->
        Printf.sprintf "Parent Node %s is not in the Tree instance"
          (Node_id.show parent)
    | Invalid_child_node child ->
        Printf.sprintf "Child Node %s is not in the Tree instance"
          (Node_id.show child)
    | Invalid_input_node node ->
        Printf.sprintf "Supplied Node %s is not in the Tree instance"
          (Node_id.show node)
end

type 'a result = ('a, Error.t) Result.t

module NodeData = struct
  type t = {
    style : Style.style;
    mutable unrounded_layout : Layout.t;
    mutable final_layout : Layout.t;
    has_context : bool;
    mutable cache : Cache.t;
  }

  let create style =
    {
      style;
      unrounded_layout = Layout.empty;
      final_layout = Layout.empty;
      has_context = false;
      cache = Cache.empty ();
    }

  let mark_dirty node_data = Cache.clear node_data.cache
end

module Config = struct
  type t = { use_rounding : bool }

  let default = { use_rounding = true }
end

module Tree = struct
  type 'context t = {
    mutable nodes : (Node_id.t, NodeData.t) Hashtbl.t;
    mutable node_context_data : (Node_id.t, 'context) Hashtbl.t;
    mutable children : (Node_id.t, Node_id.t array) Hashtbl.t;
    mutable parents : (Node_id.t, Node_id.t option) Hashtbl.t;
    mutable config : Config.t;
    mutable next_id : int64;
  }

  let create () =
    {
      nodes = Hashtbl.create 100;
      node_context_data = Hashtbl.create 100;
      children = Hashtbl.create 100;
      parents = Hashtbl.create 100;
      config = Config.default;
      next_id = 0L;
    }

  let new_leaf t style =
    let id = Node_id.of_int64 t.next_id in
    t.next_id <- Int64.succ t.next_id;
    let node_data = NodeData.create style in
    Hashtbl.add t.nodes id node_data;
    Hashtbl.add t.children id [||];
    Hashtbl.add t.parents id None;
    id

  let new_with_children t style children =
    let id = Node_id.of_int64 t.next_id in
    t.next_id <- Int64.succ t.next_id;
    let node_data = NodeData.create style in
    Hashtbl.add t.nodes id node_data;
    Hashtbl.add t.children id (Array.of_list children);
    Hashtbl.add t.parents id None;

    (* Set parent pointers for children *)
    List.iter
      (fun child_id -> Hashtbl.replace t.parents child_id (Some id))
      children;

    id

  let remove t node_id =
    (* Remove from parent's children list *)
    (match Hashtbl.find_opt t.parents node_id with
    | Some (Some parent_id) ->
        let parent_children = Hashtbl.find t.children parent_id in
        let new_children =
          Array.to_list parent_children
          |> List.filter (fun id -> not (Node_id.equal id node_id))
          |> Array.of_list
        in
        Hashtbl.replace t.children parent_id new_children
    | _ -> ());

    (* Recursively remove children *)
    let rec remove_recursive id =
      (match Hashtbl.find_opt t.children id with
      | Some children -> Array.iter remove_recursive children
      | None -> ());

      Hashtbl.remove t.nodes id;
      Hashtbl.remove t.node_context_data id;
      Hashtbl.remove t.children id;
      Hashtbl.remove t.parents id
    in

    remove_recursive node_id;
    Ok ()

  let set_node_context t node_id context =
    match Hashtbl.find_opt t.nodes node_id with
    | None -> Error (Error.Invalid_input_node node_id)
    | Some _ ->
        Hashtbl.replace t.node_context_data node_id context;
        Ok ()

  let get_node_context t node_id =
    match Hashtbl.find_opt t.nodes node_id with
    | None -> Error (Error.Invalid_input_node node_id)
    | Some _ -> Ok (Hashtbl.find_opt t.node_context_data node_id)

  let set_style t node_id style =
    match Hashtbl.find_opt t.nodes node_id with
    | None -> Error (Error.Invalid_input_node node_id)
    | Some node_data ->
        Hashtbl.replace t.nodes node_id { node_data with style };
        let _ = NodeData.mark_dirty node_data in
        Ok ()

  let style t node_id =
    match Hashtbl.find_opt t.nodes node_id with
    | None -> Error (Error.Invalid_input_node node_id)
    | Some node_data -> Ok node_data.style

  let layout t node_id =
    match Hashtbl.find_opt t.nodes node_id with
    | None -> Error (Error.Invalid_input_node node_id)
    | Some node_data -> Ok node_data.final_layout

  let mark_dirty t node_id =
    match Hashtbl.find_opt t.nodes node_id with
    | None -> Error (Error.Invalid_input_node node_id)
    | Some node_data ->
        let _ = NodeData.mark_dirty node_data in
        Ok ()

  let dirty t node_id =
    match Hashtbl.find_opt t.nodes node_id with
    | None -> Error (Error.Invalid_input_node node_id)
    | Some node_data -> Ok (Cache.is_empty node_data.cache)

  let add_child t parent_id child_id =
    match Hashtbl.find_opt t.nodes parent_id with
    | None -> Error (Error.Invalid_parent_node parent_id)
    | Some _ -> (
        match Hashtbl.find_opt t.nodes child_id with
        | None -> Error (Error.Invalid_child_node child_id)
        | Some _ ->
            let children = Hashtbl.find t.children parent_id in
            let new_children = Array.append children [| child_id |] in
            Hashtbl.replace t.children parent_id new_children;
            Hashtbl.replace t.parents child_id (Some parent_id);
            Ok ())

  let insert_child_at_index t parent_id index child_id =
    match Hashtbl.find_opt t.nodes parent_id with
    | None -> Error (Error.Invalid_parent_node parent_id)
    | Some _ -> (
        match Hashtbl.find_opt t.nodes child_id with
        | None -> Error (Error.Invalid_child_node child_id)
        | Some _ ->
            let children = Hashtbl.find t.children parent_id in
            let child_count = Array.length children in
            if index > child_count then
              Error
                (Error.Child_index_out_of_bounds
                   { parent = parent_id; child_index = index; child_count })
            else
              let new_children =
                Array.init (child_count + 1) (fun i ->
                    if i < index then children.(i)
                    else if i = index then child_id
                    else children.(i - 1))
              in
              Hashtbl.replace t.children parent_id new_children;
              Hashtbl.replace t.parents child_id (Some parent_id);
              Ok ())

  let set_children t parent_id new_children =
    match Hashtbl.find_opt t.nodes parent_id with
    | None -> Error (Error.Invalid_parent_node parent_id)
    | Some _ ->
        (* Verify all children exist *)
        let all_exist =
          List.for_all
            (fun child_id -> Hashtbl.mem t.nodes child_id)
            new_children
        in

        if not all_exist then
          let invalid_child =
            List.find
              (fun child_id -> not (Hashtbl.mem t.nodes child_id))
              new_children
          in
          Error (Error.Invalid_child_node invalid_child)
        else
          (* Clear parent pointers of old children *)
          let old_children = Hashtbl.find t.children parent_id in
          Array.iter
            (fun child_id -> Hashtbl.replace t.parents child_id None)
            old_children;

          (* Set new children and their parent pointers *)
          let new_children_array = Array.of_list new_children in
          Hashtbl.replace t.children parent_id new_children_array;
          Array.iter
            (fun child_id ->
              Hashtbl.replace t.parents child_id (Some parent_id))
            new_children_array;

          Ok ()

  let remove_child t parent_id child_id =
    match Hashtbl.find_opt t.nodes parent_id with
    | None -> Error (Error.Invalid_parent_node parent_id)
    | Some _ ->
        let children = Hashtbl.find t.children parent_id in
        let new_children =
          Array.to_list children
          |> List.filter (fun id -> not (Node_id.equal id child_id))
          |> Array.of_list
        in
        Hashtbl.replace t.children parent_id new_children;
        Hashtbl.replace t.parents child_id None;
        Ok ()

  let remove_child_at_index t parent_id index =
    match Hashtbl.find_opt t.nodes parent_id with
    | None -> Error (Error.Invalid_parent_node parent_id)
    | Some _ ->
        let children = Hashtbl.find t.children parent_id in
        let child_count = Array.length children in
        if index >= child_count then
          Error
            (Error.Child_index_out_of_bounds
               { parent = parent_id; child_index = index; child_count })
        else
          let child_id = children.(index) in
          let new_children =
            Array.init (child_count - 1) (fun i ->
                if i < index then children.(i) else children.(i + 1))
          in
          Hashtbl.replace t.children parent_id new_children;
          Hashtbl.replace t.parents child_id None;
          Ok child_id

  let replace_child_at_index t parent_id index new_child_id =
    match Hashtbl.find_opt t.nodes parent_id with
    | None -> Error (Error.Invalid_parent_node parent_id)
    | Some _ -> (
        match Hashtbl.find_opt t.nodes new_child_id with
        | None -> Error (Error.Invalid_child_node new_child_id)
        | Some _ ->
            let children = Hashtbl.find t.children parent_id in
            let child_count = Array.length children in
            if index >= child_count then
              Error
                (Error.Child_index_out_of_bounds
                   { parent = parent_id; child_index = index; child_count })
            else
              let old_child_id = children.(index) in
              children.(index) <- new_child_id;
              Hashtbl.replace t.parents old_child_id None;
              Hashtbl.replace t.parents new_child_id (Some parent_id);
              Ok old_child_id)

  let child_count t parent_id =
    match Hashtbl.find_opt t.nodes parent_id with
    | None -> Error (Error.Invalid_parent_node parent_id)
    | Some _ ->
        let children = Hashtbl.find t.children parent_id in
        Ok (Array.length children)

  let children t parent_id =
    match Hashtbl.find_opt t.nodes parent_id with
    | None -> Error (Error.Invalid_parent_node parent_id)
    | Some _ ->
        let children = Hashtbl.find t.children parent_id in
        Ok (Array.to_list children)

  let child_at_index t parent_id index =
    match Hashtbl.find_opt t.nodes parent_id with
    | None -> Error (Error.Invalid_parent_node parent_id)
    | Some _ ->
        let children = Hashtbl.find t.children parent_id in
        let child_count = Array.length children in
        if index >= child_count then
          Error
            (Error.Child_index_out_of_bounds
               { parent = parent_id; child_index = index; child_count })
        else Ok children.(index)

  let parent t child_id =
    match Hashtbl.find_opt t.nodes child_id with
    | None -> Error (Error.Invalid_child_node child_id)
    | Some _ -> Ok (Hashtbl.find t.parents child_id)

  let disable_rounding t = t.config <- { use_rounding = false }
  let enable_rounding t = t.config <- { use_rounding = true }
  let set_rounding_enabled t enabled = t.config <- { use_rounding = enabled }

  let rec compute_layout t root_id available_space =
    compute_layout_with_measure t root_id available_space
      (fun ~known_dimensions:_ ~available_space:_ _node_id _context _style ->
        Geometry.Size.zero)

  and compute_layout_with_measure (type ctx) (t : ctx t) root_id available_space
      measure_fn =
    match Hashtbl.find_opt t.nodes root_id with
    | None -> Error (Error.Invalid_input_node root_id)
    | Some root_data ->
        (* Compute known dimensions for root node based on style *)
        let parent_size =
          {
            Geometry.width =
              available_space.Geometry.width
              |> Style.Available_space.into_option;
            Geometry.height =
              available_space.Geometry.height
              |> Style.Available_space.into_option;
          }
        in
        let style = root_data.NodeData.style in

        let known_dimensions =
          (* For now, we'll compute the styled size for the root.
             The full implementation would handle block layout special cases *)
          let size =
            Resolve.maybe_resolve_size Resolve.maybe_resolve_dimension
              (Style.size style) parent_size (fun () basis -> basis)
          in
          size
        in

        (* Create the layout input *)
        let layout_input : Layout_input.t =
          {
            run_mode = Run_mode.Perform_layout;
            sizing_mode = Sizing_mode.Inherent_size;
            axis = Requested_axis.Both;
            known_dimensions;
            parent_size;
            available_space;
            vertical_margins_are_collapsible = { start = false; end_ = false };
          }
        in

        (* Recursive layout computation *)
        let rec compute_node_layout node_id node_data input : Layout_output.t =
          let children =
            match Hashtbl.find_opt t.children node_id with
            | Some arr -> arr
            | None -> [||]
          in
          let child_count = Array.length children in

          if child_count = 0 then (
            (* Leaf node - use measure function *)
            let layout_output =
              Leaf_layout.compute_leaf_layout ~inputs:input
                ~style:node_data.NodeData.style
                ~resolve_calc_value:(fun () basis -> basis)
                ~measure_function:(fun known_dimensions available_space ->
                  let context = Hashtbl.find_opt t.node_context_data node_id in
                  measure_fn ~known_dimensions ~available_space node_id context
                    node_data.NodeData.style)
            in
            node_data.unrounded_layout <- Layout.empty;
            node_data.final_layout <-
              {
                order = 0;
                location = Geometry.point_zero;
                size = layout_output.Layout_output.size;
                content_size = layout_output.size;
                scrollbar_size = Geometry.size_zero;
                border = Geometry.rect_zero;
                padding = Geometry.rect_zero;
                margin = Geometry.rect_zero;
              };
            layout_output)
          else
            (* Container node - dispatch based on display mode *)
            match node_data.NodeData.style.display with
            | Style.Flex ->
                (* Create module adapter for flexbox *)
                let module TreeAdapter = struct
                  type nonrec t = ctx t
                  type child_iter = Node_id.t list
                  type core_container_style = Style.style
                  type flexbox_container_style = Style.style
                  type flexbox_item_style = Style.style

                  let child_ids _ node_id =
                    match Hashtbl.find_opt t.children node_id with
                    | Some children ->
                        Array.to_list (Array.map Node_id.of_int64 children)
                    | None -> []

                  let child_count _ node_id =
                    match Hashtbl.find_opt t.children node_id with
                    | Some children -> Array.length children
                    | None -> 0

                  let get_child_id _ node_id index =
                    match Hashtbl.find_opt t.children node_id with
                    | Some children ->
                        if index < Array.length children then
                          Node_id.of_int64 children.(index)
                        else failwith "Invalid child index"
                    | None -> failwith "Invalid child index"

                  let resolve_calc_value _ ~ptr:_ ~basis = basis

                  let set_unrounded_layout _ node_id layout =
                    match Hashtbl.find_opt t.nodes node_id with
                    | Some node_data ->
                        node_data.NodeData.unrounded_layout <- layout;
                        node_data.NodeData.final_layout <- layout
                    | None -> ()

                  let compute_child_layout _ child_id input =
                    match Hashtbl.find_opt t.nodes child_id with
                    | Some child_data ->
                        let layout_output =
                          compute_node_layout child_id child_data input
                        in
                        layout_output
                    | None -> Layout_output.hidden

                  let get_flexbox_container_style _ node_id =
                    match Hashtbl.find_opt t.nodes node_id with
                    | Some node_data -> node_data.NodeData.style
                    | None -> Style.default

                  let get_flexbox_child_style _ node_id =
                    match Hashtbl.find_opt t.nodes node_id with
                    | Some node_data -> node_data.NodeData.style
                    | None -> Style.default

                  let get_core_container_style _ node_id =
                    match Hashtbl.find_opt t.nodes node_id with
                    | Some node_data -> node_data.NodeData.style
                    | None -> Style.default
                end in
                let layout_output =
                  Flexbox_layout.compute_flexbox_layout
                    (module TreeAdapter)
                    t node_id input
                in

                node_data.unrounded_layout <-
                  {
                    order = 0;
                    location = Geometry.point_zero;
                    size = layout_output.Layout_output.size;
                    content_size = layout_output.Layout_output.size;
                    scrollbar_size = Geometry.size_zero;
                    border = Geometry.rect_zero;
                    padding = Geometry.rect_zero;
                    margin = Geometry.rect_zero;
                  };
                node_data.final_layout <- node_data.unrounded_layout;
                layout_output
            | Style.Block ->
                (* Create module adapter for block layout *)
                let module TreeAdapter = struct
                  type nonrec t = ctx t
                  type child_iter = Node_id.t list
                  type core_container_style = Style.style
                  type block_container_style = Style.style
                  type block_item_style = Style.style

                  let child_ids _ node_id =
                    match Hashtbl.find_opt t.children node_id with
                    | Some children ->
                        Array.to_list (Array.map Node_id.of_int64 children)
                    | None -> []

                  let child_count _ node_id =
                    match Hashtbl.find_opt t.children node_id with
                    | Some children -> Array.length children
                    | None -> 0

                  let get_child_id _ node_id index =
                    match Hashtbl.find_opt t.children node_id with
                    | Some children ->
                        if index < Array.length children then
                          Node_id.of_int64 children.(index)
                        else failwith "Invalid child index"
                    | None -> failwith "Invalid child index"

                  let get_core_container_style _ node_id =
                    match Hashtbl.find_opt t.nodes node_id with
                    | Some node -> node.NodeData.style
                    | None -> Style.default

                  let get_block_container_style _ node_id =
                    match Hashtbl.find_opt t.nodes node_id with
                    | Some node -> node.NodeData.style
                    | None -> Style.default

                  let get_block_child_style _ node_id =
                    match Hashtbl.find_opt t.nodes node_id with
                    | Some node -> node.NodeData.style
                    | None -> Style.default

                  let resolve_calc_value _ ~ptr:_ ~basis = basis

                  let set_unrounded_layout _ node_id layout =
                    match Hashtbl.find_opt t.nodes node_id with
                    | Some node_data ->
                        node_data.NodeData.unrounded_layout <- layout;
                        node_data.NodeData.final_layout <- layout
                    | None -> ()

                  let compute_child_layout _ child_id input =
                    match Hashtbl.find_opt t.nodes child_id with
                    | Some child_data ->
                        compute_node_layout child_id child_data input
                    | None -> Layout_output.hidden
                end in
                let layout_output =
                  Block_layout.compute_block_layout
                    (module TreeAdapter)
                    t node_id input
                in
                (* Update layout *)
                node_data.unrounded_layout <-
                  {
                    order = 0;
                    location = Geometry.point_zero;
                    size = layout_output.Layout_output.size;
                    content_size = layout_output.size;
                    scrollbar_size = Geometry.size_zero;
                    border = Geometry.rect_zero;
                    padding = Geometry.rect_zero;
                    margin = Geometry.rect_zero;
                  };
                node_data.final_layout <- node_data.unrounded_layout;
                layout_output
            | Style.Grid ->
                (* Create module adapter for grid layout *)
                let module TreeAdapter = struct
                  type nonrec t = ctx t
                  type child_iter = Node_id.t list
                  type core_container_style = Style.style
                  type grid_container_style = Style.style
                  type grid_item_style = Style.style

                  let child_ids _ node_id =
                    match Hashtbl.find_opt t.children node_id with
                    | Some child_array -> Array.to_list child_array
                    | None -> []

                  let get_child_id _ node_id index =
                    match Hashtbl.find_opt t.children node_id with
                    | Some child_array ->
                        if index < Array.length child_array then
                          child_array.(index)
                        else Node_id.of_int 0
                    | None -> Node_id.of_int 0

                  let child_count _ node_id =
                    match Hashtbl.find_opt t.children node_id with
                    | Some child_array -> Array.length child_array
                    | None -> 0

                  let resolve_calc_value _ ~ptr:_ ~basis:_ = 0.0

                  let get_grid_container_style _ node_id =
                    match Hashtbl.find_opt t.nodes node_id with
                    | Some node_data -> node_data.NodeData.style
                    | None -> Style.default

                  let get_grid_child_style _ node_id =
                    match Hashtbl.find_opt t.nodes node_id with
                    | Some node_data -> node_data.NodeData.style
                    | None -> Style.default

                  let get_core_container_style _ node_id =
                    match Hashtbl.find_opt t.nodes node_id with
                    | Some node_data -> node_data.NodeData.style
                    | None -> Style.default

                  let set_unrounded_layout _ node_id layout =
                    match Hashtbl.find_opt t.nodes node_id with
                    | Some node_data -> node_data.unrounded_layout <- layout
                    | None -> ()

                  let _get_cache _ _ = None
                  let _set_cached_measurement _ _ _ _ = ()

                  let compute_child_layout _ child_id input =
                    match Hashtbl.find_opt t.nodes child_id with
                    | Some child_data ->
                        compute_node_layout child_id child_data input
                    | None -> Layout_output.hidden

                  let _perform_child_layout _ child_id ~known_dimensions
                      ~parent_size ~available_space ~sizing_mode
                      ~vertical_margins_are_collapsible =
                    let layout_input =
                      {
                        Layout_input.known_dimensions;
                        parent_size;
                        available_space;
                        axis = Requested_axis.Both;
                        run_mode = Run_mode.Perform_layout;
                        sizing_mode;
                        vertical_margins_are_collapsible;
                      }
                    in
                    compute_child_layout () child_id layout_input

                  module DetailedGridInfo = struct
                    type t = unit
                  end

                  let set_detailed_grid_info _ _ _ = ()
                end in
                let layout_output =
                  Grid_layout.compute_grid_layout
                    (module TreeAdapter)
                    t node_id input
                in

                node_data.unrounded_layout <-
                  {
                    order = 0;
                    location = Geometry.point_zero;
                    size = layout_output.Layout_output.size;
                    content_size = layout_output.Layout_output.size;
                    scrollbar_size = Geometry.size_zero;
                    border = Geometry.rect_zero;
                    padding = Geometry.rect_zero;
                    margin = Geometry.rect_zero;
                  };
                node_data.final_layout <- node_data.unrounded_layout;
                layout_output
            | Style.None ->
                (* Hidden nodes *)
                node_data.unrounded_layout <- Layout.empty;
                node_data.final_layout <- Layout.empty;
                Layout_output.hidden
        in

        let layout_output =
          compute_node_layout root_id root_data layout_input
        in

        (* Set the root node's final layout *)
        let style = root_data.NodeData.style in
        let padding =
          Resolve.resolve_or_zero_rect_with_option
            Resolve.resolve_or_zero_length_percentage (Style.padding style)
            parent_size.width (fun () basis -> basis)
        in
        let border =
          Resolve.resolve_or_zero_rect_with_option
            Resolve.resolve_or_zero_length_percentage (Style.border style)
            parent_size.width (fun () basis -> basis)
        in
        let margin =
          Resolve.resolve_or_zero_rect_with_option
            Resolve.resolve_or_zero_length_percentage_auto (Style.margin style)
            parent_size.width (fun () basis -> basis)
        in
        let scrollbar_size =
          let overflow = Style.overflow style in
          {
            Geometry.width =
              (if overflow.y = Style.Scroll then Style.scrollbar_width style
               else 0.0);
            Geometry.height =
              (if overflow.x = Style.Scroll then Style.scrollbar_width style
               else 0.0);
          }
        in

        root_data.unrounded_layout <-
          {
            order = 0;
            location = Geometry.point_zero;
            size = layout_output.Layout_output.size;
            content_size = layout_output.size;
            scrollbar_size;
            border;
            padding;
            margin;
          };
        root_data.final_layout <- root_data.unrounded_layout;

        (* Apply rounding if enabled *)
        (if t.config.use_rounding then
           let module RoundTreeImpl = struct
             type nonrec t = ctx t
             type child_iter = Node_id.t list

             let child_ids t node_id =
               match Hashtbl.find_opt t.children node_id with
               | Some children ->
                   Array.to_list (Array.map Node_id.of_int64 children)
               | None -> []

             let child_count t node_id =
               match Hashtbl.find_opt t.children node_id with
               | Some children -> Array.length children
               | None -> 0

             let get_child_id t node_id index =
               match Hashtbl.find_opt t.children node_id with
               | Some children ->
                   if index < Array.length children then
                     Node_id.of_int64 children.(index)
                   else Node_id.of_int 0 (* This shouldn't happen *)
               | None -> Node_id.of_int 0

             let get_unrounded_layout t node_id =
               match Hashtbl.find_opt t.nodes node_id with
               | Some node_data -> node_data.NodeData.unrounded_layout
               | None -> Layout.empty

             let set_final_layout t node_id layout =
               match Hashtbl.find_opt t.nodes node_id with
               | Some node_data -> node_data.NodeData.final_layout <- layout
               | None -> ()
           end in
           Compute.round_layout (module RoundTreeImpl) t root_id);

        Ok ()

  let print_tree t root_id =
    (* Implementation following taffy's print_tree function *)
    Printf.printf "TREE\n";

    let rec print_node node_id has_sibling lines_string =
      match Hashtbl.find_opt t.nodes node_id with
      | None -> ()
      | Some node_data ->
          let layout = node_data.final_layout in
          let children =
            match Hashtbl.find_opt t.children node_id with
            | Some child_array -> Array.to_list child_array
            | None -> []
          in
          let num_children = List.length children in

          (* Get display label based on node style *)
          let display =
            match node_data.style.display with
            | Style.None -> "NONE"
            | Style.Block -> "BLOCK"
            | Style.Flex -> "FLEX"
            | Style.Grid -> "GRID"
          in

          let fork_string = if has_sibling then "├── " else "└── " in

          (* Print node info *)
          Printf.printf
            "%s%s %s [x: %-4.1f y: %-4.1f width: %-4.1f height: %-4.1f] (%s)\n"
            lines_string fork_string display layout.location.x layout.location.y
            layout.size.width layout.size.height (Node_id.show node_id);

          (* Update lines for children *)
          let bar = if has_sibling then "│   " else "    " in
          let new_lines_string = lines_string ^ bar in

          (* Recurse into children *)
          List.iteri
            (fun index child_id ->
              let child_has_sibling = index < num_children - 1 in
              print_node child_id child_has_sibling new_lines_string)
            children
    in

    print_node root_id false ""
end
