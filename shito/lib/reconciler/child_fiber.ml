open Types
open Fiber

(* Thenable state for handling suspended components *)
type thenable_state = {
  mutable thenables: (unit -> unit) list;
}

let thenable_state : thenable_state option ref = ref None
let thenable_index_counter = ref 0

(* Create thenable state *)
let create_thenable_state () = {
  thenables = [];
}

(* Track used thenable *)
let track_used_thenable _state thenable _index =
  (* In a full implementation, this would track the thenable for Suspense *)
  thenable ()

(* Unwrap thenable using tracked state *)
let unwrap_thenable thenable =
  let index = !thenable_index_counter in
  thenable_index_counter := !thenable_index_counter + 1;
  let state = match !thenable_state with
    | None -> 
      let s = create_thenable_state () in
      thenable_state := Some s;
      s
    | Some s -> s
  in
  track_used_thenable state thenable index

(* Debug info tracking - used for React DevTools *)
let current_debug_info : unit option ref = ref None

(* Push debug info onto stack *)
let push_debug_info _debug_info =
  let prev = !current_debug_info in
  (* In production build, we don't track debug info *)
  prev

(* Map type for existing children during reconciliation *)
module ChildMap = Map.Make(struct
  type t = string option  (* key *)
  let compare = compare
end)

(* Check if a value is an array - used to determine reconciliation path *)
let is_array = function
  | Child_array _ -> true
  | _ -> false

(* Get iterator function for iterables - used for handling iterables as children *)
let get_iterator_fn = function
  | Child_array lst -> Some (fun () -> lst)
  | Child_list lst -> Some (fun () -> lst)
  | _ -> None

(* Create child reconciler with side effect tracking control *)
let create_child_reconciler should_track_side_effects =
  
  (* Delete a child fiber *)
  let delete_child return_fiber child_to_delete =
    if not should_track_side_effects then
      ()
    else
      let deletions = return_fiber.Fiber.deletions in
      if deletions = [] then begin
        return_fiber.Fiber.deletions <- [child_to_delete];
        return_fiber.Fiber.flags <- return_fiber.Fiber.flags lor Fiber_flags.child_deletion
      end else
        return_fiber.Fiber.deletions <- child_to_delete :: deletions
  in
  
  (* Delete all remaining children *)
  let rec delete_remaining_children return_fiber current_first_child =
    if not should_track_side_effects then
      None
    else
      match current_first_child with
      | None -> None
      | Some child ->
        delete_child return_fiber child;
        delete_remaining_children return_fiber child.sibling
  in
  
  (* Map remaining children for efficient lookup *)
  let rec map_remaining_children current_first_child acc =
    match current_first_child with
    | None -> acc
    | Some child ->
      let key = match child.Fiber.key with
        | Some k -> Some k
        | None -> Some (string_of_int child.Fiber.index)
      in
      let acc' = ChildMap.add key child acc in
      map_remaining_children child.Fiber.sibling acc'
  in
  
  (* Reuse existing fiber *)
  let use_fiber fiber pending_props =
    (* Create work in progress from existing fiber *)
    let clone = Fiber.create_work_in_progress fiber pending_props in
    clone.Fiber.index <- 0;
    clone.Fiber.sibling <- None;
    clone
  in
  
  (* Place child in the right position *)
  let place_child new_fiber last_placed_index new_index =
    new_fiber.Fiber.index <- new_index;
    if not should_track_side_effects then begin
      new_fiber.Fiber.flags <- new_fiber.Fiber.flags lor Fiber_flags.forked;
      last_placed_index
    end else
      match new_fiber.Fiber.alternate with
      | Some current ->
        let old_index = current.Fiber.index in
        if old_index < last_placed_index then begin
          (* This is a move *)
          new_fiber.Fiber.flags <- new_fiber.Fiber.flags lor Fiber_flags.placement;
          last_placed_index
        end else
          (* This item can stay in place *)
          old_index
      | None ->
        (* This is an insertion *)
        new_fiber.Fiber.flags <- new_fiber.Fiber.flags lor Fiber_flags.placement;
        last_placed_index
  in
  
  (* Place single child *)
  let place_single_child new_fiber =
    if should_track_side_effects && new_fiber.Fiber.alternate = None then
      new_fiber.Fiber.flags <- new_fiber.Fiber.flags lor Fiber_flags.placement;
    new_fiber
  in
  
  (* Update text node *)
  let update_text_node return_fiber current text_content lanes =
    match current with
    | None -> 
      (* Insert new text node *)
      let created = Fiber.create_fiber_from_text text_content return_fiber.Fiber.mode lanes in
      created.Fiber.return <- Some return_fiber;
      created
    | Some existing when existing.Fiber.tag = Work_tags.Host_text ->
      (* Update existing text node - wrap in Text_props *)
      let updated = use_fiber existing (Text_props text_content) in
      updated.Fiber.return <- Some return_fiber;
      updated
    | Some _ ->
      (* Replace non-text with text *)
      let created = Fiber.create_fiber_from_text text_content return_fiber.Fiber.mode lanes in
      created.Fiber.return <- Some return_fiber;
      created
  in
  
  (* Update element *)
  let update_element return_fiber current element lanes =
      match current with
      | Some existing when Fiber.can_reuse_fiber existing (Child_element element) ->
        (* Reuse existing fiber - wrap props in Element_props *)
        let props = Element_props element.props in
        let updated = use_fiber existing props in
        updated.Fiber.return <- Some return_fiber;
        updated
      | _ ->
        (* Create new fiber *)
        let created = Fiber.create_fiber_from_element (Child_element element) return_fiber.mode lanes in
        created.Fiber.return <- Some return_fiber;
        created
  
  (* Update fragment *)
  and update_fragment return_fiber current children lanes key =
    match current with
    | Some existing when existing.Fiber.tag = Work_tags.Fragment ->
      (* Update existing fragment - wrap children in Fragment_props *)
      let props = Fragment_props children in
      let updated = use_fiber existing props in
      updated.Fiber.return <- Some return_fiber;
      updated
    | _ ->
      (* Create new fragment *)
      let created = Fiber.create_fiber_from_fragment children return_fiber.Fiber.mode lanes key in
      created.Fiber.return <- Some return_fiber;
      created
  in
  
  (* Update portal *)
  let update_portal return_fiber current portal lanes =
    match portal with
    | Portal_child { children; container_info; implementation; key } ->
      (match current with
      | Some existing when 
          existing.tag = Work_tags.Host_portal &&
          (match existing.state_node with
          | Portal_state c ->
            c = container_info
          | _ -> false) ->
        (* Update existing portal *)
        let updated = use_fiber existing (Portal_props children) in
        updated.return <- Some return_fiber;
        updated
      | _ ->
        (* Create new portal *)
        let created = Fiber.create_fiber_from_portal 
          ~children ~container_info ~implementation ~key return_fiber.Fiber.mode lanes in
        created.return <- Some return_fiber;
        created)
  in
  
  (* Create child from any value *)
  let rec create_child return_fiber new_child lanes =
    match new_child with
    | Child_string text | Child_number text | Child_bigint text when text <> "" ->
      let created = Fiber.create_fiber_from_text text return_fiber.Fiber.mode lanes in
      created.return <- Some return_fiber;
      Some created
    
    | Child_element element ->
      let created = update_element return_fiber None element lanes in
      Some created
    
    | Child_portal portal ->
      let created = update_portal return_fiber None portal lanes in
      Some created
    
    | Child_array children | Child_list children ->
      let created = Fiber.create_fiber_from_fragment 
        children return_fiber.Fiber.mode lanes None in
      created.return <- Some return_fiber;
      Some created
    
    | Child_thenable thenable ->
      (* Unwrap thenable and recursively create child *)
      let unwrapped = unwrap_thenable thenable in
      create_child return_fiber unwrapped lanes
    
    | Child_context _ ->
      (* Context reading would require readContextDuringReconciliation *)
      (* For now, return None as contexts need more infrastructure *)
      None
    
    | _ -> None
  in
  
  (* Update slot - try to update existing fiber if keys match *)
  let update_slot return_fiber old_fiber new_child lanes =
    let old_key = match old_fiber with
      | Some f -> f.key
      | None -> None
    in
    
    match new_child with
    | Child_string text | Child_number text | Child_bigint text when text <> "" ->
      if old_key <> None then
        None  (* Text nodes don't have keys *)
      else
        Some (update_text_node return_fiber old_fiber text lanes)
    
    | Child_element element ->
      let element_key = Fiber.get_element_key element in
      if element_key = old_key then
        Some (update_element return_fiber old_fiber element lanes)
      else
        None
    
    | Child_portal (Portal_child { key; _ } as portal) ->
      if key = old_key then
        Some (update_portal return_fiber old_fiber portal lanes)
      else
        None
    
    | Child_array children | Child_list children ->
      if old_key <> None then
        None  (* Arrays/lists as children don't have keys *)
      else
        Some (update_fragment return_fiber old_fiber children lanes None)
    
    | _ -> None
  in
  
  (* Update from map - used during reconciliation with remaining children *)
  let update_from_map existing_children return_fiber new_idx new_child lanes =
    match new_child with
    | Child_string text | Child_number text | Child_bigint text when text <> "" ->
      let matched_fiber = ChildMap.find_opt (Some (string_of_int new_idx)) existing_children in
      Some (update_text_node return_fiber matched_fiber text lanes)
    
    | Child_element element ->
      let key = match Fiber.get_element_key element with
        | Some k -> Some k
        | None -> Some (string_of_int new_idx)
      in
      let matched_fiber = ChildMap.find_opt key existing_children in
      Some (update_element return_fiber matched_fiber element lanes)
    
    | Child_portal (Portal_child { key; _ } as portal) ->
      let key = match key with
        | Some k -> Some k
        | None -> Some (string_of_int new_idx)
      in
      let matched_fiber = ChildMap.find_opt key existing_children in
      Some (update_portal return_fiber matched_fiber portal lanes)
    
    | Child_array children | Child_list children ->
      let matched_fiber = ChildMap.find_opt (Some (string_of_int new_idx)) existing_children in
      Some (update_fragment return_fiber matched_fiber children lanes None)
    
    | _ -> None
  in
  
  (* Reconcile children array *)
  let reconcile_children_array return_fiber current_first_child new_children lanes =
    let rec process_updates old_fiber new_idx last_placed_index result_first prev_new =
      if new_idx >= List.length new_children then
        (* We've reached the end of new children, delete the rest *)
        let _ = delete_remaining_children return_fiber old_fiber in
        result_first
      else
        match old_fiber with
        | Some current when current.index <= new_idx ->
          (* Try to update existing fiber *)
          let new_child = List.nth new_children new_idx in
          let next_old = current.sibling in
          (match update_slot return_fiber (Some current) new_child lanes with
          | Some new_fiber ->
            (* Successfully updated *)
            if should_track_side_effects && new_fiber.alternate = None then
              delete_child return_fiber current;
            let last_placed = place_child new_fiber last_placed_index new_idx in
            new_fiber.sibling <- None;
            (match prev_new with
            | None -> 
              process_updates next_old (new_idx + 1) last_placed (Some new_fiber) (Some new_fiber)
            | Some prev ->
              prev.sibling <- Some new_fiber;
              process_updates next_old (new_idx + 1) last_placed result_first (Some new_fiber))
          | None ->
            (* Keys don't match, enter slow path *)
            reconcile_with_map current new_idx new_children lanes last_placed_index result_first prev_new)
        | _ ->
          (* No more existing children, create new ones *)
          create_remaining new_idx new_children lanes last_placed_index result_first prev_new
    
    and create_remaining new_idx new_children lanes last_placed_index result_first prev_new =
      if new_idx >= List.length new_children then
        result_first
      else
        let new_child = List.nth new_children new_idx in
        match create_child return_fiber new_child lanes with
        | Some new_fiber ->
          let last_placed = place_child new_fiber last_placed_index new_idx in
          new_fiber.sibling <- None;
          (match prev_new with
          | None ->
            create_remaining (new_idx + 1) new_children lanes last_placed (Some new_fiber) (Some new_fiber)
          | Some prev ->
            prev.sibling <- Some new_fiber;
            create_remaining (new_idx + 1) new_children lanes last_placed result_first (Some new_fiber))
        | None ->
          create_remaining (new_idx + 1) new_children lanes last_placed_index result_first prev_new
    
    and reconcile_with_map old_fiber new_idx new_children lanes last_placed_index result_first prev_new =
      (* Build map of remaining children *)
      let existing_map = map_remaining_children (Some old_fiber) ChildMap.empty in
      
      let rec process_from_map idx last_placed result prev existing_map =
        if idx >= List.length new_children then begin
          (* Delete remaining children in the map *)
          if should_track_side_effects then
            ChildMap.iter (fun _ child -> delete_child return_fiber child) existing_map;
          result
        end else
          let new_child = List.nth new_children idx in
          match update_from_map existing_map return_fiber idx new_child lanes with
          | Some new_fiber ->
            (* Remove from map if reused *)
            let existing_map' = 
              if should_track_side_effects && new_fiber.alternate <> None then
                let key = match new_fiber.key with
                  | Some k -> Some k
                  | None -> Some (string_of_int idx)
                in
                ChildMap.remove key existing_map
              else
                existing_map
            in
            let last_placed' = place_child new_fiber last_placed idx in
            new_fiber.sibling <- None;
            (match prev with
            | None ->
              process_from_map (idx + 1) last_placed' (Some new_fiber) (Some new_fiber) existing_map'
            | Some p ->
              p.sibling <- Some new_fiber;
              process_from_map (idx + 1) last_placed' result (Some new_fiber) existing_map')
          | None ->
            process_from_map (idx + 1) last_placed result prev existing_map
      in
      
      process_from_map new_idx last_placed_index result_first prev_new existing_map
    in
    
    process_updates current_first_child 0 0 None None
  in
  
  (* Reconcile single element *)
  let reconcile_single_element return_fiber current_first_child element lanes =
    let rec check_existing child =
      match child with
      | None -> None
      | Some existing ->
        if Fiber.can_reuse_fiber existing (Child_element element) then begin
          (* Delete siblings since we only need one *)
          let _ = delete_remaining_children return_fiber existing.sibling in
          (* Reuse this fiber - wrap element props *)
          let props = Element_props element.props in
          let existing' = use_fiber existing props in
          existing'.Fiber.return <- Some return_fiber;
          Some existing'
        end else begin
          delete_child return_fiber existing;
          check_existing existing.sibling
        end
    in
    
    match check_existing current_first_child with
    | Some fiber -> fiber
    | None ->
      (* Create new fiber *)
      let created = Fiber.create_fiber_from_element 
        (Child_element element) return_fiber.mode lanes in
      created.Fiber.return <- Some return_fiber;
      created
  in
  
  (* Reconcile single text node *)
  let reconcile_single_text_node return_fiber current_first_child text lanes =
    match current_first_child with
    | Some existing when existing.tag = Work_tags.Host_text ->
      (* Delete siblings *)
      let _ = delete_remaining_children return_fiber existing.sibling in
      (* Update existing text node *)
      let existing' = use_fiber existing (Text_props text) in
      existing'.return <- Some return_fiber;
      existing'
    | _ ->
      (* Delete all existing children and create new text node *)
      let _ = delete_remaining_children return_fiber current_first_child in
      let created = Fiber.create_fiber_from_text text return_fiber.Fiber.mode lanes in
      created.return <- Some return_fiber;
      created
  in
  
  (* Reconcile single portal *)
  let reconcile_single_portal return_fiber current_first_child portal lanes =
    match portal with
    | Portal_child { children; container_info; implementation; key } ->
      let rec check_existing child =
        match child with
        | None -> None
        | Some existing ->
          if existing.tag = Work_tags.Host_portal &&
             (match existing.state_node with
              | Portal_state c ->
                c = container_info
              | _ -> false) then begin
            (* Delete siblings *)
            let _ = delete_remaining_children return_fiber existing.sibling in
            (* Reuse this portal *)
            let existing' = use_fiber existing (Portal_props children) in
            existing'.return <- Some return_fiber;
            Some existing'
          end else begin
            delete_child return_fiber existing;
            check_existing existing.sibling
          end
      in
      
      (match check_existing current_first_child with
      | Some fiber -> fiber
      | None ->
        (* Delete all existing and create new portal *)
        let _ = delete_remaining_children return_fiber current_first_child in
        let created = Fiber.create_fiber_from_portal 
          ~children ~container_info ~implementation ~key return_fiber.Fiber.mode lanes in
        created.return <- Some return_fiber;
        created)
  in
  
  (* Main reconciliation function *)
  let reconcile_child_fibers_impl return_fiber current_first_child new_child lanes =
    match new_child with
    | Child_element element ->
      let fiber = reconcile_single_element return_fiber current_first_child element lanes in
      Some (place_single_child fiber)
    
    | Child_portal portal ->
      let fiber = reconcile_single_portal return_fiber current_first_child portal lanes in
      Some (place_single_child fiber)
    
    | Child_string text | Child_number text | Child_bigint text when text <> "" ->
      let fiber = reconcile_single_text_node return_fiber current_first_child text lanes in
      Some (place_single_child fiber)
    
    | Child_array children ->
      reconcile_children_array return_fiber current_first_child children lanes
    
    | Child_list children ->
      (* Lists are also treated as arrays *)
      reconcile_children_array return_fiber current_first_child children lanes
    
    | Child_fragment element ->
      (* Fragments are treated based on whether they have a key *)
      (match element.key with
      | None ->
        (* Unkeyed fragments - extract children and treat as array *)
        (* For now, treat as empty array - proper implementation would extract children from props *)
        reconcile_children_array return_fiber current_first_child [] lanes
      | Some _ ->
        (* Keyed fragments are treated as single elements *)
        let fiber = reconcile_single_element return_fiber current_first_child element lanes in
        Some (place_single_child fiber))
    
    | Child_thenable _ ->
      (* Handle suspended components - for now we skip *)
      None
    
    | Child_context _ ->
      (* Handle context consumers - for now we skip *)
      None
    
    | _ ->
      (* All other cases delete remaining children *)
      delete_remaining_children return_fiber current_first_child
  in
  
  (* Public reconciliation function with error handling *)
  let reconcile_child_fibers ~return_fiber ~current_first_child ~new_child ~lanes =
    thenable_index_counter := 0;
    try
      let result = reconcile_child_fibers_impl return_fiber current_first_child new_child lanes in
      thenable_state := None;
      result
    with
    | exn ->
      (* Handle suspense and errors *)
      thenable_state := None;
      raise exn
  in
  
  reconcile_child_fibers

(* Create the two exported reconcilers *)
let reconcile_child_fibers ~return_fiber ~current_first_child ~new_child ~lanes =
  let reconciler = create_child_reconciler true in
  reconciler ~return_fiber ~current_first_child ~new_child ~lanes

let mount_child_fibers ~return_fiber ~current_first_child ~new_child ~lanes =
  let reconciler = create_child_reconciler false in
  reconciler ~return_fiber ~current_first_child ~new_child ~lanes

(* Clone child fibers *)
let clone_child_fibers ~current ~work_in_progress =
  match current, work_in_progress.child with
  | Some c, Some w when Some w <> c.child ->
    failwith "Resuming work not yet implemented"
  | _ ->
    match work_in_progress.child with
    | None -> ()
    | Some child ->
      let rec clone_siblings current_child new_child =
        new_child.return <- Some work_in_progress;
        match current_child.sibling with
        | None -> ()
        | Some sibling ->
          let new_sibling = Fiber.create_work_in_progress sibling sibling.pending_props in
          new_child.sibling <- Some new_sibling;
          clone_siblings sibling new_sibling
      in
      let new_child = Fiber.create_work_in_progress child child.pending_props in
      work_in_progress.child <- Some new_child;
      clone_siblings child new_child

(* Reset thenable state *)
let reset_child_reconciler_on_unwind () =
  thenable_state := None;
  thenable_index_counter := 0