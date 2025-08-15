module type S = sig
  module Incr : sig
    type 'a t
  end

  type 'a t = 'a Incr.t

  (* For time-based incremental nodes *)
  type 'a before_or_after =
    | Before of 'a
    | After of 'a

  module Var : sig
    type 'a t

    val create : 'a -> 'a t
    val set : 'a t -> 'a -> unit
    val value : 'a t -> 'a
    val watch : 'a t -> 'a Incr.t
  end

  module Observer : sig
    type 'a t

    val observing : 'a t -> 'a Incr.t
    val value_exn : 'a t -> 'a

    module Update : sig
      type 'a t = Initialized of 'a | Changed of 'a * 'a | Invalidated
    end

    val on_update_exn : 'a t -> f:('a Update.t -> unit) -> unit
    val disallow_future_use : 'a t -> unit
  end

  val observe : 'a t -> 'a Observer.t
  val const : 'a -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  val map3 : 'a t -> 'b t -> 'c t -> f:('a -> 'b -> 'c -> 'd) -> 'd t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val if_ : bool t -> then_:'a t -> else_:'a t -> 'a t
  val join : 'a t t -> 'a t
  val freeze : ?when_:('a -> bool) -> 'a t -> 'a t

  module Infix : sig
    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module Let_syntax : sig
    val return : 'a -> 'a t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
    val both : 'a t -> 'b t -> ('a * 'b) t
    val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  end

  module Cutoff : sig
    type 'a t

    val create : (old:'a -> new_:'a -> bool) -> 'a t
    val phys_equal : 'a t
    val never : 'a t
    val always : 'a t
  end

  val set_cutoff : 'a t -> 'a Cutoff.t -> unit
  val get_cutoff : 'a t -> 'a Cutoff.t
  val stabilize : unit -> unit
  val is_stabilizing : unit -> bool

  (* Clock API for time-based incremental nodes *)
  val at : Time.t -> Time.t before_or_after t
  val after : Time.Span.t -> Time.t before_or_after t
  val at_intervals : base:Time.t -> interval:Time.Span.t -> unit t
  val snapshot : 'a t -> at:Time.t t -> f:(Time.t -> 'a -> 'b) -> 'b t
  val step_function : init:'a -> (Time.t * 'a) list -> 'a t
  val advance_clock : to_:Time.t -> unit
end

module Make () : S = struct
  exception Cycle_detected
  exception Invalid_observer
  exception Not_stabilized

  module Stabilization_num = struct
    type t = int

    let none = -1
    let is_none t = t = none
    let is_some t = t >= 0
    let add1 t = t + 1
  end

  type stabilization_num = Stabilization_num.t

  (* For time-based incremental nodes *)
  type 'a before_or_after =
    | Before of 'a
    | After of 'a


  type _ kind =
    | Invalid
    | Const : 'a -> 'a kind
    | Var : 'a ref -> 'a kind
    | Map : { f : 'a -> 'b; input : 'a node } -> 'b kind
    | Map2 : {
        f : 'a -> 'b -> 'c;
        input1 : 'a node;
        input2 : 'b node;
      }
        -> 'c kind
    | Map3 : {
        f : 'a -> 'b -> 'c -> 'd;
        input1 : 'a node;
        input2 : 'b node;
        input3 : 'c node;
      }
        -> 'd kind
    | Bind_main : (_, 'b) bind_data -> 'b kind
    | Bind_lhs_change : (_, _) bind_data -> unit kind
    | If_then_else : 'a if_then_else_data -> 'a kind
    | If_test_change : _ if_then_else_data -> unit kind
    | Join_main : 'a join_data -> 'a kind
    | Join_lhs_change : _ join_data -> unit kind
    | Freeze : 'a freeze_data -> 'a kind
    | At : at_data -> Time.t before_or_after kind
    | At_intervals : at_intervals_data -> unit kind
    | Snapshot : ('a, 'b) snapshot_data -> 'b kind
    | Step_function : 'a step_function_data -> 'a kind

  and 'a node = {
    mutable kind : 'a kind;
    mutable value : 'a option;
    mutable height : int;
    mutable cutoff : old:'a -> new_:'a -> bool;
    mutable children : packed_node list;
    mutable parents : packed_node list;
    mutable observers : 'a observer list;
    mutable in_recompute_heap : bool;
    mutable height_in_recompute_heap : int;
    mutable prev_in_recompute_heap : packed_node option;
    mutable next_in_recompute_heap : packed_node option;
    mutable height_in_adjust_heights_heap : int;
    mutable prev_in_adjust_heights_heap : packed_node option;
    mutable next_in_adjust_heights_heap : packed_node option;
    mutable recomputed_at : stabilization_num;
    mutable changed_at : stabilization_num;
    mutable created_in : scope;
    mutable next_in_same_scope : packed_node option;
    mutable num_invalid_children : int;
  }

  and packed_node = Node : _ node -> packed_node
  and scope = Top | Bind of bind_any
  and bind_any = Bind_any : (_, _) bind_data -> bind_any

  and ('a, 'b) bind_data = {
    main : 'b node;
    mutable f : 'a -> 'b node;
    lhs : 'a node;
    lhs_change : unit node;
    mutable rhs : 'b node option;
    mutable rhs_scope : scope;
    mutable all_nodes_created_on_rhs : packed_node option;
  }

  and 'a if_then_else_data = {
    main : 'a node;
    test : bool node;
    test_change : unit node;
    mutable current_branch : 'a node option;
    then_ : 'a node;
    else_ : 'a node;
  }

  and 'a join_data = {
    main : 'a node;
    lhs : 'a node node;
    lhs_change : unit node;
    mutable rhs : 'a node option;
  }

  and 'a freeze_data = {
    main : 'a node;
    child : 'a node;
    only_freeze_when : 'a -> bool;
  }

  and at_data = {
    main : Time.t before_or_after node;
    at : Time.t;
    mutable alarm : alarm;
    clock : clock;
  }

  and at_intervals_data = {
    main : unit node;
    base : Time.t;
    interval : Time.Span.t;
    mutable alarm : alarm;
    clock : clock;
  }

  and ('a, 'b) snapshot_data = {
    main : 'b node;
    at : Time.t;
    before : 'b;
    value_at : 'a node;
    clock : clock;
  }

  and 'a step_function_data = {
    main : 'a node;
    init : 'a;
    steps : (Time.t * 'a) array;
    mutable current_index : int;
    mutable alarm : alarm;
    clock : clock;
  }
  
  and 'a var = {
    mutable value : 'a;
    mutable value_set_during_stabilization : 'a option;
    watch : 'a node;
  }
  
  and alarm = alarm_value Timing_wheel.alarm
  
  and alarm_value = {
    action : alarm_action;
    mutable next_fired : alarm_value option;
  }
  
  and alarm_action =
    | At of at_data
    | At_intervals of at_intervals_data
    | Snapshot : ('a, 'b) snapshot_data -> alarm_action
    | Step_function : 'a step_function_data -> alarm_action
    
  and clock = {
    timing_wheel : alarm_value Timing_wheel.t;
    now : Time.t var;
    handle_fired : alarm_value -> unit;
    mutable fired_alarm_values : alarm_value option;
  }

  and 'a observer = {
    node : 'a node;
    mutable on_update : ('a update -> unit) list;
    mutable active : bool;
    mutable old_value : 'a option; [@warning "-69"]
  }

  and 'a update = Initialized of 'a | Changed of 'a * 'a | Invalidated

  module Incr = struct
    type 'a t = 'a node
  end

  type 'a t = 'a Incr.t

  (* Heaps for stabilization *)
  type recompute_heap = {
    data : packed_node option array;
        (* Head of doubly-linked list at each height *)
    mutable min_height : int;
    mutable size : int;
    max_height : int;
  }

  type adjust_heights_heap = {
    data : packed_node option array;
        (* Head of doubly-linked list at each height *)
    mutable min_height : int;
    mutable size : int;
    mutable max_height_seen : int;
    max_height_allowed : int;
  }

  type packed_var = Var_packed : _ var -> packed_var

  type 'a pending_update = {
    update_node : 'a node;
    update_old_value : 'a option;
    update_new_value : 'a;
  }

  type packed_pending_update =
    | Pending_update : _ pending_update -> packed_pending_update

  type status =
    | Not_stabilizing
    | Stabilizing
    | Stabilize_previously_raised of exn

  type state = {
    mutable stabilization_num : stabilization_num;
    mutable status : status;
    recompute_heap : recompute_heap;
    adjust_heights_heap : adjust_heights_heap;
    mutable current_scope : scope;
    mutable propagate_invalidity : packed_node list;
    mutable vars_set_during_stabilization : packed_var list;
    mutable updates_to_handle : packed_pending_update list;
    mutable clock : clock option;
  }

  (* Helper to get null alarm *)
  let get_null_alarm () = Timing_wheel.Alarm.null ()

  let global_state = ref None

  let ensure_state () =
    match !global_state with
    | Some s -> s
    | None ->
        let s =
          {
            stabilization_num = 0;
            status = Not_stabilizing;
            recompute_heap =
              {
                data = Array.make 129 None;
                min_height = 129;
                size = 0;
                max_height = 128;
              };
            adjust_heights_heap =
              {
                data = Array.make 129 None;
                min_height = 129;
                size = 0;
                max_height_seen = 0;
                max_height_allowed = 128;
              };
            current_scope = Top;
            propagate_invalidity = [];
            vars_set_during_stabilization = [];
            updates_to_handle = [];
            clock = None;
          }
        in
        global_state := Some s;
        s

  let add_to_recompute_heap heap (Node n as packed) =
    if not n.in_recompute_heap then (
      let height = n.height in
      if height < 0 || height > heap.max_height then raise Cycle_detected;
      n.in_recompute_heap <- true;
      n.height_in_recompute_heap <- height;
      (* Add to head of doubly-linked list *)
      n.prev_in_recompute_heap <- None;
      n.next_in_recompute_heap <- heap.data.(height);
      (match heap.data.(height) with
      | Some (Node next) -> next.prev_in_recompute_heap <- Some packed
      | None -> ());
      heap.data.(height) <- Some packed;
      heap.size <- heap.size + 1;
      if height < heap.min_height then heap.min_height <- height)

  let unlink_from_recompute_heap (heap : recompute_heap) (Node n) =
    let prev = n.prev_in_recompute_heap in
    let next = n.next_in_recompute_heap in
    (match prev with
    | Some (Node p) -> p.next_in_recompute_heap <- next
    | None -> heap.data.(n.height_in_recompute_heap) <- next);
    (match next with
    | Some (Node next_n) -> next_n.prev_in_recompute_heap <- prev
    | None -> ());
    n.prev_in_recompute_heap <- None;
    n.next_in_recompute_heap <- None

  let remove_min_recompute_heap (heap : recompute_heap) =
    if heap.size = 0 then failwith "Empty recompute heap";
    while
      heap.min_height <= heap.max_height && heap.data.(heap.min_height) = None
    do
      heap.min_height <- heap.min_height + 1
    done;
    if heap.min_height > heap.max_height then failwith "Empty recompute heap";
    match heap.data.(heap.min_height) with
    | Some (Node n as packed) ->
        unlink_from_recompute_heap heap (Node n);
        heap.size <- heap.size - 1;
        n.in_recompute_heap <- false;
        n.height_in_recompute_heap <- -1;
        packed
    | None -> assert false

  let add_to_adjust_heights_heap heap (Node n as packed) =
    if n.height_in_adjust_heights_heap = -1 then (
      let height = n.height in
      n.height_in_adjust_heights_heap <- height;
      (* Add to head of doubly-linked list *)
      n.prev_in_adjust_heights_heap <- None;
      n.next_in_adjust_heights_heap <- heap.data.(height);
      (match heap.data.(height) with
      | Some (Node next) -> next.prev_in_adjust_heights_heap <- Some packed
      | None -> ());
      heap.data.(height) <- Some packed;
      heap.size <- heap.size + 1;
      if height < heap.min_height then heap.min_height <- height)

  let unlink_from_adjust_heights_heap (heap : adjust_heights_heap) (Node n) =
    let prev = n.prev_in_adjust_heights_heap in
    let next = n.next_in_adjust_heights_heap in
    (match prev with
    | Some (Node p) -> p.next_in_adjust_heights_heap <- next
    | None -> heap.data.(n.height_in_adjust_heights_heap) <- next);
    (match next with
    | Some (Node next_n) -> next_n.prev_in_adjust_heights_heap <- prev
    | None -> ());
    n.prev_in_adjust_heights_heap <- None;
    n.next_in_adjust_heights_heap <- None

  let remove_min_adjust_heights_heap (heap : adjust_heights_heap) =
    if heap.size = 0 then failwith "Empty adjust heights heap";
    while
      heap.min_height <= heap.max_height_allowed
      && heap.data.(heap.min_height) = None
    do
      heap.min_height <- heap.min_height + 1
    done;
    if heap.min_height > heap.max_height_allowed then
      failwith "Empty adjust heights heap";
    match heap.data.(heap.min_height) with
    | Some (Node n as packed) ->
        unlink_from_adjust_heights_heap heap (Node n);
        heap.size <- heap.size - 1;
        n.height_in_adjust_heights_heap <- -1;
        packed
    | None -> assert false

  let increase_height_in_recompute_heap (recompute_heap : recompute_heap)
      (Node n as packed) =
    if n.in_recompute_heap && n.height > n.height_in_recompute_heap then (
      (* Remove from old position using O(1) unlink *)
      unlink_from_recompute_heap recompute_heap (Node n);
      (* Update height *)
      let new_height = n.height in
      n.height_in_recompute_heap <- new_height;
      (* Add to new position *)
      n.prev_in_recompute_heap <- None;
      n.next_in_recompute_heap <- recompute_heap.data.(new_height);
      (match recompute_heap.data.(new_height) with
      | Some (Node next) -> next.prev_in_recompute_heap <- Some packed
      | None -> ());
      recompute_heap.data.(new_height) <- Some packed)

  let scope_add_node scope (node : _ node) =
    node.created_in <- scope;
    match scope with
    | Top -> ()
    | Bind (Bind_any bind) ->
        node.next_in_same_scope <- bind.all_nodes_created_on_rhs;
        bind.all_nodes_created_on_rhs <- Some (Node node)

  let create_node kind =
    let s = ensure_state () in
    let node =
      {
        kind;
        value = None;
        height = -1;
        cutoff = (fun ~old ~new_ -> old == new_);
        children = [];
        parents = [];
        observers = [];
        in_recompute_heap = false;
        height_in_recompute_heap = -1;
        prev_in_recompute_heap = None;
        next_in_recompute_heap = None;
        height_in_adjust_heights_heap = -1;
        prev_in_adjust_heights_heap = None;
        next_in_adjust_heights_heap = None;
        recomputed_at = Stabilization_num.none;
        changed_at = Stabilization_num.none;
        created_in = s.current_scope;
        next_in_same_scope = None;
        num_invalid_children = 0;
      }
    in
    scope_add_node s.current_scope node;
    node

  let node_same : type a b. a node -> b node -> bool =
   fun n1 n2 -> n1 == Obj.magic n2

  let is_necessary node = node.height >= 0
  let is_valid node = match node.kind with Invalid -> false | _ -> true

  let scope_is_valid = function
    | Top -> true
    | Bind (Bind_any bind) -> is_valid bind.main

  let scope_height = function
    | Top -> -1
    | Bind (Bind_any bind) -> bind.lhs_change.height

  let is_valid_in_scope node = is_valid node && scope_is_valid node.created_in

  let check_kind_consistency : type a. a node -> bool =
   fun node ->
    match node.kind with
    | Bind_main bind -> node_same bind.main node
    | Bind_lhs_change bind -> node_same bind.lhs_change node
    | If_then_else if_data -> node_same if_data.main node
    | If_test_change if_data -> node_same if_data.test_change node
    | Join_main join -> node_same join.main node
    | Join_lhs_change join -> node_same join.lhs_change node
    | Freeze freeze -> node_same freeze.main node
    | _ -> true

  let is_stale _s node =
    if not (is_necessary node) then false
    else
      (* Check if any child has changed after we were last recomputed *)
      Stabilization_num.is_none node.recomputed_at
      || List.exists
           (fun (Node child) ->
             Stabilization_num.is_some child.changed_at
             && child.changed_at > node.recomputed_at)
           node.children

  let should_be_invalidated node = node.num_invalid_children > 0

  let rec invalidate_node (Node n) =
    if is_valid n then (
      (match n.kind with
      | Bind_main bind ->
          invalidate_nodes_created_on_rhs bind.all_nodes_created_on_rhs;
          (* Clear the function to allow GC - this is why f is mutable *)
          bind.f <- (fun _ -> raise Not_stabilized)
      | _ -> ());
      n.kind <- Invalid;
      n.value <- None;
      n.changed_at <- (ensure_state ()).stabilization_num;
      (* Update parent's invalid child count *)
      List.iter
        (fun (Node parent) ->
          parent.num_invalid_children <- parent.num_invalid_children + 1)
        n.parents;
      (* Mark parents for checking *)
      List.iter
        (fun parent ->
          (ensure_state ()).propagate_invalidity <-
            parent :: (ensure_state ()).propagate_invalidity)
        n.parents)

  and invalidate_nodes_created_on_rhs nodes_opt =
    let rec loop = function
      | None -> ()
      | Some (Node n) ->
          let next = n.next_in_same_scope in
          n.next_in_same_scope <- None;
          invalidate_node (Node n);
          loop next
    in
    loop nodes_opt

  let add_parent child parent =
    let child_packed = Node child in
    let parent_packed = Node parent in
    if not (List.exists (fun (Node p) -> node_same p parent) child.parents) then
      child.parents <- parent_packed :: child.parents;
    if not (List.exists (fun (Node c) -> node_same c child) parent.children)
    then parent.children <- child_packed :: parent.children

  let remove_parent child parent =
    child.parents <-
      List.filter (fun (Node p) -> not (node_same p parent)) child.parents;
    parent.children <-
      List.filter (fun (Node c) -> not (node_same c child)) parent.children;
    (* If removing an invalid child, decrement the parent's invalid child count *)
    if not (is_valid child) then
      parent.num_invalid_children <- max 0 (parent.num_invalid_children - 1)

  let ensure_height_requirement s ~original_child ~original_parent:_ ~child
      ~parent =
    if node_same parent original_child then raise Cycle_detected;
    if child.height >= parent.height then (
      add_to_adjust_heights_heap s.adjust_heights_heap (Node parent);
      let new_height = child.height + 1 in
      if new_height > s.adjust_heights_heap.max_height_seen then (
        s.adjust_heights_heap.max_height_seen <- new_height;
        if new_height > s.adjust_heights_heap.max_height_allowed then
          raise Cycle_detected);
      parent.height <- new_height)

  let adjust_heights s ~child ~parent =
    let original_child = child in
    let original_parent = parent in
    ensure_height_requirement s ~original_child ~original_parent ~child ~parent;
    while s.adjust_heights_heap.size > 0 do
      let (Node n as child_packed) =
        remove_min_adjust_heights_heap s.adjust_heights_heap
      in
      (* If this node is in the recompute heap and its height increased, update its position *)
      increase_height_in_recompute_heap s.recompute_heap child_packed;
      List.iter
        (fun (Node p) ->
          ensure_height_requirement s ~original_child ~original_parent ~child:n
            ~parent:p)
        n.parents
    done

  let add_dependency s input output =
    add_parent input output;
    if is_necessary output && input.height >= output.height then
      adjust_heights s ~child:input ~parent:output

  let remove_dependency input output = remove_parent input output

  let rec make_necessary s (Node n) =
    if not (is_necessary n) then (
      n.height <- scope_height n.created_in + 1;
      List.iter
        (fun (Node child) ->
          make_necessary s (Node child);
          if child.height >= n.height then n.height <- child.height + 1)
        n.children;
      if is_stale s n then add_to_recompute_heap s.recompute_heap (Node n))

  let rec make_unnecessary (Node n) =
    if is_necessary n && n.parents = [] && n.observers = [] then (
      n.height <- -1;
      List.iter make_unnecessary n.children)

  let rec recompute : type a. state -> a node -> a =
   fun s n ->
    if not (is_valid_in_scope n) then raise Not_stabilized;
    assert (check_kind_consistency n);
    let new_val =
      match n.kind with
      | Invalid -> raise Not_stabilized
      | Const v -> v
      | Var r -> !r
      | Map { f; input } ->
          let v = recompute_val s input in
          f v
      | Map2 { f; input1; input2 } ->
          let v1 = recompute_val s input1 in
          let v2 = recompute_val s input2 in
          f v1 v2
      | Map3 { f; input1; input2; input3 } ->
          let v1 = recompute_val s input1 in
          let v2 = recompute_val s input2 in
          let v3 = recompute_val s input3 in
          f v1 v2 v3
      | Bind_main bind -> (
          match bind.rhs with
          | None -> raise Not_stabilized
          | Some rhs ->
              (* Like incremental's copy_child: if the child is valid, use its value *)
              if is_valid rhs then recompute_val s rhs
              else (
                (* In incremental, copy_child invalidates the parent if child is invalid *)
                invalidate_node (Node n);
                raise Not_stabilized))
      | Bind_lhs_change bind ->
          let old_rhs = bind.rhs in
          let old_all_nodes_created_on_rhs = bind.all_nodes_created_on_rhs in
          let lhs_val = recompute_val s bind.lhs in

          (* Clear all_nodes_created_on_rhs like incremental *)
          bind.all_nodes_created_on_rhs <- None;

          (* Save old scope and switch to bind's scope *)
          let old_scope = s.current_scope in
          s.current_scope <- bind.rhs_scope;

          (* Create new RHS *)
          let new_rhs = bind.f lhs_val in
          bind.rhs <- Some new_rhs;

          (* Restore scope *)
          s.current_scope <- old_scope;

          (* Update dependencies - remove old, add new *)
          (match old_rhs with
          | Some old -> remove_dependency old bind.main
          | None -> ());
          add_dependency s new_rhs bind.main;
          make_necessary s (Node new_rhs);

          (* NOW invalidate old RHS nodes after setting up new ones, like incremental *)
          (match old_rhs with
          | Some _ ->
              invalidate_nodes_created_on_rhs old_all_nodes_created_on_rhs
          | None -> ());
          (* Return () like incremental does *)
          ()
      | If_then_else if_data -> (
          match if_data.current_branch with
          | None -> raise Not_stabilized
          | Some branch -> recompute_val s branch)
      | If_test_change if_data ->
          let test_val = recompute_val s if_data.test in
          let old_branch = if_data.current_branch in
          let new_branch = if test_val then if_data.then_ else if_data.else_ in

          if old_branch <> Some new_branch then (
            (match old_branch with
            | Some old -> remove_dependency old if_data.main
            | None -> ());
            if_data.current_branch <- Some new_branch;
            add_dependency s new_branch if_data.main;
            make_necessary s (Node new_branch));
          ()
      | Join_main join -> (
          match join.rhs with
          | None -> raise Not_stabilized
          | Some rhs -> recompute_val s rhs)
      | Join_lhs_change join ->
          let lhs_val = recompute_val s join.lhs in
          let old_rhs = join.rhs in

          if old_rhs <> Some lhs_val then (
            (match old_rhs with
            | Some old -> remove_dependency old join.main
            | None -> ());
            join.rhs <- Some lhs_val;
            add_dependency s lhs_val join.main;
            make_necessary s (Node lhs_val));
          ()
      | Freeze freeze_data ->
          let child_val = recompute_val s freeze_data.child in
          if freeze_data.only_freeze_when child_val then (
            n.kind <- Const child_val;
            remove_dependency freeze_data.child n;
            if is_necessary n then n.height <- 0;
            make_unnecessary (Node n);
            child_val)
          else child_val
      | At at_data ->
          (* At nodes return Before or After based on current time vs target time *)
          (* The actual time check happens during advance_clock when alarms fire *)
          (match n.value with
          | Some v -> v
          | None -> Before at_data.at)
      | At_intervals _at_intervals_data ->
          (* At_intervals fires at regular intervals, returns unit *)
          ()
      | Snapshot snapshot_data ->
          (* Snapshot nodes return the stored 'before' value until frozen *)
          snapshot_data.before
      | Step_function step_data ->
          (* Step function returns init value or value from last step *)
          if step_data.current_index = 0 then step_data.init
          else if step_data.current_index > 0 && step_data.current_index <= Array.length step_data.steps then
            snd step_data.steps.(step_data.current_index - 1)
          else if Array.length step_data.steps > 0 then
            snd step_data.steps.(Array.length step_data.steps - 1)
          else step_data.init
    in

    n.recomputed_at <- s.stabilization_num;

    let old_value = n.value in
    (* Capture old value before updating *)

    let should_propagate =
      match old_value with
      | None -> true
      | Some old_val -> not (n.cutoff ~old:old_val ~new_:new_val)
    in

    n.value <- Some new_val;

    if should_propagate then (
      n.changed_at <- s.stabilization_num;
      List.iter
        (fun (Node p) ->
          if (not p.in_recompute_heap) && is_necessary p then
            add_to_recompute_heap s.recompute_heap (Node p))
        n.parents;
      (* Collect updates to handle after stabilization *)
      if n.observers <> [] then
        let update =
          Pending_update
            {
              update_node = n;
              update_old_value = old_value;
              update_new_value = new_val;
            }
        in
        s.updates_to_handle <- update :: s.updates_to_handle);

    new_val

  and recompute_val : type a. state -> a node -> a =
   fun s n ->
    if is_stale s n then recompute s n
    else match n.value with Some v -> v | None -> raise Not_stabilized

  and propagate_invalidity s =
    let rec process_nodes nodes =
      match nodes with
      | [] -> ()
      | Node n :: rest ->
          if is_valid n then
            if should_be_invalidated n then invalidate_node (Node n)
            else if is_necessary n && not n.in_recompute_heap then
              add_to_recompute_heap s.recompute_heap (Node n);
          process_nodes rest
    in
    let nodes = s.propagate_invalidity in
    s.propagate_invalidity <- [];
    process_nodes nodes

  module Var = struct
    type 'a t = 'a var

    let create v =
      let watch = create_node (Var (ref v)) in
      watch.value <- Some v;
      { value = v; value_set_during_stabilization = None; watch }

    let set t v =
      let s = ensure_state () in
      if s.status = Stabilizing then (
        (* During stabilization, defer the update *)
        if t.value_set_during_stabilization = None then
          s.vars_set_during_stabilization <-
            Var_packed t :: s.vars_set_during_stabilization;
        t.value_set_during_stabilization <- Some v)
      else (
        t.value <- v;
        (match t.watch.kind with Var r -> r := v | _ -> assert false);
        (* Mark as changed and add to recompute heap *)
        t.watch.changed_at <- s.stabilization_num;
        if is_necessary t.watch && not t.watch.in_recompute_heap then
          add_to_recompute_heap s.recompute_heap (Node t.watch))

    let value t = t.value
    let watch t = t.watch
  end

  (* Clock module - needs to come after Var *)
  module Clock = struct
    type t = clock

    let global_clock = ref None
    
    let get_clock () =
      match !global_clock with
      | Some clock -> clock
      | None ->
          let s = ensure_state () in
          (match s.clock with
          | Some clock -> 
              global_clock := Some clock;
              clock
          | None ->
              let timing_wheel = Timing_wheel.create ~alarm_precision:(Time.Span.of_sec 0.001) ~start:(Time.now ()) in
              let now_var = Var.create (Time.now ()) in
              let rec clock = {
                timing_wheel;
                now = now_var;
                handle_fired;
                fired_alarm_values = None;
              }
              and handle_fired (alarm_value : alarm_value) =
                alarm_value.next_fired <- clock.fired_alarm_values;
                clock.fired_alarm_values <- Some alarm_value
              in
              s.clock <- Some clock;
              global_clock := Some clock;
              clock)
    
    let now () = 
      let clock = get_clock () in
      clock.now.value
    
    let advance_clock ~to_ =
      let clock = get_clock () in
      let s = ensure_state () in
      
      (* First pass: collect fired alarms *)
      Timing_wheel.advance_clock clock.timing_wheel ~to_ ~handle_fired:clock.handle_fired;
      
      (* Update the now var *)
      Var.set clock.now to_;
      
      (* Second pass: handle fired alarms *)
      let rec handle_fired_alarms () =
        match clock.fired_alarm_values with
        | None -> ()
        | Some alarm_value ->
            clock.fired_alarm_values <- alarm_value.next_fired;
            alarm_value.next_fired <- None;
            (match alarm_value.action with
            | At at_data ->
                at_data.main.value <- Some (After at_data.at);
                at_data.main.changed_at <- s.stabilization_num;
                invalidate_node (Node at_data.main);
                if is_necessary at_data.main && not at_data.main.in_recompute_heap then
                  add_to_recompute_heap s.recompute_heap (Node at_data.main)
            | At_intervals at_intervals_data ->
                at_intervals_data.main.value <- Some ();
                at_intervals_data.main.changed_at <- s.stabilization_num;
                invalidate_node (Node at_intervals_data.main);
                if is_necessary at_intervals_data.main && not at_intervals_data.main.in_recompute_heap then
                  add_to_recompute_heap s.recompute_heap (Node at_intervals_data.main);
                (* Schedule next alarm *)
                let next_time = Time.next_multiple ~base:at_intervals_data.base ~after:to_ ~interval:at_intervals_data.interval in
                let next_alarm_value = { action = At_intervals at_intervals_data; next_fired = None } in
                at_intervals_data.alarm <- Timing_wheel.add clock.timing_wheel ~at:next_time next_alarm_value
            | Snapshot snapshot_data ->
                (* Update snapshot value *)
                (match snapshot_data.value_at.value with
                | Some v -> 
                    snapshot_data.main.value <- Some snapshot_data.before;
                    snapshot_data.main.changed_at <- s.stabilization_num;
                    invalidate_node (Node snapshot_data.main);
                    if is_necessary snapshot_data.main && not snapshot_data.main.in_recompute_heap then
                      add_to_recompute_heap s.recompute_heap (Node snapshot_data.main)
                | None -> ())
            | Step_function step_function_data ->
                (* Advance to next step *)
                let rec find_next_step idx =
                  if idx >= Array.length step_function_data.steps then
                    (* No more steps *)
                    ()
                  else
                    let (step_time, step_value) = step_function_data.steps.(idx) in
                    if Time.(step_time <= to_) then (
                      step_function_data.main.value <- Some step_value;
                      step_function_data.current_index <- idx + 1;
                      find_next_step (idx + 1)
                    ) else (
                      (* Schedule alarm for next step *)
                      let next_alarm_value = { action = Step_function step_function_data; next_fired = None } in
                      step_function_data.alarm <- Timing_wheel.add clock.timing_wheel ~at:step_time next_alarm_value
                    )
                in
                find_next_step step_function_data.current_index;
                if step_function_data.main.value <> None then (
                  step_function_data.main.changed_at <- s.stabilization_num;
                  invalidate_node (Node step_function_data.main);
                  if is_necessary step_function_data.main && not step_function_data.main.in_recompute_heap then
                    add_to_recompute_heap s.recompute_heap (Node step_function_data.main)
                )
            );
            handle_fired_alarms ()
      in
      handle_fired_alarms ()
  end

  module Observer = struct
    type 'a t = 'a observer

    module Update = struct
      type 'a t = 'a update =
        | Initialized of 'a
        | Changed of 'a * 'a
        | Invalidated
    end

    let observing t = t.node

    let value_exn t =
      if not t.active then raise Invalid_observer;
      match t.node.value with Some v -> v | None -> raise Not_stabilized

    let on_update_exn t ~f = t.on_update <- f :: t.on_update

    let disallow_future_use t =
      let _ = ensure_state () in
      t.active <- false;
      t.node.observers <- List.filter (fun o -> o != t) t.node.observers;
      make_unnecessary (Node t.node)
  end

  let observe incr =
    let s = ensure_state () in
    let obs =
      { node = incr; on_update = []; active = true; old_value = incr.value }
    in
    incr.observers <- obs :: incr.observers;
    make_necessary s (Node incr);
    obs

  let const v =
    let node = create_node (Const v) in
    node.value <- Some v;
    node

  let map a ~f =
    let node = create_node (Map { f; input = a }) in
    let s = ensure_state () in
    add_dependency s a node;
    node

  let map2 a b ~f =
    let node = create_node (Map2 { f; input1 = a; input2 = b }) in
    let s = ensure_state () in
    add_dependency s a node;
    add_dependency s b node;
    node

  let map3 a b c ~f =
    let node = create_node (Map3 { f; input1 = a; input2 = b; input3 = c }) in
    let s = ensure_state () in
    add_dependency s a node;
    add_dependency s b node;
    add_dependency s c node;
    node

  let bind (type a b) (lhs : a node) ~(f : a -> b node) : b node =
    let s = ensure_state () in
    let main = create_node Invalid in
    let lhs_change = create_node Invalid in
    let bind_data : (a, b) bind_data =
      {
        main;
        f;
        lhs;
        lhs_change;
        rhs = None;
        rhs_scope = Top;
        (* Initially Top, will be set below *)
        all_nodes_created_on_rhs = None;
      }
    in
    bind_data.rhs_scope <- Bind (Bind_any bind_data);
    (* Set after creation *)
    main.kind <- Bind_main bind_data;
    lhs_change.kind <- Bind_lhs_change bind_data;
    (* Never cutoff - always propagate when LHS changes *)
    lhs_change.cutoff <- (fun ~old:_ ~new_:_ -> false);
    add_dependency s lhs lhs_change;
    add_dependency s lhs_change main;
    main

  let if_ test ~then_ ~else_ =
    let s = ensure_state () in
    let main = create_node Invalid in
    let test_change = create_node Invalid in
    let if_data =
      { main; test; test_change; current_branch = None; then_; else_ }
    in
    main.kind <- If_then_else if_data;
    test_change.kind <- If_test_change if_data;
    test_change.cutoff <- (fun ~old:_ ~new_:_ -> false);
    add_dependency s test test_change;
    add_dependency s test_change main;
    main

  let join lhs =
    let s = ensure_state () in
    let main = create_node Invalid in
    let lhs_change = create_node Invalid in
    let join_data = { main; lhs; lhs_change; rhs = None } in
    main.kind <- Join_main join_data;
    lhs_change.kind <- Join_lhs_change join_data;
    lhs_change.cutoff <- (fun ~old:_ ~new_:_ -> false);
    add_dependency s lhs lhs_change;
    add_dependency s lhs_change main;
    main

  let freeze ?(when_ = fun _ -> true) child =
    let main = create_node Invalid in
    let freeze_data = { main; child; only_freeze_when = when_ } in
    main.kind <- Freeze freeze_data;
    let s = ensure_state () in
    add_dependency s child main;
    main

  module Infix = struct
    let ( >>| ) x f = map x ~f
    let ( >>= ) x f = bind x ~f
  end

  module Let_syntax = struct
    let return = const
    let map = map
    let bind = bind
    let both a b = map2 a b ~f:(fun x y -> (x, y))
    let map2 = map2
  end

  module Cutoff = struct
    type 'a t = old:'a -> new_:'a -> bool

    let create f = f
    let phys_equal ~old ~new_ = old == new_
    let never ~old:_ ~new_:_ = false
    let always ~old:_ ~new_:_ = true
  end

  let set_cutoff incr cutoff = incr.cutoff <- cutoff
  let get_cutoff incr = incr.cutoff

  let process_vars_set_during_stabilization s =
    let vars = s.vars_set_during_stabilization in
    s.vars_set_during_stabilization <- [];
    List.iter
      (fun (Var_packed var) ->
        match var.value_set_during_stabilization with
        | None -> ()
        | Some v ->
            var.value_set_during_stabilization <- None;
            Var.set var v)
      vars

  let stabilize () =
    let s = ensure_state () in
    match s.status with
    | Stabilize_previously_raised e -> raise e
    | Stabilizing -> failwith "Already stabilizing"
    | Not_stabilizing -> (
        try
          s.status <- Stabilizing;
          s.stabilization_num <- Stabilization_num.add1 s.stabilization_num;
          s.updates_to_handle <- [];

          (* Clear before starting *)

          (* First propagate any invalidations *)
          propagate_invalidity s;

          (* Then recompute stale nodes *)
          while s.recompute_heap.size > 0 do
            let (Node n) = remove_min_recompute_heap s.recompute_heap in
            if is_valid n then ignore (recompute s n : _);
            propagate_invalidity s
          done;

          (* New Phase: Run on-update handlers *)
          let updates = List.rev s.updates_to_handle in
          s.updates_to_handle <- [];
          List.iter
            (fun (Pending_update u) ->
              List.iter
                (fun obs ->
                  if obs.active then
                    let update =
                      match u.update_old_value with
                      | None -> Initialized u.update_new_value
                      | Some old ->
                          obs.old_value <- Some u.update_new_value;
                          Changed (old, u.update_new_value)
                    in
                    List.iter (fun f -> f update) obs.on_update)
                u.update_node.observers)
            updates;

          s.status <- Not_stabilizing;

          (* Process any vars that were set during stabilization *)
          process_vars_set_during_stabilization s
        with e ->
          s.status <- Stabilize_previously_raised e;
          raise e)

  let is_stabilizing () =
    let s = ensure_state () in
    match s.status with Stabilizing -> true | _ -> false

  (* Clock API for time-based incremental nodes *)
  module Clock_api = struct
    let at time =
      let _s = ensure_state () in
      (* Create a dummy node first *)
      let dummy_node = create_node Invalid in
      let at_data = { at = time; alarm = None } in
      dummy_node.kind <- At at_data;
      
      (* Schedule alarm if timing wheel is initialized *)
      (match (!global_state) with
      | Some s ->
          (match s.timing_wheel with
          | Some wheel ->
              let alarm = Timing_wheel.add wheel ~at:time (Node dummy_node) in
              at_data.alarm <- Some alarm
          | None -> ())
      | None -> ());
      
      dummy_node

    let after span =
      let time = Time.(Clock.now () + span) in
      at time

    let at_intervals ~base ~interval =
      let _s = ensure_state () in
      (* Create a dummy node first *)
      let dummy_node = create_node Invalid in
      let at_intervals_data = { base; interval; alarm = None } in
      dummy_node.kind <- At_intervals at_intervals_data;
      
      (* Schedule initial alarm *)
      let first_fire = Time.next_multiple ~base ~after:(Clock.now ()) ~interval in
      (match (!global_state) with
      | Some s ->
          (match s.timing_wheel with
          | Some wheel ->
              let alarm = Timing_wheel.add wheel ~at:first_fire (Node dummy_node) in
              at_intervals_data.alarm <- Some alarm
          | None -> ())
      | None -> ());
      
      dummy_node

    let snapshot at_incr ~at ~f =
      let s = ensure_state () in
      (* Create a dummy node first *)
      let dummy_node = create_node Invalid in
      let snapshot_data = { at; value_at = at_incr; f } in
      dummy_node.kind <- Snapshot snapshot_data;
      add_dependency s at dummy_node;
      add_dependency s at_incr dummy_node;
      dummy_node

    let step_function ~init steps =
      let _s = ensure_state () in
      let steps_array = Array.of_list steps in
      (* Create a dummy node first *)  
      let dummy_node = create_node Invalid in
      let step_data = { init; steps = steps_array; current_index = 0 } in
      dummy_node.kind <- Step_function step_data;
      
      (* Schedule alarm for first step if timing wheel exists *)
      if Array.length steps_array > 0 then (
        let (first_time, _) = steps_array.(0) in
        match (!global_state) with
        | Some s ->
            (match s.timing_wheel with
            | Some wheel ->
                ignore (Timing_wheel.add wheel ~at:first_time (Node dummy_node))
            | None -> ())
        | None -> ()
      );
      
      dummy_node

    let advance_clock ~to_ =
      Clock.advance_clock ~to_;
      let s = ensure_state () in
      
      (* Initialize timing wheel if needed *)
      let wheel =
        match s.timing_wheel with
        | Some w -> w
        | None ->
            let w = Timing_wheel.create ~alarm_precision:(Time.Span.of_sec 0.001) ~start:(Clock.now ()) in
            s.timing_wheel <- Some w;
            w
      in
      
      (* Advance timing wheel and fire alarms *)
      Timing_wheel.advance_clock wheel ~to_ ~handle_fired:(fun alarm_value ->
        let (Node n) = alarm_value.Timing_wheel.value in
        invalidate_node (Node n);
        s.propagate_invalidity <- (Node n) :: s.propagate_invalidity;
        
        (* Re-schedule for periodic alarms *)
        match n.kind with
        | At_intervals at_intervals_data ->
            let next_fire = Time.next_multiple ~base:at_intervals_data.base ~after:to_ ~interval:at_intervals_data.interval in
            let alarm = Timing_wheel.add wheel ~at:next_fire (Node n) in
            at_intervals_data.alarm <- Some alarm
        | Step_function step_data ->
            (* Find next step to schedule *)
            let rec find_next idx =
              if idx < Array.length step_data.steps then
                let (step_time, _) = step_data.steps.(idx) in
                if Time.(step_time > to_) then (
                  step_data.current_index <- idx;
                  ignore (Timing_wheel.add wheel ~at:step_time (Node n))
                ) else find_next (idx + 1)
            in
            find_next step_data.current_index
        | _ -> ()
      );
      
      (* Trigger stabilization to propagate changes *)
      stabilize ()
  end

  let at = Clock_api.at
  let after = Clock_api.after
  let at_intervals = Clock_api.at_intervals
  let snapshot = Clock_api.snapshot
  let step_function = Clock_api.step_function
  let advance_clock = Clock_api.advance_clock
end
