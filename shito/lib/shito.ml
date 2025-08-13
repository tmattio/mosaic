module type S = sig
  module Incr : sig
    type 'a t
  end

  type 'a t = 'a Incr.t

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
    mutable height_in_adjust_heights_heap : int;
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

  and 'a observer = {
    node : 'a node;
    mutable on_update : ('a update -> unit) list;
    mutable active : bool;
    mutable old_value : 'a option;
  }

  and 'a update = Initialized of 'a | Changed of 'a * 'a | Invalidated

  module Incr = struct
    type 'a t = 'a node
  end

  type 'a t = 'a Incr.t

  (* Heaps for stabilization *)
  type recompute_heap = {
    data : packed_node list array;
    mutable min_height : int;
    mutable size : int;
    max_height : int;
  }

  type adjust_heights_heap = {
    data : packed_node list array;
    mutable min_height : int;
    mutable size : int;
    mutable max_height_seen : int;
    max_height_allowed : int;
  }

  type state = {
    mutable stabilization_num : stabilization_num;
    mutable is_stabilizing : bool;
    recompute_heap : recompute_heap;
    adjust_heights_heap : adjust_heights_heap;
    mutable current_scope : scope;
    mutable propagate_invalidity : packed_node list;
  }

  let global_state = ref None

  let ensure_state () =
    match !global_state with
    | Some s -> s
    | None ->
        let s =
          {
            stabilization_num = 0;
            is_stabilizing = false;
            recompute_heap =
              {
                data = Array.make 129 [];
                min_height = 129;
                size = 0;
                max_height = 128;
              };
            adjust_heights_heap =
              {
                data = Array.make 129 [];
                min_height = 129;
                size = 0;
                max_height_seen = 0;
                max_height_allowed = 128;
              };
            current_scope = Top;
            propagate_invalidity = [];
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
      heap.data.(height) <- packed :: heap.data.(height);
      heap.size <- heap.size + 1;
      if height < heap.min_height then heap.min_height <- height)

  let remove_min_recompute_heap (heap : recompute_heap) =
    if heap.size = 0 then failwith "Empty recompute heap";
    while
      heap.min_height <= heap.max_height && heap.data.(heap.min_height) = []
    do
      heap.min_height <- heap.min_height + 1
    done;
    if heap.min_height > heap.max_height then failwith "Empty recompute heap";
    match heap.data.(heap.min_height) with
    | Node n :: tl ->
        heap.data.(heap.min_height) <- tl;
        heap.size <- heap.size - 1;
        n.in_recompute_heap <- false;
        n.height_in_recompute_heap <- -1;
        Node n
    | [] -> assert false

  let add_to_adjust_heights_heap heap (Node n as packed) =
    if n.height_in_adjust_heights_heap = -1 then (
      let height = n.height in
      n.height_in_adjust_heights_heap <- height;
      heap.data.(height) <- packed :: heap.data.(height);
      heap.size <- heap.size + 1;
      if height < heap.min_height then heap.min_height <- height)

  let remove_min_adjust_heights_heap (heap : adjust_heights_heap) =
    if heap.size = 0 then failwith "Empty adjust heights heap";
    while
      heap.min_height <= heap.max_height_allowed
      && heap.data.(heap.min_height) = []
    do
      heap.min_height <- heap.min_height + 1
    done;
    if heap.min_height > heap.max_height_allowed then
      failwith "Empty adjust heights heap";
    match heap.data.(heap.min_height) with
    | Node n :: tl ->
        heap.data.(heap.min_height) <- tl;
        heap.size <- heap.size - 1;
        n.height_in_adjust_heights_heap <- -1;
        Node n
    | [] -> assert false

  let increase_height_in_recompute_heap (recompute_heap : recompute_heap)
      (Node n) =
    if n.in_recompute_heap && n.height > n.height_in_recompute_heap then (
      (* Remove from old position *)
      let old_height = n.height_in_recompute_heap in
      let node_packed = Node n in
      recompute_heap.data.(old_height) <-
        List.filter
          (fun packed -> packed != node_packed)
          recompute_heap.data.(old_height);
      (* Add to new position *)
      let new_height = n.height in
      n.height_in_recompute_heap <- new_height;
      recompute_heap.data.(new_height) <-
        node_packed :: recompute_heap.data.(new_height))

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
        height_in_adjust_heights_heap = -1;
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
    if not (List.exists (fun p -> p == parent_packed) child.parents) then
      child.parents <- parent_packed :: child.parents;
    if not (List.exists (fun c -> c == child_packed) parent.children) then
      parent.children <- child_packed :: parent.children

  let remove_parent child parent =
    let child_packed = Node child in
    let parent_packed = Node parent in
    child.parents <- List.filter (fun p -> p != parent_packed) child.parents;
    parent.children <- List.filter (fun c -> c != child_packed) parent.children

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
          | Some rhs -> recompute_val s rhs)
      | Bind_lhs_change bind ->
          let old_rhs = bind.rhs in
          let lhs_val = recompute_val s bind.lhs in

          (* Save old scope and switch to bind's scope *)
          let old_scope = s.current_scope in
          s.current_scope <- bind.rhs_scope;

          (* Invalidate old RHS nodes if needed *)
          (match old_rhs with
          | Some _ ->
              invalidate_nodes_created_on_rhs bind.all_nodes_created_on_rhs
          | None -> ());
          bind.all_nodes_created_on_rhs <- None;

          (* Create new RHS *)
          let new_rhs = bind.f lhs_val in
          bind.rhs <- Some new_rhs;

          (* Restore scope *)
          s.current_scope <- old_scope;

          (* Update main node dependencies *)
          (match old_rhs with
          | Some old -> remove_dependency old bind.main
          | None -> ());
          add_dependency s new_rhs bind.main;
          make_necessary s (Node new_rhs);
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
    in

    n.recomputed_at <- s.stabilization_num;

    let should_propagate =
      match n.value with
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
      List.iter
        (fun obs ->
          if obs.active then
            let update =
              match obs.old_value with
              | None -> Initialized new_val
              | Some old ->
                  obs.old_value <- Some new_val;
                  Changed (old, new_val)
            in
            List.iter (fun f -> f update) obs.on_update)
        n.observers);

    new_val

  and recompute_val : type a. state -> a node -> a =
   fun s n ->
    if is_stale s n then recompute s n
    else match n.value with Some v -> v | None -> raise Not_stabilized

  let propagate_invalidity s =
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
    type 'a t = { value : 'a ref; node : 'a node }

    let create v =
      let node = create_node (Var (ref v)) in
      node.value <- Some v;
      { value = ref v; node }

    let set t v =
      let s = ensure_state () in
      t.value := v;
      (match t.node.kind with Var r -> r := v | _ -> assert false);
      t.node.changed_at <- s.stabilization_num;
      if is_necessary t.node && not t.node.in_recompute_heap then
        add_to_recompute_heap s.recompute_heap (Node t.node)

    let value t = !(t.value)
    let watch t = t.node
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
    lhs_change.cutoff <- (fun ~old:_ ~new_:_ -> false);
    (* Never cutoff *)
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

  let stabilize () =
    let s = ensure_state () in
    if s.is_stabilizing then failwith "Already stabilizing";
    s.is_stabilizing <- true;
    s.stabilization_num <- Stabilization_num.add1 s.stabilization_num;

    (* First propagate any invalidations *)
    propagate_invalidity s;

    (* Then recompute stale nodes *)
    while s.recompute_heap.size > 0 do
      let (Node n) = remove_min_recompute_heap s.recompute_heap in
      if is_stale s n then ignore (recompute s n : _);
      propagate_invalidity s
    done;

    s.is_stabilizing <- false

  let is_stabilizing () =
    let s = ensure_state () in
    s.is_stabilizing
end
