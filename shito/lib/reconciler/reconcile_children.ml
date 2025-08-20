module type NODE = sig
  type t

  val key : t -> Key.t
  val same_kind : t -> t -> bool
  val kind_id : t -> int
end

module Make (N : NODE) = struct
  type node = N.t
  type reuse = int option array
  type needs_placement = bool array
  type deletes = int list

  type result = {
    reuse : reuse;
    needs_placement : needs_placement;
    deletes : deletes;
  }

  module IntTbl = Hashtbl.Make (struct
    type t = int

    let equal (a : int) (b : int) = a = b
    let hash = Hashtbl.hash
  end)

  let diff ~old_nodes ~new_nodes =
    let old_len = Array.length old_nodes in
    let new_len = Array.length new_nodes in

    (* Track whether an old index has been reused. *)
    let used_old = Array.make old_len false in

    (* Reuse mapping: new index -> old index *)
    let reuse : reuse = Array.make new_len None in

    (* Placement plan *)
    let needs_placement : needs_placement = Array.make new_len false in

    (* Build maps over the old children for O(1) lookups. *)
    let keyed_map : (string, int) Hashtbl.t =
      Hashtbl.create (max 8 ((old_len / 2) + 1))
    in
    let unkeyed_buckets : int list ref IntTbl.t =
      IntTbl.create (max 8 ((old_len / 2) + 1))
    in

    (* Fill maps: for keyed nodes, map key -> index; for unkeyed, bucket per kind_id. *)
    for j = 0 to old_len - 1 do
      match N.key old_nodes.(j) with
      | Some k ->
          (* We assume sibling keys are unique; if duplicates occur, last wins. *)
          Hashtbl.replace keyed_map k j
      | None ->
          let kid = N.kind_id old_nodes.(j) in
          let bucket =
            match IntTbl.find_opt unkeyed_buckets kid with
            | Some r -> r
            | None ->
                let r = ref [] in
                IntTbl.add unkeyed_buckets kid r;
                r
          in
          (* Keep indices in ascending order to preserve match order. *)
          bucket := !bucket @ [ j ]
    done;

    (* Helper: pop next available old index from an unkeyed bucket, skipping any already used. *)
    let pop_unkeyed kid new_node =
      match IntTbl.find_opt unkeyed_buckets kid with
      | None -> None
      | Some r ->
          let rec loop () =
            match !r with
            | [] -> None
            | j :: rest ->
                r := rest;
                if used_old.(j) then loop ()
                else
                  let old_node = old_nodes.(j) in
                  if N.same_kind old_node new_node then Some j else loop ()
          in
          loop ()
    in

    (* Second pass: walk new nodes, match to old by key first, otherwise by kind bucket. *)
    let last_placed_index = ref (-1) in
    for i = 0 to new_len - 1 do
      let new_node = new_nodes.(i) in
      let match_old_index =
        match N.key new_node with
        | Some k -> (
            (* Try keyed match *)
            match Hashtbl.find_opt keyed_map k with
            | Some j
              when (not used_old.(j)) && N.same_kind old_nodes.(j) new_node ->
                Some j
            | _ -> None)
        | None ->
            (* Try unkeyed bucket by kind *)
            let kid = N.kind_id new_node in
            pop_unkeyed kid new_node
      in
      match match_old_index with
      | None ->
          (* Fresh insert *)
          reuse.(i) <- None;
          needs_placement.(i) <- true
      | Some j ->
          reuse.(i) <- Some j;
          used_old.(j) <- true;
          (* Move check: if we reuse an old index that appears before the last placed index,
             this child must be moved. Otherwise it's already in order. *)
          if j < !last_placed_index then needs_placement.(i) <- true
          else (
            needs_placement.(i) <- false;
            if j > !last_placed_index then last_placed_index := j)
    done;

    (* Any old index not used must be deleted. *)
    let deletes =
      let acc = ref [] in
      for j = 0 to old_len - 1 do
        if not used_old.(j) then acc := j :: !acc
      done;
      List.rev !acc
    in

    { reuse; needs_placement; deletes }

  let diff_list ~old_nodes ~new_nodes =
    diff ~old_nodes:(Array.of_list old_nodes)
      ~new_nodes:(Array.of_list new_nodes)
end
