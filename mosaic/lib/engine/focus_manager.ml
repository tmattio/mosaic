(** Focus manager implementation *)

type focusable = {
  key : Ui.Attr.key;
  tab_index : int option;
  auto_focus : bool;
  order : int; (* insertion order *)
}

type t = {
  mutable focusables : focusable list;
  mutable focused : Ui.Attr.key option;
  mutable router : Input_router.t option;
  mutable next_order : int; (* counter for insertion order *)
}

let create () =
  { focusables = []; focused = None; router = None; next_order = 0 }

let set_router t router = t.router <- Some router

let register t focusable =
  (* Preserve previous order if re-registering; else assign new order *)
  let prev = List.find_opt (fun f -> f.key = focusable.key) t.focusables in
  let order =
    match prev with
    | Some f -> f.order
    | None ->
        let o = t.next_order in
        t.next_order <- o + 1;
        o
  in
  let focusable = { focusable with order } in
  (* Remove if already registered to avoid duplicates *)
  t.focusables <- List.filter (fun f -> f.key <> focusable.key) t.focusables;
  t.focusables <- focusable :: t.focusables;

  (* Auto-focus if requested and nothing focused yet *)
  if focusable.auto_focus && t.focused = None then
    t.focused <- Some focusable.key

let unregister t key =
  t.focusables <- List.filter (fun f -> f.key <> key) t.focusables;
  if t.focused = Some key then t.focused <- None

let get_tab_order t =
  (* Sort by (has_index, tab_index, insertion_order) for stable ordering *)
  let rank = function Some i -> (0, i) | None -> (1, max_int) in
  let cmp a b =
    match compare (rank a.tab_index) (rank b.tab_index) with
    | 0 -> compare a.order b.order
    | c -> c
  in
  t.focusables |> List.sort cmp |> List.map (fun f -> f.key)

let focus t key =
  (* Check if the key is registered as focusable *)
  if List.exists (fun f -> f.key = key) t.focusables then (
    t.focused <- Some key;
    (* Notify router if connected *)
    Option.iter (fun r -> Input_router.set_focused r (Some key)) t.router)

let blur t =
  t.focused <- None;
  (* Notify router if connected *)
  Option.iter (fun r -> Input_router.set_focused r None) t.router

let get_focused t = t.focused

let focus_first t =
  match get_tab_order t with [] -> () | key :: _ -> focus t key

let focus_last t =
  match List.rev (get_tab_order t) with [] -> () | key :: _ -> focus t key

let focus_next t =
  let order = get_tab_order t in
  match (t.focused, order) with
  | None, [] -> ()
  | None, key :: _ -> focus t key
  | Some current, _ ->
      let rec find_next = function
        | [] -> focus_first t (* Wrap around *)
        | [ x ] when x = current -> focus_first t (* Last element, wrap *)
        | x :: y :: _ when x = current -> focus t y
        | _ :: rest -> find_next rest
      in
      find_next order

let focus_prev t =
  let order = get_tab_order t in
  match (t.focused, order) with
  | None, [] -> ()
  | None, _ -> focus_last t
  | Some current, _ ->
      let rec find_prev prev = function
        | [] -> focus_last t (* Wrap around *)
        | x :: _ when x = current -> (
            match prev with
            | Some p -> focus t p
            | None -> focus_last t (* First element, wrap to last *))
        | x :: rest -> find_prev (Some x) rest
      in
      find_prev None order

let handle_tab t ~forward = if forward then focus_next t else focus_prev t

let clear t =
  t.focusables <- [];
  t.focused <- None
