
(* Internal storage & utilities *)

module StringMap = Map.Make (String)

let next_id =
  let r = ref 0 in
  fun () -> incr r; !r

(* Deterministic, append-only mutation log for tests *)
type id = int

(* Define the op type first *)
type op =
  | Prepare_for_commit of { container : id }
  | Reset_after_commit  of { container : id }
  | Clear_container     of { container : id }
  | Create_instance     of { id : id; tag : string; attrs : (string * string) list }
  | Create_text         of { id : id; text : string }
  | Append_initial_child of { parent : id; child : id }
  | Finalize_initial_children of { id : id }
  | Append_child        of { parent : id; child : id }
  | Insert_before       of { parent : id; child : id; before : id }
  | Remove_child        of { parent : id; child : id }
  | Append_child_to_container of { container : id; child : id }
  | Insert_before_in_container of { container : id; child : id; before : id }
  | Remove_child_from_container of { container : id; child : id }
  | Commit_update       of { id : id; diff : (string * [ `Set of string | `Remove ]) list }
  | Commit_text_update  of { id : id; text : string }
  | Commit_mount        of { id : id }
  | Detach_deleted      of { id : id }

let log : op list ref = ref []

let push (e : op) = log := e :: !log

(* Host implementation *)

module Host = struct
  type primitive = string

  (* Props are string attributes; last write wins *)
  type props = (string * string) list

  (* Public instance handed to refs/tests; here it's just the integer id. *)
  type public_instance = int

  (* Opaque host objects *)
  type instance = {
    id       : int;
    tag      : string;
    mutable attrs : string StringMap.t;   (* normalized for diffs *)
    mutable children : node list;
    mutable parent   : parent;
  }

  and text_instance = {
    id     : int;
    mutable text : string;
    mutable parent : parent;
  }

  and node =
    | El of instance
    | Tx of text_instance

  and container = {
    id       : int;
    name     : string;  [@warning "-69"]  (* name is exposed for debugging/testing *)
    mutable children : node list;
  }

  and parent =
    | PContainer of container
    | PInstance  of instance
    | PNone

  (* Host-specific prepared update payload: list of key -> Set/Remove *)
  type update = (string * [ `Set of string | `Remove ]) list

  let equal_primitive = String.equal

  (* ---- Props helpers ---- *)

  let map_of_props (ps : props) : string StringMap.t =
    List.fold_left (fun acc (k,v) -> StringMap.add k v acc) StringMap.empty ps

  let props_of_map (m : string StringMap.t) : props =
    StringMap.bindings m

  let compute_diff ~(old_m:string StringMap.t) ~(new_m:string StringMap.t) : update =
    (* keys to set: present with different value or new *)
    let sets =
      StringMap.merge
        (fun _ o n ->
           match o, n with
           | _, None -> None
           | None, Some v -> Some (`Set v)
           | Some vo, Some vn when String.equal vo vn -> None
           | Some _, Some vn -> Some (`Set vn))
        old_m new_m
      |> StringMap.bindings
    in
    (* keys to remove: present old, absent new *)
    let removes =
      StringMap.merge
        (fun _k o n -> match o, n with
           | Some _, None -> Some (`Remove)
           | _ -> None)
        old_m new_m
      |> StringMap.bindings
      |> List.map (fun (k, `Remove) -> (k, `Remove))
    in
    (* Maintain deterministic ordering by key *)
    let cmp (a,_) (b,_) = Stdlib.compare a b in
    List.stable_sort cmp (sets @ removes)

  (* ---- Logging helpers ---- *)

  let id_of_node = function
    | El i -> i.id
    | Tx t -> t.id

  let _parent_id = function
    | PContainer c -> c.id
    | PInstance  i -> i.id
    | PNone -> -1

  (* ---- Creation ---- *)

  let create_instance ~primitive ~props ~(container : container) =
    let _ = container in  (* container might be used by the host for creation context *)
    let id = next_id () in
    let inst = {
      id; tag = primitive;
      attrs = map_of_props props;
      children = [];
      parent = PNone;
    } in
    push (Create_instance { id; tag = primitive; attrs = props_of_map inst.attrs });
    inst

  let create_text_instance ~text ~(container : container) =
    let _ = container in  (* container might be used by the host for creation context *)
    let id = next_id () in
    let ti = { id; text; parent = PNone } in
    push (Create_text { id; text });
    ti

  (* ---- Initial children ---- *)

  let append_initial_child ~parent ~child =
    let child_node = match child with
      | Reconciler.Either.Left i -> El i
      | Reconciler.Either.Right t -> Tx t
    in
    let child_parent =
      match child_node with El i -> i.parent | Tx t -> t.parent
    in
    (match child_parent with
     | PNone -> ()
     | PContainer c ->
         c.children <- List.filter (fun n -> id_of_node n <> id_of_node child_node) c.children
     | PInstance p ->
         p.children <- List.filter (fun n -> id_of_node n <> id_of_node child_node) p.children);
    (match child_node with
     | El i ->
         i.parent <- PInstance parent;
         parent.children <- parent.children @ [ child_node ]
     | Tx t ->
         t.parent <- PInstance parent;
         parent.children <- parent.children @ [ child_node ]);
    push (Append_initial_child { parent = parent.id; child = id_of_node child_node })

  let finalize_initial_children ~instance ~props:_ =
    push (Finalize_initial_children { id = instance.id })

  (* ---- Commit batching ---- *)

  let prepare_for_commit ~(container : container) =
    push (Prepare_for_commit { container = container.id })

  let reset_after_commit ~(container : container) =
    push (Reset_after_commit { container = container.id })

  (* ---- Updates ---- *)

  let prepare_update ~instance ~old_props:_ ~new_props =
    let old_m = instance.attrs in
    let new_m = map_of_props new_props in
    let diff = compute_diff ~old_m ~new_m in
    if diff = [] then None else Some diff

  let commit_update ~instance ~update =
    List.iter
      (fun (k, op) ->
         match op with
         | `Set v -> instance.attrs <- StringMap.add k v instance.attrs
         | `Remove -> instance.attrs <- StringMap.remove k instance.attrs)
      update;
    push (Commit_update { id = instance.id; diff = update })

  let commit_text_update ~text_instance ~new_text =
    text_instance.text <- new_text;
    push (Commit_text_update { id = text_instance.id; text = new_text })

  let commit_mount ~instance ~props:_ =
    push (Commit_mount { id = instance.id })

  (* ---- Tree mutations (non-root) ---- *)

  let remove_from_current_parent (child:node) =
    match child with
    | El i ->
        (match i.parent with
         | PContainer c -> c.children <- List.filter (fun n -> id_of_node n <> i.id) c.children
         | PInstance p  -> p.children <- List.filter (fun n -> id_of_node n <> i.id) p.children
         | PNone -> ())
    | Tx t ->
        (match t.parent with
         | PContainer c -> c.children <- List.filter (fun n -> id_of_node n <> t.id) c.children
         | PInstance p  -> p.children <- List.filter (fun n -> id_of_node n <> t.id) p.children
         | PNone -> ())

  let append_child ~parent ~child =
    let child_node = match child with
      | Reconciler.Either.Left i -> El i
      | Reconciler.Either.Right t -> Tx t
    in
    remove_from_current_parent child_node;
    (match child_node with
     | El i -> i.parent <- PInstance parent
     | Tx t -> t.parent <- PInstance parent);
    parent.children <- parent.children @ [ child_node ];
    push (Append_child { parent = parent.id; child = id_of_node child_node })

  let insert_before ~parent ~child ~before_child =
    let child_node = match child with
      | Reconciler.Either.Left i -> El i
      | Reconciler.Either.Right t -> Tx t
    in
    let before_node = match before_child with
      | Reconciler.Either.Left i -> El i
      | Reconciler.Either.Right t -> Tx t
    in
    remove_from_current_parent child_node;
    let before_id = id_of_node before_node in
    let rec insert acc = function
      | [] -> List.rev (child_node :: acc)
      | n :: rest when id_of_node n = before_id -> List.rev acc @ (child_node :: n :: rest)
      | n :: rest -> insert (n :: acc) rest
    in
    (match child_node with
     | El i -> i.parent <- PInstance parent
     | Tx t -> t.parent <- PInstance parent);
    parent.children <- insert [] parent.children;
    push (Insert_before { parent = parent.id; child = id_of_node child_node; before = before_id })

  let remove_child ~parent ~child =
    let child_node = match child with
      | Reconciler.Either.Left i -> El i
      | Reconciler.Either.Right t -> Tx t
    in
    (match child_node with
     | El i -> i.parent <- PNone
     | Tx t -> t.parent <- PNone);
    parent.children <- List.filter (fun n -> id_of_node n <> id_of_node child_node) parent.children;
    push (Remove_child { parent = parent.id; child = id_of_node child_node })

  (* ---- Root container mutations ---- *)

  let append_child_to_container ~(container : container) ~child =
    let child_node = match child with
      | Reconciler.Either.Left i -> El i
      | Reconciler.Either.Right t -> Tx t
    in
    remove_from_current_parent child_node;
    (match child_node with
     | El i -> i.parent <- PContainer container
     | Tx t -> t.parent <- PContainer container);
    container.children <- container.children @ [ child_node ];
    push (Append_child_to_container { container = container.id; child = id_of_node child_node })

  let insert_before_in_container ~(container : container) ~child ~before_child =
    let child_node = match child with
      | Reconciler.Either.Left i -> El i
      | Reconciler.Either.Right t -> Tx t
    in
    let before_node = match before_child with
      | Reconciler.Either.Left i -> El i
      | Reconciler.Either.Right t -> Tx t
    in
    remove_from_current_parent child_node;
    let before_id = id_of_node before_node in
    let rec insert acc = function
      | [] -> List.rev (child_node :: acc)
      | n :: rest when id_of_node n = before_id -> List.rev acc @ (child_node :: n :: rest)
      | n :: rest -> insert (n :: acc) rest
    in
    (match child_node with
     | El i -> i.parent <- PContainer container
     | Tx t -> t.parent <- PContainer container);
    container.children <- insert [] container.children;
    push (Insert_before_in_container { container = container.id; child = id_of_node child_node; before = before_id })

  let remove_child_from_container ~(container : container) ~child =
    let child_node = match child with
      | Reconciler.Either.Left i -> El i
      | Reconciler.Either.Right t -> Tx t
    in
    (match child_node with
     | El i -> i.parent <- PNone
     | Tx t -> t.parent <- PNone);
    container.children <- List.filter (fun n -> id_of_node n <> id_of_node child_node) container.children;
    push (Remove_child_from_container { container = container.id; child = id_of_node child_node })

  let clear_container ~(container : container) =
    List.iter
      (function
        | El i -> i.parent <- PNone
        | Tx t -> t.parent <- PNone)
      container.children;
    container.children <- [];
    push (Clear_container { container = container.id })

  (* ---- Public exposure ---- *)

  let get_public_instance ~(instance : instance) = instance.id
  let get_public_text_instance ~(text_instance : text_instance) = text_instance.id

  (* ---- Deletion hook ---- *)

  let detach_deleted_instance ~child =
    let child_node = match child with
      | Reconciler.Either.Left i -> El i
      | Reconciler.Either.Right t -> Tx t
    in
    push (Detach_deleted { id = id_of_node child_node })
end

(* Testing surface *)

type snapshot =
  | Node of {
      id       : id;
      tag      : string;
      attrs    : (string * string) list;
      children : snapshot list;
    }
  | Text of {
      id   : id;
      text : string;
    }

let create_container ?(name="container") () : Host.container =
  { Host.id = next_id (); name; children = [] }

let rec snapshot_of_node : Host.node -> snapshot = function
  | Host.El i ->
      let attrs = Host.props_of_map i.attrs in
      let children = List.map snapshot_of_node i.children in
      Node { id = i.id; tag = i.tag; attrs; children }
  | Host.Tx t ->
      Text { id = t.id; text = t.text }

let snapshot_container (c : Host.container) : snapshot list =
  List.map snapshot_of_node c.children

let pp_snapshot fmt (xs : snapshot list) =
  let open Format in
  let rec go indent = function
    | Node { id; tag; attrs; children } ->
        fprintf fmt "%sNode#%d <%s> attrs=%a@\n"
          indent id tag
          (fun fmt attrs ->
             fprintf fmt "[";
             List.iteri
               (fun i (k,v) ->
                  if i > 0 then fprintf fmt "; ";
                  fprintf fmt "%s=%S" k v)
               attrs;
             fprintf fmt "]")
          attrs;
        List.iter (go (indent ^ "  ")) children
    | Text { id; text } ->
        fprintf fmt "%sText#%d %S@\n" indent id text
  in
  List.iter (go "") xs

let drain_log () =
  let xs = List.rev !log in
  log := [];
  xs
