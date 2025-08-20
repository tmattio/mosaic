type key = string option

type 'primitive kind =
  [ `Primitive of 'primitive | `Text | `Fragment | `Suspense | `Empty ]

type ('primitive, 'props) t =
  | Primitive of {
      element_type : 'primitive;
      key : key;
      props : 'props;
      children : ('primitive, 'props) t array;
    }
  | Text of string
  | Fragment of { key : key; children : ('primitive, 'props) t array }
  | Suspense of {
      key : key;
      children : ('primitive, 'props) t array;
      fallback : ('primitive, 'props) t option;
    }
  | Empty

(* Constructors *)

let primitive ?(key = None) ~element_type ~props children =
  let children = Array.of_list children in
  Primitive { element_type; key; props; children }

let text s = Text s

let fragment ?(key = None) children =
  let children = Array.of_list children in
  Fragment { key; children }

let suspense ?(key = None) ?fallback children =
  let children = Array.of_list children in
  Suspense { key; children; fallback }

let empty = Empty

(* Accessors *)

let kind = function
  | Primitive { element_type; _ } -> `Primitive element_type
  | Text _ -> `Text
  | Fragment _ -> `Fragment
  | Suspense _ -> `Suspense
  | Empty -> `Empty

let key = function
  | Primitive { key; _ } | Fragment { key; _ } | Suspense { key; _ } -> key
  | Text _ | Empty -> None

let children = function
  | Primitive { children; _ }
  | Fragment { children; _ }
  | Suspense { children; _ } ->
      children
  | Text _ | Empty -> [||]

let children_list n = Array.to_list (children n)
let is_empty = function Empty -> true | _ -> false

let has_children = function
  | Primitive { children; _ }
  | Fragment { children; _ }
  | Suspense { children; _ } ->
      Array.length children > 0
  | Text _ | Empty -> false

(* Structural operations *)

let rec map ~f node =
  let node' = f node in
  match node' with
  | Primitive { element_type; key; props; children = ch } ->
      let ch' = Array.map (map ~f) ch in
      Primitive { element_type; key; props; children = ch' }
  | Fragment { key; children = ch } ->
      let ch' = Array.map (map ~f) ch in
      Fragment { key; children = ch' }
  | Suspense { key; children = ch; fallback } ->
      let ch' = Array.map (map ~f) ch in
      let fb' = Option.map (map ~f) fallback in
      Suspense { key; children = ch'; fallback = fb' }
  | Text _ | Empty -> node'

let rec fold ~init ~f node =
  let acc = f init node in
  match node with
  | Primitive { children; _ }
  | Fragment { children; _ }
  | Suspense { children; _ } ->
      Array.fold_left (fun a c -> fold ~init:a ~f c) acc children
  | Text _ | Empty -> acc

let iter ~f node =
  let rec go n =
    f n;
    match n with
    | Primitive { children; _ }
    | Fragment { children; _ }
    | Suspense { children; _ } ->
        Array.iter go children
    | Text _ | Empty -> ()
  in
  go node

let with_children node new_children =
  let ch = Array.of_list new_children in
  match node with
  | Primitive p -> Primitive { p with children = ch }
  | Fragment frag -> Fragment { frag with children = ch }
  | Suspense s -> Suspense { s with children = ch }
  | Text _ | Empty -> node

(* Comparison helpers *)

let equal_shallow_kind ~prim_eq a b =
  match (a, b) with
  | Primitive { element_type = p1; _ }, Primitive { element_type = p2; _ } ->
      prim_eq p1 p2
  | Text _, Text _ -> true
  | Fragment _, Fragment _ -> true
  | Suspense _, Suspense _ -> true
  | Empty, Empty -> true
  | _ -> false

(* Key validation *)

let duplicate_child_keys node =
  let ch = children node in
  (* Count keys among siblings, ignoring None *)
  let tbl : (string, int) Hashtbl.t = Hashtbl.create 8 in
  Array.iter
    (fun n ->
      match key n with
      | None -> ()
      | Some k ->
          let c = match Hashtbl.find_opt tbl k with Some x -> x | None -> 0 in
          Hashtbl.replace tbl k (c + 1))
    ch;
  Hashtbl.to_seq tbl
  |> Seq.filter (fun (_k, count) -> count > 1)
  |> Seq.map fst |> List.of_seq

let duplicate_keys_deep root =
  let rec go path acc node =
    let dups = duplicate_child_keys node in
    let acc = if dups = [] then acc else (List.rev path, dups) :: acc in
    match node with
    | Primitive { children; _ }
    | Fragment { children; _ }
    | Suspense { children; _ } ->
        Array.to_list children
        |> List.mapi (fun idx child -> (idx, child))
        |> List.fold_left
          (fun a (idx, child) -> go (idx :: path) a child)
          acc
    | Text _ | Empty -> acc
  in
  go [] [] root |> List.rev

(* Pretty printing *)

let pp (pp_primitive : Format.formatter -> 'primitive -> unit)
    (pp_props : Format.formatter -> 'props -> unit) (fmt : Format.formatter)
    (tree : ('primitive, 'props) t) =
  let open Format in
  let rec go indent = function
    | Empty -> fprintf fmt "%sEmpty@." indent
    | Text s -> fprintf fmt "%sText(%S)@." indent s
    | Primitive { element_type; key; props; children } ->
        fprintf fmt "%sPrimitive(" indent;
        pp_primitive fmt element_type;
        (match key with
        | None -> fprintf fmt ", key=None, "
        | Some k -> fprintf fmt ", key=%S, " k);
        fprintf fmt "props=";
        pp_props fmt props;
        fprintf fmt ")@.";
        Array.iter (go (indent ^ "  ")) children
    | Fragment { key; children } ->
        (match key with
        | None -> fprintf fmt "%sFragment(key=None)@." indent
        | Some k -> fprintf fmt "%sFragment(key=%S)@." indent k);
        Array.iter (go (indent ^ "  ")) children
    | Suspense { key; children; fallback } -> (
        (match key with
        | None -> fprintf fmt "%sSuspense(key=None)@." indent
        | Some k -> fprintf fmt "%sSuspense(key=%S)@." indent k);
        Array.iter (go (indent ^ "  ")) children;
        match fallback with
        | None -> ()
        | Some fb ->
            fprintf fmt "%s  Fallback:@." indent;
            go (indent ^ "    ") fb)
  in
  go "" tree
