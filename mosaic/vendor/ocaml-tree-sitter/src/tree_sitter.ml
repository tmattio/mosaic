module Language = struct
  type t
  type symbol_type = Regular | Anonymous | Supertype | Auxiliary

  external of_address : nativeint -> t = "caml_ts_language_of_address"

  external load_shared : path:string -> symbol:string -> t
    = "caml_ts_language_load"

  external version : t -> int = "caml_ts_language_version"
  external name : t -> string = "caml_ts_language_name"
  external symbol_count : t -> int = "caml_ts_language_symbol_count"
  external state_count : t -> int = "caml_ts_language_state_count"
  external symbol_name : t -> int -> string = "caml_ts_language_symbol_name"

  external symbol_for_name_native : t -> string -> bool -> int option
    = "caml_ts_language_symbol_for_name"

  external symbol_type_int : t -> int -> int = "caml_ts_language_symbol_type"
  external field_count : t -> int = "caml_ts_language_field_count"

  external field_name_for_id_native : t -> int -> string option
    = "caml_ts_language_field_name_for_id"

  external field_id_for_name_native : t -> string -> int option
    = "caml_ts_language_field_id_for_name"

  let symbol_for_name ?(is_named = false) language name =
    symbol_for_name_native language name is_named

  let symbol_type language symbol =
    match symbol_type_int language symbol with
    | 0 -> Regular
    | 1 -> Anonymous
    | 2 -> Supertype
    | _ -> Auxiliary

  let field_name_for_id = field_name_for_id_native
  let field_id_for_name = field_id_for_name_native
end

(* Tree handle type defined first so Node can reference it without circular
   dependency *)
type tree_handle

module Node = struct
  (* The internal C node handle *)
  type node_handle

  (* Node with a reference to its parent tree. The tree reference ensures the
     tree is not garbage collected while the node is still in use. *)
  type t = { node : node_handle; tree : tree_handle }

  external is_null_handle : node_handle -> bool = "caml_ts_node_is_null"
  external is_named_handle : node_handle -> bool = "caml_ts_node_is_named"
  external is_missing_handle : node_handle -> bool = "caml_ts_node_is_missing"
  external is_extra_handle : node_handle -> bool = "caml_ts_node_is_extra"
  external has_changes_handle : node_handle -> bool = "caml_ts_node_has_changes"
  external has_error_handle : node_handle -> bool = "caml_ts_node_has_error"
  external is_error_handle : node_handle -> bool = "caml_ts_node_is_error"
  external kind_handle : node_handle -> string = "caml_ts_node_type"
  external symbol_handle : node_handle -> int = "caml_ts_node_symbol"
  external language_handle : node_handle -> Language.t = "caml_ts_node_language"
  external parse_state_handle : node_handle -> int = "caml_ts_node_parse_state"

  external next_parse_state_handle : node_handle -> int
    = "caml_ts_node_next_parse_state"

  external start_byte_int32 : node_handle -> int32 = "caml_ts_node_start_byte"
  external end_byte_int32 : node_handle -> int32 = "caml_ts_node_end_byte"

  external start_point_handle : node_handle -> int * int
    = "caml_ts_node_start_point"

  external end_point_handle : node_handle -> int * int
    = "caml_ts_node_end_point"

  external child_handle : node_handle -> int -> node_handle option
    = "caml_ts_node_child"

  external named_child_handle : node_handle -> int -> node_handle option
    = "caml_ts_node_named_child"

  external child_with_descendant_handle :
    node_handle -> node_handle -> node_handle option
    = "caml_ts_node_child_with_descendant"

  external child_by_field_name_handle :
    node_handle -> string -> node_handle option
    = "caml_ts_node_child_by_field_name"

  external child_by_field_id_handle : node_handle -> int -> node_handle option
    = "caml_ts_node_child_by_field_id"

  external field_name_for_child_handle : node_handle -> int -> string option
    = "caml_ts_node_field_name_for_child"

  external field_name_for_named_child_handle :
    node_handle -> int -> string option
    = "caml_ts_node_field_name_for_named_child"

  external parent_handle : node_handle -> node_handle option
    = "caml_ts_node_parent"

  external descendant_for_byte_range_handle :
    node_handle -> int -> int -> node_handle option
    = "caml_ts_node_descendant_for_byte_range"

  external descendant_for_point_range_handle :
    node_handle -> int * int -> int * int -> node_handle option
    = "caml_ts_node_descendant_for_point_range"

  external named_descendant_for_byte_range_handle :
    node_handle -> int -> int -> node_handle option
    = "caml_ts_node_named_descendant_for_byte_range"

  external named_descendant_for_point_range_handle :
    node_handle -> int * int -> int * int -> node_handle option
    = "caml_ts_node_named_descendant_for_point_range"

  external next_sibling_handle : node_handle -> node_handle option
    = "caml_ts_node_next_sibling"

  external prev_sibling_handle : node_handle -> node_handle option
    = "caml_ts_node_prev_sibling"

  external next_named_sibling_handle : node_handle -> node_handle option
    = "caml_ts_node_next_named_sibling"

  external prev_named_sibling_handle : node_handle -> node_handle option
    = "caml_ts_node_prev_named_sibling"

  external first_child_for_byte_handle :
    node_handle -> int -> node_handle option
    = "caml_ts_node_first_child_for_byte"

  external first_named_child_for_byte_handle :
    node_handle -> int -> node_handle option
    = "caml_ts_node_first_named_child_for_byte"

  external child_count_int32 : node_handle -> int32 = "caml_ts_node_child_count"

  external named_child_count_int32 : node_handle -> int32
    = "caml_ts_node_named_child_count"

  external descendant_count_int32 : node_handle -> int32
    = "caml_ts_node_descendant_count"

  external to_sexp_handle : node_handle -> string = "caml_ts_node_to_sexp"
  external equal_handle : node_handle -> node_handle -> bool = "caml_ts_node_eq"

  let wrap_optional tree = function
    | None -> None
    | Some node -> Some { node; tree }

  let is_null t = is_null_handle t.node
  let is_named t = is_named_handle t.node
  let is_missing t = is_missing_handle t.node
  let is_extra t = is_extra_handle t.node
  let has_changes t = has_changes_handle t.node
  let has_error t = has_error_handle t.node
  let is_error t = is_error_handle t.node
  let kind t = kind_handle t.node
  let symbol t = symbol_handle t.node
  let language t = language_handle t.node
  let parse_state t = parse_state_handle t.node
  let next_parse_state t = next_parse_state_handle t.node
  let start_byte t = Int32.to_int (start_byte_int32 t.node)
  let end_byte t = Int32.to_int (end_byte_int32 t.node)
  let start_point t = start_point_handle t.node
  let end_point t = end_point_handle t.node
  let child_count t = Int32.to_int (child_count_int32 t.node)
  let named_child_count t = Int32.to_int (named_child_count_int32 t.node)
  let descendant_count t = Int32.to_int (descendant_count_int32 t.node)
  let child t idx = wrap_optional t.tree (child_handle t.node idx)
  let named_child t idx = wrap_optional t.tree (named_child_handle t.node idx)

  let child_with_descendant t desc =
    wrap_optional t.tree (child_with_descendant_handle t.node desc.node)

  let child_by_field_name t name =
    wrap_optional t.tree (child_by_field_name_handle t.node name)

  let child_by_field_id t id =
    wrap_optional t.tree (child_by_field_id_handle t.node id)

  let field_name_for_child t idx = field_name_for_child_handle t.node idx

  let field_name_for_named_child t idx =
    field_name_for_named_child_handle t.node idx

  let parent t = wrap_optional t.tree (parent_handle t.node)
  let next_sibling t = wrap_optional t.tree (next_sibling_handle t.node)
  let prev_sibling t = wrap_optional t.tree (prev_sibling_handle t.node)

  let next_named_sibling t =
    wrap_optional t.tree (next_named_sibling_handle t.node)

  let prev_named_sibling t =
    wrap_optional t.tree (prev_named_sibling_handle t.node)

  let first_child_for_byte t ~byte =
    wrap_optional t.tree (first_child_for_byte_handle t.node byte)

  let first_named_child_for_byte t ~byte =
    wrap_optional t.tree (first_named_child_for_byte_handle t.node byte)

  let descendant_for_byte_range t ~start ~end_ =
    wrap_optional t.tree (descendant_for_byte_range_handle t.node start end_)

  let descendant_for_point_range t ~start_point ~end_point =
    wrap_optional t.tree
      (descendant_for_point_range_handle t.node start_point end_point)

  let named_descendant_for_byte_range t ~start ~end_ =
    wrap_optional t.tree
      (named_descendant_for_byte_range_handle t.node start end_)

  let named_descendant_for_point_range t ~start_point ~end_point =
    wrap_optional t.tree
      (named_descendant_for_point_range_handle t.node start_point end_point)

  let to_sexp t = to_sexp_handle t.node
  let equal t1 t2 = equal_handle t1.node t2.node
end

module Tree = struct
  type t = tree_handle

  type range = {
    start_byte : int;
    end_byte : int;
    start_point : int * int;
    end_point : int * int;
  }

  external copy : t -> t = "caml_ts_tree_copy"
  external root_node_handle : t -> Node.node_handle = "caml_ts_tree_root_node"
  external root_sexp : t -> string = "caml_ts_tree_root_sexp"

  external edit :
    t ->
    start_byte:int ->
    old_end_byte:int ->
    new_end_byte:int ->
    start_point:int * int ->
    old_end_point:int * int ->
    new_end_point:int * int ->
    unit = "caml_ts_tree_edit_bytecode" "caml_ts_tree_edit_native"

  external language : t -> Language.t = "caml_ts_tree_language"

  external included_ranges_native : t -> range array
    = "caml_ts_tree_included_ranges"

  external changed_ranges_native : t -> t -> range array
    = "caml_ts_tree_get_changed_ranges"

  let root_node tree : Node.t =
    let node = root_node_handle tree in
    { Node.node; tree }

  let included_ranges = included_ranges_native
  let changed_ranges ~old new_tree = changed_ranges_native old new_tree
end

module Parser = struct
  type t

  external create : unit -> t = "caml_ts_parser_new"
  external delete : t -> unit = "caml_ts_parser_delete"

  external set_language : t -> Language.t -> unit
    = "caml_ts_parser_set_language"

  external parse_string_native : t -> string -> Tree.t
    = "caml_ts_parser_parse_string"

  external parse_string_with_old : t -> Tree.t option -> string -> Tree.t
    = "caml_ts_parser_parse_string_old"

  external set_timeout_micros : t -> int64 -> unit
    = "caml_ts_parser_set_timeout_micros"

  external timeout_micros : t -> int64 = "caml_ts_parser_timeout_micros"
  external reset : t -> unit = "caml_ts_parser_reset"

  let parse_string ?old parser source =
    match old with
    | None -> parse_string_native parser source
    | Some _ -> parse_string_with_old parser old source
end

module Query = struct
  type t
  type quantifier = Zero | Zero_or_one | Zero_or_more | One_or_more

  external create_native : Language.t -> string -> t = "caml_ts_query_new"
  external delete : t -> unit = "caml_ts_query_delete"
  external capture_count_int32 : t -> int32 = "caml_ts_query_capture_count"
  external pattern_count_int32 : t -> int32 = "caml_ts_query_pattern_count"
  external string_count_int32 : t -> int32 = "caml_ts_query_string_count"

  external capture_name_for_id : t -> int -> string option
    = "caml_ts_query_capture_name_for_id"

  external capture_index_for_name : t -> string -> int option
    = "caml_ts_query_capture_index_for_name"

  external capture_quantifier_int : t -> int -> int -> int
    = "caml_ts_query_capture_quantifier_for_id"

  external string_value_for_id : t -> int -> string option
    = "caml_ts_query_string_value_for_id"

  external start_byte_for_pattern_int32 : t -> int -> int32
    = "caml_ts_query_start_byte_for_pattern"

  external end_byte_for_pattern_int32 : t -> int -> int32
    = "caml_ts_query_end_byte_for_pattern"

  external is_pattern_rooted_native : t -> int -> bool
    = "caml_ts_query_is_pattern_rooted"

  external is_pattern_non_local_native : t -> int -> bool
    = "caml_ts_query_is_pattern_non_local"

  external is_pattern_guaranteed_at_step_native : t -> int -> bool
    = "caml_ts_query_is_pattern_guaranteed_at_step"

  external disable_capture_native : t -> string -> unit
    = "caml_ts_query_disable_capture"

  external disable_pattern_native : t -> int -> unit
    = "caml_ts_query_disable_pattern"

  let create language ~source = create_native language source
  let capture_count t = Int32.to_int (capture_count_int32 t)
  let pattern_count t = Int32.to_int (pattern_count_int32 t)
  let string_count t = Int32.to_int (string_count_int32 t)

  let capture_quantifier t ~pattern ~capture =
    match capture_quantifier_int t pattern capture with
    | 0 -> Zero
    | 1 -> Zero_or_one
    | 2 -> Zero_or_more
    | _ -> One_or_more

  let start_byte_for_pattern t ~pattern =
    Int32.to_int (start_byte_for_pattern_int32 t pattern)

  let end_byte_for_pattern t ~pattern =
    Int32.to_int (end_byte_for_pattern_int32 t pattern)

  let is_pattern_rooted t ~pattern = is_pattern_rooted_native t pattern
  let is_pattern_non_local t ~pattern = is_pattern_non_local_native t pattern

  let is_pattern_guaranteed_at_step t ~byte_offset =
    is_pattern_guaranteed_at_step_native t byte_offset

  let disable_capture t ~name = disable_capture_native t name
  let disable_pattern t ~pattern = disable_pattern_native t pattern
end

module Query_cursor = struct
  type t

  type capture = {
    capture_index : int;
    pattern_index : int;
    match_id : int;
    node : Node.t;
  }

  type match_result = {
    match_id : int;
    pattern_index : int;
    captures : (int * Node.t) array;
  }

  external create : unit -> t = "caml_ts_query_cursor_new"
  external delete : t -> unit = "caml_ts_query_cursor_delete"

  external exec_handle : t -> Query.t -> Node.node_handle -> unit
    = "caml_ts_query_cursor_exec"

  external set_byte_range_native : t -> int -> int -> unit
    = "caml_ts_query_cursor_set_byte_range"

  external set_point_range_native : t -> int * int -> int * int -> unit
    = "caml_ts_query_cursor_set_point_range"

  external reset_handle : t -> Query.t -> Node.node_handle -> unit
    = "caml_ts_query_cursor_reset"

  external set_timeout_micros : t -> int64 -> unit
    = "caml_ts_query_cursor_set_timeout_micros"

  external timeout_micros : t -> int64 = "caml_ts_query_cursor_timeout_micros"

  external set_match_limit : t -> int -> unit
    = "caml_ts_query_cursor_set_match_limit"

  external match_limit : t -> int = "caml_ts_query_cursor_match_limit"

  external did_exceed_match_limit : t -> bool
    = "caml_ts_query_cursor_did_exceed_match_limit"

  external next_match_raw :
    t -> (int32 * int * (int * Node.node_handle) array) option
    = "caml_ts_query_cursor_next_match"

  external next_capture_raw :
    t -> Query.t -> (int * int * int32 * Node.node_handle) option
    = "caml_ts_query_cursor_next_capture"

  (* Store the tree reference for wrapping nodes returned by
     next_match/next_capture *)
  let current_tree : Tree.t option ref = ref None

  let exec t query (node : Node.t) =
    current_tree := Some node.tree;
    exec_handle t query node.node

  let reset t ~query ~(node : Node.t) =
    current_tree := Some node.tree;
    reset_handle t query node.node

  let set_byte_range t ~start ~end_ = set_byte_range_native t start end_

  let set_point_range t ~start_point ~end_point =
    set_point_range_native t start_point end_point

  let next_match cursor =
    match (next_match_raw cursor, !current_tree) with
    | None, _ -> None
    | Some _, None ->
        failwith
          "Query_cursor.next_match: cursor not initialized with exec or reset"
    | Some (match_id, pattern_index, captures), Some tree ->
        let captures =
          Array.map
            (fun (idx, node_handle) ->
              (idx, ({ Node.node = node_handle; tree } : Node.t)))
            captures
        in
        Some { match_id = Int32.to_int match_id; pattern_index; captures }

  let next_capture cursor query =
    match (next_capture_raw cursor query, !current_tree) with
    | None, _ -> None
    | Some _, None ->
        failwith
          "Query_cursor.next_capture: cursor not initialized with exec or reset"
    | Some (capture_index, pattern_index, match_id, node_handle), Some tree ->
        Some
          {
            capture_index;
            pattern_index;
            match_id = Int32.to_int match_id;
            node = { Node.node = node_handle; tree };
          }
end
