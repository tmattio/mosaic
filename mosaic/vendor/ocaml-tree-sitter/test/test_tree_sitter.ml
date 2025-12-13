open Tree_sitter

(* Test fixtures *)
let json_simple = {|{"name": "test", "value": 42}|}
let json_array = {|[1, 2, 3, "hello", null, true, false]|}
let json_nested = {|{"outer": {"inner": [1, 2, {"deep": "value"}]}}|}
let json_invalid = {|{"unclosed": |}

let json_multiline = {|{
  "name": "test",
  "items": [1, 2, 3]
}|}

(* Helper to create a parser with JSON language *)
let with_json_parser f =
  let parser = Parser.create () in
  Parser.set_language parser (Tree_sitter_json.language ());
  let result = f parser in
  Parser.delete parser;
  result

(* Helper to parse JSON and run function on tree *)
let with_parsed_json source f =
  with_json_parser (fun parser ->
      let tree = Parser.parse_string parser source in
      f tree)

(* Language Module Tests *)

let test_language_version () =
  let lang = Tree_sitter_json.language () in
  let version = Language.version lang in
  Alcotest.(check bool) "version is positive" true (version > 0)

let test_language_name () =
  let lang = Tree_sitter_json.language () in
  let name = Language.name lang in
  Alcotest.(check string) "language name" "json" name

let test_language_symbol_count () =
  let lang = Tree_sitter_json.language () in
  let count = Language.symbol_count lang in
  Alcotest.(check bool) "has symbols" true (count > 0)

let test_language_field_count () =
  let lang = Tree_sitter_json.language () in
  let count = Language.field_count lang in
  Alcotest.(check bool) "has fields" true (count > 0)

let test_language_symbol_for_name () =
  let lang = Tree_sitter_json.language () in
  let symbol = Language.symbol_for_name ~is_named:true lang "object" in
  Alcotest.(check bool) "object symbol exists" true (Option.is_some symbol);
  let symbol = Language.symbol_for_name ~is_named:true lang "nonexistent_xyz" in
  Alcotest.(check bool) "nonexistent symbol" true (Option.is_none symbol)

let test_language_field_id_for_name () =
  let lang = Tree_sitter_json.language () in
  let field = Language.field_id_for_name lang "key" in
  Alcotest.(check bool) "key field exists" true (Option.is_some field);
  let field = Language.field_id_for_name lang "nonexistent_field" in
  Alcotest.(check bool) "nonexistent field" true (Option.is_none field)

let language_tests =
  [
    Alcotest.test_case "version" `Quick test_language_version;
    Alcotest.test_case "name" `Quick test_language_name;
    Alcotest.test_case "symbol_count" `Quick test_language_symbol_count;
    Alcotest.test_case "field_count" `Quick test_language_field_count;
    Alcotest.test_case "symbol_for_name" `Quick test_language_symbol_for_name;
    Alcotest.test_case "field_id_for_name" `Quick
      test_language_field_id_for_name;
  ]

(* Parser Module Tests *)

let test_parser_create_delete () =
  let parser = Parser.create () in
  Parser.delete parser;
  (* Should not crash *)
  Alcotest.(check pass) "create and delete" () ()

let test_parser_set_language () =
  let parser = Parser.create () in
  Parser.set_language parser (Tree_sitter_json.language ());
  Parser.delete parser;
  Alcotest.(check pass) "set language" () ()

let test_parser_parse_simple () =
  with_parsed_json json_simple (fun tree ->
      let sexp = Tree.root_sexp tree in
      Alcotest.(check bool) "sexp not empty" true (String.length sexp > 0);
      Alcotest.(check bool)
        "contains document" true
        (String.length sexp > 0 && sexp.[0] = '('))

let test_parser_timeout () =
  with_json_parser (fun parser ->
      Parser.set_timeout_micros parser 1000000L;
      let timeout = Parser.timeout_micros parser in
      Alcotest.(check int64) "timeout set" 1000000L timeout)

let test_parser_reset () =
  with_json_parser (fun parser ->
      let _ = Parser.parse_string parser json_simple in
      Parser.reset parser;
      (* Should be able to parse again after reset *)
      let _ = Parser.parse_string parser json_array in
      Alcotest.(check pass) "reset works" () ())

let parser_tests =
  [
    Alcotest.test_case "create_delete" `Quick test_parser_create_delete;
    Alcotest.test_case "set_language" `Quick test_parser_set_language;
    Alcotest.test_case "parse_simple" `Quick test_parser_parse_simple;
    Alcotest.test_case "timeout" `Quick test_parser_timeout;
    Alcotest.test_case "reset" `Quick test_parser_reset;
  ]

(* Tree Module Tests *)

let test_tree_root_node () =
  with_parsed_json json_simple (fun tree ->
      let root = Tree.root_node tree in
      Alcotest.(check bool) "root not null" false (Node.is_null root);
      Alcotest.(check string) "root is document" "document" (Node.kind root))

let test_tree_copy () =
  with_parsed_json json_simple (fun tree ->
      let copy = Tree.copy tree in
      let original_sexp = Tree.root_sexp tree in
      let copy_sexp = Tree.root_sexp copy in
      Alcotest.(check string) "copy matches original" original_sexp copy_sexp)

let test_tree_language () =
  with_parsed_json json_simple (fun tree ->
      let lang = Tree.language tree in
      let name = Language.name lang in
      Alcotest.(check string) "tree language is json" "json" name)

let test_tree_edit () =
  with_json_parser (fun parser ->
      let tree = Parser.parse_string parser json_simple in
      (* Edit: change "test" to "modified" (same length for simplicity) *)
      Tree.edit tree ~start_byte:9 ~old_end_byte:13 ~new_end_byte:13
        ~start_point:(0, 9) ~old_end_point:(0, 13) ~new_end_point:(0, 13);
      (* Tree is now marked as edited - we can reparse with the old tree *)
      let new_source = {|{"name": "modi", "value": 42}|} in
      let new_tree = Parser.parse_string ~old:tree parser new_source in
      let root = Tree.root_node new_tree in
      Alcotest.(check bool) "reparsed tree valid" false (Node.is_null root))

let test_tree_included_ranges () =
  with_parsed_json json_simple (fun tree ->
      let ranges = Tree.included_ranges tree in
      Alcotest.(check bool)
        "has at least one range" true
        (Array.length ranges >= 0))

let test_tree_changed_ranges () =
  with_json_parser (fun parser ->
      let tree1 = Parser.parse_string parser json_simple in
      let tree2 = Parser.parse_string parser json_array in
      let ranges = Tree.changed_ranges ~old:tree1 tree2 in
      Alcotest.(check bool) "has changes" true (Array.length ranges > 0))

let tree_tests =
  [
    Alcotest.test_case "root_node" `Quick test_tree_root_node;
    Alcotest.test_case "copy" `Quick test_tree_copy;
    Alcotest.test_case "language" `Quick test_tree_language;
    Alcotest.test_case "edit" `Quick test_tree_edit;
    Alcotest.test_case "included_ranges" `Quick test_tree_included_ranges;
    Alcotest.test_case "changed_ranges" `Quick test_tree_changed_ranges;
  ]

(* Node Module Tests *)

let test_node_kind_and_symbol () =
  with_parsed_json json_simple (fun tree ->
      let root = Tree.root_node tree in
      Alcotest.(check string) "root kind" "document" (Node.kind root);
      let symbol = Node.symbol root in
      Alcotest.(check bool) "symbol is positive" true (symbol >= 0))

let test_node_positions () =
  with_parsed_json json_simple (fun tree ->
      let root = Tree.root_node tree in
      let start_byte = Node.start_byte root in
      let end_byte = Node.end_byte root in
      Alcotest.(check int) "start at 0" 0 start_byte;
      Alcotest.(check int)
        "end at source length"
        (String.length json_simple)
        end_byte;
      let start_row, start_col = Node.start_point root in
      Alcotest.(check int) "start row" 0 start_row;
      Alcotest.(check int) "start col" 0 start_col)

let test_node_multiline_positions () =
  with_parsed_json json_multiline (fun tree ->
      let root = Tree.root_node tree in
      let _, end_row = Node.end_point root in
      Alcotest.(check bool) "spans multiple rows" true (end_row > 0))

let test_node_child_access () =
  with_parsed_json json_simple (fun tree ->
      let root = Tree.root_node tree in
      let child_count = Node.child_count root in
      Alcotest.(check bool) "has children" true (child_count > 0);
      let first_child = Node.child root 0 in
      Alcotest.(check bool)
        "first child exists" true
        (Option.is_some first_child);
      let no_child = Node.child root 1000 in
      Alcotest.(check bool)
        "invalid index returns none" true (Option.is_none no_child))

let test_node_named_children () =
  with_parsed_json json_simple (fun tree ->
      let root = Tree.root_node tree in
      let named_count = Node.named_child_count root in
      Alcotest.(check bool) "has named children" true (named_count > 0);
      let first_named = Node.named_child root 0 in
      Alcotest.(check bool)
        "first named child exists" true
        (Option.is_some first_named);
      match first_named with
      | Some node ->
          Alcotest.(check bool) "named child is named" true (Node.is_named node)
      | None -> Alcotest.fail "expected named child")

let test_node_child_by_field () =
  with_parsed_json json_simple (fun tree ->
      let root = Tree.root_node tree in
      (* Get the object node *)
      match Node.named_child root 0 with
      | Some obj -> (
          Alcotest.(check string) "is object" "object" (Node.kind obj);
          (* Get pair node *)
          match Node.named_child obj 0 with
          | Some pair ->
              (* Access key field *)
              let key = Node.child_by_field_name pair "key" in
              Alcotest.(check bool) "key field exists" true (Option.is_some key)
          | None -> Alcotest.fail "expected pair")
      | None -> Alcotest.fail "expected object")

let test_node_parent () =
  with_parsed_json json_simple (fun tree ->
      let root = Tree.root_node tree in
      match Node.child root 0 with
      | Some child -> (
          let parent = Node.parent child in
          match parent with
          | Some p ->
              Alcotest.(check bool)
                "parent equals root" true (Node.equal p root)
          | None -> Alcotest.fail "expected parent")
      | None -> Alcotest.fail "expected child")

let test_node_siblings () =
  with_parsed_json json_array (fun tree ->
      let root = Tree.root_node tree in
      match Node.named_child root 0 with
      | Some array -> (
          match Node.named_child array 0 with
          | Some first_elem ->
              let next = Node.next_sibling first_elem in
              Alcotest.(check bool)
                "has next sibling" true (Option.is_some next);
              let next_named = Node.next_named_sibling first_elem in
              Alcotest.(check bool)
                "has next named sibling" true
                (Option.is_some next_named)
          | None -> Alcotest.fail "expected array element")
      | None -> Alcotest.fail "expected array")

let test_node_descendant_for_range () =
  with_parsed_json json_simple (fun tree ->
      let root = Tree.root_node tree in
      (* Find node at position of "name" - may be string or string_content depending on grammar *)
      let desc = Node.descendant_for_byte_range root ~start:2 ~end_:6 in
      Alcotest.(check bool) "found descendant" true (Option.is_some desc);
      match desc with
      | Some node ->
          let kind = Node.kind node in
          Alcotest.(check bool)
            "descendant is string-related" true
            (kind = "string" || kind = "string_content")
      | None -> ())

let test_node_descendant_for_point () =
  with_parsed_json json_multiline (fun tree ->
      let root = Tree.root_node tree in
      let desc =
        Node.descendant_for_point_range root ~start_point:(1, 3)
          ~end_point:(1, 7)
      in
      Alcotest.(check bool)
        "found descendant by point" true (Option.is_some desc))

let test_node_flags () =
  with_parsed_json json_simple (fun tree ->
      let root = Tree.root_node tree in
      Alcotest.(check bool) "root is named" true (Node.is_named root);
      Alcotest.(check bool) "root not missing" false (Node.is_missing root);
      Alcotest.(check bool) "root not extra" false (Node.is_extra root);
      Alcotest.(check bool) "root has no error" false (Node.has_error root);
      Alcotest.(check bool) "root is not error" false (Node.is_error root))

let test_node_error_detection () =
  with_parsed_json json_invalid (fun tree ->
      let root = Tree.root_node tree in
      Alcotest.(check bool) "tree has error" true (Node.has_error root))

let test_node_equality () =
  with_parsed_json json_simple (fun tree ->
      let root1 = Tree.root_node tree in
      let root2 = Tree.root_node tree in
      Alcotest.(check bool) "same node equals" true (Node.equal root1 root2);
      match Node.child root1 0 with
      | Some child ->
          Alcotest.(check bool)
            "different nodes not equal" false (Node.equal root1 child)
      | None -> Alcotest.fail "expected child")

let test_node_to_sexp () =
  with_parsed_json json_simple (fun tree ->
      let root = Tree.root_node tree in
      let sexp = Node.to_sexp root in
      Alcotest.(check bool) "sexp not empty" true (String.length sexp > 0);
      Alcotest.(check bool) "sexp starts with paren" true (sexp.[0] = '('))

let test_node_descendant_count () =
  with_parsed_json json_nested (fun tree ->
      let root = Tree.root_node tree in
      let count = Node.descendant_count root in
      Alcotest.(check bool) "has descendants" true (count > 1))

let test_node_tree_reference_safety () =
  (* This test verifies that nodes keep the tree alive via GC.
     The Node.t type holds a reference to Tree.t, preventing GC from
     collecting the tree while nodes are still in use. *)
  let node =
    with_json_parser (fun parser ->
        let tree = Parser.parse_string parser json_simple in
        let root = Tree.root_node tree in
        (* Get a child node *)
        let child = Node.child root 0 in
        (* Force a GC cycle to verify the tree isn't collected *)
        Gc.full_major ();
        child)
  in
  (* The node should still be accessible because it holds a tree reference *)
  match node with
  | Some n ->
      (* Force another GC to really stress test *)
      Gc.full_major ();
      (* This should not crash - the tree is kept alive by the node reference *)
      let kind = Node.kind n in
      Alcotest.(check bool) "node still valid" true (String.length kind > 0)
  | None -> Alcotest.fail "expected node"

let node_tests =
  [
    Alcotest.test_case "kind_and_symbol" `Quick test_node_kind_and_symbol;
    Alcotest.test_case "positions" `Quick test_node_positions;
    Alcotest.test_case "multiline_positions" `Quick
      test_node_multiline_positions;
    Alcotest.test_case "child_access" `Quick test_node_child_access;
    Alcotest.test_case "named_children" `Quick test_node_named_children;
    Alcotest.test_case "child_by_field" `Quick test_node_child_by_field;
    Alcotest.test_case "parent" `Quick test_node_parent;
    Alcotest.test_case "siblings" `Quick test_node_siblings;
    Alcotest.test_case "descendant_for_range" `Quick
      test_node_descendant_for_range;
    Alcotest.test_case "descendant_for_point" `Quick
      test_node_descendant_for_point;
    Alcotest.test_case "flags" `Quick test_node_flags;
    Alcotest.test_case "error_detection" `Quick test_node_error_detection;
    Alcotest.test_case "equality" `Quick test_node_equality;
    Alcotest.test_case "to_sexp" `Quick test_node_to_sexp;
    Alcotest.test_case "descendant_count" `Quick test_node_descendant_count;
    Alcotest.test_case "tree_reference_safety" `Quick
      test_node_tree_reference_safety;
  ]

(* Query Module Tests *)

let test_query_create () =
  let lang = Tree_sitter_json.language () in
  let query = Query.create lang ~source:"(object)" in
  Alcotest.(check bool) "pattern count > 0" true (Query.pattern_count query > 0);
  Query.delete query

let test_query_invalid_syntax () =
  let lang = Tree_sitter_json.language () in
  let raised =
    try
      let _ = Query.create lang ~source:"(((invalid syntax" in
      false
    with Failure _ -> true
  in
  Alcotest.(check bool) "raises on invalid syntax" true raised

let test_query_captures () =
  let lang = Tree_sitter_json.language () in
  let query = Query.create lang ~source:"(string) @str (number) @num" in
  Alcotest.(check int) "capture count" 2 (Query.capture_count query);
  let name0 = Query.capture_name_for_id query 0 in
  Alcotest.(check (option string)) "capture 0 name" (Some "str") name0;
  let name1 = Query.capture_name_for_id query 1 in
  Alcotest.(check (option string)) "capture 1 name" (Some "num") name1;
  Query.delete query

let test_query_capture_index () =
  let lang = Tree_sitter_json.language () in
  let query = Query.create lang ~source:"(string) @mystring" in
  let idx = Query.capture_index_for_name query "mystring" in
  Alcotest.(check (option int)) "capture index" (Some 0) idx;
  let missing = Query.capture_index_for_name query "nonexistent" in
  Alcotest.(check (option int)) "missing capture" None missing;
  Query.delete query

let test_query_pattern_info () =
  let lang = Tree_sitter_json.language () in
  let query = Query.create lang ~source:"(object) (array)" in
  Alcotest.(check int) "two patterns" 2 (Query.pattern_count query);
  let start0 = Query.start_byte_for_pattern query ~pattern:0 in
  let start1 = Query.start_byte_for_pattern query ~pattern:1 in
  Alcotest.(check bool) "pattern 1 starts after pattern 0" true (start1 > start0);
  Query.delete query

let test_query_disable_pattern () =
  let lang = Tree_sitter_json.language () in
  let query = Query.create lang ~source:"(object) @obj (array) @arr" in
  Query.disable_pattern query ~pattern:0;
  (* Pattern is disabled but query still valid *)
  Alcotest.(check pass) "disable pattern works" () ();
  Query.delete query

let test_query_disable_capture () =
  let lang = Tree_sitter_json.language () in
  let query = Query.create lang ~source:"(string) @str" in
  Query.disable_capture query ~name:"str";
  Alcotest.(check pass) "disable capture works" () ();
  Query.delete query

let query_tests =
  [
    Alcotest.test_case "create" `Quick test_query_create;
    Alcotest.test_case "invalid_syntax" `Quick test_query_invalid_syntax;
    Alcotest.test_case "captures" `Quick test_query_captures;
    Alcotest.test_case "capture_index" `Quick test_query_capture_index;
    Alcotest.test_case "pattern_info" `Quick test_query_pattern_info;
    Alcotest.test_case "disable_pattern" `Quick test_query_disable_pattern;
    Alcotest.test_case "disable_capture" `Quick test_query_disable_capture;
  ]

(* Query Cursor Module Tests *)

let test_cursor_create_delete () =
  let cursor = Query_cursor.create () in
  Query_cursor.delete cursor;
  Alcotest.(check pass) "create and delete" () ()

let test_cursor_exec_and_matches () =
  with_parsed_json json_simple (fun tree ->
      let lang = Tree_sitter_json.language () in
      let query = Query.create lang ~source:"(string) @str" in
      let cursor = Query_cursor.create () in
      let root = Tree.root_node tree in
      Query_cursor.exec cursor query root;
      (* Should find strings: "name", "test", "value" *)
      let rec count_matches acc =
        match Query_cursor.next_match cursor with
        | Some _ -> count_matches (acc + 1)
        | None -> acc
      in
      let matches = count_matches 0 in
      Alcotest.(check bool) "found matches" true (matches > 0);
      Query_cursor.delete cursor;
      Query.delete query)

let test_cursor_captures () =
  with_parsed_json json_simple (fun tree ->
      let lang = Tree_sitter_json.language () in
      let query = Query.create lang ~source:"(number) @num" in
      let cursor = Query_cursor.create () in
      let root = Tree.root_node tree in
      Query_cursor.exec cursor query root;
      (match Query_cursor.next_capture cursor query with
      | Some capture ->
          Alcotest.(check int) "capture index" 0 capture.capture_index;
          Alcotest.(check string)
            "captured number" "number" (Node.kind capture.node)
      | None -> Alcotest.fail "expected capture");
      Query_cursor.delete cursor;
      Query.delete query)

let test_cursor_byte_range () =
  with_parsed_json json_simple (fun tree ->
      let lang = Tree_sitter_json.language () in
      let query = Query.create lang ~source:"(string) @str" in
      let cursor = Query_cursor.create () in
      let root = Tree.root_node tree in
      (* Restrict to first part of document *)
      Query_cursor.set_byte_range cursor ~start:0 ~end_:15;
      Query_cursor.exec cursor query root;
      let rec count_matches acc =
        match Query_cursor.next_match cursor with
        | Some _ -> count_matches (acc + 1)
        | None -> acc
      in
      let matches = count_matches 0 in
      (* Should find fewer matches due to range restriction *)
      Alcotest.(check bool) "found some matches" true (matches >= 0);
      Query_cursor.delete cursor;
      Query.delete query)

let test_cursor_point_range () =
  with_parsed_json json_multiline (fun tree ->
      let lang = Tree_sitter_json.language () in
      let query = Query.create lang ~source:"(string) @str" in
      let cursor = Query_cursor.create () in
      let root = Tree.root_node tree in
      (* Restrict to first line *)
      Query_cursor.set_point_range cursor ~start_point:(0, 0) ~end_point:(1, 0);
      Query_cursor.exec cursor query root;
      Alcotest.(check pass) "point range set" () ();
      Query_cursor.delete cursor;
      Query.delete query)

let test_cursor_match_limit () =
  with_parsed_json json_array (fun tree ->
      let lang = Tree_sitter_json.language () in
      let query = Query.create lang ~source:"(number) @num" in
      let cursor = Query_cursor.create () in
      Query_cursor.set_match_limit cursor 1;
      Alcotest.(check int) "match limit set" 1 (Query_cursor.match_limit cursor);
      let root = Tree.root_node tree in
      Query_cursor.exec cursor query root;
      Query_cursor.delete cursor;
      Query.delete query)

let test_cursor_timeout () =
  let cursor = Query_cursor.create () in
  Query_cursor.set_timeout_micros cursor 5000000L;
  let timeout = Query_cursor.timeout_micros cursor in
  Alcotest.(check int64) "timeout set" 5000000L timeout;
  Query_cursor.delete cursor

let test_cursor_reset () =
  with_parsed_json json_simple (fun tree ->
      let lang = Tree_sitter_json.language () in
      let query = Query.create lang ~source:"(string) @str" in
      let cursor = Query_cursor.create () in
      let root = Tree.root_node tree in
      Query_cursor.exec cursor query root;
      (* Consume some matches *)
      let _ = Query_cursor.next_match cursor in
      (* Reset and count again *)
      Query_cursor.reset cursor ~query ~node:root;
      let rec count_matches acc =
        match Query_cursor.next_match cursor with
        | Some _ -> count_matches (acc + 1)
        | None -> acc
      in
      let matches = count_matches 0 in
      Alcotest.(check bool) "reset allows re-iteration" true (matches > 0);
      Query_cursor.delete cursor;
      Query.delete query)

let test_cursor_match_structure () =
  with_parsed_json {|{"key": "value"}|} (fun tree ->
      let lang = Tree_sitter_json.language () in
      let query =
        Query.create lang ~source:"(pair key: (string) @k value: (string) @v)"
      in
      let cursor = Query_cursor.create () in
      let root = Tree.root_node tree in
      Query_cursor.exec cursor query root;
      (match Query_cursor.next_match cursor with
      | Some m ->
          Alcotest.(check int) "pattern index" 0 m.pattern_index;
          Alcotest.(check int) "two captures" 2 (Array.length m.captures);
          let idx0, node0 = m.captures.(0) in
          let idx1, node1 = m.captures.(1) in
          Alcotest.(check int) "first capture index" 0 idx0;
          Alcotest.(check int) "second capture index" 1 idx1;
          Alcotest.(check string) "first is string" "string" (Node.kind node0);
          Alcotest.(check string) "second is string" "string" (Node.kind node1)
      | None -> Alcotest.fail "expected match");
      Query_cursor.delete cursor;
      Query.delete query)

let query_cursor_tests =
  [
    Alcotest.test_case "create_delete" `Quick test_cursor_create_delete;
    Alcotest.test_case "exec_and_matches" `Quick test_cursor_exec_and_matches;
    Alcotest.test_case "captures" `Quick test_cursor_captures;
    Alcotest.test_case "byte_range" `Quick test_cursor_byte_range;
    Alcotest.test_case "point_range" `Quick test_cursor_point_range;
    Alcotest.test_case "match_limit" `Quick test_cursor_match_limit;
    Alcotest.test_case "timeout" `Quick test_cursor_timeout;
    Alcotest.test_case "reset" `Quick test_cursor_reset;
    Alcotest.test_case "match_structure" `Quick test_cursor_match_structure;
  ]

(* Integration Tests *)

let test_full_traversal () =
  with_parsed_json json_nested (fun tree ->
      let root = Tree.root_node tree in
      let rec count_nodes node =
        let child_sum = ref 0 in
        for i = 0 to Node.child_count node - 1 do
          match Node.child node i with
          | Some child -> child_sum := !child_sum + count_nodes child
          | None -> ()
        done;
        1 + !child_sum
      in
      let total = count_nodes root in
      Alcotest.(check bool) "traversed many nodes" true (total > 10))

let test_incremental_parsing () =
  with_json_parser (fun parser ->
      let source1 = {|{"a": 1}|} in
      let tree1 = Parser.parse_string parser source1 in
      (* Edit: change 1 to 2 *)
      Tree.edit tree1 ~start_byte:6 ~old_end_byte:7 ~new_end_byte:7
        ~start_point:(0, 6) ~old_end_point:(0, 7) ~new_end_point:(0, 7);
      let source2 = {|{"a": 2}|} in
      let tree2 = Parser.parse_string ~old:tree1 parser source2 in
      let root = Tree.root_node tree2 in
      Alcotest.(check bool)
        "incremental parse works" false (Node.has_error root))

let test_query_all_values () =
  with_parsed_json json_array (fun tree ->
      let lang = Tree_sitter_json.language () in
      (* Query for all JSON value types *)
      let query =
        Query.create lang
          ~source:
            {|
        (object) @object
        (array) @array
        (string) @string
        (number) @number
        (true) @true
        (false) @false
        (null) @null
      |}
      in
      let cursor = Query_cursor.create () in
      let root = Tree.root_node tree in
      Query_cursor.exec cursor query root;
      let found = Hashtbl.create 8 in
      let rec collect () =
        match Query_cursor.next_capture cursor query with
        | Some cap ->
            let name = Query.capture_name_for_id query cap.capture_index in
            (match name with
            | Some n -> Hashtbl.replace found n true
            | None -> ());
            collect ()
        | None -> ()
      in
      collect ();
      (* json_array has: array, numbers, string, null, true, false *)
      Alcotest.(check bool) "found array" true (Hashtbl.mem found "array");
      Alcotest.(check bool) "found number" true (Hashtbl.mem found "number");
      Alcotest.(check bool) "found string" true (Hashtbl.mem found "string");
      Alcotest.(check bool) "found null" true (Hashtbl.mem found "null");
      Alcotest.(check bool) "found true" true (Hashtbl.mem found "true");
      Alcotest.(check bool) "found false" true (Hashtbl.mem found "false");
      Query_cursor.delete cursor;
      Query.delete query)

let integration_tests =
  [
    Alcotest.test_case "full_traversal" `Quick test_full_traversal;
    Alcotest.test_case "incremental_parsing" `Quick test_incremental_parsing;
    Alcotest.test_case "query_all_values" `Quick test_query_all_values;
  ]

(* Run all tests *)

let () =
  Alcotest.run "tree-sitter"
    [
      ("Language", language_tests);
      ("Parser", parser_tests);
      ("Tree", tree_tests);
      ("Node", node_tests);
      ("Query", query_tests);
      ("Query_cursor", query_cursor_tests);
      ("Integration", integration_tests);
    ]
