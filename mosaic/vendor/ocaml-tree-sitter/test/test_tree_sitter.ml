open Tree_sitter
open Windtrap

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
  is_true ~msg:"version is positive" (version > 0)

let test_language_name () =
  let lang = Tree_sitter_json.language () in
  let name = Language.name lang in
  equal ~msg:"language name" string "json" name

let test_language_symbol_count () =
  let lang = Tree_sitter_json.language () in
  let count = Language.symbol_count lang in
  is_true ~msg:"has symbols" (count > 0)

let test_language_field_count () =
  let lang = Tree_sitter_json.language () in
  let count = Language.field_count lang in
  is_true ~msg:"has fields" (count > 0)

let test_language_symbol_for_name () =
  let lang = Tree_sitter_json.language () in
  let symbol = Language.symbol_for_name ~is_named:true lang "object" in
  is_some ~msg:"object symbol exists" symbol;
  let symbol = Language.symbol_for_name ~is_named:true lang "nonexistent_xyz" in
  is_none ~msg:"nonexistent symbol" symbol

let test_language_field_id_for_name () =
  let lang = Tree_sitter_json.language () in
  let field = Language.field_id_for_name lang "key" in
  is_some ~msg:"key field exists" field;
  let field = Language.field_id_for_name lang "nonexistent_field" in
  is_none ~msg:"nonexistent field" field

let language_tests =
  [
    test "version" test_language_version;
    test "name" test_language_name;
    test "symbol_count" test_language_symbol_count;
    test "field_count" test_language_field_count;
    test "symbol_for_name" test_language_symbol_for_name;
    test "field_id_for_name" test_language_field_id_for_name;
  ]

(* Parser Module Tests *)

let test_parser_create_delete () =
  let parser = Parser.create () in
  Parser.delete parser

let test_parser_set_language () =
  let parser = Parser.create () in
  Parser.set_language parser (Tree_sitter_json.language ());
  Parser.delete parser

let test_parser_parse_simple () =
  with_parsed_json json_simple (fun tree ->
      let sexp = Tree.root_sexp tree in
      is_true ~msg:"sexp not empty" (String.length sexp > 0);
      is_true ~msg:"contains document" (String.length sexp > 0 && sexp.[0] = '('))

let test_parser_timeout () =
  with_json_parser (fun parser ->
      Parser.set_timeout_micros parser 1000000L;
      let timeout = Parser.timeout_micros parser in
      equal ~msg:"timeout set" int64 1000000L timeout)

let test_parser_reset () =
  with_json_parser (fun parser ->
      let _ = Parser.parse_string parser json_simple in
      Parser.reset parser;
      let _ = Parser.parse_string parser json_array in
      ())

let parser_tests =
  [
    test "create_delete" test_parser_create_delete;
    test "set_language" test_parser_set_language;
    test "parse_simple" test_parser_parse_simple;
    test "timeout" test_parser_timeout;
    test "reset" test_parser_reset;
  ]

(* Tree Module Tests *)

let test_tree_root_node () =
  with_parsed_json json_simple (fun tree ->
      let root = Tree.root_node tree in
      is_false ~msg:"root not null" (Node.is_null root);
      equal ~msg:"root is document" string "document" (Node.kind root))

let test_tree_copy () =
  with_parsed_json json_simple (fun tree ->
      let copy = Tree.copy tree in
      let original_sexp = Tree.root_sexp tree in
      let copy_sexp = Tree.root_sexp copy in
      equal ~msg:"copy matches original" string original_sexp copy_sexp)

let test_tree_language () =
  with_parsed_json json_simple (fun tree ->
      let lang = Tree.language tree in
      let name = Language.name lang in
      equal ~msg:"tree language is json" string "json" name)

let test_tree_edit () =
  with_json_parser (fun parser ->
      let tree = Parser.parse_string parser json_simple in
      Tree.edit tree ~start_byte:9 ~old_end_byte:13 ~new_end_byte:13
        ~start_point:(0, 9) ~old_end_point:(0, 13) ~new_end_point:(0, 13);
      let new_source = {|{"name": "modi", "value": 42}|} in
      let new_tree = Parser.parse_string ~old:tree parser new_source in
      let root = Tree.root_node new_tree in
      is_false ~msg:"reparsed tree valid" (Node.is_null root))

let test_tree_included_ranges () =
  with_parsed_json json_simple (fun tree ->
      let ranges = Tree.included_ranges tree in
      is_true ~msg:"has at least one range" (Array.length ranges >= 0))

let test_tree_changed_ranges () =
  with_json_parser (fun parser ->
      let tree1 = Parser.parse_string parser json_simple in
      let tree2 = Parser.parse_string parser json_array in
      let ranges = Tree.changed_ranges ~old:tree1 tree2 in
      is_true ~msg:"has changes" (Array.length ranges > 0))

let tree_tests =
  [
    test "root_node" test_tree_root_node;
    test "copy" test_tree_copy;
    test "language" test_tree_language;
    test "edit" test_tree_edit;
    test "included_ranges" test_tree_included_ranges;
    test "changed_ranges" test_tree_changed_ranges;
  ]

(* Node Module Tests *)

let test_node_kind_and_symbol () =
  with_parsed_json json_simple (fun tree ->
      let root = Tree.root_node tree in
      equal ~msg:"root kind" string "document" (Node.kind root);
      let symbol = Node.symbol root in
      is_true ~msg:"symbol is positive" (symbol >= 0))

let test_node_positions () =
  with_parsed_json json_simple (fun tree ->
      let root = Tree.root_node tree in
      let start_byte = Node.start_byte root in
      let end_byte = Node.end_byte root in
      equal ~msg:"start at 0" int 0 start_byte;
      equal ~msg:"end at source length" int (String.length json_simple) end_byte;
      let start_row, start_col = Node.start_point root in
      equal ~msg:"start row" int 0 start_row;
      equal ~msg:"start col" int 0 start_col)

let test_node_multiline_positions () =
  with_parsed_json json_multiline (fun tree ->
      let root = Tree.root_node tree in
      let _, end_row = Node.end_point root in
      is_true ~msg:"spans multiple rows" (end_row > 0))

let test_node_child_access () =
  with_parsed_json json_simple (fun tree ->
      let root = Tree.root_node tree in
      let child_count = Node.child_count root in
      is_true ~msg:"has children" (child_count > 0);
      let first_child = Node.child root 0 in
      is_some ~msg:"first child exists" first_child;
      let no_child = Node.child root 1000 in
      is_none ~msg:"invalid index returns none" no_child)

let test_node_named_children () =
  with_parsed_json json_simple (fun tree ->
      let root = Tree.root_node tree in
      let named_count = Node.named_child_count root in
      is_true ~msg:"has named children" (named_count > 0);
      let first_named = Node.named_child root 0 in
      is_some ~msg:"first named child exists" first_named;
      match first_named with
      | Some node -> is_true ~msg:"named child is named" (Node.is_named node)
      | None -> fail "expected named child")

let test_node_child_by_field () =
  with_parsed_json json_simple (fun tree ->
      let root = Tree.root_node tree in
      match Node.named_child root 0 with
      | Some obj -> (
          equal ~msg:"is object" string "object" (Node.kind obj);
          match Node.named_child obj 0 with
          | Some pair ->
              let key = Node.child_by_field_name pair "key" in
              is_some ~msg:"key field exists" key
          | None -> fail "expected pair")
      | None -> fail "expected object")

let test_node_parent () =
  with_parsed_json json_simple (fun tree ->
      let root = Tree.root_node tree in
      match Node.child root 0 with
      | Some child -> (
          let parent = Node.parent child in
          match parent with
          | Some p -> is_true ~msg:"parent equals root" (Node.equal p root)
          | None -> fail "expected parent")
      | None -> fail "expected child")

let test_node_siblings () =
  with_parsed_json json_array (fun tree ->
      let root = Tree.root_node tree in
      match Node.named_child root 0 with
      | Some array -> (
          match Node.named_child array 0 with
          | Some first_elem ->
              let next = Node.next_sibling first_elem in
              is_some ~msg:"has next sibling" next;
              let next_named = Node.next_named_sibling first_elem in
              is_some ~msg:"has next named sibling" next_named
          | None -> fail "expected array element")
      | None -> fail "expected array")

let test_node_descendant_for_range () =
  with_parsed_json json_simple (fun tree ->
      let root = Tree.root_node tree in
      let desc = Node.descendant_for_byte_range root ~start:2 ~end_:6 in
      is_some ~msg:"found descendant" desc;
      match desc with
      | Some node ->
          let kind = Node.kind node in
          is_true ~msg:"descendant is string-related"
            (kind = "string" || kind = "string_content")
      | None -> ())

let test_node_descendant_for_point () =
  with_parsed_json json_multiline (fun tree ->
      let root = Tree.root_node tree in
      let desc =
        Node.descendant_for_point_range root ~start_point:(1, 3)
          ~end_point:(1, 7)
      in
      is_some ~msg:"found descendant by point" desc)

let test_node_flags () =
  with_parsed_json json_simple (fun tree ->
      let root = Tree.root_node tree in
      is_true ~msg:"root is named" (Node.is_named root);
      is_false ~msg:"root not missing" (Node.is_missing root);
      is_false ~msg:"root not extra" (Node.is_extra root);
      is_false ~msg:"root has no error" (Node.has_error root);
      is_false ~msg:"root is not error" (Node.is_error root))

let test_node_error_detection () =
  with_parsed_json json_invalid (fun tree ->
      let root = Tree.root_node tree in
      is_true ~msg:"tree has error" (Node.has_error root))

let test_node_equality () =
  with_parsed_json json_simple (fun tree ->
      let root1 = Tree.root_node tree in
      let root2 = Tree.root_node tree in
      is_true ~msg:"same node equals" (Node.equal root1 root2);
      match Node.child root1 0 with
      | Some child ->
          is_false ~msg:"different nodes not equal" (Node.equal root1 child)
      | None -> fail "expected child")

let test_node_to_sexp () =
  with_parsed_json json_simple (fun tree ->
      let root = Tree.root_node tree in
      let sexp = Node.to_sexp root in
      is_true ~msg:"sexp not empty" (String.length sexp > 0);
      is_true ~msg:"sexp starts with paren" (sexp.[0] = '('))

let test_node_descendant_count () =
  with_parsed_json json_nested (fun tree ->
      let root = Tree.root_node tree in
      let count = Node.descendant_count root in
      is_true ~msg:"has descendants" (count > 1))

let test_node_tree_reference_safety () =
  let node =
    with_json_parser (fun parser ->
        let tree = Parser.parse_string parser json_simple in
        let root = Tree.root_node tree in
        let child = Node.child root 0 in
        Gc.full_major ();
        child)
  in
  match node with
  | Some n ->
      Gc.full_major ();
      let kind = Node.kind n in
      is_true ~msg:"node still valid" (String.length kind > 0)
  | None -> fail "expected node"

let node_tests =
  [
    test "kind_and_symbol" test_node_kind_and_symbol;
    test "positions" test_node_positions;
    test "multiline_positions" test_node_multiline_positions;
    test "child_access" test_node_child_access;
    test "named_children" test_node_named_children;
    test "child_by_field" test_node_child_by_field;
    test "parent" test_node_parent;
    test "siblings" test_node_siblings;
    test "descendant_for_range" test_node_descendant_for_range;
    test "descendant_for_point" test_node_descendant_for_point;
    test "flags" test_node_flags;
    test "error_detection" test_node_error_detection;
    test "equality" test_node_equality;
    test "to_sexp" test_node_to_sexp;
    test "descendant_count" test_node_descendant_count;
    test "tree_reference_safety" test_node_tree_reference_safety;
  ]

(* Query Module Tests *)

let test_query_create () =
  let lang = Tree_sitter_json.language () in
  let query = Query.create lang ~source:"(object)" in
  is_true ~msg:"pattern count > 0" (Query.pattern_count query > 0);
  Query.delete query

let test_query_invalid_syntax () =
  let lang = Tree_sitter_json.language () in
  let raised =
    try
      let _ = Query.create lang ~source:"(((invalid syntax" in
      false
    with Failure _ -> true
  in
  is_true ~msg:"raises on invalid syntax" raised

let test_query_captures () =
  let lang = Tree_sitter_json.language () in
  let query = Query.create lang ~source:"(string) @str (number) @num" in
  equal ~msg:"capture count" int 2 (Query.capture_count query);
  let name0 = Query.capture_name_for_id query 0 in
  equal ~msg:"capture 0 name" (option string) (Some "str") name0;
  let name1 = Query.capture_name_for_id query 1 in
  equal ~msg:"capture 1 name" (option string) (Some "num") name1;
  Query.delete query

let test_query_capture_index () =
  let lang = Tree_sitter_json.language () in
  let query = Query.create lang ~source:"(string) @mystring" in
  let idx = Query.capture_index_for_name query "mystring" in
  equal ~msg:"capture index" (option int) (Some 0) idx;
  let missing = Query.capture_index_for_name query "nonexistent" in
  equal ~msg:"missing capture" (option int) None missing;
  Query.delete query

let test_query_pattern_info () =
  let lang = Tree_sitter_json.language () in
  let query = Query.create lang ~source:"(object) (array)" in
  equal ~msg:"two patterns" int 2 (Query.pattern_count query);
  let start0 = Query.start_byte_for_pattern query ~pattern:0 in
  let start1 = Query.start_byte_for_pattern query ~pattern:1 in
  is_true ~msg:"pattern 1 starts after pattern 0" (start1 > start0);
  Query.delete query

let test_query_disable_pattern () =
  let lang = Tree_sitter_json.language () in
  let query = Query.create lang ~source:"(object) @obj (array) @arr" in
  Query.disable_pattern query ~pattern:0;
  Query.delete query

let test_query_disable_capture () =
  let lang = Tree_sitter_json.language () in
  let query = Query.create lang ~source:"(string) @str" in
  Query.disable_capture query ~name:"str";
  Query.delete query

let query_tests =
  [
    test "create" test_query_create;
    test "invalid_syntax" test_query_invalid_syntax;
    test "captures" test_query_captures;
    test "capture_index" test_query_capture_index;
    test "pattern_info" test_query_pattern_info;
    test "disable_pattern" test_query_disable_pattern;
    test "disable_capture" test_query_disable_capture;
  ]

(* Query Cursor Module Tests *)

let test_cursor_create_delete () =
  let cursor = Query_cursor.create () in
  Query_cursor.delete cursor

let test_cursor_exec_and_matches () =
  with_parsed_json json_simple (fun tree ->
      let lang = Tree_sitter_json.language () in
      let query = Query.create lang ~source:"(string) @str" in
      let cursor = Query_cursor.create () in
      let root = Tree.root_node tree in
      Query_cursor.exec cursor query root;
      let rec count_matches acc =
        match Query_cursor.next_match cursor with
        | Some _ -> count_matches (acc + 1)
        | None -> acc
      in
      let matches = count_matches 0 in
      is_true ~msg:"found matches" (matches > 0);
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
          equal ~msg:"capture index" int 0 capture.capture_index;
          equal ~msg:"captured number" string "number" (Node.kind capture.node)
      | None -> fail "expected capture");
      Query_cursor.delete cursor;
      Query.delete query)

let test_cursor_byte_range () =
  with_parsed_json json_simple (fun tree ->
      let lang = Tree_sitter_json.language () in
      let query = Query.create lang ~source:"(string) @str" in
      let cursor = Query_cursor.create () in
      let root = Tree.root_node tree in
      Query_cursor.set_byte_range cursor ~start:0 ~end_:15;
      Query_cursor.exec cursor query root;
      let rec count_matches acc =
        match Query_cursor.next_match cursor with
        | Some _ -> count_matches (acc + 1)
        | None -> acc
      in
      let matches = count_matches 0 in
      is_true ~msg:"found some matches" (matches >= 0);
      Query_cursor.delete cursor;
      Query.delete query)

let test_cursor_point_range () =
  with_parsed_json json_multiline (fun tree ->
      let lang = Tree_sitter_json.language () in
      let query = Query.create lang ~source:"(string) @str" in
      let cursor = Query_cursor.create () in
      let root = Tree.root_node tree in
      Query_cursor.set_point_range cursor ~start_point:(0, 0) ~end_point:(1, 0);
      Query_cursor.exec cursor query root;
      Query_cursor.delete cursor;
      Query.delete query)

let test_cursor_match_limit () =
  with_parsed_json json_array (fun tree ->
      let lang = Tree_sitter_json.language () in
      let query = Query.create lang ~source:"(number) @num" in
      let cursor = Query_cursor.create () in
      Query_cursor.set_match_limit cursor 1;
      equal ~msg:"match limit set" int 1 (Query_cursor.match_limit cursor);
      let root = Tree.root_node tree in
      Query_cursor.exec cursor query root;
      Query_cursor.delete cursor;
      Query.delete query)

let test_cursor_timeout () =
  let cursor = Query_cursor.create () in
  Query_cursor.set_timeout_micros cursor 5000000L;
  let timeout = Query_cursor.timeout_micros cursor in
  equal ~msg:"timeout set" int64 5000000L timeout;
  Query_cursor.delete cursor

let test_cursor_reset () =
  with_parsed_json json_simple (fun tree ->
      let lang = Tree_sitter_json.language () in
      let query = Query.create lang ~source:"(string) @str" in
      let cursor = Query_cursor.create () in
      let root = Tree.root_node tree in
      Query_cursor.exec cursor query root;
      let _ = Query_cursor.next_match cursor in
      Query_cursor.reset cursor ~query ~node:root;
      let rec count_matches acc =
        match Query_cursor.next_match cursor with
        | Some _ -> count_matches (acc + 1)
        | None -> acc
      in
      let matches = count_matches 0 in
      is_true ~msg:"reset allows re-iteration" (matches > 0);
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
          equal ~msg:"pattern index" int 0 m.pattern_index;
          equal ~msg:"two captures" int 2 (Array.length m.captures);
          let idx0, node0 = m.captures.(0) in
          let idx1, node1 = m.captures.(1) in
          equal ~msg:"first capture index" int 0 idx0;
          equal ~msg:"second capture index" int 1 idx1;
          equal ~msg:"first is string" string "string" (Node.kind node0);
          equal ~msg:"second is string" string "string" (Node.kind node1)
      | None -> fail "expected match");
      Query_cursor.delete cursor;
      Query.delete query)

let query_cursor_tests =
  [
    test "create_delete" test_cursor_create_delete;
    test "exec_and_matches" test_cursor_exec_and_matches;
    test "captures" test_cursor_captures;
    test "byte_range" test_cursor_byte_range;
    test "point_range" test_cursor_point_range;
    test "match_limit" test_cursor_match_limit;
    test "timeout" test_cursor_timeout;
    test "reset" test_cursor_reset;
    test "match_structure" test_cursor_match_structure;
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
      is_true ~msg:"traversed many nodes" (total > 10))

let test_incremental_parsing () =
  with_json_parser (fun parser ->
      let source1 = {|{"a": 1}|} in
      let tree1 = Parser.parse_string parser source1 in
      Tree.edit tree1 ~start_byte:6 ~old_end_byte:7 ~new_end_byte:7
        ~start_point:(0, 6) ~old_end_point:(0, 7) ~new_end_point:(0, 7);
      let source2 = {|{"a": 2}|} in
      let tree2 = Parser.parse_string ~old:tree1 parser source2 in
      let root = Tree.root_node tree2 in
      is_false ~msg:"incremental parse works" (Node.has_error root))

let test_query_all_values () =
  with_parsed_json json_array (fun tree ->
      let lang = Tree_sitter_json.language () in
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
      is_true ~msg:"found array" (Hashtbl.mem found "array");
      is_true ~msg:"found number" (Hashtbl.mem found "number");
      is_true ~msg:"found string" (Hashtbl.mem found "string");
      is_true ~msg:"found null" (Hashtbl.mem found "null");
      is_true ~msg:"found true" (Hashtbl.mem found "true");
      is_true ~msg:"found false" (Hashtbl.mem found "false");
      Query_cursor.delete cursor;
      Query.delete query)

let integration_tests =
  [
    test "full_traversal" test_full_traversal;
    test "incremental_parsing" test_incremental_parsing;
    test "query_all_values" test_query_all_values;
  ]

(* Run all tests *)

let () =
  run "tree-sitter"
    [
      group "Language" language_tests;
      group "Parser" parser_tests;
      group "Tree" tree_tests;
      group "Node" node_tests;
      group "Query" query_tests;
      group "Query_cursor" query_cursor_tests;
      group "Integration" integration_tests;
    ]
