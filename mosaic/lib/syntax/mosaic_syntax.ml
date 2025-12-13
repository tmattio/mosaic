type filetype = string

module Injection_mapping = struct
  type t = {
    node_types : (string, filetype) Hashtbl.t;
    info_string_map : (string, filetype) Hashtbl.t;
  }
end

module Language = struct
  type t = {
    filetype : filetype;
    ts_language : Tree_sitter.Language.t;
    highlight_query : Tree_sitter.Query.t;
    injection_query : Tree_sitter.Query.t option;
    injection_mapping : Injection_mapping.t option;
  }
end

module Highlight = struct
  type meta = {
    is_injection : bool;
    injection_lang : filetype option;
    contains_injection : bool;
    conceal : string option;
    conceal_lines : string option;
  }

  type t = {
    start_offset : int;
    end_offset : int;
    group : string;
    meta : meta option;
  }
end

type options = { parsers : Language.t list }
type parser_entry = { lang : Language.t; parser : Tree_sitter.Parser.t }
type t = { languages : (filetype, parser_entry) Hashtbl.t }

let create_parser (lang : Language.t) =
  let parser = Tree_sitter.Parser.create () in
  Tree_sitter.Parser.set_language parser lang.ts_language;
  { lang; parser }

let create { parsers } =
  let table = Hashtbl.create (max 8 (List.length parsers)) in
  List.iter
    (fun lang ->
      let entry = create_parser lang in
      Hashtbl.replace table lang.Language.filetype entry)
    parsers;
  { languages = table }

let register_language t (lang : Language.t) =
  let entry =
    match Hashtbl.find_opt t.languages lang.Language.filetype with
    | None -> create_parser lang
    | Some existing ->
        Tree_sitter.Parser.set_language existing.parser lang.ts_language;
        { existing with lang }
  in
  Hashtbl.replace t.languages lang.Language.filetype entry

let has_language t filetype = Hashtbl.mem t.languages filetype

let highlight_once t ~filetype ~content =
  match Hashtbl.find_opt t.languages filetype with
  | None ->
      Error (Printf.sprintf " no language registered for filetype %S" filetype)
  | Some entry ->
      let open Language in
      let tree = Tree_sitter.Parser.parse_string entry.parser content in
      let root = Tree_sitter.Tree.root_node tree in
      let cursor = Tree_sitter.Query_cursor.create () in
      (* Restrict to full document range. *)
      Tree_sitter.Query_cursor.set_byte_range cursor ~start:0
        ~end_:(String.length content);
      Tree_sitter.Query_cursor.exec cursor entry.lang.highlight_query root;
      let rec gather acc =
        match
          Tree_sitter.Query_cursor.next_capture cursor
            entry.lang.highlight_query
        with
        | None -> acc
        | Some capture ->
            let acc =
              match
                Tree_sitter.Query.capture_name_for_id entry.lang.highlight_query
                  capture.capture_index
              with
              | None -> acc
              | Some group ->
                  let node = capture.node in
                  let start_offset = Tree_sitter.Node.start_byte node in
                  let end_offset = Tree_sitter.Node.end_byte node in
                  if start_offset < end_offset then
                    let h : Highlight.t =
                      { start_offset; end_offset; group; meta = None }
                    in
                    h :: acc
                  else acc
            in
            gather acc
      in
      let highlights = gather [] in
      Tree_sitter.Query_cursor.delete cursor;
      let arr = Array.of_list highlights in
      Array.sort
        (fun a b ->
          match Int.compare a.Highlight.start_offset b.start_offset with
          | 0 -> Int.compare a.end_offset b.end_offset
          | c -> c)
        arr;
      Ok arr

(* -------------------------------------------------------------------------- *)
(* Default, process-wide client *)

let default_client_ref : t option ref = ref None

let make_default_parsers () : Language.t list =
  let ocaml_language () =
    let ts_language = Tree_sitter_ocaml.ocaml () in
    let highlight_query =
      Tree_sitter.Query.create ts_language
        ~source:
          {|
      (comment) @comment
      (string) @string
      (character) @string
      (constructor_name) @type
      (type_constructor) @type
      (module_name) @module
      ["let" "in" "match" "with" "function" "fun" "if" "then" "else" "type" "module" "open" "struct" "end" "sig" "val" "and" "rec" "of" "true" "false"] @keyword
      (value_name) @variable
      (number) @number
      |}
    in
    Language.
      {
        filetype = "ml";
        ts_language;
        highlight_query;
        injection_query = None;
        injection_mapping = None;
      }
  in
  let json_language () =
    let ts_language = Tree_sitter_json.language () in
    let highlight_query =
      Tree_sitter.Query.create ts_language
        ~source:
          {|
      (string) @string
      (number) @number
      (null) @constant
      (true) @constant
      (false) @constant
      (pair key: (string) @property)
      |}
    in
    Language.
      {
        filetype = "json";
        ts_language;
        highlight_query;
        injection_query = None;
        injection_mapping = None;
      }
  in
  [ ocaml_language (); json_language () ]

let default_client () =
  match !default_client_ref with
  | Some client -> client
  | None ->
      let client = create { parsers = make_default_parsers () } in
      default_client_ref := Some client;
      client
