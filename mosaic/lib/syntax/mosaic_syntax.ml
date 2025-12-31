type filetype = string

type error =
  | Unknown_filetype of filetype
  | Query_compile_failed of {
      filetype : filetype;
      query : [ `Highlights | `Injections ];
      message : string;
    }

module Highlight = struct
  type origin = [ `Host | `Injection of filetype ]
  type t = { start_byte : int; end_byte : int; group : string; origin : origin }

  let compare a b =
    match Int.compare a.start_byte b.start_byte with
    | 0 -> Int.compare a.end_byte b.end_byte
    | c -> c
end

module Language = struct
  type t = {
    filetype : filetype;
    ts_language : Tree_sitter.Language.t;
    highlight_query : Tree_sitter.Query.t;
    injection_query : Tree_sitter.Query.t option;
    capture_groups : string list;
  }

  let filetype t = t.filetype
  let ts_language t = t.ts_language
  let capture_groups t = t.capture_groups

  let compile_query ~filetype ~query_kind ts_language source =
    try Ok (Tree_sitter.Query.create ts_language ~source)
    with exn ->
      Error
        (Query_compile_failed
           { filetype; query = query_kind; message = Printexc.to_string exn })

  let compute_capture_groups (q : Tree_sitter.Query.t) =
    (* This assumes bindings expose [Tree_sitter.Query.capture_count]. If your
       binding uses a different name, adjust here. *)
    let count = try Tree_sitter.Query.capture_count q with _ -> 0 in
    let tbl = Hashtbl.create (max 8 count) in
    for i = 0 to count - 1 do
      match Tree_sitter.Query.capture_name_for_id q i with
      | None -> ()
      | Some name -> if name <> "" then Hashtbl.replace tbl name ()
    done;
    let names = Hashtbl.fold (fun k () acc -> k :: acc) tbl [] in
    List.sort_uniq String.compare names

  let make ~filetype ~ts_language ~highlights ?injections () =
    match
      compile_query ~filetype ~query_kind:`Highlights ts_language highlights
    with
    | Error _ as e -> e
    | Ok highlight_query ->
        (match injections with
          | None -> Ok None
          | Some src ->
              compile_query ~filetype ~query_kind:`Injections ts_language src
              |> Result.map Option.some)
        |> Result.map (fun injection_query ->
            let capture_groups = compute_capture_groups highlight_query in
            {
              filetype;
              ts_language;
              highlight_query;
              injection_query;
              capture_groups;
            })

  let make_exn ~filetype ~ts_language ~highlights ?injections () =
    match make ~filetype ~ts_language ~highlights ?injections () with
    | Ok x -> x
    | Error (Query_compile_failed { message; _ }) ->
        invalid_arg
          (Printf.sprintf "Mosaic_syntax.Language.make_exn: %s" message)
    | Error (Unknown_filetype ft) ->
        invalid_arg
          (Printf.sprintf "Mosaic_syntax.Language.make_exn: unknown filetype %S"
             ft)
end

module Set = struct
  type resolve_injection = host:filetype -> selector:string -> filetype option

  type t = {
    table : (filetype, Language.t) Hashtbl.t;
    languages : Language.t list;
    resolve_injection : resolve_injection;
  }

  let default_resolve_injection ~host:_ ~selector =
    let s = String.trim selector in
    if s = "" then None
    else
      let first =
        (* first whitespace-separated token *)
        let len = String.length s in
        let rec find_ws i =
          if i >= len then len
          else
            match s.[i] with
            | ' ' | '\t' | '\n' | '\r' -> i
            | _ -> find_ws (i + 1)
        in
        let j = find_ws 0 in
        String.sub s 0 j
      in
      let ft = String.lowercase_ascii first in
      if ft = "" then None else Some ft

  let create ?(resolve_injection = default_resolve_injection) langs =
    let table = Hashtbl.create (max 8 (List.length langs)) in
    List.iter
      (fun (l : Language.t) -> Hashtbl.replace table (Language.filetype l) l)
      langs;

    (* Return a stable “last wins” list of unique languages by filetype. *)
    let seen = Hashtbl.create (max 8 (List.length langs)) in
    let unique_rev =
      List.fold_left
        (fun acc (l : Language.t) ->
          let ft = Language.filetype l in
          if Hashtbl.mem seen ft then acc
          else (
            Hashtbl.replace seen ft ();
            l :: acc))
        [] (List.rev langs)
    in
    let languages = List.rev unique_rev in
    { table; languages; resolve_injection }

  let languages t = t.languages
  let find t ft = Hashtbl.find_opt t.table ft
  let resolve_injection t ~host ~selector = t.resolve_injection ~host ~selector
end

module Session = struct
  type t = {
    set : Set.t;
    filetype : filetype;
    lang : Language.t;
    parser : Tree_sitter.Parser.t;
    cursor : Tree_sitter.Query_cursor.t;
    mutable tree : Tree_sitter.Tree.t option;
    injection_parsers : (filetype, Tree_sitter.Parser.t) Hashtbl.t;
    mutable closed : bool;
  }

  let filetype t = t.filetype

  let create set ~filetype =
    match Set.find set filetype with
    | None -> Error (Unknown_filetype filetype)
    | Some lang ->
        let parser = Tree_sitter.Parser.create () in
        Tree_sitter.Parser.set_language parser (Language.ts_language lang);
        let cursor = Tree_sitter.Query_cursor.create () in
        Ok
          {
            set;
            filetype;
            lang;
            parser;
            cursor;
            tree = None;
            injection_parsers = Hashtbl.create 8;
            closed = false;
          }

  let create_exn set ~filetype =
    match create set ~filetype with
    | Ok s -> s
    | Error (Unknown_filetype ft) ->
        invalid_arg
          (Printf.sprintf
             "Mosaic_syntax.Session.create_exn: unknown filetype %S" ft)
    | Error (Query_compile_failed { message; _ }) ->
        invalid_arg
          (Printf.sprintf "Mosaic_syntax.Session.create_exn: %s" message)

  let reset t = t.tree <- None

  let close t =
    if not t.closed then (
      t.closed <- true;
      t.tree <- None;
      Tree_sitter.Query_cursor.delete t.cursor;
      Tree_sitter.Parser.delete t.parser;
      Hashtbl.iter (fun _ p -> Tree_sitter.Parser.delete p) t.injection_parsers;
      Hashtbl.reset t.injection_parsers)

  let capture_name query index =
    Tree_sitter.Query.capture_name_for_id query index

  let should_skip_capture ~include_private name =
    name = ""
    || ((not include_private) && String.length name > 0 && name.[0] = '_')

  let slice_bytes (s : string) (start_ : int) (end_ : int) =
    let len = String.length s in
    let start_ = max 0 (min start_ len) in
    let end_ = max 0 (min end_ len) in
    if end_ <= start_ then "" else String.sub s start_ (end_ - start_)

  let gather_highlights_with_cursor ~cursor ~query ~root ~content ~origin
      ~base_offset ~include_private =
    Tree_sitter.Query_cursor.set_byte_range cursor ~start:0
      ~end_:(String.length content);
    Tree_sitter.Query_cursor.exec cursor query root;
    let rec loop acc =
      match Tree_sitter.Query_cursor.next_capture cursor query with
      | None -> acc
      | Some cap -> (
          match capture_name query cap.capture_index with
          | Some name when not (should_skip_capture ~include_private name) ->
              let node = cap.node in
              let s = Tree_sitter.Node.start_byte node in
              let e = Tree_sitter.Node.end_byte node in
              if s < e then
                loop
                  ({
                     Highlight.start_byte = base_offset + s;
                     end_byte = base_offset + e;
                     group = name;
                     origin;
                   }
                  :: acc)
              else loop acc
          | _ -> loop acc)
    in
    loop []

  (* --- Injection match iteration ---------------------------------------- *)

  type injection = { start_byte : int; end_byte : int; selector : string }

  let gather_injections (lang : Language.t) ~(content : string)
      ~(root : Tree_sitter.Node.t) : injection list =
    match lang.Language.injection_query with
    | None -> []
    | Some inj_query ->
        let cursor = Tree_sitter.Query_cursor.create () in
        Tree_sitter.Query_cursor.set_byte_range cursor ~start:0
          ~end_:(String.length content);
        Tree_sitter.Query_cursor.exec cursor inj_query root;

        let rec loop acc =
          match Tree_sitter.Query_cursor.next_match cursor with
          | None -> acc
          | Some m ->
              let content_node = ref None in
              let lang_node = ref None in
              Array.iter
                (fun (capture_idx, node) ->
                  match capture_name inj_query capture_idx with
                  | Some "injection.content" ->
                      if !content_node = None then content_node := Some node
                  | Some "injection.language" ->
                      if !lang_node = None then lang_node := Some node
                  | _ -> ())
                m.captures;

              let acc =
                match (!content_node, !lang_node) with
                | Some cnode, Some lnode ->
                    let s = Tree_sitter.Node.start_byte cnode in
                    let e = Tree_sitter.Node.end_byte cnode in
                    if s < e then
                      let ls = Tree_sitter.Node.start_byte lnode in
                      let le = Tree_sitter.Node.end_byte lnode in
                      let selector = slice_bytes content ls le in
                      { start_byte = s; end_byte = e; selector } :: acc
                    else acc
                | _ -> acc
              in
              loop acc
        in
        let res = loop [] in
        Tree_sitter.Query_cursor.delete cursor;
        List.rev res

  let get_injection_parser t (ft : filetype) (lang : Language.t) =
    match Hashtbl.find_opt t.injection_parsers ft with
    | Some p -> p
    | None ->
        let p = Tree_sitter.Parser.create () in
        Tree_sitter.Parser.set_language p (Language.ts_language lang);
        Hashtbl.replace t.injection_parsers ft p;
        p

  let rec highlight_fragment t ~(lang : Language.t) ~(host_ft : filetype)
      ~(origin : Highlight.origin) ~(base_offset : int) ~(content : string)
      ~(include_private : bool) ~(depth : int) : Highlight.t list =
    if depth < 0 then []
    else
      let parser =
        match origin with
        | `Host -> t.parser
        | `Injection ft -> get_injection_parser t ft lang
      in
      let tree = Tree_sitter.Parser.parse_string parser content in
      let root = Tree_sitter.Tree.root_node tree in

      (* Highlight captures *)
      let cursor = Tree_sitter.Query_cursor.create () in
      let host_highlights =
        gather_highlights_with_cursor ~cursor
          ~query:lang.Language.highlight_query ~root ~content ~origin
          ~base_offset ~include_private
      in
      Tree_sitter.Query_cursor.delete cursor;

      (* Injections (recursive) *)
      let injected =
        if depth = 0 then []
        else
          let injections = gather_injections lang ~content ~root in
          List.fold_left
            (fun acc inj ->
              let selector = inj.selector in
              match Set.resolve_injection t.set ~host:host_ft ~selector with
              | None -> acc
              | Some ft -> (
                  match Set.find t.set ft with
                  | None -> acc
                  | Some inj_lang ->
                      let slice =
                        slice_bytes content inj.start_byte inj.end_byte
                      in
                      let base_offset' = base_offset + inj.start_byte in
                      let highlights =
                        highlight_fragment t ~lang:inj_lang
                          ~host_ft:(Language.filetype inj_lang)
                          ~origin:(`Injection ft) ~base_offset:base_offset'
                          ~content:slice ~include_private ~depth:(depth - 1)
                      in
                      highlights @ acc))
            [] injections
      in
      host_highlights @ injected

  let highlight ?(include_private = false) t ~content =
    if t.closed then
      invalid_arg "Mosaic_syntax.Session.highlight: session is closed";

    (* Incremental parse host content *)
    let new_tree =
      match t.tree with
      | None -> Tree_sitter.Parser.parse_string t.parser content
      | Some old -> Tree_sitter.Parser.parse_string ~old t.parser content
    in
    t.tree <- Some new_tree;
    let root = Tree_sitter.Tree.root_node new_tree in

    (* Host highlights *)
    let host =
      gather_highlights_with_cursor ~cursor:t.cursor
        ~query:t.lang.Language.highlight_query ~root ~content ~origin:`Host
        ~base_offset:0 ~include_private
    in

    (* Injections: highlight injected regions as nested fragments *)
    let injected =
      match t.lang.Language.injection_query with
      | None -> []
      | Some _ ->
          (* depth limit prevents cycles / runaway recursion *)
          let depth = 2 in
          let injections = gather_injections t.lang ~content ~root in
          List.fold_left
            (fun acc inj ->
              match
                Set.resolve_injection t.set ~host:t.filetype
                  ~selector:inj.selector
              with
              | None -> acc
              | Some ft -> (
                  match Set.find t.set ft with
                  | None -> acc
                  | Some inj_lang ->
                      let slice =
                        slice_bytes content inj.start_byte inj.end_byte
                      in
                      let highlights =
                        highlight_fragment t ~lang:inj_lang
                          ~host_ft:(Language.filetype inj_lang)
                          ~origin:(`Injection ft) ~base_offset:inj.start_byte
                          ~content:slice ~include_private ~depth:(depth - 1)
                      in
                      highlights @ acc))
            [] injections
    in

    let all = host @ injected in
    let arr = Array.of_list all in
    Array.sort Highlight.compare arr;
    arr
end

(* --- Builtins --- *)

let builtins () : Set.t =
  let ocaml_language =
    let ts_language = Tree_sitter_ocaml.ocaml () in
    let highlights =
      {|
      (comment) @comment
      (string) @string
      (character) @string
      (constructor_name) @type
      (type_constructor) @type
      (module_name) @module
      ["let" "in" "match" "with" "function" "fun" "if" "then" "else"
       "type" "module" "open" "struct" "end" "sig" "val" "and" "rec" "of"
       "true" "false"] @keyword
      (value_name) @variable
      (number) @number
      |}
    in
    Language.make_exn ~filetype:"ocaml" ~ts_language ~highlights ()
  in

  let json_language =
    let ts_language = Tree_sitter_json.language () in
    let highlights =
      {|
      (pair value: (string) @string)
      (array (string) @string)
      (number) @number
      (null) @constant
      (true) @constant
      (false) @constant
      (pair key: (string) @property)
      |}
    in
    Language.make_exn ~filetype:"json" ~ts_language ~highlights ()
  in

  Set.create [ ocaml_language; json_language ]
