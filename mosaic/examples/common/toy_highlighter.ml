let highlight ~language ~content =
  let keywords =
    match String.lowercase_ascii language with
    | "ocaml" ->
        [
          "let";
          "in";
          "fun";
          "function";
          "match";
          "with";
          "type";
          "module";
          "open";
          "rec";
          "if";
          "then";
          "else";
          "of";
        ]
    | "json" -> [ "true"; "false"; "null" ]
    | _ -> []
  in
  let word = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
    | _ -> false
  in
  let ranges = ref [] in
  let pos = ref 0 in
  while !pos < String.length content do
    let start = !pos in
    incr pos;
    let scope =
      match content.[start] with
      | '"' ->
          while !pos < String.length content && content.[!pos] <> '"' do
            if content.[!pos] = '\\' && !pos + 1 < String.length content then
              incr pos;
            incr pos
          done;
          if !pos < String.length content then incr pos;
          Some "string"
      | '0' .. '9' ->
          while
            !pos < String.length content
            && match content.[!pos] with '0' .. '9' | '.' -> true | _ -> false
          do
            incr pos
          done;
          Some "number"
      | c when word c ->
          while !pos < String.length content && word content.[!pos] do
            incr pos
          done;
          if List.mem (String.sub content start (!pos - start)) keywords then
            Some "keyword"
          else None
      | _ -> None
    in
    Option.iter (fun scope -> ranges := (start, !pos, scope) :: !ranges) scope
  done;
  Mosaic_ui.Syntax_highlight.of_triples (List.rev !ranges)
