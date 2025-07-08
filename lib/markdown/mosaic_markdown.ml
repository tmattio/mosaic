open Mosaic
module Style = Markdown_style

type list_state = { ordered : bool; mutable counter : int }

type renderer = {
  style : Style.t;
  _width : int; (* TODO: use for word wrapping *)
  mutable list_stack : list_state list;
}

let string_of_inlines inline : string =
  let folder = Cmarkit.Folder.make ~inline:(fun _f acc i -> 
    match i with
    | Cmarkit.Inline.Text (t, _) -> 
        `Fold (acc ^ t)
    | _ -> `Default) () in
  Cmarkit.Folder.fold_inline folder "" inline

let _word_wrap text width =
  if width <= 0 then [ text ]
  else
    let words = String.split_on_char ' ' text in
    let lines, last_line =
      List.fold_left
        (fun (lines, current_line) word ->
          let new_line =
            if current_line = "" then word else current_line ^ " " ^ word
          in
          let len = Uutf.String.fold_utf_8 (fun acc _ _ -> acc + 1) 0 new_line in
          if len > width then (current_line :: lines, word)
          else (lines, new_line))
        ([], "") words
    in
    List.rev (last_line :: lines)

let render_inlines (r : renderer) inline : Ui.element =
  let acc = ref [] in
  let buffer = ref [] in
  let flush_buffer style =
    if !buffer <> [] then
      let text = String.concat "" (List.rev !buffer) in
      acc := !acc @ [ Ui.text ~style text ];
      buffer := []
  in
  let folder = Cmarkit.Folder.make ~inline:(fun f style i ->
    match i with
    | Cmarkit.Inline.Text (t, _) ->
        buffer := t :: !buffer;
        `Fold style
    | Cmarkit.Inline.Emphasis (e, _) ->
        flush_buffer style;
        let new_style = Mosaic.Style.(style ++ r.style.emph) in
        let inner = Cmarkit.Inline.Emphasis.inline e in
        ignore (Cmarkit.Folder.fold_inline f new_style inner);
        `Fold style
    | Cmarkit.Inline.Strong_emphasis (e, _) ->
        flush_buffer style;
        let new_style = Mosaic.Style.(style ++ r.style.strong) in
        let inner = Cmarkit.Inline.Emphasis.inline e in
        ignore (Cmarkit.Folder.fold_inline f new_style inner);
        `Fold style
    | Cmarkit.Inline.Code_span (cs, _) ->
        flush_buffer style;
        let text = Cmarkit.Inline.Code_span.code cs in
        acc := !acc @ [ Ui.text ~style:r.style.code (" " ^ text ^ " ") ];
        `Fold style
    | Cmarkit.Inline.Link (l, _) ->
        flush_buffer style;
        let uri = match Cmarkit.Inline.Link.reference l with
          | `Inline (ld, _) -> 
              (match Cmarkit.Link_definition.dest ld with
               | Some (u, _) -> u
               | None -> "")
          | `Ref _ -> "" (* TODO: resolve reference *)
        in
        let text = string_of_inlines (Cmarkit.Inline.Link.text l) in
        acc := !acc @ [ Ui.text ~style:Mosaic.Style.(r.style.link ++ link uri) text ];
        `Fold style
    | Cmarkit.Inline.Image (l, _) ->
        flush_buffer style;
        let uri = match Cmarkit.Inline.Link.reference l with
          | `Inline (ld, _) -> 
              (match Cmarkit.Link_definition.dest ld with
               | Some (u, _) -> u
               | None -> "")
          | `Ref _ -> "" (* TODO: resolve reference *)
        in
        let alt = string_of_inlines (Cmarkit.Inline.Link.text l) in
        let text = Printf.sprintf "[Image: %s <%s>]" alt uri in
        acc := !acc @ [ Ui.text ~style:Mosaic.Style.(r.style.image ++ link uri) text ];
        `Fold style
    | _ -> `Default) () in
  ignore (Cmarkit.Folder.fold_inline folder Mosaic.Style.empty inline);
  flush_buffer Mosaic.Style.empty;
  Ui.hbox !acc

let rec render_block (r : renderer) block : Ui.element list =
  let open Cmarkit in
  match block with
  | Block.Paragraph (p, _) ->
      let text = render_inlines r (Block.Paragraph.inline p) in
      let p_style = r.style.paragraph in
      let content =
        Ui.vbox ~padding:(Ui.padding_xy p_style.padding_left p_style.padding_right) [ text ]
      in
      [
        Ui.space p_style.margin_top;
        content;
        Ui.space p_style.margin_bottom;
      ]
  | Block.Heading (h, _) ->
      let level = Block.Heading.level h in
      let h_style =
        match level with
        | 1 -> r.style.h1
        | 2 -> r.style.h2
        | 3 -> r.style.h3
        | 4 -> r.style.h4
        | 5 -> r.style.h5
        | _ -> r.style.h6
      in
      let full_style = { h_style with style = Mosaic.Style.(r.style.heading.style ++ h_style.style) } in
      let prefix = String.make level '#' ^ " " in
      let content =
        Ui.hbox ~gap:1 [ Ui.text ~style:full_style.style prefix; render_inlines r (Block.Heading.inline h) ]
      in
      [
        Ui.space full_style.margin_top;
        content;
        Ui.space full_style.margin_bottom;
      ]
  | Block.Block_quote (bq, _) ->
      let children = render_block r (Block.Block_quote.block bq) in
      let bq_style = r.style.block_quote in
      let content = Ui.vbox children in
      let bordered =
        Ui.hbox ~gap:1
          [
            Ui.text ~style:bq_style.style "│";
            content;
          ]
      in
      [
        Ui.space bq_style.margin_top;
        bordered;
        Ui.space bq_style.margin_bottom;
      ]
  | Block.List (l, _) ->
      let list_ty = Block.List'.type' l in
      let start = match list_ty with
        | `Ordered (n, _) -> n
        | _ -> 1
      in
      let state =
        { ordered = (match list_ty with `Ordered _ -> true | _ -> false); counter = start }
      in
      r.list_stack <- state :: r.list_stack;
      let items = List.concat_map (fun (item, _) ->
        render_list_item r item
      ) (Block.List'.items l) in
      r.list_stack <- List.tl r.list_stack;
      let l_style = r.style.list in
      let content =
        Ui.vbox ~padding:(Ui.padding_all l_style.block.padding_left) items
      in
      [
        Ui.space l_style.block.margin_top;
        content;
        Ui.space l_style.block.margin_bottom;
      ]
  | Block.Code_block (cb, _) ->
      let code_lines = Block.Code_block.code cb in
      let code = String.concat "\n" (List.map Cmarkit.Block_line.to_string code_lines) in
      let lang = match Block.Code_block.info_string cb with
        | Some (s, _) -> s
        | None -> ""
      in
      let cb_style = r.style.code_block in
      let fence = "```" in
      let lang_elem = Ui.text ~style:cb_style.lang_style lang in
      let fence_elem = Ui.text ~style:cb_style.fence_style fence in
      let code_lines =
        String.split_on_char '\n' code
        |> List.map (fun line -> Ui.text ~style:cb_style.block.style line)
      in
      let content =
        Ui.vbox ~padding:(Ui.padding_xy cb_style.block.padding_left 2)
          ( [ Ui.hbox [ fence_elem; lang_elem ] ]
          @ code_lines
          @ [ fence_elem ] )
      in
      [
        Ui.space cb_style.block.margin_top;
        content;
        Ui.space cb_style.block.margin_bottom;
      ]
  | Block.Thematic_break (_, _) ->
      let style, str = r.style.horizontal_rule in
      [ Ui.text ~style str ]
  | Block.Blocks (blocks, _) ->
      List.concat_map (fun b -> render_block r b) blocks
  | _ -> []

and render_list_item (r : renderer) item =
  let l_style = r.style.list in
  let prefix, prefix_style =
    match r.list_stack with
    | state :: _ ->
        let p =
          if state.ordered then (
            let s = string_of_int state.counter ^ "." in
            state.counter <- state.counter + 1;
            s)
          else l_style.item_prefix
        in
        (p, l_style.item_prefix_style)
    | [] -> ("", Mosaic.Style.empty)
  in
  let indent_size =
    (List.length r.list_stack - 1) * l_style.level_indent
  in
  let indent = Ui.space indent_size in
  let children = render_block r (Cmarkit.Block.List_item.block item) in
  let content =
    Ui.hbox ~gap:l_style.item_gap
      [
        Ui.text ~style:prefix_style prefix;
        Ui.vbox children;
      ]
  in
  [ Ui.hbox [ indent; content ] ]

let render ?(style = Style.default) ?(width = 80) markdown_text =
  let r = { style; _width = width; list_stack = [] } in
  let doc = Cmarkit.Doc.of_string markdown_text in
  let elements = render_block r (Cmarkit.Doc.block doc) in
  let s = r.style.document in
  Ui.vbox ~width
    ~padding:(Ui.padding_xy s.padding_left s.padding_right)
    ([ Ui.space s.margin_top ] @ elements @ [ Ui.space s.margin_bottom ])