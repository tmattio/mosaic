module Style = Markdown_style

type list_state = { ordered : bool; mutable counter : int }
type segment = S_text of string * Ui.Style.t | S_hard_break

type renderer = {
  style : Markdown_style.t;
  syntax_theme : Mosaic_syntax.theme;
  mutable list_stack : list_state list;
  mutable quote_depth : int;
  link_defs : (string, string) Hashtbl.t;
  footnotes : (string, Cmarkit.Block.t) Hashtbl.t;
}

let u_length s = Uutf.String.fold_utf_8 (fun acc _ _ -> acc + 1) 0 s

let string_of_inlines i =
  let plain = Cmarkit.Inline.to_plain_text ~break_on_soft:false i in
  String.concat "" (List.map (String.concat "") plain)

let collect_link_defs block link_defs footnotes =
  let folder =
    Cmarkit.Folder.make
      ~block:(fun _f _acc b ->
        match b with
        | Cmarkit.Block.Link_reference_definition (ld, _) ->
            (match Cmarkit.Link_definition.defined_label ld with
            | Some label ->
                let key = Cmarkit.Label.key label in
                let dest =
                  match Cmarkit.Link_definition.dest ld with
                  | Some (u, _) -> u
                  | None -> ""
                in
                Hashtbl.add link_defs key dest
            | None -> ());
            Cmarkit.Folder.default
        | Cmarkit.Block.Ext_footnote_definition (fd, _) ->
            (match Cmarkit.Block.Footnote.defined_label fd with
            | Some label ->
                let key = Cmarkit.Label.key label in
                let block = Cmarkit.Block.Footnote.block fd in
                Hashtbl.add footnotes key block
            | None -> ());
            Cmarkit.Folder.default
        | _ -> Cmarkit.Folder.default)
      ()
  in
  ignore (Cmarkit.Folder.fold_block folder () block)

let wrap_segments available_width segments =
  let lines = ref [] in
  let current = ref [] in
  let current_len = ref 0 in
  let add_to_current text st =
    current := (text, st) :: !current;
    current_len := !current_len + u_length text
  in
  let flush () =
    if !current <> [] then (
      lines := List.rev !current :: !lines;
      current := [];
      current_len := 0)
  in
  List.iter
    (fun seg ->
      match seg with
      | S_hard_break -> flush ()
      | S_text (text, st) ->
          if text = "" then ()
          else
            let words = String.split_on_char ' ' text in
            let has_leading = match words with "" :: _ -> true | _ -> false in
            let has_trailing =
              match List.rev words with "" :: _ -> true | _ -> false
            in
            let filtered = List.filter (( <> ) "") words in
            if filtered = [] then ()
            else (
              if has_leading then (
                let sp = " " in
                let splen = u_length sp in
                if !current_len > 0 && !current_len + splen > available_width
                then flush ();
                add_to_current sp st);
              List.iteri
                (fun i word ->
                  let word_with_space = if i > 0 then " " ^ word else word in
                  let wlen = u_length word_with_space in
                  if !current_len > 0 && !current_len + wlen > available_width
                  then flush ();
                  add_to_current word_with_space st)
                filtered;
              if has_trailing then (
                let sp = " " in
                let splen = u_length sp in
                if !current_len + splen > available_width then flush ();
                add_to_current sp st)))
    segments;
  flush ();
  List.rev !lines

let render_inlines r ~available_width ~initial_style inline =
  let segments = ref [] in
  let buffer = ref [] in
  let flush_buffer current_style =
    if !buffer <> [] then (
      let text = String.concat "" (List.rev !buffer) in
      segments := S_text (text, current_style) :: !segments;
      buffer := [])
  in
  let folder =
    Cmarkit.Folder.make
      ~inline:(fun f current_style i ->
        match i with
        | Cmarkit.Inline.Text (t, _) ->
            buffer := t :: !buffer;
            Cmarkit.Folder.ret current_style
        | Cmarkit.Inline.Emphasis (e, _) ->
            flush_buffer current_style;
            let new_style = Ui.Style.(current_style ++ r.style.emph) in
            ignore
              (Cmarkit.Folder.fold_inline f new_style
                 (Cmarkit.Inline.Emphasis.inline e));
            flush_buffer new_style;
            Cmarkit.Folder.ret current_style
        | Cmarkit.Inline.Strong_emphasis (e, _) ->
            flush_buffer current_style;
            let new_style = Ui.Style.(current_style ++ r.style.strong) in
            ignore
              (Cmarkit.Folder.fold_inline f new_style
                 (Cmarkit.Inline.Emphasis.inline e));
            flush_buffer new_style;
            Cmarkit.Folder.ret current_style
        | Cmarkit.Inline.Ext_strikethrough (s, _) ->
            flush_buffer current_style;
            let new_style = Ui.Style.(current_style ++ r.style.strike) in
            ignore
              (Cmarkit.Folder.fold_inline f new_style
                 (Cmarkit.Inline.Strikethrough.inline s));
            flush_buffer new_style;
            Cmarkit.Folder.ret current_style
        | Cmarkit.Inline.Code_span (cs, _) ->
            flush_buffer current_style;
            let code = Cmarkit.Inline.Code_span.code cs in
            segments :=
              S_text (code, Ui.Style.(current_style ++ r.style.code))
              :: !segments;
            Cmarkit.Folder.ret current_style
        | Cmarkit.Inline.Link (l, _) ->
            flush_buffer current_style;
            let uri =
              match Cmarkit.Inline.Link.reference l with
              | `Inline ld_node -> (
                  let ld, _ = ld_node in
                  match Cmarkit.Link_definition.dest ld with
                  | Some (u, _) -> u
                  | None -> "")
              | `Ref (_, _, def_label) ->
                  let key = Cmarkit.Label.key def_label in
                  Hashtbl.find_opt r.link_defs key |> Option.value ~default:""
            in
            let new_style =
              Ui.Style.(current_style ++ r.style.link ++ link uri)
            in
            ignore
              (Cmarkit.Folder.fold_inline f new_style
                 (Cmarkit.Inline.Link.text l));
            flush_buffer new_style;
            Cmarkit.Folder.ret current_style
        | Cmarkit.Inline.Image (l, _) ->
            flush_buffer current_style;
            let uri =
              match Cmarkit.Inline.Link.reference l with
              | `Inline ld_node -> (
                  let ld, _ = ld_node in
                  match Cmarkit.Link_definition.dest ld with
                  | Some (u, _) -> u
                  | None -> "")
              | `Ref (_, _, def_label) ->
                  let key = Cmarkit.Label.key def_label in
                  Hashtbl.find_opt r.link_defs key |> Option.value ~default:""
            in
            let alt = string_of_inlines (Cmarkit.Inline.Link.text l) in
            let text = Printf.sprintf "[Image: %s <%s>]" alt uri in
            segments :=
              S_text (text, Ui.Style.(current_style ++ r.style.image))
              :: !segments;
            Cmarkit.Folder.ret current_style
        | Cmarkit.Inline.Autolink (al, _) ->
            flush_buffer current_style;
            let uri, _ = Cmarkit.Inline.Autolink.link al in
            segments :=
              S_text (uri, Ui.Style.(current_style ++ r.style.link ++ link uri))
              :: !segments;
            Cmarkit.Folder.ret current_style
        | Cmarkit.Inline.Raw_html (h, _) ->
            flush_buffer current_style;
            let html =
              String.concat "" (List.map Cmarkit.Block_line.tight_to_string h)
            in
            segments :=
              S_text (html, Ui.Style.(current_style ++ r.style.html))
              :: !segments;
            Cmarkit.Folder.ret current_style
        | Cmarkit.Inline.Break (b, _) ->
            flush_buffer current_style;
            (match Cmarkit.Inline.Break.type' b with
            | `Hard -> segments := S_hard_break :: !segments
            | `Soft -> buffer := " " :: !buffer);
            Cmarkit.Folder.ret current_style
        | Cmarkit.Inline.Inlines (inlines, _) ->
            List.iter
              (fun inl ->
                ignore (Cmarkit.Folder.fold_inline f current_style inl))
              inlines;
            Cmarkit.Folder.ret current_style
        | _ -> Cmarkit.Folder.default)
      ()
  in
  ignore (Cmarkit.Folder.fold_inline folder initial_style inline);
  flush_buffer initial_style;
  let segments = List.rev !segments in
  let wrapped_lines = wrap_segments available_width segments in
  let line_els =
    List.map
      (fun line_segments ->
        Ui.hbox (List.map (fun (s, st) -> Ui.text ~style:st s) line_segments))
      wrapped_lines
  in
  Ui.vbox line_els

let rec render_block r ~available_width ~base_style block =
  let open Cmarkit.Block in
  match block with
  | Paragraph (p, _) ->
      let p_style = r.style.paragraph in
      let avail =
        available_width - p_style.padding_left - p_style.padding_right
      in
      let content =
        render_inlines r ~available_width:avail
          ~initial_style:Ui.Style.(base_style ++ p_style.style)
          (Paragraph.inline p)
      in
      [
        Ui.box
          ~margin:
            (Ui.sides ~top:p_style.margin_top ~bottom:p_style.margin_bottom ())
          ~padding:(Ui.xy p_style.padding_left p_style.padding_right)
          [ content ];
      ]
  | Heading (h, _) ->
      let level = Heading.level h in
      let h_style =
        match level with
        | 1 -> r.style.h1
        | 2 -> r.style.h2
        | 3 -> r.style.h3
        | 4 -> r.style.h4
        | 5 -> r.style.h5
        | _ -> r.style.h6
      in
      let full_style =
        {
          h_style with
          style =
            Ui.Style.(base_style ++ r.style.heading.style ++ h_style.style);
        }
      in
      let prefix_str = String.make level '#' ^ " " in
      let prefix =
        Ui.text
          ~style:Ui.Style.(r.style.heading_prefix ++ full_style.style)
          prefix_str
      in
      let text =
        render_inlines r
          ~available_width:(available_width - u_length prefix_str)
          ~initial_style:full_style.style (Heading.inline h)
      in
      [
        Ui.box
          ~margin:
            (Ui.sides ~top:full_style.margin_top
               ~bottom:full_style.margin_bottom ())
          [ Ui.hbox [ prefix; text ] ];
      ]
  | Block_quote (bq, _) ->
      let bq_style = r.style.block_quote in
      r.quote_depth <- r.quote_depth + 1;

      (* Build prefix - just one bar for the current level *)
      let prefix_el =
        Ui.divider ~orientation:`Vertical ~style:bq_style.style ()
      in

      (* Calculate available width *)
      let prefix_width = 2 in
      (* "│ " = 2 chars *)
      let avail =
        available_width - prefix_width - bq_style.padding_left
        - bq_style.padding_right
      in

      let bq_block = Block_quote.block bq in

      (* Special handling for blockquote content to avoid extra margins *)
      let rec render_blockquote_content blk =
        match blk with
        | Cmarkit.Block.Paragraph (p, _) ->
            (* Render paragraph without margins *)
            let content =
              render_inlines r ~available_width:avail
                ~initial_style:Ui.Style.(base_style ++ bq_style.style)
                (Cmarkit.Block.Paragraph.inline p)
            in
            [ content ]
        | Cmarkit.Block.Blank_line (_, _) ->
            (* Render a blank line as empty text to preserve spacing *)
            [ Ui.text "" ]
        | Cmarkit.Block.Blocks (blocks, _) ->
            (* Render multiple blocks without margins between them *)
            List.concat_map render_blockquote_content blocks
        | Cmarkit.Block.Block_quote _ ->
            (* Nested blockquote - render without outer margins *)
            render_block r ~available_width:avail
              ~base_style:Ui.Style.(base_style ++ bq_style.style)
              blk
        | _ ->
            render_block r ~available_width:avail
              ~base_style:Ui.Style.(base_style ++ bq_style.style)
              blk
      in

      let children = render_blockquote_content bq_block in

      (* Create the quote content with proper spacing *)
      let content =
        Ui.hbox ~gap:(`Cells bq_style.padding_left)
          [
            prefix_el;
            Ui.box
              ~padding:(Ui.xy 0 bq_style.padding_right)
              [ Ui.vbox ~gap:(`Cells 0) children ];
          ]
      in

      r.quote_depth <- r.quote_depth - 1;

      (* Only add margins if we're not inside another blockquote *)
      if r.quote_depth = 0 then
        [
          Ui.box
            ~margin:
              (Ui.sides ~top:bq_style.margin_top ~bottom:bq_style.margin_bottom
                 ())
            [ content ];
        ]
      else [ content ]
  | List (l, _) ->
      let l_style = r.style.list in
      let list_ty = List'.type' l in
      let start = match list_ty with `Ordered (n, _) -> n | _ -> 1 in
      let state =
        {
          ordered = (match list_ty with `Ordered _ -> true | _ -> false);
          counter = start;
        }
      in
      let is_nested = List.length r.list_stack > 0 in
      r.list_stack <- state :: r.list_stack;
      let items =
        List'.items l
        |> List.concat_map (render_list_item r ~available_width ~base_style)
      in
      r.list_stack <- List.tl r.list_stack;
      (* Only add padding for top-level lists *)
      let list_content =
        if is_nested then Ui.vbox items
        else
          Ui.vbox
            ~padding:
              (Ui.xy l_style.block.padding_left l_style.block.padding_right)
            items
      in
      (* Only add margins for top-level lists *)
      if is_nested then [ list_content ]
      else
        [
          Ui.box
            ~margin:
              (Ui.sides ~top:l_style.block.margin_top
                 ~bottom:l_style.block.margin_bottom ())
            [ list_content ];
        ]
  | Code_block (cb, _) ->
      let cb_style = r.style.code_block in
      let code_lines =
        Code_block.code cb |> List.map Cmarkit.Block_line.to_string
        |> fun lines ->
        (* Trim empty lines from beginning and end *)
        let rec trim_start = function
          | "" :: rest -> trim_start rest
          | lines -> lines
        in
        let rec trim_end lines =
          match List.rev lines with
          | "" :: rest -> trim_end (List.rev rest)
          | _ -> lines
        in
        lines |> trim_start |> trim_end
      in
      let code = String.concat "\n" code_lines in
      let lang_opt =
        Option.bind (Code_block.info_string cb) (fun (s, _) ->
            Code_block.language_of_info_string s)
      in
      let lang = match lang_opt with Some (l, _) -> l | None -> "" in
      let fence = "```" in

      (* Create the highlighted code element *)
      let highlighted =
        let plain =
          List.map
            (fun line ->
              Ui.text
                ~style:Ui.Style.(base_style ++ cb_style.block.style)
                ~wrap:`Clip line)
            code_lines
          |> Ui.vbox ~gap:(`Cells 0)
        in
        if lang = "" then plain
        else
          let lang_type_opt =
            match String.lowercase_ascii lang with
            | "ocaml" | "ml" -> Some `OCaml
            | "mli" | "ocaml-interface" -> Some `OCaml_interface
            | "dune" -> Some `Dune
            | "sh" | "bash" | "shell" -> Some `Shell
            | "diff" -> Some `Diff
            | _ -> None
          in
          match lang_type_opt with
          | None -> plain
          | Some lang_type -> (
              match
                Mosaic_syntax.highlight ~theme:r.syntax_theme ~lang:lang_type
                  code
              with
              | Ok el -> el
              | Error _ -> plain)
      in

      (* Apply consistent padding to all lines *)
      let padding_str = String.make cb_style.block.padding_left ' ' in

      (* Add padding to each code line *)
      let highlighted_with_padding =
        if lang = "" then
          (* For plain code, add padding manually *)
          List.map
            (fun line ->
              Ui.text
                ~style:Ui.Style.(base_style ++ cb_style.block.style)
                (padding_str ^ line))
            code_lines
          |> Ui.vbox ~gap:(`Cells 0)
        else
          (* For syntax highlighted code, we need to handle it differently *)
          (* The syntax highlighter returns a UI element, so we wrap it *)
          Ui.box ~padding:(Ui.xy cb_style.block.padding_left 0) [ highlighted ]
      in

      let fence_line_start =
        Ui.hbox
          [
            Ui.text ~style:cb_style.fence_style (padding_str ^ fence);
            Ui.text ~style:cb_style.lang_style lang;
          ]
      in
      let fence_line_end =
        Ui.text ~style:cb_style.fence_style (padding_str ^ fence)
      in

      let content =
        Ui.vbox ~gap:(`Cells 0)
          [ fence_line_start; highlighted_with_padding; fence_line_end ]
      in
      [ content ]
  | Html_block (lines, _) ->
      let hb_style = r.style.paragraph in
      let text =
        lines |> List.map Cmarkit.Block_line.to_string |> String.concat "\n"
      in
      [
        Ui.box
          ~margin:
            (Ui.sides ~top:hb_style.margin_top ~bottom:hb_style.margin_bottom ())
          [ Ui.text ~style:Ui.Style.(base_style ++ r.style.html) text ];
      ]
  | Thematic_break (_, _) ->
      let hr_style, hr_char = r.style.horizontal_rule in
      let hr_width = min available_width 80 in
      let hr_text = String.make hr_width hr_char.[0] in
      [
        Ui.box
          ~margin:(Ui.sides ~top:0 ~bottom:0 ())
          [ Ui.text ~style:Ui.Style.(base_style ++ hr_style) hr_text ];
      ]
  | Ext_table (tbl, _) ->
      let tb_style = r.style.table in
      let base = Ui.Style.(base_style ++ tb_style.block.style) in
      let rows = Cmarkit.Block.Table.rows tbl in
      let get_row_type ((r, _), _) = r in

      (* Extract headers and data *)
      let header_rows =
        List.filter
          (fun row ->
            match get_row_type row with `Header _ -> true | _ -> false)
          rows
      in
      let data_rows =
        List.filter
          (fun row ->
            match get_row_type row with `Data _ -> true | _ -> false)
          rows
      in
      let sep_row_opt =
        List.find_opt
          (fun row -> match get_row_type row with `Sep _ -> true | _ -> false)
          rows
      in

      (* Get alignments from separator row *)
      let alignments =
        match sep_row_opt with
        | Some ((`Sep seps, _), _) ->
            List.map
              (fun ((a, _), _) ->
                match Option.value ~default:`Left a with
                | `Left -> `Left
                | `Right -> `Right
                | `Center -> `Center)
              seps
        | _ -> []
      in

      (* Extract cell content *)
      let get_cells r =
        match r with
        | `Header cs -> List.map (fun (i, _) -> string_of_inlines i) cs
        | `Data cs -> List.map (fun (i, _) -> string_of_inlines i) cs
        | `Sep _ -> []
      in

      (* Create headers and data as string lists *)
      let headers =
        match header_rows with
        | [] -> []
        | row :: _ -> get_cells (get_row_type row)
      in
      let table_rows =
        List.map (fun row -> get_cells (get_row_type row)) data_rows
      in

      (* Create columns with alignments *)
      let columns =
        List.mapi
          (fun i header ->
            let justify = try List.nth alignments i with _ -> `Left in
            Ui.Table.
              {
                (default_column ~header) with
                justify;
                style = base;
                header_style = tb_style.header_style;
              })
          headers
      in

      (* Create the table *)
      let table_el =
        Ui.table ~columns ~rows:table_rows ~box_style:Ui.Table.Simple
          ~style:base ~header_style:tb_style.header_style
          ~border_style:(fst tb_style.separator_style)
          ~show_header:true ~padding:(0, 1, 0, 1) ()
      in

      let padded =
        Ui.vbox
          ~padding:
            (Ui.xy tb_style.block.padding_left tb_style.block.padding_right)
          [ table_el ]
      in
      [
        Ui.box
          ~margin:
            (Ui.sides ~top:tb_style.block.margin_top
               ~bottom:tb_style.block.margin_bottom ())
          [ padded ];
      ]
  | Blocks (blocks, _) ->
      List.concat_map (render_block r ~available_width ~base_style) blocks
  | Blank_line (_, _) ->
      (* Render blank lines as empty text to preserve markdown spacing *)
      [ Ui.text "" ]
  | _ -> []

and render_list_item r ~available_width ~base_style
    (item_node : Cmarkit.Block.List_item.t Cmarkit.node) =
  let item, _ = item_node in
  let l_style = r.style.list in
  (* No additional indentation - parent list items handle positioning *)
  let indent_size = 0 in
  let task_marker = Cmarkit.Block.List_item.ext_task_marker item in
  let prefix_element, state_change =
    match task_marker with
    | Some marker_node ->
        let marker, _ = marker_node in
        let status =
          Cmarkit.Block.List_item.task_status_of_task_marker marker
        in
        let checked = match status with `Checked -> true | _ -> false in
        let style =
          if checked then l_style.checked_style else l_style.task_style
        in
        (Ui.checkbox ~checked ~label:"" ~style (), fun () -> ())
    | None -> (
        match List.hd r.list_stack with
        | { ordered = true; counter; _ } as state ->
            let p = string_of_int counter ^ "." in
            ( Ui.text ~style:l_style.item_prefix_style p,
              fun () -> state.counter <- counter + 1 )
        | _ ->
            ( Ui.text ~style:l_style.item_prefix_style l_style.item_prefix,
              fun () -> () ))
  in
  let prefix_width =
    match task_marker with
    | Some _ -> 3 (* checkbox is "[x]" or "[ ]" - 3 chars *)
    | None -> (
        match List.hd r.list_stack with
        | { ordered = true; counter; _ } ->
            u_length (string_of_int counter ^ ".")
        | _ -> u_length l_style.item_prefix)
  in
  let avail = available_width - indent_size - prefix_width - l_style.item_gap in
  let item_block = Cmarkit.Block.List_item.block item in
  let children =
    match item_block with
    | Cmarkit.Block.Paragraph (p, _) ->
        (* For paragraph items, render inline content directly without margins *)
        let content =
          render_inlines r ~available_width:avail ~initial_style:base_style
            (Cmarkit.Block.Paragraph.inline p)
        in
        [ content ]
    | Cmarkit.Block.Blocks (blocks, _) ->
        (* For blocks (e.g., paragraph + nested list), handle specially *)
        List.concat_map
          (fun blk ->
            match blk with
            | Cmarkit.Block.Paragraph (p, _) ->
                (* Render paragraph without margins *)
                let content =
                  render_inlines r ~available_width:avail
                    ~initial_style:base_style
                    (Cmarkit.Block.Paragraph.inline p)
                in
                [ content ]
            | Cmarkit.Block.List _ ->
                (* Render nested list with no top margin *)
                render_block r ~available_width:avail ~base_style blk
            | _ -> render_block r ~available_width:avail ~base_style blk)
          blocks
    | _ ->
        (* For other blocks, render normally *)
        render_block r ~available_width:avail ~base_style item_block
  in
  let content =
    Ui.hbox ~gap:(`Cells l_style.item_gap) [ prefix_element; Ui.vbox children ]
  in
  state_change ();
  [ Ui.box ~padding:(Ui.sides ~left:indent_size ()) [ content ] ]

let render ?(style = Markdown_style.default) ?(width = 80) ?(strict = false)
    ?(syntax_theme = Mosaic_syntax.default_dark_theme) markdown_text =
  let r =
    {
      style;
      syntax_theme;
      list_stack = [];
      quote_depth = 0;
      link_defs = Hashtbl.create 16;
      footnotes = Hashtbl.create 16;
    }
  in
  let doc = Cmarkit.Doc.of_string ~strict ~layout:true markdown_text in
  collect_link_defs (Cmarkit.Doc.block doc) r.link_defs r.footnotes;
  let s = r.style.document in
  let avail = width - s.padding_left - s.padding_right in
  let elements =
    render_block r ~available_width:avail ~base_style:Ui.Style.empty
      (Cmarkit.Doc.block doc)
  in
  let footnote_elements =
    if Hashtbl.length r.footnotes = 0 then []
    else
      let fn_heading = Ui.text ~style:r.style.heading.style "Footnotes" in
      let fns =
        Hashtbl.fold
          (fun key b acc ->
            let prefix =
              Ui.text ~style:r.style.heading_prefix ("[" ^ key ^ "]: ")
            in
            let content =
              render_block r ~available_width:avail ~base_style:Ui.Style.empty b
            in
            Ui.hbox [ prefix; Ui.vbox content ] :: acc)
          r.footnotes []
        |> List.sort (fun _ _ -> 0)
        (* sort by key if needed *)
      in
      Ui.box ~margin:(Ui.sides ~top:1 ~bottom:1 ()) [ fn_heading ] :: fns
  in
  Ui.box ~width:(`Cells width)
    ~margin:(Ui.sides ~top:s.margin_top ~bottom:s.margin_bottom ())
    ~padding:(Ui.xy s.padding_left s.padding_right)
    [ Ui.vbox (elements @ footnote_elements) ]
