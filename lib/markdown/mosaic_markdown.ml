open Mosaic
module Style = Markdown_style

type list_state = { ordered : bool; mutable counter : int }
type segment = S_text of string * Mosaic.Style.t | S_hard_break

type renderer = {
  style : Markdown_style.t;
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
            let new_style = Mosaic.Style.(current_style ++ r.style.emph) in
            ignore
              (Cmarkit.Folder.fold_inline f new_style
                 (Cmarkit.Inline.Emphasis.inline e));
            flush_buffer new_style;
            Cmarkit.Folder.ret current_style
        | Cmarkit.Inline.Strong_emphasis (e, _) ->
            flush_buffer current_style;
            let new_style = Mosaic.Style.(current_style ++ r.style.strong) in
            ignore
              (Cmarkit.Folder.fold_inline f new_style
                 (Cmarkit.Inline.Emphasis.inline e));
            flush_buffer new_style;
            Cmarkit.Folder.ret current_style
        | Cmarkit.Inline.Ext_strikethrough (s, _) ->
            flush_buffer current_style;
            let new_style = Mosaic.Style.(current_style ++ r.style.strike) in
            ignore
              (Cmarkit.Folder.fold_inline f new_style
                 (Cmarkit.Inline.Strikethrough.inline s));
            flush_buffer new_style;
            Cmarkit.Folder.ret current_style
        | Cmarkit.Inline.Code_span (cs, _) ->
            flush_buffer current_style;
            let code = Cmarkit.Inline.Code_span.code cs in
            segments := S_text (code, r.style.code) :: !segments;
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
              Mosaic.Style.(current_style ++ r.style.link ++ link uri)
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
            segments := S_text (text, r.style.image) :: !segments;
            Cmarkit.Folder.ret current_style
        | Cmarkit.Inline.Autolink (al, _) ->
            flush_buffer current_style;
            let uri, _ = Cmarkit.Inline.Autolink.link al in
            segments :=
              S_text
                (uri, Mosaic.Style.(current_style ++ r.style.link ++ link uri))
              :: !segments;
            Cmarkit.Folder.ret current_style
        | Cmarkit.Inline.Raw_html (h, _) ->
            flush_buffer current_style;
            let html =
              String.concat "" (List.map Cmarkit.Block_line.tight_to_string h)
            in
            segments := S_text (html, r.style.html) :: !segments;
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
          ~initial_style:Mosaic.Style.(base_style ++ p_style.style)
          (Paragraph.inline p)
      in
      let padded =
        Ui.vbox
          ~padding:(Ui.padding_xy p_style.padding_left p_style.padding_right)
          [ content ]
      in
      [ Ui.spacer p_style.margin_top; padded; Ui.spacer p_style.margin_bottom ]
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
            Mosaic.Style.(base_style ++ r.style.heading.style ++ h_style.style);
        }
      in
      let prefix_str = String.make level '#' ^ " " in
      let prefix =
        Ui.text
          ~style:Mosaic.Style.(r.style.heading_prefix ++ full_style.style)
          prefix_str
      in
      let text =
        render_inlines r
          ~available_width:(available_width - u_length prefix_str)
          ~initial_style:full_style.style (Heading.inline h)
      in
      let content = Ui.hbox [ prefix; text ] in
      [
        Ui.spacer full_style.margin_top;
        content;
        Ui.spacer full_style.margin_bottom;
      ]
  | Block_quote (bq, _) ->
      let bq_style = r.style.block_quote in
      r.quote_depth <- r.quote_depth + 1;
      let prefix_str =
        String.concat "" (List.init r.quote_depth (fun _ -> "│ "))
      in
      let prefix = Ui.text ~style:bq_style.style prefix_str in
      let avail =
        available_width - u_length prefix_str - bq_style.padding_left
        - bq_style.padding_right
      in
      let bq_block = Block_quote.block bq in
      let children =
        render_block r ~available_width:avail
          ~base_style:Mosaic.Style.(base_style ++ bq_style.style)
          bq_block
      in
      let content = Ui.hbox [ prefix; Ui.vbox children ] in
      r.quote_depth <- r.quote_depth - 1;
      let padded =
        Ui.vbox
          ~padding:(Ui.padding_xy bq_style.padding_left bq_style.padding_right)
          [ content ]
      in
      [
        Ui.spacer bq_style.margin_top; padded; Ui.spacer bq_style.margin_bottom;
      ]
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
      r.list_stack <- state :: r.list_stack;
      let items =
        List'.items l
        |> List.concat_map (render_list_item r ~available_width ~base_style)
      in
      r.list_stack <- List.tl r.list_stack;
      let padded =
        Ui.vbox
          ~padding:
            (Ui.padding_xy l_style.block.padding_left
               l_style.block.padding_right)
          items
      in
      [
        Ui.spacer l_style.block.margin_top;
        padded;
        Ui.spacer l_style.block.margin_bottom;
      ]
  | Code_block (cb, _) ->
      let cb_style = r.style.code_block in
      let code_lines =
        Code_block.code cb |> List.map Cmarkit.Block_line.to_string
      in
      let lang_opt =
        Option.bind (Code_block.info_string cb) (fun (s, _) ->
            Code_block.language_of_info_string s)
      in
      let lang = match lang_opt with Some (l, _) -> l | None -> "" in
      let fence = "```" in
      let lang_el = Ui.text ~style:cb_style.lang_style lang in
      let fence_el = Ui.text ~style:cb_style.fence_style fence in
      let code_els =
        List.map
          (fun line ->
            Ui.text
              ~style:Mosaic.Style.(base_style ++ cb_style.block.style)
              ~wrap:false line)
          code_lines
      in
      let content =
        Ui.vbox
          ~padding:
            (Ui.padding_xy cb_style.block.padding_left
               cb_style.block.padding_right)
          ([ Ui.hbox [ fence_el; lang_el ] ] @ code_els @ [ fence_el ])
      in
      [
        Ui.spacer cb_style.block.margin_top;
        content;
        Ui.spacer cb_style.block.margin_bottom;
      ]
  | Html_block (lines, _) ->
      let hb_style = r.style.paragraph in
      let text =
        lines |> List.map Cmarkit.Block_line.to_string |> String.concat "\n"
      in
      let content =
        Ui.text ~style:Mosaic.Style.(base_style ++ r.style.html) text
      in
      [
        Ui.spacer hb_style.margin_top; content; Ui.spacer hb_style.margin_bottom;
      ]
  | Thematic_break (_, _) ->
      let hr_style, hr_char = r.style.horizontal_rule in
      let line = String.make available_width hr_char.[0] in
      [
        Ui.spacer 1;
        Ui.text ~style:Mosaic.Style.(base_style ++ hr_style) line;
        Ui.spacer 1;
      ]
  | Ext_table (tbl, _) ->
      let tb_style = r.style.table in
      let base = Mosaic.Style.(base_style ++ tb_style.block.style) in
      let sep_style, sep_char = tb_style.separator_style in
      let col_count = Cmarkit.Block.Table.col_count tbl in
      let rows = Cmarkit.Block.Table.rows tbl in
      let get_row_type ((r, _), _) = r in
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
      let alignments =
        match sep_row_opt with
        | Some ((`Sep seps, _), _) ->
            Array.of_list
              (List.map (fun ((a, _), _) -> Option.value ~default:`Left a) seps)
        | _ -> Array.init col_count (fun _ -> `Left)
      in
      let get_cells r =
        match r with
        | `Header cs -> List.map (fun (i, _) -> i) cs
        | `Data cs -> List.map (fun (i, _) -> i) cs
        | `Sep _ -> []
      in
      let col_widths = Array.init col_count (fun _ -> 0) in
      let update_widths rows_list =
        List.iter
          (fun row_node ->
            let cells = get_cells (get_row_type row_node) in
            List.iteri
              (fun i inline ->
                let len = u_length (string_of_inlines inline) in
                if len > col_widths.(i) then col_widths.(i) <- len)
              cells)
          rows_list
      in
      update_widths header_rows;
      update_widths data_rows;
      let pad_text align text w =
        let len = u_length text in
        if len >= w then text
        else
          let pad = w - len in
          match align with
          | `Left -> text ^ String.make pad ' '
          | `Right -> String.make pad ' ' ^ text
          | `Center ->
              let left = pad / 2 in
              String.make left ' ' ^ text ^ String.make (pad - left) ' '
      in
      let render_row cells row_style alignments =
        let cell_texts =
          List.mapi
            (fun i inline ->
              pad_text alignments.(i) (string_of_inlines inline) col_widths.(i))
            cells
        in
        let cell_els =
          List.map (fun text -> Ui.text ~style:row_style text) cell_texts
        in
        Ui.hbox
          (Ui.text ~style:sep_style "|"
          :: List.concat
               (List.map
                  (fun el -> [ el; Ui.text ~style:sep_style " |" ])
                  cell_els))
      in
      let header_els =
        List.map
          (fun row ->
            render_row
              (get_cells (get_row_type row))
              tb_style.header_style alignments)
          header_rows
      in
      let data_els =
        List.map
          (fun row -> render_row (get_cells (get_row_type row)) base alignments)
          data_rows
      in
      let sep_el =
        let dashes =
          Array.to_list
            (Array.map (fun w -> String.make w sep_char.[0]) col_widths)
        in
        Ui.hbox
          (Ui.text ~style:sep_style "|"
          :: List.concat
               (List.map
                  (fun d ->
                    [ Ui.text ~style:sep_style d; Ui.text ~style:sep_style "|" ])
                  dashes))
      in
      let content = Ui.vbox (header_els @ [ sep_el ] @ data_els) in
      let padded =
        Ui.vbox
          ~padding:
            (Ui.padding_xy tb_style.block.padding_left
               tb_style.block.padding_right)
          [ content ]
      in
      [
        Ui.spacer tb_style.block.margin_top;
        padded;
        Ui.spacer tb_style.block.margin_bottom;
      ]
  | Blocks (blocks, _) ->
      List.concat_map (render_block r ~available_width ~base_style) blocks
  | _ -> []

and render_list_item r ~available_width ~base_style
    (item_node : Cmarkit.Block.List_item.t Cmarkit.node) =
  let item, _ = item_node in
  let l_style = r.style.list in
  let level = List.length r.list_stack in
  let indent_size = (level - 1) * l_style.level_indent in
  let task_marker = Cmarkit.Block.List_item.ext_task_marker item in
  let prefix, prefix_style, state_change =
    match task_marker with
    | Some marker_node ->
        let marker, _ = marker_node in
        let status =
          Cmarkit.Block.List_item.task_status_of_task_marker marker
        in
        let checked = match status with `Checked -> true | _ -> false in
        let p = if checked then "[x]" else "[ ]" in
        let ps =
          if checked then l_style.checked_style else l_style.task_style
        in
        (p, ps, fun () -> ())
    | None -> (
        match List.hd r.list_stack with
        | { ordered = true; counter; _ } as state ->
            let p = string_of_int counter ^ "." in
            ( p,
              l_style.item_prefix_style,
              fun () -> state.counter <- counter + 1 )
        | _ -> (l_style.item_prefix, l_style.item_prefix_style, fun () -> ()))
  in
  let avail =
    available_width - indent_size - u_length prefix - l_style.item_gap
  in
  let item_block = Cmarkit.Block.List_item.block item in
  let children = render_block r ~available_width:avail ~base_style item_block in
  let content =
    Ui.hbox ~gap:l_style.item_gap
      [ Ui.text ~style:prefix_style prefix; Ui.vbox children ]
  in
  state_change ();
  [ Ui.hbox [ Ui.spacer indent_size; content ] ]

let render ?(style = Markdown_style.default) ?(width = 80) ?(strict = false)
    markdown_text =
  let r =
    {
      style;
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
    render_block r ~available_width:avail ~base_style:Mosaic.Style.empty
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
              render_block r ~available_width:avail
                ~base_style:Mosaic.Style.empty b
            in
            Ui.hbox [ prefix; Ui.vbox content ] :: acc)
          r.footnotes []
        |> List.sort (fun _ _ -> 0)
        (* sort by key if needed *)
      in
      Ui.spacer 1 :: fn_heading :: Ui.spacer 1 :: fns
  in
  Ui.vbox ~width
    ~padding:(Ui.padding_xy s.padding_left s.padding_right)
    ([ Ui.spacer s.margin_top ]
    @ elements @ footnote_elements
    @ [ Ui.spacer s.margin_bottom ])
