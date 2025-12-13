module Ui = Mosaic_ui
module Style = Markdown_style

(* --- Props --- *)

module Props = struct
  type t = {
    style : Style.t;
    width : int;
    strict : bool;
    syntax_client : Mosaic_syntax.t;
    content : string;
  }

  let make ?(style = Style.default) ?(width = 80) ?(strict = false)
      ?(syntax_client = Mosaic_syntax.default_client ()) ?(content = "") () =
    { style; width; strict; syntax_client; content }

  let default = make ()

  let equal a b =
    a.style == b.style && a.width = b.width && a.strict = b.strict
    && a.syntax_client == b.syntax_client
    && String.equal a.content b.content
end

(* --- Internal Types --- *)

type list_state = { ordered : bool; mutable counter : int }

type render_ctx = {
  parent : Ui.Renderable.t;
  style : Style.t;
  width : int option;
  link_defs : (string, string) Hashtbl.t;
  syntax_client : Mosaic_syntax.t;
  mutable list_stack : list_state list;
  mutable quote_depth : int;
}

type t = {
  node : Ui.Renderable.t;
  content : Ui.Renderable.t;
  slot : Ui.Renderable.t option;
  mutable props : Props.t;
}

(* --- Layout Helpers --- *)

let lp n = Toffee.Style.Length_percentage.length (Float.of_int n)
let lpa n = Toffee.Style.Length_percentage_auto.length (Float.of_int n)
let dim n = Toffee.Style.Dimension.length (Float.of_int n)

let gap n =
  let lp = Toffee.Style.Length_percentage.length (Float.of_int n) in
  Toffee.Geometry.Size.make lp lp

let width_pct100 =
  Toffee.Geometry.Size.make
    (Toffee.Style.Dimension.pct 100.)
    Toffee.Style.Dimension.auto

let block_margin (block : Style.block) =
  if block.margin_top = 0 && block.margin_bottom = 0 then None
  else
    Some
      (Toffee.Geometry.Rect.make ~left:(lpa 0) ~right:(lpa 0)
         ~top:(lpa block.margin_top) ~bottom:(lpa block.margin_bottom))

let block_padding (block : Style.block) =
  if block.padding_left = 0 && block.padding_right = 0 then None
  else
    Some
      (Toffee.Geometry.Rect.make ~left:(lp block.padding_left)
         ~right:(lp block.padding_right) ~top:(lp 0) ~bottom:(lp 0))

(* --- Node Creation Helpers --- *)

let create_node ctx ?style () =
  match Ui.Renderable.create_child ~parent:ctx.parent () with
  | Error _ -> None
  | Ok node -> (
      match style with
      | None -> Some node
      | Some s -> (
          match Ui.Renderable.set_style node s with
          | Ok () -> Some node
          | Error _ -> None))

let append_child ~parent ~child =
  match Ui.Renderable.append_child ~parent ~child with
  | Ok () -> true
  | Error _ -> false

let append_children ~parent children =
  List.for_all (fun child -> append_child ~parent ~child) children

let make_box ctx ?(flex_direction = Toffee.Style.Flex_direction.Column) ?gap
    ?margin ?padding ?size ?flex_grow ?flex_shrink ?border ?border_sides
    ?border_color ?should_fill children =
  let style =
    Toffee.Style.make ~flex_direction ?gap ?margin ?padding ?size ?flex_grow
      ?flex_shrink ()
  in
  match create_node ctx ~style () with
  | None -> None
  | Some node ->
      let box_props =
        Ui.Box.Props.make ?border ?border_sides ?border_color ?should_fill ()
      in
      let _ = Ui.Box.mount ~props:box_props node in
      if append_children ~parent:node children then Some node else None

let make_text ctx ?text_style ?wrap_mode ?size ?flex_grow ?flex_shrink content =
  let style = Toffee.Style.make ?size ?flex_grow ?flex_shrink () in
  match create_node ctx ~style () with
  | None -> None
  | Some node ->
      let text_props = Ui.Text.Props.make ?text_style ?wrap_mode ~content () in
      let _ = Ui.Text.mount ~props:text_props node in
      Some node

let make_text_fragments ctx ?text_style ?wrap_mode ?size ?flex_grow ?flex_shrink
    fragments =
  let style = Toffee.Style.make ?size ?flex_grow ?flex_shrink () in
  match create_node ctx ~style () with
  | None -> None
  | Some node ->
      let text_props =
        Ui.Text.Props.make ?text_style ?wrap_mode ~content:"" ()
      in
      let t = Ui.Text.mount ~props:text_props node in
      Ui.Text.set_fragments t fragments;
      Some node

let make_code ctx ?syntax_style ?filetype ?wrap_mode content =
  let style = Toffee.Style.make () in
  match create_node ctx ~style () with
  | None -> None
  | Some node ->
      let code_props =
        Ui.Code.Props.make ?syntax_style ?filetype
          ~syntax_client:ctx.syntax_client ?wrap_mode ~content ()
      in
      let _ = Ui.Code.mount ~props:code_props node in
      Some node

let make_table ctx ~columns ~rows ?box_style ?border_style ?cell_style
    ?show_header () =
  let style = Toffee.Style.make () in
  match create_node ctx ~style () with
  | None -> None
  | Some node ->
      let table_props =
        Ui.Table.Props.make ~columns ~rows ?box_style ?border_style ?cell_style
          ?show_header ()
      in
      let _ = Ui.Table.mount ~props:table_props node in
      Some node

let block_container ctx ?(flex_direction = Toffee.Style.Flex_direction.Column)
    ?gap block children =
  make_box ctx ~flex_direction ?gap ?margin:(block_margin block)
    ?padding:(block_padding block) ~size:width_pct100 ~should_fill:false
    children

(* --- Style Helpers --- *)

let merge_style base overlay = Ansi.Style.merge ~base ~overlay

let default_syntax_overlays =
  let palette idx = Ansi.Color.of_palette_index idx in
  let style ?bold ?italic idx =
    Ansi.Style.make ?bold ?italic ~fg:(palette idx) ()
  in
  [
    ("comment", style ~italic:true 244);
    ("comment.line", style ~italic:true 244);
    ("comment.block", style ~italic:true 244);
    ("keyword", style ~bold:true 204);
    ("keyword.control", style ~bold:true 204);
    ("keyword.operator", style 204);
    ("operator", style 204);
    ("number", style 180);
    ("constant", style 180);
    ("string", style 214);
    ("string.special", style 216);
    ("escape", style 216);
    ("variable.parameter", style ~italic:true 110);
    ("property", style 110);
    ("function", style 79);
    ("function.method", style 79);
    ("function.builtin", style ~bold:true 120);
    ("type", style 81);
    ("type.builtin", style ~bold:true 81);
    ("constructor", style 215);
    ("module", style 75);
    ("tag", style 170);
    ("punctuation.bracket", style 244);
    ("punctuation.delimiter", style 244);
    ("punctuation.special", style 244);
  ]

let build_syntax_style default =
  let rules =
    List.map
      (fun (capture, overlay) -> (capture, merge_style default overlay))
      default_syntax_overlays
  in
  Ui.Code.Syntax_style.create ~default rules

(* --- Inline Rendering --- *)

module Fragment = Ui.Text.Fragment

let rec fragments_of_inline ctx inline =
  let open Cmarkit.Inline in
  match inline with
  | Text (text, _) -> [ Fragment.text text ]
  | Code_span (code, _) ->
      [ Fragment.text ~style:ctx.style.code (Code_span.code code) ]
  | Emphasis (em, _) ->
      let frags = fragments_of_inline ctx (Emphasis.inline em) in
      [ Fragment.span ~style:ctx.style.emph frags ]
  | Strong_emphasis (em, _) ->
      let frags = fragments_of_inline ctx (Emphasis.inline em) in
      [ Fragment.span ~style:ctx.style.strong frags ]
  | Ext_strikethrough (strike, _) ->
      let frags = fragments_of_inline ctx (Strikethrough.inline strike) in
      [ Fragment.span ~style:ctx.style.strike frags ]
  | Link (link, _) -> render_link ctx link
  | Autolink (auto, _) -> render_autolink ctx auto
  | Image (image, _) -> render_image ctx image
  | Raw_html (html, _) -> render_raw_html ctx html
  | Break (br, _) ->
      let text = if Break.type' br = `Hard then "\n" else " " in
      [ Fragment.text text ]
  | Inlines (nodes, _) -> List.concat_map (fragments_of_inline ctx) nodes
  | Ext_math_span (ms, _) -> [ Fragment.text (Math_span.tex ms) ]
  | other -> render_fallback_inline other

and render_link ctx link =
  let open Cmarkit.Inline in
  let frags = fragments_of_inline ctx (Link.text link) in
  let dest =
    match Link.reference link with
    | `Inline def -> (
        let defn, _ = def in
        match Cmarkit.Link_definition.dest defn with
        | Some (uri, _) -> uri
        | None -> "")
    | `Ref (_, _, label) ->
        let key = Cmarkit.Label.key label in
        Hashtbl.find_opt ctx.link_defs key |> Option.value ~default:""
  in
  let style =
    if dest = "" then ctx.style.link
    else Ansi.Style.hyperlink dest ctx.style.link
  in
  [ Fragment.span ~style frags ]

and render_autolink ctx auto =
  let uri, _ = Cmarkit.Inline.Autolink.link auto in
  let style = Ansi.Style.hyperlink uri ctx.style.link in
  [ Fragment.text ~style uri ]

and render_image ctx image =
  let open Cmarkit.Inline in
  let alt =
    Link.text image
    |> to_plain_text ~break_on_soft:false
    |> List.map (String.concat "")
    |> String.concat ""
  in
  let uri =
    match Link.reference image with
    | `Inline def -> (
        let defn, _ = def in
        match Cmarkit.Link_definition.dest defn with
        | Some (dest, _) -> dest
        | None -> "")
    | `Ref (_, _, label) ->
        let key = Cmarkit.Label.key label in
        Hashtbl.find_opt ctx.link_defs key |> Option.value ~default:""
  in
  let text =
    if uri = "" then Printf.sprintf "[Image: %s]" alt
    else Printf.sprintf "[Image: %s <%s>]" alt uri
  in
  [ Fragment.text ~style:ctx.style.image text ]

and render_raw_html ctx html =
  let content =
    List.map Cmarkit.Block_line.tight_to_string html |> String.concat ""
  in
  [ Fragment.text ~style:ctx.style.html content ]

and render_fallback_inline inline =
  let plain =
    Cmarkit.Inline.to_plain_text ~break_on_soft:false inline
    |> List.map (String.concat "")
    |> String.concat "\n"
  in
  if plain = "" then [] else [ Fragment.text plain ]

let inline_plain_text inline =
  Cmarkit.Inline.to_plain_text ~break_on_soft:false inline
  |> List.map (String.concat "")
  |> String.concat ""

(* --- Block Helpers --- *)

let normalize_block_spacing block ~next_is_thematic_break =
  if next_is_thematic_break then { block with Style.margin_bottom = 0 }
  else block

(* --- Block Rendering --- *)

let rec render_block ctx ~base_style ?next block =
  let next_is_thematic_break =
    match next with Some (Cmarkit.Block.Thematic_break _) -> true | _ -> false
  in
  let open Cmarkit.Block in
  match block with
  | Paragraph (para, _) ->
      render_paragraph ctx ~base_style ~next_is_thematic_break para
  | Heading (heading, _) ->
      render_heading ctx ~base_style ~next_is_thematic_break heading
  | Block_quote (quote, _) ->
      render_block_quote ctx ~base_style ~next_is_thematic_break quote
  | List (lst, _) -> render_list ctx ~base_style ~next_is_thematic_break lst
  | Code_block (code, _) ->
      render_code_block ctx ~base_style ~next_is_thematic_break code
  | Thematic_break _ -> render_thematic_break ctx
  | Html_block (html, _) ->
      render_html_block ctx ~base_style ~next_is_thematic_break html
  | Ext_table (tbl, _) -> render_table ctx ~next_is_thematic_break tbl
  | Blocks (blocks, _) -> render_blocks ctx ~base_style blocks
  | Blank_line _ -> []
  | _ -> []

and render_blocks ctx ~base_style blocks =
  let rec next_non_blank = function
    | [] -> None
    | b :: rest -> (
        let open Cmarkit.Block in
        match b with Blank_line _ -> next_non_blank rest | _ -> Some b)
  in
  let rec aux = function
    | [] -> []
    | [ last ] -> render_block ctx ~base_style last
    | block :: rest ->
        let next = next_non_blank rest in
        render_block ctx ~base_style ?next block @ aux rest
  in
  aux blocks

and render_paragraph ctx ~base_style ~next_is_thematic_break paragraph =
  let inline = Cmarkit.Block.Paragraph.inline paragraph in
  let fragments = fragments_of_inline ctx inline in
  let text_style = merge_style base_style ctx.style.paragraph.text_style in
  let block_style =
    normalize_block_spacing ctx.style.paragraph ~next_is_thematic_break
  in
  let text_node =
    match fragments with
    | [] ->
        make_text ctx ~text_style ~wrap_mode:`Word (inline_plain_text inline)
    | _ -> make_text_fragments ctx ~text_style ~wrap_mode:`Word fragments
  in
  match text_node with
  | None -> []
  | Some node -> (
      match block_container ctx block_style [ node ] with
      | None -> []
      | Some container -> [ container ])

and render_heading ctx ~base_style ~next_is_thematic_break heading =
  let level = Cmarkit.Block.Heading.level heading in
  let block_style =
    match level with
    | 1 -> ctx.style.h1
    | 2 -> ctx.style.h2
    | 3 -> ctx.style.h3
    | 4 -> ctx.style.h4
    | 5 -> ctx.style.h5
    | _ -> ctx.style.h6
  in
  let block_style =
    normalize_block_spacing block_style ~next_is_thematic_break
  in
  let fragments =
    fragments_of_inline ctx (Cmarkit.Block.Heading.inline heading)
  in
  let base = merge_style base_style ctx.style.heading.text_style in
  let text_style = merge_style base block_style.text_style in
  let heading_text =
    make_text_fragments ctx ~text_style ~wrap_mode:`None fragments
  in
  let hashes = String.make level '#' in
  let prefix =
    make_text ctx
      ~text_style:(merge_style base ctx.style.heading_prefix)
      ~wrap_mode:`None hashes
  in
  match (prefix, heading_text) with
  | Some p, Some h -> (
      match
        make_box ctx ~flex_direction:Toffee.Style.Flex_direction.Row
          ~gap:(gap 1) [ p; h ]
      with
      | None -> []
      | Some row -> (
          match block_container ctx block_style [ row ] with
          | None -> []
          | Some container -> [ container ]))
  | _ -> []

and render_block_quote ctx ~base_style ~next_is_thematic_break quote =
  let block_style =
    normalize_block_spacing ctx.style.block_quote ~next_is_thematic_break
  in
  ctx.quote_depth <- ctx.quote_depth + 1;
  let prefix_width = 1 in
  let available_width =
    match ctx.width with
    | Some w ->
        let padding = block_style.padding_left + block_style.padding_right in
        let width = w - padding - prefix_width in
        if width > 1 then Some width else Some 1
    | None -> None
  in
  let children =
    render_blockquote_content ctx ~base_style ~available_width
      (Cmarkit.Block.Block_quote.block quote)
  in
  ctx.quote_depth <- ctx.quote_depth - 1;
  if ctx.quote_depth < 0 then ctx.quote_depth <- 0;
  let content_margin =
    let left = block_style.padding_left in
    let right = block_style.padding_right in
    if left = 0 && right = 0 then None
    else
      Some
        (Toffee.Geometry.Rect.make ~left:(lpa left) ~right:(lpa right)
           ~top:(lpa 0) ~bottom:(lpa 0))
  in
  let container_style =
    if content_margin = None then block_style
    else { block_style with padding_left = 0; padding_right = 0 }
  in
  let border_color =
    match block_style.text_style.Ansi.Style.fg with
    | Some color -> color
    | None -> Ansi.Color.of_rgb 128 128 128
  in
  let size =
    available_width
    |> Option.map (fun w ->
        Toffee.Geometry.Size.make (dim w) Toffee.Style.Dimension.auto)
  in
  match
    make_box ctx ~flex_direction:Toffee.Style.Flex_direction.Column ~gap:(gap 0)
      ~flex_grow:1.0 ~flex_shrink:1.0 ?size children
  with
  | None -> []
  | Some core -> (
      let content_box =
        match content_margin with
        | None -> Some core
        | Some margin ->
            make_box ctx ~flex_direction:Toffee.Style.Flex_direction.Column
              ~gap:(gap 0) ~margin [ core ]
      in
      match content_box with
      | None -> []
      | Some box -> (
          match
            make_box ctx ~flex_direction:Toffee.Style.Flex_direction.Column
              ~gap:(gap 0) ~size:width_pct100 ~border:true
              ~border_sides:[ `Left ] ~border_color [ box ]
          with
          | None -> []
          | Some bordered ->
              if ctx.quote_depth = 0 then
                match block_container ctx container_style [ bordered ] with
                | None -> []
                | Some container -> [ container ]
              else [ bordered ]))

and render_blockquote_content ctx ~base_style ~available_width block =
  let open Cmarkit.Block in
  match block with
  | Paragraph (p, _) -> (
      let fragments = fragments_of_inline ctx (Paragraph.inline p) in
      let size =
        available_width
        |> Option.map (fun w ->
            Toffee.Geometry.Size.make (dim w) Toffee.Style.Dimension.auto)
      in
      match
        make_text_fragments ctx ~text_style:base_style ~wrap_mode:`Word ?size
          ~flex_grow:1.0 ~flex_shrink:1.0 fragments
      with
      | None -> []
      | Some node -> [ node ])
  | Blank_line _ -> (
      match make_text ctx "" with None -> [] | Some node -> [ node ])
  | Blocks (blocks, _) ->
      List.concat_map
        (render_blockquote_content ctx ~base_style ~available_width)
        blocks
  | _ -> render_block ctx ~base_style block

and render_list ctx ~base_style ~next_is_thematic_break lst =
  let type' = Cmarkit.Block.List'.type' lst in
  let start, ordered =
    match type' with `Ordered (n, _) -> (n, true) | `Unordered _ -> (1, false)
  in
  ctx.list_stack <- { ordered; counter = start } :: ctx.list_stack;
  let items =
    List.filter_map
      (render_list_item ctx ~base_style)
      (Cmarkit.Block.List'.items lst)
  in
  ctx.list_stack <- (match ctx.list_stack with _ :: tl -> tl | [] -> []);
  let block =
    let base_block = ctx.style.list.block in
    let base_block =
      normalize_block_spacing base_block ~next_is_thematic_break
    in
    let depth = List.length ctx.list_stack in
    if depth > 0 && base_block.margin_bottom <> 0 then
      { base_block with margin_bottom = 0 }
    else base_block
  in
  match
    make_box ctx ~flex_direction:Toffee.Style.Flex_direction.Column ~gap:(gap 0)
      items
  with
  | None -> []
  | Some inner -> (
      match block_container ctx block [ inner ] with
      | None -> []
      | Some container -> [ container ])

and render_list_item ctx ~base_style
    (item_node : Cmarkit.Block.List_item.t Cmarkit.node) =
  let item, _ = item_node in
  let state = List.hd ctx.list_stack in
  let list_style = ctx.style.list in
  let depth =
    let len = List.length ctx.list_stack in
    if len <= 1 then 0 else (len - 1) / 2
  in
  let indent_prefix =
    if depth <= 0 then "" else String.make list_style.level_indent ' '
  in
  let marker_text, marker_style =
    match Cmarkit.Block.List_item.ext_task_marker item with
    | Some (mark, _) -> (
        let status = Cmarkit.Block.List_item.task_status_of_task_marker mark in
        match status with
        | `Checked -> ("[x]", list_style.checked_style)
        | `Unchecked -> ("[ ]", list_style.task_style)
        | `Cancelled -> ("[-]", list_style.task_style)
        | `Other _ -> ("[?]", list_style.task_style))
    | None ->
        if state.ordered then (
          let label = Printf.sprintf "%d." state.counter in
          state.counter <- state.counter + 1;
          (label, list_style.item_prefix_style))
        else (list_style.item_prefix, list_style.item_prefix_style)
  in
  let marker_content =
    if indent_prefix = "" then marker_text else indent_prefix ^ marker_text
  in
  match
    make_text ctx ~text_style:marker_style ~wrap_mode:`None marker_content
  with
  | None -> None
  | Some marker -> (
      let content_style = merge_style base_style list_style.block.text_style in
      let content_nodes =
        render_list_content ctx ~base_style:content_style
          (Cmarkit.Block.List_item.block item)
      in
      match
        make_box ctx ~flex_direction:Toffee.Style.Flex_direction.Column
          ~gap:(gap 0) content_nodes
      with
      | None -> None
      | Some content_box -> (
          match
            make_box ctx ~flex_direction:Toffee.Style.Flex_direction.Row
              ~gap:(gap list_style.item_gap) [ marker; content_box ]
          with
          | None -> None
          | Some row ->
              make_box ctx ~flex_direction:Toffee.Style.Flex_direction.Column
                [ row ]))

and render_list_content ctx ~base_style block =
  let open Cmarkit.Block in
  match block with
  | Paragraph (p, _) -> (
      let fragments = fragments_of_inline ctx (Paragraph.inline p) in
      match
        make_text_fragments ctx ~text_style:base_style ~wrap_mode:`Word
          fragments
      with
      | None -> []
      | Some node -> [ node ])
  | Blocks (blocks, _) ->
      List.concat_map (render_list_content ctx ~base_style) blocks
  | Blank_line _ -> []
  | _ -> render_block ctx ~base_style block

and render_code_block ctx ~base_style ~next_is_thematic_break code =
  let block_style =
    normalize_block_spacing ctx.style.code_block.block ~next_is_thematic_break
  in
  let text_style = merge_style base_style block_style.text_style in
  let lines =
    Cmarkit.Block.Code_block.code code |> List.map Cmarkit.Block_line.to_string
  in
  let language =
    match Cmarkit.Block.Code_block.info_string code with
    | None -> ""
    | Some (info, _) -> (
        match Cmarkit.Block.Code_block.language_of_info_string info with
        | None -> ""
        | Some (lang, _) -> lang)
  in
  let code_content = String.concat "\n" lines in
  let filetype =
    match String.lowercase_ascii language with "" -> None | lang -> Some lang
  in
  let syntax_style = build_syntax_style text_style in
  let code_node =
    make_code ctx ~syntax_style ?filetype ~wrap_mode:`None code_content
  in
  match code_node with
  | None -> []
  | Some code_nd -> (
      let children =
        if ctx.style.code_block.show_fences then
          let fence_style = ctx.style.code_block.fence_style in
          let start_fence_text =
            if language = "" then "```" else "```" ^ language
          in
          let start_fence =
            make_text ctx ~text_style:fence_style ~wrap_mode:`None
              start_fence_text
          in
          let end_fence =
            make_text ctx ~text_style:fence_style ~wrap_mode:`None "```"
          in
          match (start_fence, end_fence) with
          | Some sf, Some ef -> [ sf; code_nd; ef ]
          | _ -> [ code_nd ]
        else [ code_nd ]
      in
      match
        make_box ctx ~flex_direction:Toffee.Style.Flex_direction.Column children
      with
      | None -> []
      | Some inner -> (
          match block_container ctx block_style [ inner ] with
          | None -> []
          | Some container -> [ container ]))

and render_thematic_break ctx =
  let style, char_seq = ctx.style.horizontal_rule in
  let width = ctx.width |> Option.value ~default:40 in
  let char_seq = if String.length char_seq > 0 then char_seq else "-" in
  let line =
    let buf = Buffer.create (width * String.length char_seq) in
    for _ = 1 to width do
      Buffer.add_string buf char_seq
    done;
    Buffer.contents buf
  in
  let block_style =
    { ctx.style.paragraph with margin_top = 0; margin_bottom = 0 }
  in
  match make_text ctx ~text_style:style ~wrap_mode:`None line with
  | None -> []
  | Some text_node -> (
      match block_container ctx block_style [ text_node ] with
      | None -> []
      | Some container -> [ container ])

and render_html_block ctx ~base_style ~next_is_thematic_break lines =
  let text_style = merge_style base_style ctx.style.html in
  let content =
    List.map Cmarkit.Block_line.to_string lines |> String.concat "\n"
  in
  let block_style =
    normalize_block_spacing ctx.style.paragraph ~next_is_thematic_break
  in
  match make_text ctx ~text_style ~wrap_mode:`None content with
  | None -> []
  | Some text_node -> (
      match block_container ctx block_style [ text_node ] with
      | None -> []
      | Some container -> [ container ])

and render_table ctx ~next_is_thematic_break table =
  let rows = Cmarkit.Block.Table.rows table in
  let extract_cells row =
    match row with
    | `Header cells | `Data cells ->
        List.map (fun (inline, _) -> inline_plain_text inline) cells
    | `Sep _ -> []
  in
  let headers =
    rows
    |> List.find_map (fun ((row, _), _) ->
        match row with
        | `Header cells -> Some (extract_cells (`Header cells))
        | _ -> None)
    |> Option.value ~default:[]
  in
  let body =
    rows
    |> List.filter_map (fun ((row, _), _) ->
        match row with
        | `Data cells -> Some (extract_cells (`Data cells))
        | _ -> None)
  in
  if headers = [] && body = [] then []
  else
    let column_count =
      match headers with
      | [] -> ( match body with [] -> 0 | row :: _ -> List.length row)
      | row -> List.length row
    in
    let columns =
      List.init column_count (fun idx ->
          let header =
            List.nth_opt headers idx
            |> Option.map (fun text ->
                Ui.Table.cell ~style:ctx.style.table.header_style text)
          in
          Ui.Table.column ?header (Printf.sprintf "col-%d" idx))
    in
    let table_rows =
      List.map
        (fun values ->
          let padded =
            if List.length values < column_count then
              values
              @ List.init (column_count - List.length values) (fun _ -> "")
            else values
          in
          let cells =
            List.map
              (fun text -> Ui.Table.cell ~style:ctx.style.table.cell_style text)
              padded
          in
          Ui.Table.row cells)
        body
    in
    let block =
      normalize_block_spacing ctx.style.table.block ~next_is_thematic_break
    in
    match
      make_table ctx ~columns ~rows:table_rows
        ~box_style:ctx.style.table.box_style
        ~border_style:(fst ctx.style.table.separator_style)
        ~cell_style:ctx.style.table.cell_style ~show_header:(headers <> []) ()
    with
    | None -> []
    | Some table_node -> (
        match block_container ctx block [ table_node ] with
        | None -> []
        | Some container -> [ container ])

(* --- Link Definition Collection --- *)

let collect_link_defs block table =
  let folder =
    Cmarkit.Folder.make
      ~block:(fun _ _ blk ->
        match blk with
        | Cmarkit.Block.Link_reference_definition (def, _) -> (
            match Cmarkit.Link_definition.defined_label def with
            | None -> Cmarkit.Folder.default
            | Some label ->
                let key = Cmarkit.Label.key label in
                let dest =
                  match Cmarkit.Link_definition.dest def with
                  | Some (uri, _) -> uri
                  | None -> ""
                in
                Hashtbl.replace table key dest;
                Cmarkit.Folder.default)
        | _ -> Cmarkit.Folder.default)
      ()
  in
  ignore (Cmarkit.Folder.fold_block folder () block)

(* --- Content Rendering --- *)

let clear_children t =
  List.iter
    (fun child ->
      match t.slot with
      | Some slot when slot == child -> ()
      | _ -> ignore (Ui.Renderable.remove child))
    (Ui.Renderable.children t.content)

let render_content t =
  let props = t.props in
  clear_children t;
  if props.content <> "" then (
    let ctx =
      {
        parent = t.content;
        style = props.style;
        width = (if props.width > 0 then Some props.width else None);
        link_defs = Hashtbl.create 16;
        syntax_client = props.syntax_client;
        list_stack = [];
        quote_depth = 0;
      }
    in
    let doc =
      Cmarkit.Doc.of_string ~strict:props.strict ~layout:true props.content
    in
    collect_link_defs (Cmarkit.Doc.block doc) ctx.link_defs;
    let children =
      render_block ctx ~base_style:props.style.document.text_style
        (Cmarkit.Doc.block doc)
    in
    ignore (append_children ~parent:t.content children));
  Ui.Renderable.request_render t.node

(* --- Public API --- *)

let node t = t.node

let mount ?(props = Props.default) node =
  (* Create an internal slot for reconciler-managed children and a dedicated
     content container for the rendered markdown tree. *)
  let slot =
    match Ui.Renderable.create_child ~parent:node () with
    | Error _ -> None
    | Ok slot ->
        let _ =
          Ui.Renderable.set_style slot
            (Toffee.Style.make ~display:Toffee.Style.Display.None ())
        in
        ignore (Ui.Renderable.append_child ~parent:node ~child:slot);
        Some slot
  in
  let content =
    match Ui.Renderable.create_child ~parent:node () with
    | Error _ -> node
    | Ok content ->
        let style =
          Toffee.Style.make ~flex_direction:Toffee.Style.Flex_direction.Column
            ~size:width_pct100 ()
        in
        ignore (Ui.Renderable.set_style content style);
        ignore (Ui.Renderable.append_child ~parent:node ~child:content);
        content
  in
  Option.iter
    (fun slot ->
      Ui.Renderable.set_child_sink node
        (Some
           (fun ~child ~index ->
             ignore child;
             (slot, index)));
      Ui.Renderable.set_reconcile_parent node slot)
    slot;
  let style =
    Toffee.Style.make ~flex_direction:Toffee.Style.Flex_direction.Column
      ?margin:(block_margin props.style.document)
      ?padding:(block_padding props.style.document)
      ~size:
        (Toffee.Geometry.Size.make (dim props.width) Toffee.Style.Dimension.auto)
      ()
  in
  ignore (Ui.Renderable.set_style node style);
  let t = { node; content; slot; props } in
  render_content t;
  t

let set_content t content =
  if not (String.equal t.props.content content) then (
    t.props <- { t.props with content };
    render_content t)

let set_style t style =
  if t.props.style != style then (
    t.props <- { t.props with style };
    render_content t)

let set_width t width =
  if t.props.width <> width then (
    t.props <- { t.props with width };
    let size =
      Toffee.Geometry.Size.make (dim width) Toffee.Style.Dimension.auto
    in
    ignore (Ui.Renderable.set_style t.node (Toffee.Style.make ~size ()));
    render_content t)

let set_strict t strict =
  if t.props.strict <> strict then (
    t.props <- { t.props with strict };
    render_content t)

let set_syntax_client t client =
  if t.props.syntax_client != client then (
    t.props <- { t.props with syntax_client = client };
    render_content t)

let apply_props t (props : Props.t) =
  set_style t props.style;
  set_width t props.width;
  set_strict t props.strict;
  set_syntax_client t props.syntax_client;
  set_content t props.content

(* --- Element API --- *)

let markdown ?id ?visible ?z_index ?buffer ?live ?display ?box_sizing ?position
    ?overflow ?scrollbar_width ?inset ?size ?min_size ?max_size ?aspect_ratio
    ?margin ?padding ?gap ?align_items ?align_self ?align_content ?justify_items
    ?justify_self ?justify_content ?flex_direction ?flex_wrap ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_row ?grid_column ?style ?width ?strict ?syntax_client ?content
    ?on_mount () : Ui.element =
  let layout_style =
    Toffee.Style.make ?display ?box_sizing ?position ?overflow ?scrollbar_width
      ?inset ?size ?min_size ?max_size ?aspect_ratio ?margin ?padding ?gap
      ?align_items ?align_self ?align_content ?justify_items ?justify_self
      ?justify_content ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink
      ?flex_basis ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas ?grid_row
      ?grid_column ()
  in
  let markdown_props =
    Props.make ?style ?width ?strict ?syntax_client ?content ()
  in
  let ctor (renderer : Ui.Renderer.t) (markdown_props : Props.t) =
    let id = Option.value id ~default:(Ui.Renderer.gen_id renderer) in
    let host_props =
      Ui.Renderable.Props.make ~id ?visible ?z_index ?buffer ?live ()
    in
    match Ui.create_node renderer ~id ~host_props ~style:layout_style () with
    | Error _ as e -> e
    | Ok n ->
        let m = mount ~props:markdown_props n in
        Option.iter (fun f -> f m) on_mount;
        Ok (node m)
  in
  Ui.make (Ui.Renderable ctor) markdown_props []
