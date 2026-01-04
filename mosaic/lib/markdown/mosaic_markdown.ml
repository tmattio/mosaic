module Ui = Mosaic_ui

(* --- Small helpers --- *)

let merge_style base overlay = Ansi.Style.merge ~base ~overlay
let palette idx = Ansi.Color.of_palette_index idx
let clamp lo hi x = if x < lo then lo else if x > hi then hi else x

(* Toffee length helpers *)
let lp n = Toffee.Style.Length_percentage.length (Float.of_int n)
let lpa n = Toffee.Style.Length_percentage_auto.length (Float.of_int n)

let gap n =
  let lp = Toffee.Style.Length_percentage.length (Float.of_int n) in
  Toffee.Geometry.Size.make lp lp

let width_pct100 =
  Toffee.Geometry.Size.make
    (Toffee.Style.Dimension.pct 100.)
    Toffee.Style.Dimension.auto

(* --- Public Style --- *)

module Style = struct
  type insets = { top : int; right : int; bottom : int; left : int }

  let insets ?(top = 0) ?(right = 0) ?(bottom = 0) ?(left = 0) () =
    { top; right; bottom; left }

  let zero_insets = insets ()

  type block = { text : Ansi.Style.t; margin : insets; padding : insets }

  let block ?(text = Ansi.Style.default) ?(margin = zero_insets)
      ?(padding = zero_insets) () =
    { text; margin; padding }

  type headings = { base : block; levels : block list; prefix : Ansi.Style.t }

  type list_style = {
    block : block;
    unordered_marker : string;
    unordered_marker_style : Ansi.Style.t;
    ordered_marker_style : Ansi.Style.t;
    ordered_suffix : string;
    item_gap : int;
    indent_per_level : int;
    task_unchecked : string * Ansi.Style.t;
    task_checked : string * Ansi.Style.t;
    task_cancelled : string * Ansi.Style.t;
    task_other : string * Ansi.Style.t;
  }

  type code_block = {
    block : block;
    fence : Ansi.Style.t;
    language : Ansi.Style.t;
  }

  type thematic_break = { style : Ansi.Style.t; glyph : string }

  type table_style = {
    block : block;
    header : Ansi.Style.t;
    cell : Ansi.Style.t;
    border : Ansi.Style.t;
    box_style : Mosaic_ui.Table.box_style;
  }

  type inline = {
    emph : Ansi.Style.t;
    strong : Ansi.Style.t;
    code : Ansi.Style.t;
    link : Ansi.Style.t;
    image : Ansi.Style.t;
    raw_html : Ansi.Style.t;
    strike : Ansi.Style.t;
  }

  type t = {
    document : block;
    paragraph : block;
    headings : headings;
    block_quote : block;
    list : list_style;
    code_block : code_block;
    thematic_break : thematic_break;
    table : table_style;
    inline : inline;
  }

  let merge_insets (base : insets) (overlay : insets) : insets =
    {
      top = (if overlay.top <> 0 then overlay.top else base.top);
      right = (if overlay.right <> 0 then overlay.right else base.right);
      bottom = (if overlay.bottom <> 0 then overlay.bottom else base.bottom);
      left = (if overlay.left <> 0 then overlay.left else base.left);
    }

  let merge_block (base : block) (overlay : block) : block =
    {
      text = merge_style base.text overlay.text;
      margin = merge_insets base.margin overlay.margin;
      padding = merge_insets base.padding overlay.padding;
    }

  let heading (t : t) ~level =
    let level = clamp 1 6 level in
    let base = t.headings.base in
    match t.headings.levels with
    | [] -> base
    | levels ->
        let idx = level - 1 in
        let overlay =
          match List.nth_opt levels idx with
          | Some b -> b
          | None -> List.nth levels (List.length levels - 1)
        in
        merge_block base overlay

  let with_heading_palette colors (t : t) =
    let colors = match colors with [] -> [ palette 39 ] | xs -> xs in
    let color_for_level i =
      let last = List.nth colors (List.length colors - 1) in
      match List.nth_opt colors i with Some c -> c | None -> last
    in
    let set_fg (s : Ansi.Style.t) (c : Ansi.Color.t) =
      merge_style s (Ansi.Style.make ~fg:c ())
    in
    let levels =
      List.init 6 (fun i ->
          let existing =
            match List.nth_opt t.headings.levels i with
            | Some b -> b
            | None -> block ()
          in
          { existing with text = set_fg existing.text (color_for_level i) })
    in
    { t with headings = { t.headings with levels } }

  let map_document f t = { t with document = f t.document }
  let map_paragraph f t = { t with paragraph = f t.paragraph }
  let map_block_quote f t = { t with block_quote = f t.block_quote }
  let map_list f t = { t with list = f t.list }
  let map_code_block f t = { t with code_block = f t.code_block }
  let map_table f t = { t with table = f t.table }
  let map_inline f t = { t with inline = f t.inline }
  let map_headings f t = { t with headings = f t.headings }

  (* Defaults *)

  let default_dark =
    let heading_palette =
      [ 228; 220; 214; 208; 202; 196 ] |> List.map palette
    in
    let paragraph = block ~margin:(insets ~bottom:1 ()) () in
    let headings =
      let base =
        block
          ~text:(Ansi.Style.make ~bold:true ())
          ~margin:(insets ~bottom:1 ()) ()
      in
      let levels =
        List.map
          (fun c -> block ~text:(Ansi.Style.make ~fg:c ()) ())
          heading_palette
      in
      { base; levels; prefix = Ansi.Style.make ~fg:(palette 244) () }
    in
    let list =
      {
        block = block ~margin:(insets ~bottom:1 ()) ();
        unordered_marker = "•";
        unordered_marker_style = Ansi.Style.make ~fg:(palette 39) ();
        ordered_marker_style = Ansi.Style.make ~fg:(palette 39) ();
        ordered_suffix = ".";
        item_gap = 1;
        indent_per_level = 0;
        task_unchecked = ("[ ]", Ansi.Style.make ~fg:(palette 244) ());
        task_checked = ("[x]", Ansi.Style.make ~fg:(palette 39) ());
        task_cancelled = ("[-]", Ansi.Style.make ~fg:(palette 244) ());
        task_other = ("[?]", Ansi.Style.make ~fg:(palette 244) ());
      }
    in
    let code_block =
      {
        block =
          block
            ~text:(Ansi.Style.make ~fg:(palette 252) ())
            ~margin:(insets ~bottom:1 ()) ();
        fence = Ansi.Style.make ~fg:(palette 240) ();
        language = Ansi.Style.make ~fg:(palette 244) ();
      }
    in
    let table =
      {
        block = block ~margin:(insets ~bottom:1 ()) ();
        header = Ansi.Style.make ~bold:true ();
        cell = Ansi.Style.default;
        border = Ansi.Style.make ~fg:(palette 240) ();
        box_style = Mosaic_ui.Table.Heavy_head;
      }
    in
    let inline =
      {
        emph = Ansi.Style.make ~italic:true ();
        strong = Ansi.Style.make ~bold:true ();
        code = Ansi.Style.make ~fg:(palette 203) ~bg:(palette 236) ();
        link = Ansi.Style.make ~fg:(palette 30) ~underline:true ();
        image = Ansi.Style.make ~fg:(palette 212) ();
        raw_html = Ansi.Style.make ~fg:(palette 244) ~italic:true ();
        strike = Ansi.Style.make ~strikethrough:true ();
      }
    in
    {
      document = block ();
      paragraph;
      headings;
      block_quote =
        block ~padding:(insets ~left:1 ()) ~margin:(insets ~bottom:1 ()) ();
      list;
      code_block;
      thematic_break =
        { style = Ansi.Style.make ~fg:(palette 240) (); glyph = "─" };
      table;
      inline;
    }

  let default_light =
    (* A conservative light theme. Users can customize as desired. *)
    let heading_palette = [ 25; 24; 23; 22; 21; 20 ] |> List.map palette in
    let base = default_dark in
    base
    |> with_heading_palette heading_palette
    |> map_inline (fun i ->
        {
          i with
          link = Ansi.Style.make ~fg:(palette 19) ~underline:true ();
          raw_html = Ansi.Style.make ~fg:(palette 241) ~italic:true ();
        })
end

(* --- Public Props --- *)

module Props = struct
  type wrap_width = [ `Auto | `Columns of int ]
  type wrap_mode = [ `None | `Word | `Char ]
  type raw_html = [ `Show_as_text | `Drop ]
  type unknown = [ `Drop | `Plain_text | `Debug ]

  type link =
    | Hyperlink
    | Plain
    | Inline_url of {
        left : string;
        right : string;
        style : Ansi.Style.t option;
      }
    | Custom of
        (text:Ui.Text.Fragment.t list ->
        dest:string option ->
        Ui.Text.Fragment.t list)

  type image =
    | Alt_only
    | Alt_and_url
    | Hidden
    | Custom of (alt:string -> uri:string option -> Ui.Text.Fragment.t list)

  type headings = { show_prefix : bool; wrap : wrap_mode }

  type code_blocks = {
    show_fences : bool;
    wrap : wrap_mode;
    syntax : [ `Auto | `Theme of Ui.Code.Theme.t ];
  }

  let headings ?(show_prefix = true) ?(wrap = `Word) () = { show_prefix; wrap }

  let code_blocks ?(show_fences = true) ?(wrap = `None) ?(syntax = `Auto) () =
    { show_fences; wrap; syntax }

  type t = {
    content : string;
    style : Style.t;
    wrap_width : wrap_width;
    paragraph_wrap : wrap_mode;
    block_quote_wrap : wrap_mode;
    headings : headings;
    code_blocks : code_blocks;
    raw_html : raw_html;
    links : link;
    images : image;
    unknown_inline : unknown;
    unknown_block : unknown;
    languages : Mosaic_syntax.Set.t;
  }

  let default =
    {
      content = "";
      style = Style.default_dark;
      wrap_width = `Auto;
      paragraph_wrap = `Word;
      block_quote_wrap = `Word;
      headings = headings ();
      code_blocks = code_blocks ();
      raw_html = `Show_as_text;
      links = Hyperlink;
      images = Alt_and_url;
      unknown_inline = `Plain_text;
      unknown_block = `Plain_text;
      languages = Mosaic_syntax.builtins ();
    }

  let make ?(content = default.content) ?(style = default.style)
      ?(wrap_width = default.wrap_width)
      ?(paragraph_wrap = default.paragraph_wrap)
      ?(block_quote_wrap = default.block_quote_wrap)
      ?(headings = default.headings) ?(code_blocks = default.code_blocks)
      ?(raw_html = default.raw_html) ?(links = default.links)
      ?(images = default.images) ?(unknown_inline = default.unknown_inline)
      ?(unknown_block = default.unknown_block) ?(languages = default.languages)
      () =
    {
      content;
      style;
      wrap_width;
      paragraph_wrap;
      block_quote_wrap;
      headings;
      code_blocks;
      raw_html;
      links;
      images;
      unknown_inline;
      unknown_block;
      languages;
    }

  let equal a b =
    String.equal a.content b.content
    && a.style == b.style
    && a.wrap_width = b.wrap_width
    && a.paragraph_wrap = b.paragraph_wrap
    && a.block_quote_wrap = b.block_quote_wrap
    && a.headings = b.headings
    && a.code_blocks = b.code_blocks
    && a.raw_html = b.raw_html && a.links = b.links && a.images = b.images
    && a.unknown_inline = b.unknown_inline
    && a.unknown_block = b.unknown_block
    && a.languages == b.languages
end

(* --- Internal layout helpers from Style --- *)

let insets_is_zero (i : Style.insets) =
  i.top = 0 && i.right = 0 && i.bottom = 0 && i.left = 0

let margin_rect (i : Style.insets) =
  if insets_is_zero i then None
  else
    Some
      (Toffee.Geometry.Rect.make ~top:(lpa i.top) ~right:(lpa i.right)
         ~bottom:(lpa i.bottom) ~left:(lpa i.left))

let padding_rect (i : Style.insets) =
  if insets_is_zero i then None
  else
    Some
      (Toffee.Geometry.Rect.make ~top:(lp i.top) ~right:(lp i.right)
         ~bottom:(lp i.bottom) ~left:(lp i.left))

(* --- Node creation helpers --- *)

let create_node (ctx_parent : Ui.Renderable.t) ?style () =
  match Ui.Renderable.create_child ~parent:ctx_parent () with
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

let make_box ~parent ?(flex_direction = Toffee.Style.Flex_direction.Column) ?gap
    ?margin ?padding ?size ?flex_grow ?flex_shrink ?border ?border_sides
    ?border_color ?should_fill children =
  let style =
    Toffee.Style.make ~flex_direction ?gap ?margin ?padding ?size ?flex_grow
      ?flex_shrink ()
  in
  match create_node parent ~style () with
  | None -> None
  | Some node ->
      let box_props =
        Ui.Box.Props.make ?border ?border_sides ?border_color ?should_fill ()
      in
      let _ = Ui.Box.mount ~props:box_props node in
      if append_children ~parent:node children then Some node else None

(* Default selection colors for markdown text - a subtle highlight *)
let default_selection_bg = Some (Ansi.Color.of_rgb 88 166 255)
let default_selection_fg = Some Ansi.Color.white

let make_text ~parent ?style ?wrap_mode ?size ?flex_grow ?flex_shrink content =
  let layout_style = Toffee.Style.make ?size ?flex_grow ?flex_shrink () in
  match create_node parent ~style:layout_style () with
  | None -> None
  | Some node ->
      let text_props =
        Ui.Text.Props.make ?style ?wrap_mode ~content
          ?selection_bg:default_selection_bg ?selection_fg:default_selection_fg
          ()
      in
      let _ = Ui.Text.mount ~props:text_props node in
      Some node

let make_text_fragments ~parent ?style ?wrap_mode ?size ?flex_grow ?flex_shrink
    fragments =
  let layout_style = Toffee.Style.make ?size ?flex_grow ?flex_shrink () in
  match create_node parent ~style:layout_style () with
  | None -> None
  | Some node ->
      let text_props =
        Ui.Text.Props.make ?style ?wrap_mode ~content:""
          ?selection_bg:default_selection_bg ?selection_fg:default_selection_fg
          ()
      in
      let t = Ui.Text.mount ~props:text_props node in
      Ui.Text.set_fragments t fragments;
      Some node

let make_code ~parent ?theme ?filetype ?wrap_mode ~languages content =
  let style = Toffee.Style.make () in
  match create_node parent ~style () with
  | None -> None
  | Some node ->
      let code_props =
        Ui.Code.Props.make ?theme ?filetype ~languages ?wrap_mode
          ?selection_bg:default_selection_bg ?selection_fg:default_selection_fg
          ~content ()
      in
      let _ = Ui.Code.mount ~props:code_props node in
      Some node

let make_table ~parent ~columns ~rows ?box_style ?border_style ?cell_style
    ?show_header () =
  let style = Toffee.Style.make () in
  match create_node parent ~style () with
  | None -> None
  | Some node ->
      let table_props =
        Ui.Table.Props.make ~columns ~rows ?box_style ?border_style ?cell_style
          ?show_header ()
      in
      let _ = Ui.Table.mount ~props:table_props node in
      Some node

let block_container ~parent (block : Style.block) children =
  make_box ~parent ~flex_direction:Toffee.Style.Flex_direction.Column
    ?margin:(margin_rect block.margin)
    ?padding:(padding_rect block.padding)
    ~size:width_pct100 ~should_fill:false children

(* --- Syntax highlighting overlays (Auto) --- *)

let default_syntax_overlays =
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

let build_theme base = Ui.Code.Theme.create ~base default_syntax_overlays

(* --- Rendering context --- *)

type list_state = { ordered : bool; mutable counter : int }

type render_ctx = {
  parent : Ui.Renderable.t;
  props : Props.t;
  width : int option;
  link_defs : (string, string) Hashtbl.t;
  mutable list_stack : list_state list;
  mutable quote_depth : int;
  mutable paragraph_wrap : Props.wrap_mode;
}

let resolve_width = function
  | `Auto -> None
  | `Columns n ->
      let n = if n <= 0 then 1 else n in
      Some n

let ui_text_wrap_mode (w : Props.wrap_mode) =
  match w with
  | `None -> `None
  | `Word -> `Word
  | `Char ->
      (* Mosaic_ui.Text may or may not support char-wrapping; degrade safely. *)
      `Word

let ui_code_wrap_mode (w : Props.wrap_mode) =
  match w with `None -> `None | `Word -> `Word | `Char -> `Word

module Fragment = Ui.Text.Fragment

let inline_plain_text inline =
  Cmarkit.Inline.to_plain_text ~break_on_soft:false inline
  |> List.map (String.concat "")
  |> String.concat ""

let normalize_block_spacing (block : Style.block) ~next_is_thematic_break =
  if next_is_thematic_break then
    let m = block.margin in
    { block with margin = { m with bottom = 0 } }
  else block

(* --- Link def collection --- *)

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

(* --- Inline rendering --- *)

let render_unknown_inline ctx (inline : Cmarkit.Inline.t) =
  let plain = inline_plain_text inline in
  match ctx.props.unknown_inline with
  | `Drop ->
      ignore "mosaic.markdown: dropped unknown inline node";
      []
  | `Plain_text -> if plain = "" then [] else [ Fragment.text plain ]
  | `Debug ->
      ignore "mosaic.markdown: unknown inline node (debug)";
      let msg =
        if plain = "" then "[unsupported inline]" else "[unsupported inline] "
      in
      let s =
        merge_style ctx.props.style.inline.raw_html
          (Ansi.Style.make ~bold:true ())
      in
      if plain = "" then [ Fragment.text ~style:s msg ]
      else [ Fragment.text ~style:s (msg ^ plain) ]

let render_raw_html_inline ctx html =
  match ctx.props.raw_html with
  | `Drop -> []
  | `Show_as_text ->
      let content =
        List.map Cmarkit.Block_line.tight_to_string html |> String.concat ""
      in
      [ Fragment.text ~style:ctx.props.style.inline.raw_html content ]

let render_link_dest ctx (link : Cmarkit.Inline.Link.t) =
  let open Cmarkit.Inline in
  match Link.reference link with
  | `Inline def -> (
      let defn, _ = def in
      match Cmarkit.Link_definition.dest defn with
      | Some (uri, _) -> uri
      | None -> "")
  | `Ref (_, _, label) ->
      let key = Cmarkit.Label.key label in
      Hashtbl.find_opt ctx.link_defs key |> Option.value ~default:""

let rec fragments_of_inline ctx (inline : Cmarkit.Inline.t) =
  let open Cmarkit.Inline in
  match inline with
  | Text (text, _) -> [ Fragment.text text ]
  | Code_span (code, _) ->
      [ Fragment.text ~style:ctx.props.style.inline.code (Code_span.code code) ]
  | Emphasis (em, _) ->
      let frags = fragments_of_inline ctx (Emphasis.inline em) in
      [ Fragment.span ~style:ctx.props.style.inline.emph frags ]
  | Strong_emphasis (em, _) ->
      let frags = fragments_of_inline ctx (Emphasis.inline em) in
      [ Fragment.span ~style:ctx.props.style.inline.strong frags ]
  | Ext_strikethrough (strike, _) ->
      let frags = fragments_of_inline ctx (Strikethrough.inline strike) in
      [ Fragment.span ~style:ctx.props.style.inline.strike frags ]
  | Link (link, _) -> render_link ctx link
  | Autolink (auto, _) -> render_autolink ctx auto
  | Image (image, _) -> render_image ctx image
  | Raw_html (html, _) -> render_raw_html_inline ctx html
  | Break (br, _) ->
      let text = if Break.type' br = `Hard then "\n" else " " in
      [ Fragment.text text ]
  | Inlines (nodes, _) -> List.concat_map (fragments_of_inline ctx) nodes
  | Ext_math_span (ms, _) -> [ Fragment.text (Math_span.tex ms) ]
  | other -> render_unknown_inline ctx other

and render_link ctx (link : Cmarkit.Inline.Link.t) =
  let open Cmarkit.Inline in
  let text_frags = fragments_of_inline ctx (Link.text link) in
  let dest =
    let d = render_link_dest ctx link in
    if d = "" then None else Some d
  in
  match ctx.props.links with
  | Props.Hyperlink ->
      let style =
        match dest with
        | None -> ctx.props.style.inline.link
        | Some uri -> Ansi.Style.hyperlink uri ctx.props.style.inline.link
      in
      [ Fragment.span ~style text_frags ]
  | Props.Plain ->
      [ Fragment.span ~style:ctx.props.style.inline.link text_frags ]
  | Props.Inline_url { left; right; style } ->
      let link_text =
        [ Fragment.span ~style:ctx.props.style.inline.link text_frags ]
      in
      let url_frags =
        match dest with
        | None -> []
        | Some uri ->
            let s = Option.value style ~default:ctx.props.style.inline.link in
            let suffix =
              let l = if left = "" then "(" else left in
              let r = if right = "" then ")" else right in
              " " ^ l ^ uri ^ r
            in
            [ Fragment.text ~style:s suffix ]
      in
      link_text @ url_frags
  | Props.Custom f -> f ~text:text_frags ~dest

and render_autolink ctx (auto : Cmarkit.Inline.Autolink.t) =
  let uri, _ = Cmarkit.Inline.Autolink.link auto in
  let frags = [ Fragment.text uri ] in
  match ctx.props.links with
  | Props.Hyperlink ->
      let style = Ansi.Style.hyperlink uri ctx.props.style.inline.link in
      [ Fragment.span ~style frags ]
  | Props.Plain -> [ Fragment.span ~style:ctx.props.style.inline.link frags ]
  | Props.Inline_url { left; right; style } ->
      let s = Option.value style ~default:ctx.props.style.inline.link in
      let l = if left = "" then "(" else left in
      let r = if right = "" then ")" else right in
      [
        Fragment.span ~style:ctx.props.style.inline.link frags;
        Fragment.text ~style:s (" " ^ l ^ uri ^ r);
      ]
  | Props.Custom f -> f ~text:frags ~dest:(Some uri)

and render_image ctx (image : Cmarkit.Inline.Link.t) =
  let open Cmarkit.Inline in
  let alt = Link.text image |> inline_plain_text in
  let uri =
    let d = render_link_dest ctx image in
    if d = "" then None else Some d
  in
  match ctx.props.images with
  | Props.Hidden -> []
  | Props.Alt_only ->
      [
        Fragment.text ~style:ctx.props.style.inline.image
          (Printf.sprintf "[Image: %s]" alt);
      ]
  | Props.Alt_and_url ->
      let text =
        match uri with
        | None -> Printf.sprintf "[Image: %s]" alt
        | Some u -> Printf.sprintf "[Image: %s <%s>]" alt u
      in
      [ Fragment.text ~style:ctx.props.style.inline.image text ]
  | Props.Custom f -> f ~alt ~uri

(* --- Block plain-text fallback (best-effort) --- *)

let block_best_effort_plain_text (blk : Cmarkit.Block.t) : string =
  let buf = Buffer.create 128 in
  let add_line s =
    if s <> "" then (
      if Buffer.length buf > 0 then Buffer.add_char buf '\n';
      Buffer.add_string buf s)
  in
  let folder =
    Cmarkit.Folder.make
      ~block:(fun _ _ b ->
        let open Cmarkit.Block in
        match b with
        | Paragraph (p, _) ->
            add_line (inline_plain_text (Paragraph.inline p));
            Cmarkit.Folder.default
        | Heading (h, _) ->
            add_line (inline_plain_text (Heading.inline h));
            Cmarkit.Folder.default
        | Code_block (c, _) ->
            let lines =
              Code_block.code c |> List.map Cmarkit.Block_line.to_string
            in
            add_line (String.concat "\n" lines);
            Cmarkit.Folder.default
        | Html_block (lines, _) ->
            add_line
              (List.map Cmarkit.Block_line.to_string lines |> String.concat "\n");
            Cmarkit.Folder.default
        | _ -> Cmarkit.Folder.default)
      ()
  in
  ignore (Cmarkit.Folder.fold_block folder () blk);
  Buffer.contents buf

(* --- Block rendering --- *)

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
  | other -> render_unknown_block ctx ~base_style other

and render_unknown_block ctx ~base_style (blk : Cmarkit.Block.t) =
  match ctx.props.unknown_block with
  | `Drop ->
      ignore "mosaic.markdown: dropped unknown block node";
      []
  | `Plain_text -> (
      let text = block_best_effort_plain_text blk in
      if text = "" then []
      else
        let block_style =
          normalize_block_spacing ctx.props.style.paragraph
            ~next_is_thematic_break:false
        in
        let style = merge_style base_style block_style.text in
        let node =
          make_text ~parent:ctx.parent ~style
            ~wrap_mode:(ui_text_wrap_mode ctx.paragraph_wrap)
            text
        in
        match node with
        | None -> []
        | Some n -> (
            match block_container ~parent:ctx.parent block_style [ n ] with
            | None -> []
            | Some c -> [ c ]))
  | `Debug -> (
      ignore "mosaic.markdown: unknown block node (debug)";
      let msg = "[unsupported block]" in
      let s =
        merge_style ctx.props.style.inline.raw_html
          (Ansi.Style.make ~bold:true ())
      in
      let node =
        make_text ~parent:ctx.parent ~style:s
          ~wrap_mode:(ui_text_wrap_mode ctx.paragraph_wrap)
          msg
      in
      match node with
      | None -> []
      | Some n -> (
          match
            block_container ~parent:ctx.parent ctx.props.style.paragraph [ n ]
          with
          | None -> []
          | Some c -> [ c ]))

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
  let block_style =
    normalize_block_spacing ctx.props.style.paragraph ~next_is_thematic_break
  in
  let style = merge_style base_style block_style.text in
  let wrap_mode = ui_text_wrap_mode ctx.paragraph_wrap in
  let text_node =
    match fragments with
    | [] ->
        make_text ~parent:ctx.parent ~style ~wrap_mode
          (inline_plain_text inline)
    | _ -> make_text_fragments ~parent:ctx.parent ~style ~wrap_mode fragments
  in
  match text_node with
  | None -> []
  | Some node -> (
      match block_container ~parent:ctx.parent block_style [ node ] with
      | None -> []
      | Some container -> [ container ])

and render_heading ctx ~base_style ~next_is_thematic_break heading =
  let level = Cmarkit.Block.Heading.level heading in
  let block_style =
    Style.heading ctx.props.style ~level |> fun b ->
    normalize_block_spacing b ~next_is_thematic_break
  in
  let fragments =
    fragments_of_inline ctx (Cmarkit.Block.Heading.inline heading)
  in
  let style = merge_style base_style block_style.text in
  let wrap_mode = ui_text_wrap_mode ctx.props.headings.wrap in
  let heading_text =
    make_text_fragments ~parent:ctx.parent ~style ~wrap_mode fragments
  in
  let row =
    if ctx.props.headings.show_prefix then
      let hashes = String.make (clamp 1 6 level) '#' in
      let prefix_style =
        merge_style base_style ctx.props.style.headings.prefix
      in
      let prefix =
        make_text ~parent:ctx.parent ~style:prefix_style ~wrap_mode:`None hashes
      in
      match (prefix, heading_text) with
      | Some p, Some h ->
          make_box ~parent:ctx.parent
            ~flex_direction:Toffee.Style.Flex_direction.Row ~gap:(gap 1)
            [ p; h ]
      | _ -> None
    else
      match heading_text with
      | None -> None
      | Some h ->
          make_box ~parent:ctx.parent
            ~flex_direction:Toffee.Style.Flex_direction.Row ~gap:(gap 0) [ h ]
  in
  match row with
  | None -> []
  | Some r -> (
      match block_container ~parent:ctx.parent block_style [ r ] with
      | None -> []
      | Some container -> [ container ])

and render_blockquote_content ctx ~base_style block =
  let open Cmarkit.Block in
  match block with
  | Paragraph (p, _) -> (
      let fragments = fragments_of_inline ctx (Paragraph.inline p) in
      match
        make_text_fragments ~parent:ctx.parent ~style:base_style
          ~wrap_mode:(ui_text_wrap_mode ctx.paragraph_wrap)
          fragments
      with
      | None -> []
      | Some node -> [ node ])
  | Blocks (blocks, _) ->
      List.concat_map (render_blockquote_content ctx ~base_style) blocks
  | Blank_line _ -> []
  | _ -> render_block ctx ~base_style block

and render_block_quote ctx ~base_style ~next_is_thematic_break quote =
  let block_style =
    normalize_block_spacing ctx.props.style.block_quote ~next_is_thematic_break
  in
  ctx.quote_depth <- ctx.quote_depth + 1;
  let prev_wrap = ctx.paragraph_wrap in
  ctx.paragraph_wrap <- ctx.props.block_quote_wrap;

  let quote_base_style = merge_style base_style block_style.text in
  let inner_children =
    render_blockquote_content ctx ~base_style:quote_base_style
      (Cmarkit.Block.Block_quote.block quote)
  in

  ctx.paragraph_wrap <- prev_wrap;
  ctx.quote_depth <- ctx.quote_depth - 1;
  if ctx.quote_depth < 0 then ctx.quote_depth <- 0;

  let border_color =
    match block_style.text.Ansi.Style.fg with
    | Some c -> c
    | None -> Ansi.Color.of_rgb 128 128 128
  in

  (* Apply block_quote.padding inside the border container, and block_quote.margin outside. *)
  let inner_box =
    match
      make_box ~parent:ctx.parent
        ~flex_direction:Toffee.Style.Flex_direction.Column ~gap:(gap 0)
        inner_children
    with
    | None -> None
    | Some n -> Some n
  in
  match inner_box with
  | None -> []
  | Some inner -> (
      match
        make_box ~parent:ctx.parent
          ~flex_direction:Toffee.Style.Flex_direction.Column ~gap:(gap 0)
          ?padding:(padding_rect block_style.padding)
          ~border:true ~border_sides:[ `Left ] ~border_color [ inner ]
      with
      | None -> []
      | Some bordered -> (
          let outer_style = { block_style with padding = Style.zero_insets } in
          match
            make_box ~parent:ctx.parent
              ~flex_direction:Toffee.Style.Flex_direction.Column ~gap:(gap 0)
              ?margin:(margin_rect outer_style.margin)
              ~size:width_pct100 ~should_fill:false [ bordered ]
          with
          | None -> [ bordered ]
          | Some outer -> [ outer ]))

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
  let depth = List.length ctx.list_stack in
  let base_block =
    normalize_block_spacing ctx.props.style.list.block ~next_is_thematic_break
  in
  let block =
    if depth > 0 then
      (* nested list: suppress bottom margin to reduce stacked whitespace *)
      let m = base_block.margin in
      { base_block with margin = { m with bottom = 0 } }
    else base_block
  in
  match
    make_box ~parent:ctx.parent
      ~flex_direction:Toffee.Style.Flex_direction.Column ~gap:(gap 0) items
  with
  | None -> []
  | Some inner -> (
      match block_container ~parent:ctx.parent block [ inner ] with
      | None -> []
      | Some container -> [ container ])

and render_list_item ctx ~base_style
    (item_node : Cmarkit.Block.List_item.t Cmarkit.node) =
  let item, _ = item_node in
  let state = List.hd ctx.list_stack in
  let ls = ctx.props.style.list in
  let depth =
    let d = List.length ctx.list_stack - 1 in
    if d < 0 then 0 else d
  in
  let indent_prefix = String.make (depth * ls.indent_per_level) ' ' in
  let marker_text, marker_style =
    match Cmarkit.Block.List_item.ext_task_marker item with
    | Some (mark, _) -> (
        let status = Cmarkit.Block.List_item.task_status_of_task_marker mark in
        match status with
        | `Checked -> ls.task_checked
        | `Unchecked -> ls.task_unchecked
        | `Cancelled -> ls.task_cancelled
        | `Other _ -> ls.task_other)
    | None ->
        if state.ordered then (
          let label = Printf.sprintf "%d%s" state.counter ls.ordered_suffix in
          state.counter <- state.counter + 1;
          (label, ls.ordered_marker_style))
        else (ls.unordered_marker, ls.unordered_marker_style)
  in
  let marker_content = indent_prefix ^ marker_text in
  match
    make_text ~parent:ctx.parent ~style:marker_style ~wrap_mode:`None
      marker_content
  with
  | None -> None
  | Some marker -> (
      let content_style = merge_style base_style ls.block.text in
      let content_nodes =
        render_list_content ctx ~base_style:content_style
          (Cmarkit.Block.List_item.block item)
      in
      match
        make_box ~parent:ctx.parent
          ~flex_direction:Toffee.Style.Flex_direction.Column ~gap:(gap 0)
          content_nodes
      with
      | None -> None
      | Some content_box -> (
          match
            make_box ~parent:ctx.parent
              ~flex_direction:Toffee.Style.Flex_direction.Row
              ~gap:(gap ls.item_gap) [ marker; content_box ]
          with
          | None -> None
          | Some row ->
              make_box ~parent:ctx.parent
                ~flex_direction:Toffee.Style.Flex_direction.Column ~gap:(gap 0)
                [ row ]))

and render_list_content ctx ~base_style block =
  let open Cmarkit.Block in
  match block with
  | Paragraph (p, _) -> (
      let fragments = fragments_of_inline ctx (Paragraph.inline p) in
      match
        make_text_fragments ~parent:ctx.parent ~style:base_style
          ~wrap_mode:(ui_text_wrap_mode ctx.paragraph_wrap)
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
    normalize_block_spacing ctx.props.style.code_block.block
      ~next_is_thematic_break
  in
  let text_style = merge_style base_style block_style.text in
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
  let theme =
    match ctx.props.code_blocks.syntax with
    | `Theme t -> t
    | `Auto -> build_theme text_style
  in
  let code_node =
    make_code ~parent:ctx.parent ~theme ?filetype
      ~wrap_mode:(ui_code_wrap_mode ctx.props.code_blocks.wrap)
      ~languages:ctx.props.languages code_content
  in
  match code_node with
  | None -> []
  | Some code_nd -> (
      let children =
        if ctx.props.code_blocks.show_fences then
          let fence_style = ctx.props.style.code_block.fence in
          let lang_style = ctx.props.style.code_block.language in
          let start_frags =
            if language = "" then [ Fragment.text ~style:fence_style "```" ]
            else
              [
                Fragment.text ~style:fence_style "```";
                Fragment.text ~style:lang_style language;
              ]
          in
          let start_fence =
            make_text_fragments ~parent:ctx.parent ~wrap_mode:`None start_frags
          in
          let end_fence =
            make_text ~parent:ctx.parent ~style:fence_style ~wrap_mode:`None
              "```"
          in
          match (start_fence, end_fence) with
          | Some sf, Some ef -> [ sf; code_nd; ef ]
          | _ -> [ code_nd ]
        else [ code_nd ]
      in
      match
        make_box ~parent:ctx.parent
          ~flex_direction:Toffee.Style.Flex_direction.Column ~gap:(gap 0)
          children
      with
      | None -> []
      | Some inner -> (
          match block_container ~parent:ctx.parent block_style [ inner ] with
          | None -> []
          | Some container -> [ container ]))

and render_thematic_break ctx =
  let style = ctx.props.style.thematic_break.style in
  let glyph =
    if ctx.props.style.thematic_break.glyph = "" then "-"
    else ctx.props.style.thematic_break.glyph
  in
  let width = ctx.width |> Option.value ~default:40 in
  let line =
    let buf = Buffer.create (width * max 1 (String.length glyph)) in
    for _ = 1 to width do
      Buffer.add_string buf glyph
    done;
    Buffer.contents buf
  in
  let block_style =
    (* Thematic breaks generally should not add extra paragraph spacing. *)
    Style.
      {
        ctx.props.style.paragraph with
        margin = { ctx.props.style.paragraph.margin with top = 0; bottom = 0 };
      }
  in
  match make_text ~parent:ctx.parent ~style ~wrap_mode:`None line with
  | None -> []
  | Some text_node -> (
      match block_container ~parent:ctx.parent block_style [ text_node ] with
      | None -> []
      | Some container -> [ container ])

and render_html_block ctx ~base_style ~next_is_thematic_break lines =
  match ctx.props.raw_html with
  | `Drop -> []
  | `Show_as_text -> (
      let style = merge_style base_style ctx.props.style.inline.raw_html in
      let content =
        List.map Cmarkit.Block_line.to_string lines |> String.concat "\n"
      in
      let block_style =
        normalize_block_spacing ctx.props.style.paragraph
          ~next_is_thematic_break
      in
      match
        make_text ~parent:ctx.parent ~style
          ~wrap_mode:(ui_text_wrap_mode ctx.paragraph_wrap)
          content
      with
      | None -> []
      | Some text_node -> (
          match
            block_container ~parent:ctx.parent block_style [ text_node ]
          with
          | None -> []
          | Some container -> [ container ]))

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
      | [] -> ( match body with [] -> 0 | r :: _ -> List.length r)
      | r -> List.length r
    in
    let columns =
      List.init column_count (fun idx ->
          let header =
            List.nth_opt headers idx
            |> Option.map (fun text ->
                Ui.Table.cell ~style:ctx.props.style.table.header text)
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
              (fun text -> Ui.Table.cell ~style:ctx.props.style.table.cell text)
              padded
          in
          Ui.Table.row cells)
        body
    in
    let block =
      normalize_block_spacing ctx.props.style.table.block
        ~next_is_thematic_break
    in
    match
      make_table ~parent:ctx.parent ~columns ~rows:table_rows
        ~box_style:ctx.props.style.table.box_style
        ~border_style:ctx.props.style.table.border
        ~cell_style:ctx.props.style.table.cell ~show_header:(headers <> []) ()
    with
    | None -> []
    | Some table_node -> (
        match block_container ~parent:ctx.parent block [ table_node ] with
        | None -> []
        | Some container -> [ container ])

(* --- Parsing --- *)

let parse ?(strict = false) s = Cmarkit.Doc.of_string ~strict ~layout:true s

(* --- Component state --- *)

type t = {
  node : Ui.Renderable.t;
  content : Ui.Renderable.t;
  slot : Ui.Renderable.t option;
  mutable props : Props.t;
}

let node t = t.node
let props t = t.props

let clear_children t =
  List.iter
    (fun child ->
      match t.slot with
      | Some slot when slot == child -> ()
      | _ -> ignore (Ui.Renderable.remove child))
    (Ui.Renderable.children t.content)

let render_content t =
  let p = t.props in
  clear_children t;

  let doc = parse p.content in
  let link_defs = Hashtbl.create 16 in
  collect_link_defs (Cmarkit.Doc.block doc) link_defs;

  let ctx =
    {
      parent = t.content;
      props = p;
      width = resolve_width p.wrap_width;
      link_defs;
      list_stack = [];
      quote_depth = 0;
      paragraph_wrap = p.paragraph_wrap;
    }
  in

  let children =
    render_block ctx ~base_style:p.style.document.text (Cmarkit.Doc.block doc)
  in

  (* Apply document theming as a root container, without touching host layout. *)
  (match block_container ~parent:ctx.parent p.style.document children with
  | None -> ignore (append_children ~parent:t.content children)
  | Some root -> ignore (append_child ~parent:t.content ~child:root));

  Ui.Renderable.request_render t.node

(* --- Mount & updates --- *)

let mount ?(props = Props.default) node =
  (* slot: internal sink for reconciler-managed children *)
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

  (* content container: markdown renders here *)
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

  let t = { node; content; slot; props } in
  render_content t;
  t

let apply_props t (p : Props.t) =
  if not (Props.equal t.props p) then (
    t.props <- p;
    render_content t)

let update t f = apply_props t (f t.props)
let set_content t content = update t (fun p -> { p with content })
let set_style t style = update t (fun p -> { p with style })
let set_languages t languages = update t (fun p -> { p with languages })

(* --- Element API --- *)

let default_layout =
  Toffee.Style.make ~flex_direction:Toffee.Style.Flex_direction.Column
    ~size:width_pct100 ()

let markdown ?id ?visible ?z_index ?buffer ?live ?layout ?props ?on_mount () :
    Ui.element =
  let layout_style = Option.value layout ~default:default_layout in
  let props = Option.value props ~default:Props.default in
  let ctor (renderer : Ui.Renderer.t) (props : Props.t) =
    let id = Option.value id ~default:(Ui.Renderer.gen_id renderer) in
    let host_props =
      Ui.Renderable.Props.make ~id ?visible ?z_index ?buffer ?live ()
    in
    match Ui.create_node renderer ~id ~host_props ~style:layout_style () with
    | Error _ as e -> e
    | Ok n ->
        let m = mount ~props n in
        Option.iter (fun f -> f m) on_mount;
        Ok (node m)
  in
  Ui.make (Ui.Renderable ctor) props []
