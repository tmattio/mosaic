(* Use local modules directly within mosaic.ui to avoid cyclic deps *)

module Syntax_style = struct
  type t = { default : Ansi.Style.t; styles : (string, Ansi.Style.t) Hashtbl.t }

  let create ~default rules =
    let styles = Hashtbl.create 32 in
    List.iter (fun (scope, style) -> Hashtbl.replace styles scope style) rules;
    { default; styles }

  let default t = t.default

  let rec find_style t scope =
    match Hashtbl.find_opt t.styles scope with
    | Some s -> Some s
    | None -> (
        match String.index_opt scope '.' with
        | None -> None
        | Some i ->
            let prefix = String.sub scope 0 i in
            find_style t prefix)

  let resolve t scope =
    match find_style t scope with
    | None -> t.default
    | Some overlay -> Ansi.Style.merge ~base:t.default ~overlay

  let of_default_theme ?(base = Ansi.Style.default) () =
    let palette idx = Ansi.Color.of_palette_index idx in
    let style ?bold ?italic idx =
      Ansi.Style.make ?bold ?italic ~fg:(palette idx) ()
    in
    let default_overlays =
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
    in
    let rules =
      List.map
        (fun (cap, overlay) -> (cap, Ansi.Style.merge ~base ~overlay))
        default_overlays
    in
    create ~default:base rules
end

type grammar = {
  ts_language : Tree_sitter.Language.t;
  query : Tree_sitter.Query.t;
}

module Props = struct
  type wrap_mode = [ `None | `Char | `Word ]

  type t = {
    content : string;
    filetype : string option;
    grammar : grammar option;
    grammar_resolver : (string -> grammar option) option;
    tree_syntax : Mosaic_syntax.t option;
    syntax_style : Syntax_style.t;
    conceal : bool;
    draw_unstyled_text : bool;
    wrap_mode : wrap_mode;
    tab_indicator : int option;
    tab_indicator_color : Ansi.Color.t option;
    selection_bg : Ansi.Color.t option;
    selection_fg : Ansi.Color.t option;
    default_style : Ansi.Style.t;
    selectable : bool;
  }

  let make ?(content = "") ?filetype ?grammar ?grammar_resolvers
      ?(conceal = true) ?(draw_unstyled_text = true)
      ?(wrap_mode = (`Word : wrap_mode)) ?tab_indicator ?tab_indicator_color
      ?selection_bg ?selection_fg ?(default_style = Ansi.Style.default)
      ?(selectable = true) ?tree_syntax ?syntax_style () =
    let compose_resolvers (rs : (string -> grammar option) list) :
        string -> grammar option =
     fun filetype ->
      let rec loop = function
        | [] -> None
        | r :: tl -> ( match r filetype with None -> loop tl | some -> some)
      in
      loop rs
    in
    let grammar_resolver = Option.map compose_resolvers grammar_resolvers in
    let syntax_style =
      match syntax_style with
      | Some s -> s
      | None -> Syntax_style.of_default_theme ~base:default_style ()
    in
    {
      content;
      filetype;
      grammar;
      grammar_resolver;
      tree_syntax;
      syntax_style;
      conceal;
      draw_unstyled_text;
      wrap_mode;
      tab_indicator;
      tab_indicator_color;
      selection_bg;
      selection_fg;
      default_style;
      selectable;
    }

  let default = make ()

  let equal a b =
    let lang_eq l r =
      match (l, r) with
      | None, None -> true
      | Some x, Some y -> x == y
      | _ -> false
    in
    String.equal a.content b.content
    && Option.equal String.equal a.filetype b.filetype
    && lang_eq a.grammar b.grammar
    && lang_eq a.tree_syntax b.tree_syntax
    && a.syntax_style == b.syntax_style
    && Bool.equal
         (Option.is_some a.grammar_resolver)
         (Option.is_some b.grammar_resolver)
    && Bool.equal a.conceal b.conceal
    && Bool.equal a.draw_unstyled_text b.draw_unstyled_text
    && a.wrap_mode = b.wrap_mode
    && Option.equal Int.equal a.tab_indicator b.tab_indicator
    && Option.equal Ansi.Color.equal a.tab_indicator_color b.tab_indicator_color
    && Option.equal Ansi.Color.equal a.selection_bg b.selection_bg
    && Option.equal Ansi.Color.equal a.selection_fg b.selection_fg
    && Ansi.Style.equal a.default_style b.default_style
    && Bool.equal a.selectable b.selectable
end

module Highlighter = struct
  type t = {
    parser : Tree_sitter.Parser.t;
    cursor : Tree_sitter.Query_cursor.t;
    query : Tree_sitter.Query.t;
    mutable tree : Tree_sitter.Tree.t option;
  }

  type token = { start_byte : int; end_byte : int; capture : string }

  let create (g : grammar) =
    let parser = Tree_sitter.Parser.create () in
    Tree_sitter.Parser.set_language parser g.ts_language;
    let cursor = Tree_sitter.Query_cursor.create () in
    { parser; cursor; query = g.query; tree = None }

  let cleanup t =
    t.tree <- None;
    Tree_sitter.Query_cursor.delete t.cursor;
    Tree_sitter.Parser.delete t.parser

  let capture_name query index =
    Tree_sitter.Query.capture_name_for_id query index

  let should_skip_capture name = String.length name = 0 || name.[0] = '_'

  let highlight t content =
    let new_tree =
      match t.tree with
      | None -> Tree_sitter.Parser.parse_string t.parser content
      | Some old -> Tree_sitter.Parser.parse_string ~old t.parser content
    in
    t.tree <- Some new_tree;
    let root = Tree_sitter.Tree.root_node new_tree in
    Tree_sitter.Query_cursor.exec t.cursor t.query root;
    let rec gather acc =
      match Tree_sitter.Query_cursor.next_capture t.cursor t.query with
      | None -> acc
      | Some capture ->
          let acc =
            match capture_name t.query capture.capture_index with
            | Some name when not (should_skip_capture name) ->
                let node = capture.node in
                let start_byte = Tree_sitter.Node.start_byte node in
                let end_byte = Tree_sitter.Node.end_byte node in
                if start_byte < end_byte then
                  { start_byte; end_byte; capture = name } :: acc
                else acc
            | _ -> acc
          in
          gather acc
    in
    let tokens = gather [] in
    let tokens =
      List.sort
        (fun a b ->
          match Int.compare a.start_byte b.start_byte with
          | 0 -> Int.compare a.end_byte b.end_byte
          | c -> c)
        tokens
    in
    Array.of_list tokens
end

let background_at grid ~x ~y =
  let idx = (y * Grid.width grid) + x in
  Grid.get_background grid idx

type viewport = { x : int; y : int; width : int; height : int }

type t = {
  node : Renderable.t;
  buffer : Text_buffer.t;
  view : Text_buffer_view.t;
  mutable wrap_mode : Props.wrap_mode;
  mutable applied_wrap_mode : Text_buffer.wrap_mode;
  mutable default_style : Ansi.Style.t;
  mutable wrap_width_hint : int option;
  mutable viewport : viewport option;
  mutable selection_bg : Ansi.Color.t option;
  mutable selection_fg : Ansi.Color.t option;
  (* code/highlighting state *)
  mutable content : string;
  mutable filetype : string option;
  mutable grammar : grammar option;
  mutable grammar_resolver : (string -> grammar option) option;
  mutable tree_syntax : Mosaic_syntax.t option;
  mutable syntax_style : Syntax_style.t;
  mutable pending_update : bool;
  mutable conceal : bool;
  mutable draw_unstyled_text : bool;
  mutable should_render_text_buffer : bool;
  mutable highlighter : Highlighter.t option;
}

let node t = t.node

let set_wrap_mode t mode =
  if t.wrap_mode <> mode then (
    t.wrap_mode <- mode;
    (match mode with
    | `None -> Text_buffer_view.set_wrap_width t.view None
    | (`Char | `Word) as m ->
        t.applied_wrap_mode <- m;
        Text_buffer_view.set_wrap_mode t.view m);
    ignore (Renderable.mark_layout_dirty t.node);
    Renderable.request_render t.node)

let set_tab_width t w =
  Text_buffer.set_tab_width t.buffer w;
  Renderable.request_render t.node

let set_tab_indicator t code =
  Text_buffer_view.set_tab_indicator t.view code;
  Renderable.request_render t.node

let set_tab_indicator_color t c =
  Text_buffer_view.set_tab_indicator_color t.view c;
  Renderable.request_render t.node

let set_selection_bg t c =
  t.selection_bg <- c;
  Renderable.request_render t.node

let set_selection_fg t c =
  t.selection_fg <- c;
  Renderable.request_render t.node

let plain_text t = Text_buffer.get_plain_text t.buffer

let has_selection t =
  match Text_buffer_view.selection_bounds t.view with
  | Some _ -> true
  | None -> false

let get_selected_text t = Text_buffer_view.get_selected_text t.view

let write_plain_text t text style =
  Text_buffer.reset t.buffer;
  if text <> "" then (
    let chunk =
      Text_buffer.Chunk.
        {
          text = Bytes.of_string text;
          fg = style.Ansi.Style.fg;
          bg = style.Ansi.Style.bg;
          attrs = style.Ansi.Style.attrs;
          link = style.Ansi.Style.link;
        }
    in
    ignore (Text_buffer.write_chunk t.buffer chunk);
    Text_buffer.finalise t.buffer;
    ignore (Renderable.mark_layout_dirty t.node);
    Renderable.request_render t.node)

type boundary_kind = Start | End
type boundary = { offset : int; kind : boundary_kind; index : int }

let compare_boundary a b =
  match Int.compare a.offset b.offset with
  | 0 -> (
      match (a.kind, b.kind) with End, Start -> -1 | Start, End -> 1 | _ -> 0)
  | c -> c

let specificity_of_scope scope =
  let rec loop acc idx =
    match String.index_from_opt scope idx '.' with
    | None -> acc
    | Some i -> loop (acc + 1) (i + 1)
  in
  loop 1 0

let update_buffer_with_tokens t tokens =
  let default_style = t.default_style in
  let content_len = String.length t.content in
  Text_buffer.reset t.buffer;
  let len = Array.length tokens in
  let boundaries =
    let count = ref 0 in
    let tmp =
      if len = 0 then [||]
      else Array.make (len * 2) { offset = 0; kind = Start; index = 0 }
    in
    for i = 0 to len - 1 do
      let token = tokens.(i) in
      let start_offset = token.Highlighter.start_byte in
      let end_offset = token.Highlighter.end_byte in
      if start_offset < end_offset then (
        tmp.(!count) <- { offset = start_offset; kind = Start; index = i };
        incr count;
        tmp.(!count) <- { offset = end_offset; kind = End; index = i };
        incr count)
    done;
    if !count = 0 then [||] else Array.sub tmp 0 !count
  in
  if Array.length boundaries = 0 then (
    if content_len > 0 then
      let text = String.sub t.content 0 content_len in
      let chunk =
        Text_buffer.Chunk.
          {
            text = Bytes.of_string text;
            fg = default_style.Ansi.Style.fg;
            bg = default_style.Ansi.Style.bg;
            attrs = default_style.Ansi.Style.attrs;
            link = default_style.Ansi.Style.link;
          }
      in
      ignore (Text_buffer.write_chunk t.buffer chunk))
  else (
    Array.sort compare_boundary boundaries;
    let active : int list ref = ref [] in
    let current_offset = ref 0 in
    let write_chunk ~style ~text =
      if text <> "" then
        let chunk =
          Text_buffer.Chunk.
            {
              text = Bytes.of_string text;
              fg = style.Ansi.Style.fg;
              bg = style.Ansi.Style.bg;
              attrs = style.Ansi.Style.attrs;
              link = style.Ansi.Style.link;
            }
        in
        ignore (Text_buffer.write_chunk t.buffer chunk)
    in
    let write_range ~style start finish =
      let slice_start = max 0 start in
      let slice_end = min content_len finish in
      if slice_end > slice_start then
        let text = String.sub t.content slice_start (slice_end - slice_start) in
        write_chunk ~style ~text
    in
    let conceal_replacement active_indices =
      if not t.conceal then None
      else
        let rec find = function
          | [] -> None
          | idx :: rest ->
              let capture = tokens.(idx).Highlighter.capture in
              if String.equal capture "conceal.with.space" then Some " "
              else if
                String.length capture >= 7 && String.sub capture 0 7 = "conceal"
              then Some ""
              else find rest
        in
        find active_indices
    in
    let resolve_style_for_active active_indices =
      let groups =
        List.map
          (fun idx ->
            let capture = tokens.(idx).Highlighter.capture in
            let spec = specificity_of_scope capture in
            (capture, spec, idx))
          active_indices
      in
      let groups =
        List.sort
          (fun (_, spec_a, idx_a) (_, spec_b, idx_b) ->
            match Int.compare spec_a spec_b with
            | 0 -> Int.compare idx_a idx_b
            | c -> c)
          groups
      in
      List.fold_left
        (fun acc (group, _, _) ->
          let style = Syntax_style.resolve t.syntax_style group in
          Ansi.Style.merge ~base:acc ~overlay:style)
        default_style groups
    in
    Array.iter
      (fun b ->
        let offset = b.offset in
        if !current_offset < offset then
          match !active with
          | [] -> write_range ~style:default_style !current_offset offset
          | active_indices -> (
              (match conceal_replacement active_indices with
              | Some replacement ->
                  write_chunk ~style:default_style ~text:replacement
              | None ->
                  let style = resolve_style_for_active active_indices in
                  write_range ~style !current_offset offset);
              current_offset := offset;
              match b.kind with
              | Start -> active := b.index :: !active
              | End -> active := List.filter (fun idx -> idx <> b.index) !active
              ))
      boundaries;
    if !current_offset < content_len then
      match !active with
      | [] -> write_range ~style:default_style !current_offset content_len
      | active_indices -> (
          match conceal_replacement active_indices with
          | Some replacement ->
              write_chunk ~style:default_style ~text:replacement
          | None ->
              let style = resolve_style_for_active active_indices in
              write_range ~style !current_offset content_len));
  Text_buffer.finalise t.buffer;
  ignore (Renderable.mark_layout_dirty t.node);
  Renderable.request_render t.node

let tokens_of_highlights (highlights : Mosaic_syntax.Highlight.t array) :
    Highlighter.token array =
  let should_skip_capture name = String.length name = 0 || name.[0] = '_' in
  let acc = ref [] in
  Array.iter
    (fun (h : Mosaic_syntax.Highlight.t) ->
      let open Mosaic_syntax.Highlight in
      let { start_offset; end_offset; group; _ } = h in
      if start_offset < end_offset && not (should_skip_capture group) then
        let token =
          Highlighter.
            {
              start_byte = start_offset;
              end_byte = end_offset;
              capture = group;
            }
        in
        acc := token :: !acc)
    highlights;
  Array.of_list (List.rev !acc)

let cleanup_highlighter_opt = function
  | None -> ()
  | Some h -> Highlighter.cleanup h

let update_grammar t grammar_opt =
  cleanup_highlighter_opt t.highlighter;
  t.highlighter <- Option.map Highlighter.create grammar_opt

let apply_highlighting t =
  match t.highlighter with
  | Some highlighter -> (
      try
        let tokens = Highlighter.highlight highlighter t.content in
        t.should_render_text_buffer <- true;
        update_buffer_with_tokens t tokens
      with _ ->
        t.should_render_text_buffer <- true;
        write_plain_text t t.content t.default_style)
  | None -> (
      match t.filetype with
      | None ->
          t.should_render_text_buffer <- true;
          write_plain_text t t.content t.default_style
      | Some ft ->
          let client =
            match t.tree_syntax with
            | Some c -> c
            | None -> Mosaic_syntax.default_client ()
          in
          let result =
            try
              Mosaic_syntax.highlight_once client ~filetype:ft
                ~content:t.content
            with _ -> Error "tree_syntax: unexpected exception"
          in
          (match result with
          | Error _ ->
              t.should_render_text_buffer <- true;
              write_plain_text t t.content t.default_style
          | Ok highlights ->
              let tokens = tokens_of_highlights highlights in
              if Array.length tokens = 0 then (
                t.should_render_text_buffer <- true;
                write_plain_text t t.content t.default_style)
              else (
                t.should_render_text_buffer <- true;
                update_buffer_with_tokens t tokens));
          Renderable.request_render t.node)

let update_content t content =
  t.content <- content;
  if String.length content = 0 then (
    (* Avoid heavy work for empty content; just clear buffer. *)
    t.should_render_text_buffer <- false;
    write_plain_text t "" t.default_style)
  else (
    (* Show fallback first. Optionally suppress drawing until highlight. *)
    write_plain_text t content t.default_style;
    if (not t.draw_unstyled_text) && Option.is_some t.filetype then
      t.should_render_text_buffer <- false
    else t.should_render_text_buffer <- true;
    apply_highlighting t)

let schedule_update t =
  if not t.pending_update then (
    t.pending_update <- true;
    Renderable.request_render t.node)

let set_content t s =
  if not (String.equal t.content s) then schedule_update t;
  t.content <- s

let set_filetype t ft =
  if t.filetype <> ft then (
    t.filetype <- ft;
    schedule_update t)

let set_grammar t grammar_opt =
  if t.grammar != grammar_opt then (
    t.grammar <- grammar_opt;
    update_grammar t grammar_opt;
    schedule_update t)

let set_tree_syntax t tree_syntax_opt =
  if t.tree_syntax != tree_syntax_opt then (
    t.tree_syntax <- tree_syntax_opt;
    schedule_update t)

let set_syntax_style t ss =
  (* physical equality check when possible *)
  if t.syntax_style != ss then (
    t.syntax_style <- ss;
    t.default_style <- Syntax_style.default ss;
    (* Push defaults into buffer so styles compose once. *)
    Text_buffer.set_default_fg t.buffer t.default_style.Ansi.Style.fg;
    Text_buffer.set_default_bg t.buffer t.default_style.Ansi.Style.bg;
    Text_buffer.set_default_attrs t.buffer
      (Some t.default_style.Ansi.Style.attrs);
    Text_buffer.finalise t.buffer;
    schedule_update t)

let set_conceal t v =
  if t.conceal <> v then (
    t.conceal <- v;
    (* Placeholder: conceal not transforming content currently. Still re-run. *)
    schedule_update t)

let set_draw_unstyled_text t v =
  if t.draw_unstyled_text <> v then (
    t.draw_unstyled_text <- v;
    schedule_update t)

let apply_props t (props : Props.t) =
  (* Core content and language configuration *)
  set_content t props.content;
  set_filetype t props.filetype;
  set_grammar t props.grammar;
  (* Grammar resolver presence controls lazy grammar resolution; update
     the resolver function and schedule a refresh when it changes. *)
  (match (t.grammar_resolver, props.grammar_resolver) with
  | Some old_r, Some new_r when old_r == new_r -> ()
  | _ ->
      t.grammar_resolver <- props.grammar_resolver;
      schedule_update t);
  set_tree_syntax t props.tree_syntax;
  set_syntax_style t props.syntax_style;
  (* Rendering behaviour *)
  set_conceal t props.conceal;
  set_draw_unstyled_text t props.draw_unstyled_text;
  set_wrap_mode t props.wrap_mode;
  (* Selection and tab indicators *)
  set_selection_bg t props.selection_bg;
  set_selection_fg t props.selection_fg;
  set_tab_indicator t props.tab_indicator;
  set_tab_indicator_color t props.tab_indicator_color

(* Rendering and measurement largely mirror Text_surface to avoid composition. *)

let measure t ~known_dimensions ~available_space ~style:_ =
  (match t.wrap_mode with
  | `None -> ()
  | (`Char | `Word) as m ->
      if t.applied_wrap_mode <> m then (
        Text_buffer_view.set_wrap_mode t.view m;
        t.applied_wrap_mode <- m));
  let wrap_enabled = match t.wrap_mode with `None -> false | _ -> true in
  let resolved_width =
    let from_known =
      match known_dimensions with
      | Toffee.Geometry.Size.{ width = Some w; _ } when w > 0. -> Some w
      | _ -> None
    in
    match from_known with
    | Some _ as w -> w
    | None -> (
        let from_available =
          match available_space with
          | Toffee.Geometry.Size.{ width; _ } ->
              Toffee.Available_space.to_option width
        in
        match from_available with
        | Some w when w > 0. -> Some w
        | _ -> (
            match t.viewport with
            | Some vp when vp.width > 0 -> Some (float vp.width)
            | _ -> None))
  in
  let resolved_height =
    let from_known =
      match known_dimensions with
      | Toffee.Geometry.Size.{ height = Some h; _ } when h > 0. -> Some h
      | _ -> None
    in
    match from_known with
    | Some _ as h -> h
    | None -> (
        let from_available =
          match available_space with
          | Toffee.Geometry.Size.{ height; _ } ->
              Toffee.Available_space.to_option height
        in
        match from_available with
        | Some h when h > 0. -> Some h
        | _ -> (
            match t.viewport with
            | Some vp when vp.height > 0 -> Some (float vp.height)
            | _ -> None))
  in
  let wrap_hint =
    match (wrap_enabled, resolved_width) with
    | false, _ -> None
    | true, Some w -> Some (max 1 (int_of_float (Float.floor w)))
    | true, None -> None
  in
  let previous_hint = t.wrap_width_hint in
  Text_buffer_view.set_wrap_width t.view wrap_hint;
  t.wrap_width_hint <- wrap_hint;
  Text_buffer.finalise t.buffer;
  if previous_hint <> t.wrap_width_hint then
    ignore (Renderable.mark_layout_dirty t.node);
  let width_for_measure = match wrap_hint with Some w -> w | None -> 0 in
  let height_hint =
    match resolved_height with
    | Some h when h > 0. -> int_of_float (Float.floor h)
    | _ -> 0
  in
  let measured =
    Text_buffer_view.measure_for_dimensions t.view ~width:width_for_measure
      ~height:height_hint
  in
  let measured_width = max 1 measured.max_width in
  let measured_height = max 1 measured.line_count in
  let final_width =
    match resolved_width with
    | Some w when w > 0. -> min measured_width (int_of_float (Float.floor w))
    | _ -> measured_width
  in
  let final_height =
    match resolved_height with
    | Some h when h > 0. -> min measured_height (int_of_float (Float.floor h))
    | _ -> measured_height
  in
  Toffee.Geometry.Size.
    { width = float final_width; height = float final_height }

let render t (_rn : Renderable.t) (grid : Grid.t) ~delta:_ =
  if not t.should_render_text_buffer then ()
  else
    let lx = Renderable.x t.node in
    let ly = Renderable.y t.node in
    let lw = Renderable.width t.node in
    let lh = Renderable.height t.node in
    let requested_width =
      match t.wrap_mode with
      | `None -> None
      | _ -> (
          match t.viewport with
          | Some vp when vp.width > 0 -> Some vp.width
          | _ when lw > 0 -> Some lw
          | _ -> None)
    in
    let need_set_mode =
      match t.wrap_mode with
      | `None -> false
      | (`Char | `Word) as m -> t.applied_wrap_mode <> m
    in
    let previous_hint = t.wrap_width_hint in
    let need_set_width = previous_hint <> requested_width in
    (if need_set_mode then
       match t.wrap_mode with
       | `None -> ()
       | (`Char | `Word) as m ->
           Text_buffer_view.set_wrap_mode t.view m;
           t.applied_wrap_mode <- m);
    if need_set_width then (
      Text_buffer_view.set_wrap_width t.view requested_width;
      t.wrap_width_hint <- requested_width);
    if need_set_mode || need_set_width then (
      Text_buffer.finalise t.buffer;
      if need_set_width then ignore (Renderable.mark_layout_dirty t.node));
    let gwm = Grid.width_method grid in
    Text_buffer.set_width_method t.buffer gwm;
    let view = Text_buffer.View.create t.buffer in
    let lines = Text_buffer_view.virtual_lines t.view in
    let curr_tab_indicator = Text_buffer_view.tab_indicator t.view in
    let curr_tab_indicator_color =
      Text_buffer_view.tab_indicator_color t.view
    in
    let tab_w = Text_buffer.tab_width t.buffer in
    let sel_bounds = Text_buffer_view.selection_bounds t.view in
    let sel_style = Text_buffer_view.selection_style t.view in
    let resolve_link idx =
      let raw = Text_buffer.View.raw_link view idx in
      if raw = -1 then None else Text_buffer.link_at_index t.buffer raw
    in
    let apply_selection base_fg base_bg idx =
      match sel_bounds with
      | None -> (base_fg, base_bg)
      | Some (s, e) when idx >= s && idx < e -> (
          match t.selection_bg with
          | Some sbg ->
              let sfg =
                match t.selection_fg with Some c -> c | None -> base_fg
              in
              (sfg, sbg)
          | None -> (
              match sel_style with
              | Some st -> (
                  match st.Ansi.Style.bg with
                  | Some sbg ->
                      let sfg =
                        match st.Ansi.Style.fg with
                        | Some c -> c
                        | None -> base_fg
                      in
                      (sfg, sbg)
                  | None ->
                      let _r, _g, _b, a = Ansi.Color.to_rgba base_bg in
                      let inv_fg =
                        if a > 0 then base_bg else Ansi.Color.of_rgba 0 0 0 255
                      in
                      (inv_fg, base_fg))
              | None ->
                  let _r, _g, _b, a = Ansi.Color.to_rgba base_bg in
                  let inv_fg =
                    if a > 0 then base_bg else Ansi.Color.of_rgba 0 0 0 255
                  in
                  (inv_fg, base_fg)))
      | _ -> (base_fg, base_bg)
    in
    let buffer_width = Grid.width grid in
    let buffer_height = Grid.height grid in
    let write_cell = Grid.set_cell_alpha in
    let vp = t.viewport in
    let start_line = match vp with Some v -> max 0 v.y | None -> 0 in
    let max_lines =
      let visible_h =
        match vp with
        | Some v when v.height > 0 -> v.height
        | _ -> if lh > 0 then lh else buffer_height
      in
      if lh > 0 then min visible_h lh else visible_h
    in
    let end_line = min (Array.length lines) (start_line + max_lines) in
    let draw_width_limit =
      match vp with
      | Some v when v.width > 0 -> v.width
      | _ -> if lw > 0 then lw else max 0 (buffer_width - max 0 lx)
    in
    for line_idx = start_line to end_line - 1 do
      let line = lines.(line_idx) in
      let dest_y = ly + (line_idx - start_line) in
      if dest_y >= 0 && dest_y < buffer_height then
        let rec loop i column =
          if i >= line.length || column >= draw_width_limit then ()
          else
            let idx = line.start_index + i in
            let code = Text_buffer.View.code view idx in
            let base_width = Text_buffer.View.width view idx in
            let fg =
              match Text_buffer.View.fg_opt view idx with
              | Some c -> c
              | None ->
                  Option.value t.default_style.Ansi.Style.fg
                    ~default:Ansi.Color.white
            in
            let attrs = Text_buffer.View.attrs view idx in
            if code = 10 then loop (i + 1) column
            else
              let dest_x = lx + column in
              if dest_x >= 0 && dest_x < buffer_width then (
                let initial_bg =
                  match Text_buffer.View.bg_opt view idx with
                  | Some c -> c
                  | None ->
                      Option.value t.default_style.Ansi.Style.bg
                        ~default:(background_at grid ~x:dest_x ~y:dest_y)
                in
                let bg =
                  if
                    (not (Grid.respect_alpha grid))
                    && Ansi.Color.equal initial_bg Ansi.Color.default
                  then background_at grid ~x:dest_x ~y:dest_y
                  else initial_bg
                in
                let is_tab = code = 9 in
                if is_tab then (
                  let w = max 1 tab_w in
                  let next = ((column / w) + 1) * w in
                  let draw_width = max 1 (next - column) in
                  let base_link = resolve_link idx in
                  for off = 0 to draw_width - 1 do
                    let char_code =
                      if off = 0 then
                        match curr_tab_indicator with
                        | Some cp -> cp
                        | None -> 32
                      else 32
                    in
                    let char_fg =
                      if off = 0 then
                        match curr_tab_indicator_color with
                        | Some c -> c
                        | None -> fg
                      else fg
                    in
                    let sel_fg, draw_bg = apply_selection char_fg bg idx in
                    let inv = Ansi.Attr.mem Ansi.Attr.Inverse attrs in
                    let cell_attrs =
                      if inv then Ansi.Attr.remove Ansi.Attr.Inverse attrs
                      else attrs
                    in
                    let dx = dest_x + off in
                    if dx >= 0 && dx < buffer_width then
                      let out_fg =
                        if off = 0 then
                          match curr_tab_indicator_color with
                          | Some c -> c
                          | None -> sel_fg
                        else sel_fg
                      in
                      let out_fg, draw_bg =
                        if inv then (draw_bg, out_fg) else (out_fg, draw_bg)
                      in
                      write_cell grid ~x:dx ~y:dest_y ~code:char_code ~fg:out_fg
                        ~bg:draw_bg ~attrs:cell_attrs ?link:base_link ()
                  done;
                  let next_column = column + draw_width in
                  loop (i + 1) next_column)
                else
                  let is_cont = Glyph.is_continuation code in
                  let draw_width = if is_cont then 0 else max 1 base_width in
                  let draw_fg, draw_bg = apply_selection fg bg idx in
                  let link = resolve_link idx in
                  let inv = Ansi.Attr.mem Ansi.Attr.Inverse attrs in
                  let cell_attrs =
                    if inv then Ansi.Attr.remove Ansi.Attr.Inverse attrs
                    else attrs
                  in
                  let draw_fg, draw_bg =
                    if inv then (draw_bg, draw_fg) else (draw_fg, draw_bg)
                  in
                  write_cell grid ~x:dest_x ~y:dest_y ~code ~fg:draw_fg
                    ~bg:draw_bg ~attrs:cell_attrs ?link ();
                  let next_column =
                    column + if draw_width > 0 then draw_width else 0
                  in
                  loop (i + 1) next_column)
        in
        let start_i =
          match vp with
          | Some v when v.x > 0 ->
              let idx_at =
                Text_buffer_view.position_to_index t.view ~x:v.x ~y:line_idx
              in
              max 0 (idx_at - line.start_index)
          | _ -> 0
        in
        loop start_i 0
    done

let mount ?(props = Props.default) (node : Renderable.t) =
  let default_capacity = 128 in
  let pool =
    match Renderable.Internal.glyph_pool node with
    | Some p -> p
    | None -> Glyph.create_pool ()
  in
  let buffer =
    Text_buffer.create ~glyph_pool:pool ~capacity:default_capacity
      ~width_method:`Unicode ()
  in
  let view = Text_buffer_view.create buffer in
  Option.iter
    (fun i -> Text_buffer_view.set_tab_indicator view (Some i))
    props.tab_indicator;
  Option.iter
    (fun c -> Text_buffer_view.set_tab_indicator_color view (Some c))
    props.tab_indicator_color;
  let t =
    {
      node;
      buffer;
      view;
      wrap_mode = props.wrap_mode;
      applied_wrap_mode =
        (match props.wrap_mode with
        | `None -> `Word
        | `Char -> `Char
        | `Word -> `Word);
      default_style = props.default_style;
      wrap_width_hint = None;
      viewport = None;
      selection_bg = props.selection_bg;
      selection_fg = props.selection_fg;
      content = props.content;
      filetype = props.filetype;
      grammar = props.grammar;
      grammar_resolver = props.grammar_resolver;
      tree_syntax = props.tree_syntax;
      syntax_style = props.syntax_style;
      pending_update = false;
      conceal = props.conceal;
      draw_unstyled_text = props.draw_unstyled_text;
      should_render_text_buffer = true;
      highlighter = None;
    }
  in
  Text_buffer.set_default_fg buffer t.default_style.Ansi.Style.fg;
  Text_buffer.set_default_bg buffer t.default_style.Ansi.Style.bg;
  Text_buffer.set_default_attrs buffer (Some t.default_style.Ansi.Style.attrs);
  Text_buffer.finalise buffer;
  update_grammar t t.grammar;

  (match (t.grammar, t.filetype, t.grammar_resolver) with
  | None, Some ft, Some resolve -> update_grammar t (resolve ft)
  | _ -> ());

  Renderable.set_render node (render t);
  Renderable.set_measure node (Some (measure t));
  Renderable.set_on_size_change node
    (Some
       (fun n ->
         let w = Renderable.width n in
         let h = Renderable.height n in
         Text_buffer_view.set_viewport_size t.view ~width:w ~height:h;
         match t.viewport with
         | Some vp when vp.width = w && vp.height = h -> ()
         | Some vp ->
             t.viewport <- Some { vp with width = w; height = h };
             Renderable.request_render n
         | None ->
             t.viewport <- Some { x = 0; y = 0; width = w; height = h };
             Renderable.request_render n));

  let selection_capability : Renderable.Select.capability =
    {
      should_start =
        (fun ~x ~y ->
          if not props.selectable then false
          else
            let nx = Renderable.x node and ny = Renderable.y node in
            let nw = Renderable.width node and nh = Renderable.height node in
            x >= nx && x < nx + nw && y >= ny && y < ny + nh);
      on_change =
        (fun sel_opt ->
          match sel_opt with
          | None ->
              Text_buffer_view.clear_selection t.view;
              Text_buffer.finalise t.buffer;
              Renderable.request_render node;
              false
          | Some sel -> (
              let bounds =
                Selection.convert_global_to_local (Some sel)
                  ~local_origin_x:(Renderable.x node)
                  ~local_origin_y:(Renderable.y node)
              in
              match bounds with
              | None ->
                  Text_buffer_view.clear_selection t.view;
                  Text_buffer.finalise t.buffer;
                  Renderable.request_render node;
                  false
              | Some b ->
                  let style =
                    Ansi.Style.make ?fg:t.selection_fg ?bg:t.selection_bg ()
                  in
                  let _changed =
                    Text_buffer_view.set_local_selection t.view
                      ~anchor_x:b.anchor_x ~anchor_y:b.anchor_y
                      ~focus_x:b.focus_x ~focus_y:b.focus_y ~style
                  in
                  Text_buffer.finalise t.buffer;
                  Renderable.request_render node;
                  Option.is_some (Text_buffer_view.selection_bounds t.view)));
      clear =
        (fun () ->
          Text_buffer_view.clear_selection t.view;
          Text_buffer.finalise t.buffer;
          Renderable.request_render node);
      get_text = (fun () -> get_selected_text t);
    }
  in
  if props.selectable then
    Renderable.set_selection node (Some selection_capability)
  else Renderable.set_selection node None;

  Renderable.set_on_frame node
    (Some
       (fun _ ~delta:_ ->
         if t.pending_update then (
           t.pending_update <- false;
           (match (t.grammar, t.filetype, t.grammar_resolver) with
           | None, Some ft, Some resolve -> update_grammar t (resolve ft)
           | _ -> ());
           update_content t t.content)));

  update_content t t.content;
  t
