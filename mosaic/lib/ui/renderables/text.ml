type fragment =
  | Text of { text : string; style : Ansi.Style.t option }
  | Span of { style : Ansi.Style.t option; children : fragment list }

type span = { text : string; style : Ansi.Style.t option }
type wrap_mode = [ `None | `Char | `Word ]

module Props = struct
  type t = {
    text_style : Ansi.Style.t;
    content : string;
    wrap_mode : wrap_mode;
    tab_indicator : int option;
    tab_indicator_color : Ansi.Color.t option;
    selection_bg : Ansi.Color.t option;
    selection_fg : Ansi.Color.t option;
    selectable : bool;
  }

  let make ?(text_style = Ansi.Style.default) ?(content = "")
      ?wrap_mode:(wrap_mode' = (`Word : wrap_mode)) ?tab_indicator
      ?tab_indicator_color ?selection_bg ?selection_fg ?(selectable = true) () =
    let wrap_mode = wrap_mode' in
    {
      text_style;
      content;
      wrap_mode;
      tab_indicator;
      tab_indicator_color;
      selection_bg;
      selection_fg;
      selectable;
    }

  let default = make ()

  let equal a b =
    Ansi.Style.equal a.text_style b.text_style
    && String.equal a.content b.content
    && a.wrap_mode = b.wrap_mode
    && Option.equal Int.equal a.tab_indicator b.tab_indicator
    && Option.equal Ansi.Color.equal a.tab_indicator_color b.tab_indicator_color
    && Option.equal Ansi.Color.equal a.selection_bg b.selection_bg
    && Option.equal Ansi.Color.equal a.selection_fg b.selection_fg
    && Bool.equal a.selectable b.selectable
end

module Fragment = struct
  type t = fragment

  let text ?style text = Text { text; style }
  let span ?style children = Span { style; children }
  let span_style style children = span ~style children
  let bold children = span_style (Ansi.Style.make ~bold:true ()) children
  let italic children = span_style (Ansi.Style.make ~italic:true ()) children

  let underline children =
    span_style (Ansi.Style.make ~underline:true ()) children

  let dim children = span_style (Ansi.Style.make ~dim:true ()) children
  let blink children = span_style (Ansi.Style.make ~blink:true ()) children
  let inverse children = span_style (Ansi.Style.make ~inverse:true ()) children
  let hidden children = span_style (Ansi.Style.make ~hidden:true ()) children

  let strikethrough children =
    span_style (Ansi.Style.make ~strikethrough:true ()) children

  let bold_italic children =
    span_style (Ansi.Style.make ~bold:true ~italic:true ()) children

  let bold_underline children =
    span_style (Ansi.Style.make ~bold:true ~underline:true ()) children

  let italic_underline children =
    span_style (Ansi.Style.make ~italic:true ~underline:true ()) children

  let bold_italic_underline children =
    span_style
      (Ansi.Style.make ~bold:true ~italic:true ~underline:true ())
      children

  let fg color children = span_style (Ansi.Style.make ~fg:color ()) children
  let bg color children = span_style (Ansi.Style.make ~bg:color ()) children
  let color = fg
  let bg_color = bg
  let styled style children = span ~style children
end

type t = {
  surface : Text_surface.t;
  mutable fragments : fragment list;
  mutable flat_cache : span list option;
}

let node t = Text_surface.node t.surface

let merge_style base = function
  | None -> base
  | Some overlay -> Ansi.Style.merge ~base ~overlay

let write_chunk buffer style text =
  if text = "" then ()
  else
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
    ignore (Text_buffer.write_chunk buffer chunk)

let rec write_fragment buffer current_style = function
  | Text { text; style } ->
      (* Preserve hierarchical fragment styles by merging parent -> child,
         but do NOT bake the global default style into chunks. *)
      let effective = merge_style current_style style in
      write_chunk buffer effective text
  | Span { style; children } ->
      let next_style = merge_style current_style style in
      List.iter (write_fragment buffer next_style) children

let rebuild_buffer t =
  (* Traverse fragments starting from the current surface default style, so
     inherited defaults (fg/bg/attrs) are preserved. We seed traversal with
     the renderable defaults. Per-chunk styles stay minimal: Text_buffer will
     compose defaults with encoded base styles during rebuild_styles. *)
  let base = Text_surface.default_style t.surface in
  Text_surface.replace_content t.surface (fun buffer ->
      match t.fragments with
      | [] -> ()
      | fragments -> List.iter (write_fragment buffer base) fragments)

let flat_style_option ~default_style effective =
  if Ansi.Style.equal effective default_style then None else Some effective

let rec collect_spans acc default_style current_style = function
  | [] -> acc
  | fragment :: rest ->
      let acc =
        match fragment with
        | Text { text; style } ->
            if text = "" then acc
            else
              let effective = merge_style current_style style in
              let style_opt = flat_style_option ~default_style effective in
              { text; style = style_opt } :: acc
        | Span { style; children } ->
            let next_style = merge_style current_style style in
            collect_spans acc default_style next_style children
      in
      collect_spans acc default_style current_style rest

let spans t =
  match t.flat_cache with
  | Some spans -> spans
  | None ->
      let default_style = Text_surface.default_style t.surface in
      let collected =
        List.rev (collect_spans [] default_style default_style t.fragments)
      in
      t.flat_cache <- Some collected;
      collected

let fragments t = t.fragments

let plain_text t =
  let buf = Buffer.create 32 in
  let rec append = function
    | [] -> ()
    | Text { text; _ } :: rest ->
        Buffer.add_string buf text;
        append rest
    | Span { children; _ } :: rest ->
        append children;
        append rest
  in
  append t.fragments;
  Buffer.contents buf

let rec fragments_equal a b =
  match (a, b) with
  | [], [] -> true
  | Text left :: rest_left, Text right :: rest_right ->
      String.equal left.text right.text
      && Option.equal Ansi.Style.equal left.style right.style
      && fragments_equal rest_left rest_right
  | Span left :: rest_left, Span right :: rest_right ->
      Option.equal Ansi.Style.equal left.style right.style
      && fragments_equal left.children right.children
      && fragments_equal rest_left rest_right
  | _ -> false

let normalize_fragments fragments =
  let rec aux acc = function
    | [] -> List.rev acc
    | Text { text = ""; _ } :: rest -> aux acc rest
    | Text { text; style } :: rest ->
        let acc =
          match acc with
          | Text { text = prev_text; style = prev_style } :: tail
            when Option.equal Ansi.Style.equal style prev_style ->
              Text { text = prev_text ^ text; style = prev_style } :: tail
          | _ -> Text { text; style } :: acc
        in
        aux acc rest
    | Span { style; children } :: rest ->
        let normalized_children = aux [] children in
        if normalized_children = [] then aux acc rest
        else aux (Span { style; children = normalized_children } :: acc) rest
  in
  aux [] fragments

let set_fragments t fragments =
  let normalized = normalize_fragments fragments in
  if fragments_equal t.fragments normalized then ()
  else (
    t.fragments <- normalized;
    t.flat_cache <- None;
    rebuild_buffer t)

let fragment_of_span { text; style } =
  match style with
  | None -> Fragment.text text
  | Some style -> Fragment.text ~style text

let set_spans t new_spans =
  let fragments = List.map fragment_of_span new_spans in
  set_fragments t fragments;
  ignore (spans t)

let append_span t span =
  let updated = spans t @ [ span ] in
  set_spans t updated

let clear_spans t =
  t.fragments <- [];
  t.flat_cache <- Some [];
  rebuild_buffer t

let set_content t content =
  set_fragments t [ Fragment.text content ];
  if content = "" then t.flat_cache <- Some []
  else t.flat_cache <- Some [ { text = content; style = None } ]

let set_text_style t style =
  Text_surface.set_default_style t.surface style;
  t.flat_cache <- None

let wrap_mode t =
  match Text_surface.wrap_mode t.surface with
  | `None -> `None
  | `Char -> `Char
  | `Word -> `Word

let set_wrap_mode t mode =
  match mode with
  | `None -> Text_surface.set_wrap_mode t.surface `None
  | `Char -> Text_surface.set_wrap_mode t.surface `Char
  | `Word -> Text_surface.set_wrap_mode t.surface `Word

let set_tab_width t w = Text_surface.set_tab_width t.surface w
let set_tab_indicator t code = Text_surface.set_tab_indicator t.surface code

let set_tab_indicator_char t chr =
  match chr with
  | None -> set_tab_indicator t None
  | Some s ->
      if String.length s = 0 then set_tab_indicator t None
      else
        let dec = String.get_utf_8_uchar s 0 in
        if Uchar.utf_decode_is_valid dec then
          set_tab_indicator t (Some (Uchar.to_int (Uchar.utf_decode_uchar dec)))
        else set_tab_indicator t None

let set_tab_indicator_color t c =
  Text_surface.set_tab_indicator_color t.surface c

let set_selection_bg t c = Text_surface.set_selection_bg t.surface c
let set_selection_fg t c = Text_surface.set_selection_fg t.surface c
let selectable t = Text_surface.selectable t.surface
let set_selectable t v = Text_surface.set_selectable t.surface v

let should_start_selection t ~x ~y =
  Renderable.Internal.should_start_selection (node t) ~x ~y

let set_selection t selection = Text_surface.set_selection t.surface selection
let clear_selection t = Text_surface.clear_selection t.surface
let set_style t style = Renderable.set_style (Text_surface.node t.surface) style

let mount ?(props = Props.default) (rnode : Renderable.t) =
  let surface =
    Text_surface.mount
      ~props:
        (Text_surface.Props.make
           ~wrap_mode:
             (match props.wrap_mode with
             | `None -> `None
             | `Char -> `Char
             | `Word -> `Word)
           ?tab_indicator:props.tab_indicator
           ?tab_indicator_color:props.tab_indicator_color
           ?selection_bg:props.selection_bg ?selection_fg:props.selection_fg
           ~selectable:props.selectable ~default_style:props.text_style ())
      rnode
  in
  (* Ensure base text style is applied to the underlying buffer immediately
     so initial content composes against the correct defaults. *)
  Text_surface.set_default_style surface props.text_style;
  let initial_fragments, initial_cache =
    if props.content = "" then ([], Some [])
    else
      ( [ Fragment.text props.content ],
        Some [ { text = props.content; style = None } ] )
  in
  let text =
    {
      surface;
      fragments = normalize_fragments initial_fragments;
      flat_cache = initial_cache;
    }
  in
  rebuild_buffer text;
  text

let apply_props t (props : Props.t) =
  (* Text content and style *)
  set_text_style t props.text_style;
  set_content t props.content;
  (* Wrapping and selection-related props *)
  set_wrap_mode t props.wrap_mode;
  set_tab_indicator t props.tab_indicator;
  set_tab_indicator_color t props.tab_indicator_color;
  set_selection_bg t props.selection_bg;
  set_selection_fg t props.selection_fg;
  set_selectable t props.selectable
