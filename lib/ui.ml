(* High-level rendering API *)

module Style = struct
  type t = Render.style

  (* Attribute type for building styles from lists *)
  type attr =
    | Fg of Ansi.color
    | Bg of Ansi.color
    | Bold
    | Dim
    | Italic
    | Underline
    | Blink
    | Reverse
    | Strikethrough
    | Link of string

  let empty = Render.default_style
  let fg color = { empty with fg = Some color }
  let bg color = { empty with bg = Some color }
  let bold = { empty with bold = true }
  let dim = { empty with dim = true }
  let italic = { empty with italic = true }
  let underline = { empty with underline = true }
  let blink = { empty with blink = true }
  let reverse = { empty with reverse = true }
  let strikethrough = { empty with strikethrough = true }
  let link uri = { empty with uri = Some uri }

  (* Create a style from a list of attributes *)
  let of_list attrs =
    List.fold_left
      (fun style attr ->
        let open Render in
        match attr with
        | Fg color -> { style with fg = Some color }
        | Bg color -> { style with bg = Some color }
        | Bold -> { style with bold = true }
        | Dim -> { style with dim = true }
        | Italic -> { style with italic = true }
        | Underline -> { style with underline = true }
        | Blink -> { style with blink = true }
        | Reverse -> { style with reverse = true }
        | Strikethrough -> { style with strikethrough = true }
        | Link uri -> { style with uri = Some uri })
      empty attrs

  let ( ++ ) (a : t) (b : t) : t =
    let open Render in
    {
      fg = (match b.fg with Some _ -> b.fg | None -> a.fg);
      bg = (match b.bg with Some _ -> b.bg | None -> a.bg);
      bold = a.bold || b.bold;
      dim = a.dim || b.dim;
      italic = a.italic || b.italic;
      underline = a.underline || b.underline;
      double_underline = a.double_underline || b.double_underline;
      blink = a.blink || b.blink;
      reverse = a.reverse || b.reverse;
      strikethrough = a.strikethrough || b.strikethrough;
      overline = a.overline || b.overline;
      uri = (match b.uri with Some _ -> b.uri | None -> a.uri);
    }

  let ansi256 n = Ansi.Index n
  let rgb r g b = Ansi.RGB (r, g, b)

  (* Color type export *)
  type color = Ansi.color =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
    | Default
    | Bright_black
    | Bright_red
    | Bright_green
    | Bright_yellow
    | Bright_blue
    | Bright_magenta
    | Bright_cyan
    | Bright_white
    | Index of int (* 256-color palette (0-255) *)
    | RGB of int * int * int (* 24-bit color (0-255 each) *)

  (* Color helpers *)
  let gray n = Index (232 + min 23 (max 0 n))

  let rgb_hex hex =
    let r = (hex lsr 16) land 0xFF in
    let g = (hex lsr 8) land 0xFF in
    let b = hex land 0xFF in
    RGB (r, g, b)
end

(* Layout types *)
type padding = { top : int; right : int; bottom : int; left : int }

let padding ?(top = 0) ?(right = 0) ?(bottom = 0) ?(left = 0) () =
  { top; right; bottom; left }

let pad ?all ?x ?y ?top ?right ?bottom ?left () =
  let base_val = Option.value all ~default:0 in
  let h_val = Option.value x ~default:base_val in
  let v_val = Option.value y ~default:base_val in
  {
    top = Option.value top ~default:v_val;
    right = Option.value right ~default:h_val;
    bottom = Option.value bottom ~default:v_val;
    left = Option.value left ~default:h_val;
  }

let padding_all n = { top = n; right = n; bottom = n; left = n }
let padding_xy x y = { top = y; right = x; bottom = y; left = x }

type border_style = Solid | Rounded | Double | Thick | ASCII
type border = { style : border_style; color : Ansi.color option }

let border ?(style = Solid) ?color () = { style; color }

type align = Start | Center | End | Stretch

(* Cache for layout results *)
type layout_cache = {
  (* The context in which this was rendered *)
  ctx_width : int;
  ctx_height : int;
  (* The computed result *)
  computed_width : int;
  computed_height : int;
  (* The full computed layout of children for redraw *)
  children_layouts : computed_element list;
}

and computed_element = {
  element : element;
  x : int;
  y : int;
  width : int;
  height : int;
}

and element =
  | Text of string * Style.t
  | Box of box_data
  | Spacer of int
  | Expand of element (* Wrapper to mark expandable elements *)

and box_data = {
  children : element list;
  options : layout_options;
  (* Add a mutable cache field to every box *)
  mutable cache : layout_cache option;
}

and layout_options = {
  direction : [ `Horizontal | `Vertical ];
  gap : int;
  width : int option;
  height : int option;
  padding : padding;
  border : border option;
  align : align;
  justify : align;
}

let text ?(style = Style.empty) s = Text (s, style)
let no_padding = padding ()

let hbox ?(gap = 0) ?width ?height ?(padding = no_padding) ?border
    ?(align_items = Stretch) ?(justify_content = Start) children =
  let options =
    {
      direction = `Horizontal;
      gap;
      width;
      height;
      padding;
      border;
      align = align_items;
      justify = justify_content;
    }
  in
  Box { children; options; cache = None }

let vbox ?(gap = 0) ?width ?height ?(padding = no_padding) ?border
    ?(align_items = Stretch) ?(justify_content = Start) children =
  let options =
    {
      direction = `Vertical;
      gap;
      width;
      height;
      padding;
      border;
      align = align_items;
      justify = justify_content;
    }
  in
  Box { children; options; cache = None }

let spacer n = Spacer n
let space = spacer (* Alias for API compatibility *)
let expand element = Expand element

(* Border drawing characters *)
let border_chars style =
  match style with
  | Solid -> ("┌", "─", "┐", "│", "└", "─", "┘", "│")
  | Rounded -> ("╭", "─", "╮", "│", "╰", "─", "╯", "│")
  | Double -> ("╔", "═", "╗", "║", "╚", "═", "╝", "║")
  | Thick -> ("┏", "━", "┓", "┃", "┗", "━", "┛", "┃")
  | ASCII -> ("+", "-", "+", "|", "+", "-", "+", "|")

(* Layout engine that renders elements to a Render buffer *)
type layout_context = {
  x : int; (* Current x position *)
  y : int; (* Current y position *)
  width : int; (* Available width *)
  height : int; (* Available height *)
}

(* Helper to draw a border *)
let draw_border buffer x y width height border_spec =
  let tl, t, tr, r, bl, b, br, l = border_chars border_spec.style in
  let style =
    match border_spec.color with
    | Some color -> Style.fg color
    | None -> Style.empty
  in

  (* Top border *)
  Render.set_string buffer x y tl style;
  for i = 1 to width - 2 do
    Render.set_string buffer (x + i) y t style
  done;
  Render.set_string buffer (x + width - 1) y tr style;

  (* Side borders *)
  for i = 1 to height - 2 do
    Render.set_string buffer x (y + i) l style;
    Render.set_string buffer (x + width - 1) (y + i) r style
  done;

  (* Bottom border *)
  Render.set_string buffer x (y + height - 1) bl style;
  for i = 1 to width - 2 do
    Render.set_string buffer (x + i) (y + height - 1) b style
  done;
  Render.set_string buffer (x + width - 1) (y + height - 1) br style

(* Get natural size of element without rendering *)
let rec measure_element element =
  match element with
  | Text (s, _) -> (Render.measure_string s, 1)
  | Spacer n -> (n, 1)
  | Expand e -> measure_element e
  | Box { children; options = opts; _ } -> (
      let children_sizes =
        List.map
          (fun child ->
            match child with
            | Spacer n -> if opts.direction = `Horizontal then (n, 1) else (1, n)
            | _ -> measure_element child)
          children
      in
      let border_space = if opts.border = None then 0 else 2 in
      let padding_h = opts.padding.left + opts.padding.right in
      let padding_v = opts.padding.top + opts.padding.bottom in

      match opts.direction with
      | `Horizontal ->
          let total_width =
            List.fold_left (fun acc (w, _) -> acc + w) 0 children_sizes
          in
          let max_height =
            List.fold_left (fun acc (_, h) -> max acc h) 0 children_sizes
          in
          let gap_space = opts.gap * max 0 (List.length children - 1) in
          let width =
            Option.value opts.width
              ~default:(total_width + gap_space + padding_h + border_space)
          in
          let height =
            Option.value opts.height
              ~default:(max_height + padding_v + border_space)
          in
          (width, height)
      | `Vertical ->
          let max_width =
            List.fold_left (fun acc (w, _) -> max acc w) 0 children_sizes
          in
          let total_height =
            List.fold_left (fun acc (_, h) -> acc + h) 0 children_sizes
          in
          let gap_space = opts.gap * max 0 (List.length children - 1) in
          let width =
            Option.value opts.width
              ~default:(max_width + padding_h + border_space)
          in
          let height =
            Option.value opts.height
              ~default:(total_height + gap_space + padding_v + border_space)
          in
          (width, height))

(* Unwrap all layers of Expand *)
let rec unwrap_expand element =
  match element with Expand e -> unwrap_expand e | _ -> element

(* Check if element is expandable *)
let is_expandable = function Expand _ -> true | _ -> false

(* Clear layout caches from previous frame *)
let rec clear_cache element =
  match element with
  | Text _ | Spacer _ -> ()
  | Expand e -> clear_cache e
  | Box data ->
      data.cache <- None;
      List.iter clear_cache data.children

(* Apply alignment offset *)
let align_offset available used align =
  match align with
  | Start -> 0
  | Center -> (available - used) / 2
  | End -> available - used
  | Stretch -> 0

(* Calculate box layout without rendering - pure function *)
let rec calculate_box_layout ctx children (opts : layout_options) =
  let box_width = Option.value opts.width ~default:ctx.width in
  let box_height = Option.value opts.height ~default:ctx.height in

  (* Calculate content area after border and padding *)
  let border_offset = if opts.border = None then 0 else 1 in
  let content_x = ctx.x + border_offset + opts.padding.left in
  let content_y = ctx.y + border_offset + opts.padding.top in
  let content_width =
    box_width - (2 * border_offset) - opts.padding.left - opts.padding.right
  in
  let content_height =
    box_height - (2 * border_offset) - opts.padding.top - opts.padding.bottom
  in

  (* Count expandable children *)
  let expandable_count = List.filter is_expandable children |> List.length in

  (* Measure non-expandable children *)
  let measured_children =
    List.map
      (fun child ->
        if is_expandable child then (child, 0, 0)
        else
          match child with
          | Spacer n ->
              let w = if opts.direction = `Horizontal then n else 1 in
              let h = if opts.direction = `Vertical then n else 1 in
              (child, w, h)
          | _ ->
              let w, h = measure_element child in
              (child, w, h))
      children
  in

  match opts.direction with
  | `Horizontal ->
      (* Calculate space for expandable items *)
      let fixed_width =
        List.fold_left (fun acc (_, w, _) -> acc + w) 0 measured_children
      in
      let gap_space = opts.gap * max 0 (List.length children - 1) in
      let available_expand = max 0 (content_width - fixed_width - gap_space) in
      let expand_each =
        if expandable_count > 0 then available_expand / expandable_count else 0
      in

      (* Calculate children layouts *)
      let total_children_width =
        fixed_width + (expand_each * expandable_count) + gap_space
      in
      let x_offset =
        align_offset content_width total_children_width opts.justify
      in

      let rec calc_h x children_with_sizes acc =
        match children_with_sizes with
        | [] -> List.rev acc
        | (child, w, _) :: rest ->
            let child_width = if is_expandable child then expand_each else w in
            let child_height = content_height in

            (* Apply vertical alignment *)
            let measured_h = snd (measure_element (unwrap_expand child)) in
            let y_offset = align_offset child_height measured_h opts.align in

            let computed =
              {
                element = child;
                x;
                y = content_y + y_offset;
                width = child_width;
                height =
                  (if opts.align = Stretch then child_height
                   else min child_height measured_h);
              }
            in

            let next_x = x + child_width + if rest = [] then 0 else opts.gap in
            calc_h next_x rest (computed :: acc)
      in

      let children_layouts =
        calc_h (content_x + x_offset) measured_children []
      in
      (box_width, box_height, children_layouts)
  | `Vertical ->
      (* Calculate space for expandable items *)
      let fixed_height =
        List.fold_left (fun acc (_, _, h) -> acc + h) 0 measured_children
      in
      let gap_space = opts.gap * max 0 (List.length children - 1) in
      let available_expand =
        max 0 (content_height - fixed_height - gap_space)
      in
      let expand_each =
        if expandable_count > 0 then available_expand / expandable_count else 0
      in

      (* Calculate children layouts *)
      let total_children_height =
        fixed_height + (expand_each * expandable_count) + gap_space
      in
      let y_offset =
        align_offset content_height total_children_height opts.justify
      in

      let rec calc_v y children_with_sizes acc =
        match children_with_sizes with
        | [] -> List.rev acc
        | (child, _, h) :: rest ->
            let child_width = content_width in
            let child_height = if is_expandable child then expand_each else h in

            (* Apply horizontal alignment *)
            let measured_w = fst (measure_element (unwrap_expand child)) in
            let x_offset = align_offset child_width measured_w opts.align in

            let computed =
              {
                element = child;
                x = content_x + x_offset;
                y;
                width =
                  (if opts.align = Stretch then child_width
                   else min child_width measured_w);
                height = child_height;
              }
            in

            let next_y = y + child_height + if rest = [] then 0 else opts.gap in
            calc_v next_y rest (computed :: acc)
      in

      let children_layouts =
        calc_v (content_y + y_offset) measured_children []
      in
      (box_width, box_height, children_layouts)

(* Redraw from cached layout information *)
and redraw_from_cache ctx buffer opts cache =
  (* Draw the border for the parent box if needed *)
  (match opts.border with
  | Some border_spec when cache.computed_width > 2 && cache.computed_height > 2
    ->
      draw_border buffer ctx.x ctx.y cache.computed_width cache.computed_height
        border_spec
  | _ -> ());

  (* Recursively render children using computed geometry *)
  List.iter
    (fun (cl : computed_element) ->
      let child_ctx =
        { x = cl.x; y = cl.y; width = cl.width; height = cl.height }
      in
      ignore (render_at child_ctx buffer cl.element))
    cache.children_layouts

(* Main render function with caching *)
and render_at ctx buffer element =
  match element with
  | Text (s, style) ->
      Render.set_string buffer ctx.x ctx.y s style;
      let width = Render.measure_string s in
      (width, 1)
  | Spacer n -> (n, 1)
  | Expand e ->
      (* Expanded elements fill available space *)
      let _w, _h = render_at ctx buffer e in
      (ctx.width, ctx.height)
  | Box data -> (
      (* 1. Check the cache *)
      match data.cache with
      | Some cache
        when cache.ctx_width = ctx.width && cache.ctx_height = ctx.height ->
          (* Cache hit! The box has been laid out in this context before.
             We can skip all calculations and just redraw. *)
          redraw_from_cache ctx buffer data.options cache;
          (cache.computed_width, cache.computed_height)
      | _ ->
          (* Cache miss. We must perform the full layout calculation. *)
          let computed_width, computed_height, children_layouts =
            calculate_box_layout ctx data.children data.options
          in

          (* 2. Store the result in the cache *)
          data.cache <-
            Some
              {
                ctx_width = ctx.width;
                ctx_height = ctx.height;
                computed_width;
                computed_height;
                children_layouts;
              };

          (* 3. Render the box and its children for the first time *)
          redraw_from_cache ctx buffer data.options (Option.get data.cache);

          (computed_width, computed_height))

let render buffer element =
  let width, height = Render.dimensions buffer in
  let ctx = { x = 0; y = 0; width; height } in
  ignore (render_at ctx buffer element)

(* Pretty-printing *)
let rec pp_element fmt = function
  | Text (s, _) -> Format.fprintf fmt "Text(%S)" s
  | Box { children; options; _ } ->
      let dir =
        match options.direction with `Horizontal -> "H" | `Vertical -> "V"
      in
      Format.fprintf fmt "%sBox[@[<hv>%a@]]" dir
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           pp_element)
        children
  | Spacer n -> Format.fprintf fmt "Spacer(%d)" n
  | Expand e -> Format.fprintf fmt "Expand(%a)" pp_element e
