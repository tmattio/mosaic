(* High-level rendering API *)

module Style = struct
  type t = Render.style

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

type element =
  | Text of string * Style.t
  | Box of element list * layout_options
  | Spacer of int
  | Expand of element (* Wrapper to mark expandable elements *)

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
    ?(align = Start) ?(justify = Start) children =
  Box
    ( children,
      {
        direction = `Horizontal;
        gap;
        width;
        height;
        padding;
        border;
        align;
        justify;
      } )

let vbox ?(gap = 0) ?width ?height ?(padding = no_padding) ?border
    ?(align = Start) ?(justify = Start) children =
  Box
    ( children,
      {
        direction = `Vertical;
        gap;
        width;
        height;
        padding;
        border;
        align;
        justify;
      } )

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
  | Box (children, opts) -> (
      let children_sizes = List.map measure_element children in
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

(* Check if element is expandable *)
let is_expandable = function Expand _ -> true | _ -> false

(* Apply alignment offset *)
let align_offset available used align =
  match align with
  | Start -> 0
  | Center -> (available - used) / 2
  | End -> available - used
  | Stretch -> 0

let rec render_at ctx buffer element =
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
  | Box (children, opts) -> (
      let box_width = Option.value opts.width ~default:ctx.width in
      let box_height = Option.value opts.height ~default:ctx.height in

      (* Draw border if specified *)
      let border_offset = if opts.border = None then 0 else 1 in
      let () =
        match opts.border with
        | Some border_spec when box_width > 2 && box_height > 2 ->
            draw_border buffer ctx.x ctx.y box_width box_height border_spec
        | _ -> ()
      in

      (* Calculate content area after border and padding *)
      let content_x = ctx.x + border_offset + opts.padding.left in
      let content_y = ctx.y + border_offset + opts.padding.top in
      let content_width =
        box_width - (2 * border_offset) - opts.padding.left - opts.padding.right
      in
      let content_height =
        box_height - (2 * border_offset) - opts.padding.top
        - opts.padding.bottom
      in

      (* Count expandable children *)
      let expandable_count =
        List.filter is_expandable children |> List.length
      in

      (* Measure non-expandable children *)
      let measured_children =
        List.map
          (fun child ->
            if is_expandable child then (child, 0, 0)
            else
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
          let available_expand =
            max 0 (content_width - fixed_width - gap_space)
          in
          let expand_each =
            if expandable_count > 0 then available_expand / expandable_count
            else 0
          in

          (* Render children *)
          let rec render_h x y children_with_sizes =
            match children_with_sizes with
            | [] -> ()
            | (child, _, _) :: rest ->
                let child_width =
                  if is_expandable child then expand_each
                  else
                    let w, _ = measure_element child in
                    w
                in
                let child_height = content_height in

                (* Apply vertical alignment *)
                let measured_h = snd (measure_element child) in
                let y_offset =
                  align_offset child_height measured_h opts.align
                in

                let child_ctx =
                  {
                    x;
                    y = y + y_offset;
                    width = child_width;
                    height = min child_height measured_h;
                  }
                in
                let _w, _h = render_at child_ctx buffer child in

                let next_x =
                  x + child_width + if rest = [] then 0 else opts.gap
                in
                render_h next_x y rest
          in

          (* Apply horizontal justification *)
          let total_children_width =
            fixed_width + (expand_each * expandable_count) + gap_space
          in
          let x_offset =
            align_offset content_width total_children_width opts.justify
          in

          render_h (content_x + x_offset) content_y measured_children;
          (box_width, box_height)
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
            if expandable_count > 0 then available_expand / expandable_count
            else 0
          in

          (* Render children *)
          let rec render_v x y children_with_sizes =
            match children_with_sizes with
            | [] -> ()
            | (child, _, _) :: rest ->
                let child_width = content_width in
                let child_height =
                  if is_expandable child then expand_each
                  else
                    let _, h = measure_element child in
                    h
                in

                (* Apply horizontal alignment *)
                let measured_w = fst (measure_element child) in
                let x_offset = align_offset child_width measured_w opts.align in

                let child_ctx =
                  {
                    x = x + x_offset;
                    y;
                    width = min child_width measured_w;
                    height = child_height;
                  }
                in
                let _w, _h = render_at child_ctx buffer child in

                let next_y =
                  y + child_height + if rest = [] then 0 else opts.gap
                in
                render_v x next_y rest
          in

          (* Apply vertical justification *)
          let total_children_height =
            fixed_height + (expand_each * expandable_count) + gap_space
          in
          let y_offset =
            align_offset content_height total_children_height opts.justify
          in

          render_v content_x (content_y + y_offset) measured_children;
          (box_width, box_height))

let render buffer element =
  let width, height = Render.dimensions buffer in
  let ctx = { x = 0; y = 0; width; height } in
  ignore (render_at ctx buffer element)
