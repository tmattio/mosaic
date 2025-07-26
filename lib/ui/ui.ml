open Element
module Style = Style
module Theme = Theme

type t = Element.t
type element = t
type padding = Padding.t
type border = Border.t
type border_style = Border.style = Solid | Rounded | Double | Thick | ASCII
type align = Element.align
type size_def = Element.size_def

let padding = Padding.make
let padding_all = Padding.all
let padding_xy = Padding.xy
let pad = Padding.pad
let border = Border.make
let normal_border = Border.normal
let ascii_border = Border.ascii
let rounded_border = Border.rounded
let double_border = Border.double
let thick_border = Border.thick
let text = text
let hbox = hbox
let vbox = vbox
let spacer = spacer
let rich_text = rich_text
let z_stack = z_stack
let flow = flow
let grid = grid
let scroll = scroll
let flex_spacer = flex_spacer
let divider = divider
let center = center
let styled = styled

type progress_bar_preset_def = Progress_bar.preset_def = {
  delimiters : (string * string) option;
  filled_char : string;
  empty_char : string;
  progress_stages : string list;
}

type progress_bar_preset = Progress_bar.preset =
  | ASCII
  | UTF8
  | Line_double
  | Line_single
  | Line_arrow
  | Block_shade_light
  | Block_shade_medium
  | Block_shade_dark
  | Block_dotted
  | Custom of progress_bar_preset_def

let progress_bar = Progress_bar.progress_bar
let checkbox = checkbox
let radio = radio

module Canvas = struct
  type t = {
    plot : x:int -> y:int -> ?style:Style.t -> string -> unit;
    draw_line :
      x1:int ->
      y1:int ->
      x2:int ->
      y2:int ->
      ?style:Style.t ->
      ?kind:[ `Line | `Braille ] ->
      unit ->
      unit;
    draw_box :
      x:int ->
      y:int ->
      width:int ->
      height:int ->
      ?style:Style.t ->
      ?border:border ->
      unit ->
      unit;
  }

  let create ?width ?height draw_fn =
    let draw ~buffer ~x:gx ~y:gy ~w:gw ~h:gh ~dark =
      let plot ~x ~y ?(style = Style.empty) str =
        let pos = (gx + x, gy + y) in
        let bounds = (String.length str, 1) in
        Drawing.draw_text
          ~clip:(Some (Render.Clip.make gx gy gw gh))
          ~buffer ~pos ~bounds ~text:str ~style ~align:`Start ~tab_width:4
          ~wrap:false ~dark ()
      in
      let draw_line ~x1 ~y1 ~x2 ~y2 ?(style = Style.empty) ?(kind = `Line) () =
        Drawing.draw_line
          ~clip:(Some (Render.Clip.make gx gy gw gh))
          ~buffer ~x1:(gx + x1) ~y1:(gy + y1) ~x2:(gx + x2) ~y2:(gy + y2) ~style
          ~dark ~kind ()
      in
      let draw_box ~x ~y ~width ~height ?(style = Style.empty) ?border () =
        Drawing.draw_box
          ~clip:(Some (Render.Clip.make gx gy gw gh))
          ~buffer ~x:(gx + x) ~y:(gy + y) ~width ~height ~style ~dark ?border ()
      in
      let d = { plot; draw_line; draw_box } in
      draw_fn d
    in
    canvas ?width ?height draw

  let plot canvas ~x ~y ?(style = Style.empty) str =
    canvas.plot ~x ~y ~style str

  let draw_line ~x1 ~y1 ~x2 ~y2 ?(style = Style.empty) ?(kind = `Line) canvas =
    canvas.draw_line ~x1 ~y1 ~x2 ~y2 ~style ~kind ()

  let draw_box ~x ~y ~width ~height ?(style = Style.empty) ?border canvas =
    canvas.draw_box ~x ~y ~width ~height ~style ?border ()
end

let canvas ?width ?height draw_fn =
  let draw ~buffer ~x:canvas_x ~y:canvas_y ~w ~h ~dark =
    (* Create clip rectangle for the canvas bounds *)
    let clip = Render.Clip.make canvas_x canvas_y w h in
    let plot ~x ~y ?(style = Style.empty) str =
      let abs_x = canvas_x + x in
      (* Note: x param is relative *)
      let abs_y = canvas_y + y in
      let text_bounds = (String.length str, 1) in
      Drawing.draw_text ~clip:(Some clip) ~buffer ~pos:(abs_x, abs_y)
        ~bounds:text_bounds ~text:str ~style ~align:`Start ~tab_width:4
        ~wrap:false ~dark ()
    in
    let draw_line ~x1 ~y1 ~x2 ~y2 ?(style = Style.empty) ?(kind = `Line) () =
      Drawing.draw_line ~clip:(Some clip) ~buffer ~x1:(canvas_x + x1)
        ~y1:(canvas_y + y1) ~x2:(canvas_x + x2) ~y2:(canvas_y + y2) ~style ~dark
        ~kind ()
    in
    let draw_box ~x ~y ~width ~height ?(style = Style.empty) ?border () =
      Drawing.draw_box ~clip:(Some clip) ~buffer ~x:(canvas_x + x)
        ~y:(canvas_y + y) ~width ~height ~style ~dark ?border ()
    in
    draw_fn { Canvas.plot; draw_line; draw_box }
  in
  Element.canvas ?width ?height draw

module Table = Table
module Panel = Panel

(* Wrapper to provide backward-compatible simple table interface *)
let table ~rows ?(headers = []) ?(col_styles = []) ?border () =
  match headers with
  | [] -> Table.table ~columns:[] ~rows ()
  | _ ->
      let columns =
        List.mapi
          (fun i h ->
            let style =
              match List.nth_opt col_styles i with
              | Some s -> s
              | None -> Style.empty
            in
            { (Table.default_column ~header:h) with style })
          headers
      in
      let box_style =
        match border with Some _ -> Table.Simple | None -> Table.NoBox
      in
      Table.table ~columns ~rows ~box_style ~show_edge:(border <> None) ()

let panel = Panel.panel
let image = image
let separator = separator
let list = list

(* Spinner *)
type spinner_kind = Spinner.spinner_kind =
  (* Braille patterns *)
  | Braille_dots
  | Braille_dots2
  | Braille_dots3
  | Braille_dots4
  | Braille_dots5
  | Braille_dots6
  | Braille_dots7
  | Braille_dots8
  | Braille_dots9
  | Braille_dots10
  | Braille_dots11
  | Braille_dots12
  | Braille_dots13
  | Braille_dots14
  | Braille_8bit
  | Braille_circle
  | Braille_sand
  (* Line and ASCII *)
  | Line_spin
  | Line_pulse
  | Pipe_spin
  | Ascii_dots
  | Ascii_dots_scroll
  | Ascii_star
  | Ascii_flip
  | Ascii_hamburger
  | Ascii_binary
  | Ascii_dqpb
  (* Bars and blocks *)
  | Bar_vertical_grow
  | Bar_horizontal_grow
  | Bar_bounce
  | Block_bounce
  | Block_wave
  | Block_square
  | Block_squish
  (* Geometric shapes *)
  | Triangle_spin
  | Square_corners
  | Circle_quarters
  | Circle_halves
  | Circle_simple
  | Arc_spin
  (* Progress indicators *)
  | Progress_bar
  | Progress_balloon
  | Progress_balloon2
  | Progress_layer
  | Progress_point
  | Progress_beta_wave
  (* Animations *)
  | Anim_pong
  | Anim_shark
  | Anim_grenade
  | Anim_ball_bounce
  | Anim_aesthetic
  | Anim_dwarf_fortress
  (* Noise and effects *)
  | Noise_fade
  | Effect_dots_bounce
  (* Toggle animations *)
  | Toggle_box
  | Toggle_box2
  | Toggle_square
  | Toggle_square2
  | Toggle_square3
  | Toggle_circle
  | Toggle_circle2
  | Toggle_circle3
  | Toggle_circle4
  | Toggle_circle5
  | Toggle_diamond
  | Toggle_shogi
  | Toggle_equals
  (* Arrows *)
  | Arrow_rotate
  | Arrow_rotate2
  | Arrow_progress
  (* Unicode and emoji *)
  | Unicode_star_pulse
  | Unicode_moon_phases
  | Unicode_earth_rotate
  | Unicode_clock
  | Unicode_weather
  (* Emoji animations *)
  | Emoji_hearts
  | Emoji_monkey
  | Emoji_faces
  | Emoji_runner
  | Emoji_christmas
  | Emoji_finger_dance
  | Emoji_fist_bump
  | Emoji_soccer
  | Emoji_mindblown
  | Emoji_speaker
  (* Pulse animations *)
  | Pulse_orange
  | Pulse_blue
  | Pulse_orange_blue
  (* Special *)
  | Time_travel
  (* Custom *)
  | Custom of { frames : string list; interval : int }

let spinner = Spinner.spinner

type tree_guide_style = Tree.guide_style = Normal | ASCII | Bold | Double

type tree_node = Tree.node = {
  label : Element.t;
  expanded : bool;
  children : tree_node list;
  guide_style : Style.t option;
}

let tree ?style ?guide_style ?guides ?hide_root ?expanded node =
  Tree.tree ?style ?guide_style ?guides ?hide_root ?expanded node

(* The ephemeral cache is created for each top-level render call.
   The key is a combination of the element's physical address and the
   layout bounds, ensuring that we cache layouts correctly for different
   available sizes. *)
module Cache_key = struct
  type t = Element.t * int * int

  let equal (e1, w1, h1) (e2, w2, h2) = e1 == e2 && w1 = w2 && h1 = h2

  let hash (e, w, h) =
    (* Use physical equality for caching - safe since elements are immutable *)
    let addr = Hashtbl.hash e in
    Hashtbl.hash (addr, w, h)
end

module Layout_cache = Hashtbl.Make (Cache_key)

let render ?(dark = true) ?theme buffer top_level_element =
  (* Validate the element tree before rendering *)
  let rec validate_element = function
    | Grid g ->
        let children = Grid.children g in
        let columns = Grid.columns g in
        let rows = Grid.rows g in
        let expected_cells = List.length columns * List.length rows in
        let actual_cells = List.length children in
        if actual_cells <> expected_cells then
          invalid_arg
            (Printf.sprintf
               "Grid: expected %d cells (%d columns × %d rows) but got %d \
                children"
               expected_cells (List.length columns) (List.length rows)
               actual_cells);
        List.iter validate_element children
    | Box b -> List.iter validate_element (Box.children b)
    | Z_stack z -> List.iter validate_element (Z_stack.children z)
    | Flow f -> List.iter validate_element (Flow.children f)
    | Scroll s -> validate_element (Scroll.child s)
    | Text _ | Rich_text _ | Spacer _ | Canvas _ -> ()
  in
  validate_element top_level_element;

  let cache = Layout_cache.create 256 in

  let rec render_layout ?(clip = None) (layout : Layout.t) =
    let element = Layout.element layout in
    let rect = Layout.geometry layout in
    let x, y, width, height = rect in
    let pos = (x, y) in
    let bounds = (width, height) in

    (* Define a clip for the current element's children. This prevents
       children from drawing outside their parent's bounds. *)
    let child_clip =
      let new_clip_rect = Render.Clip.make x y width height in
      Render.Clip.intersect_opt clip (Some new_clip_rect)
    in

    (* 1. Draw the element's own primitive features (background, border, text). *)
    (match element with
    | Box b -> (
        let options = Box.options b in
        (match options.background with
        | Some style ->
            let style =
              Option.fold ~none:style
                ~some:(fun t -> Theme.with_theme t style)
                theme
            in
            Drawing.fill_rect ~clip ~buffer ~rect ~style ~dark ()
        | None -> ());
        match options.border with
        | Some border ->
            let themed_border =
              match theme with
              | None -> border
              | Some th ->
                  if Border.color border = None then
                    Border.make ~top:(Border.top border)
                      ~bottom:(Border.bottom border) ~left:(Border.left border)
                      ~right:(Border.right border) ~style:(Border.style border)
                      ~color:th.border ()
                  else border
            in
            Drawing.draw_border ~clip ~buffer ~rect ~border:themed_border ~dark
              ()
        | None -> ())
    | Text t ->
        let content = Text.content t in
        let style = Text.style t in
        let style =
          Option.fold ~none:style
            ~some:(fun t -> Theme.with_theme t style)
            theme
        in
        let align = Text.alignment t in
        let tab_width = Text.tab_width t in
        let wrap = Text.is_wrapping t in
        Drawing.draw_text ~clip ~buffer ~pos ~bounds ~text:content ~style ~align
          ~tab_width ~wrap ~dark ()
    | Rich_text rt ->
        let segments = Rich_text.segments rt in
        let segments =
          match theme with
          | None -> segments
          | Some t ->
              List.map
                (fun (text, style) -> (text, Theme.with_theme t style))
                segments
        in
        Drawing.draw_rich_text ~clip ~buffer ~pos ~width ~segments ~dark ()
    | Flow _ | Z_stack _ | Grid _ ->
        (* Flow, Z_stack, and Grid need background fill for proper rendering *)
        (* Fill the entire element bounds with spaces to clear the area *)
        for y = 0 to height - 1 do
          for x = 0 to width - 1 do
            let cell_x = x + fst pos in
            let cell_y = y + snd pos in
            Render.set_char ?clip buffer cell_x cell_y (Uchar.of_char ' ')
              Render.default_attr
          done
        done
    | Canvas c -> (
        let clip_rect = Render.Clip.make x y width height in
        let effective_clip = Render.Clip.intersect_opt clip (Some clip_rect) in
        match effective_clip with
        | Some cr ->
            let cx, cy, cw, ch =
              Render.Clip.(x cr, y cr, width cr, height cr)
            in
            Element.Canvas.draw c ~buffer ~x:cx ~y:cy ~w:cw ~h:ch ~dark
        | None -> ())
    | _ ->
        (* Other elements like Spacer, Scroll have no primitive visuals themselves. *)
        ());

    (* 2. Recursively render the layouts of all children. *)
    let children_layouts = Layout.children layout in
    (* For Box elements, compute content clip adjusting for faked borders *)
    let final_child_clip =
      match element with
      | Box b ->
          let options = Box.options b in
          let border_left_space =
            match options.border with
            | Some brd -> if Border.left brd then 1 else 0
            | None -> 0
          in
          let border_right_space =
            match options.border with
            | Some brd -> if Border.right brd then 1 else 0
            | None -> 0
          in
          let border_top_space =
            match options.border with
            | Some brd -> if Border.top brd then 1 else 0
            | None -> 0
          in
          let border_bottom_space =
            match options.border with
            | Some brd -> if Border.bottom brd then 1 else 0
            | None -> 0
          in
          let content_x =
            x + border_left_space + Padding.left options.padding
          in
          let content_y = y + border_top_space + Padding.top options.padding in
          let content_w =
            max 0
              (width - border_left_space - border_right_space
              - Padding.left options.padding
              - Padding.right options.padding)
          in
          let content_h =
            max 0
              (height - border_top_space - border_bottom_space
              - Padding.top options.padding
              - Padding.bottom options.padding)
          in

          (* Use nominal content bounds for clipping *)
          let eff_content_x = content_x in
          let eff_content_y = content_y in
          let eff_content_right =
            if content_w > 0 then content_x + content_w - 1 else content_x - 1
          in
          let eff_content_bottom =
            if content_h > 0 then content_y + content_h - 1 else content_y - 1
          in

          let clip_x, clip_y, clip_w, clip_h =
            match clip with
            | Some c -> Render.Clip.(x c, y c, width c, height c)
            | None -> (x, y, width, height)
          in
          let clip_right = clip_x + clip_w - 1 in
          let clip_bottom = clip_y + clip_h - 1 in
          let fake_left =
            eff_content_x < clip_x
            &&
            match options.border with Some brd -> Border.left brd | _ -> false
          in
          let fake_right =
            eff_content_right > clip_right
            &&
            match options.border with
            | Some brd -> Border.right brd
            | _ -> false
          in
          let fake_top = false in
          (* Keep disabled for vertical to preserve lines *)
          let fake_bottom = false in
          (* Keep disabled for vertical to preserve lines *)

          let eff_content_x =
            if fake_left then max eff_content_x (clip_x + 1) else eff_content_x
          in
          let eff_content_right =
            if fake_right then min eff_content_right (clip_right - 1)
            else eff_content_right
          in
          let eff_content_y =
            if fake_top then max eff_content_y (clip_y + 1) else eff_content_y
          in
          let eff_content_bottom =
            if fake_bottom then min eff_content_bottom (clip_bottom - 1)
            else eff_content_bottom
          in
          let eff_content_w = max 0 (eff_content_right - eff_content_x + 1) in
          let eff_content_h = max 0 (eff_content_bottom - eff_content_y + 1) in

          Render.Clip.intersect_opt clip
            (Some
               (Render.Clip.make eff_content_x eff_content_y eff_content_w
                  eff_content_h))
      | _ -> child_clip
    in
    List.iter (render_layout ~clip:final_child_clip) children_layouts
  in

  (* This recursive function orchestrates layout and caching. *)
  let calculate_and_get_layout (x : int) (y : int) (width : int) (height : int)
      (elem : t) : Layout.t =
    let cache_key = (elem, width, height) in
    match Layout_cache.find_opt cache cache_key with
    | Some cached_layout -> cached_layout (* Cache hit! *)
    | None ->
        (* Cache miss: perform the calculation. *)
        (* Layout.calculate will properly recurse with correct sub-bounds *)
        let result = Layout.calculate ~x ~y ~width ~height elem in
        Layout_cache.add cache cache_key result;
        result
  in

  (* 1. Create the initial layout bounds from the screen dimensions. *)
  let width, height = Render.dimensions buffer in

  (* 2. Calculate the entire layout tree first, populating the cache. *)
  let computed_layout_tree =
    calculate_and_get_layout 0 0 width height top_level_element
  in

  (* 3. Now, render the computed tree. *)
  render_layout computed_layout_tree

let pp_element = Element.pp
let measure = Element.measure

let dump_layout element =
  let buf = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer buf in

  (* Calculate layout with a reasonable default size *)
  let width, height = measure element in
  let layout = Layout.calculate ~x:0 ~y:0 ~width ~height element in

  let rec dump_tree indent layout =
    let prefix = String.make (indent * 2) ' ' in
    let x, y, w, h = Layout.geometry layout in
    let elem = Layout.element layout in

    (* Get element type name *)
    let elem_type =
      match elem with
      | Text _ -> "Text"
      | Rich_text _ -> "Rich_text"
      | Spacer _ -> "Spacer"
      | Box b ->
          let dir =
            match Box.options b |> fun o -> o.direction with
            | `Horizontal -> "HBox"
            | `Vertical -> "VBox"
          in
          dir
      | Z_stack _ -> "Z_stack"
      | Flow _ -> "Flow"
      | Grid _ -> "Grid"
      | Scroll _ -> "Scroll"
      | Canvas _ -> "Canvas"
    in

    (* Format element info *)
    Format.fprintf fmt "%s%s [%d,%d %dx%d]" prefix elem_type x y w h;

    (* Add element-specific details *)
    (match elem with
    | Text t ->
        let content = Text.content t in
        let truncated =
          if String.length content > 20 then String.sub content 0 17 ^ "..."
          else content
        in
        Format.fprintf fmt " \"%s\"" (String.escaped truncated)
    | Spacer s ->
        let size = Spacer.size s in
        let flex = Spacer.flex s in
        Format.fprintf fmt " (size=%d, flex=%d)" size flex
    | _ -> ());

    Format.fprintf fmt "@.";

    (* Recurse on children *)
    List.iter (dump_tree (indent + 1)) (Layout.children layout)
  in

  dump_tree 0 layout;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let print ?(width = 80) ?(height = 24) ?(dark = true) ?theme element =
  try
    (* Measure the element to get its natural height *)
    let _, measured_height = measure ~width element in
    (* Use the provided height or the measured height, whichever is larger *)
    (* This ensures all content is rendered *)
    let actual_height = max height measured_height in

    let buffer = Render.create width actual_height in
    render ~dark ?theme buffer element;

    (* Build the output string manually to avoid positioning codes *)
    for y = 0 to actual_height - 1 do
      let line = Buffer.create (width * 2) in
      let last_attr = ref Render.default_attr in

      try
        for x = 0 to width - 1 do
          try
            let cell = Render.get buffer x y in

            (* Emit style changes when needed *)
            if cell.attr <> !last_attr then (
              if !last_attr <> Render.default_attr then
                Buffer.add_string line "\027[0m";

              let attrs = [] in
              let attrs =
                match cell.attr.fg with
                | Some c -> `Fg c :: attrs
                | None -> attrs
              in
              let attrs =
                match cell.attr.bg with
                | Some c -> `Bg c :: attrs
                | None -> attrs
              in
              let attrs = if cell.attr.bold then `Bold :: attrs else attrs in
              let attrs =
                if cell.attr.italic then `Italic :: attrs else attrs
              in
              let attrs =
                if cell.attr.underline then `Underline :: attrs else attrs
              in

              if attrs <> [] then Buffer.add_string line (Ansi.sgr attrs);

              last_attr := cell.attr);

            (* Add the character(s) *)
            match cell.chars with
            | [] -> Buffer.add_char line ' '
            | chars -> List.iter (Uutf.Buffer.add_utf_8 line) chars
          with _ ->
            (* If we get an error for a single cell, just render a space *)
            Buffer.add_char line ' ';
            (* In debug mode, could log the error details *)
            ()
        done;

        (* Reset style at end of line if needed *)
        if !last_attr <> Render.default_attr then
          Buffer.add_string line "\027[0m";

        print_endline (Buffer.contents line)
      with e ->
        (* If we fail to render a line, print an error line *)
        Printf.eprintf "Line render error at y=%d: %s\n" y
          (Printexc.to_string e);
        print_endline "" (* Print empty line to maintain output structure *)
    done;
    flush stdout
  with e ->
    (* If the entire render fails, print an error message *)
    Printf.eprintf "Fatal render error: %s\n" (Printexc.to_string e);
    flush stderr
