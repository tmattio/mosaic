type render_context = {
  screen : Screen.t;
  dark : bool;
  theme : Theme.t;
  viewport : Screen.Viewport.t;
}

type bounds = { x : float; y : float; width : float; height : float }

let measure_string str =
  (* Use Grid's display width calculation which handles wide chars, emoji, etc. *)
  Grid.string_width str

let truncate_string_with_ellipsis str max_width suffix =
  let suffix_width = Grid.string_width suffix in
  let str_width = Grid.string_width str in
  if str_width <= max_width then str
  else if max_width <= suffix_width then (
    (* Not enough space for suffix, just truncate *)
    let buf = Buffer.create max_width in
    let decoder = Uutf.decoder ~encoding:`UTF_8 (`String str) in
    let rec take_chars current_width =
      if current_width >= max_width then ()
      else
        match Uutf.decode decoder with
        | `Uchar u ->
            let char_width = Grid.char_width u in
            if current_width + char_width <= max_width then (
              let char_str =
                String.init (Uchar.utf_8_byte_length u) (fun i ->
                    String.get str
                      (Uutf.decoder_byte_count decoder
                      - Uchar.utf_8_byte_length u + i))
              in
              Buffer.add_string buf char_str;
              take_chars (current_width + char_width))
        | `End | `Malformed _ | `Await -> ()
    in
    take_chars 0;
    Buffer.contents buf)
  else
    (* Truncate with ellipsis *)
    let target_width = max_width - suffix_width in
    let buf = Buffer.create (String.length str) in
    let decoder = Uutf.decoder ~encoding:`UTF_8 (`String str) in
    let rec take_chars current_width =
      if current_width >= target_width then ()
      else
        match Uutf.decode decoder with
        | `Uchar u ->
            let char_width = Grid.char_width u in
            if current_width + char_width <= target_width then (
              let char_str =
                String.init (Uchar.utf_8_byte_length u) (fun i ->
                    String.get str
                      (Uutf.decoder_byte_count decoder
                      - Uchar.utf_8_byte_length u + i))
              in
              Buffer.add_string buf char_str;
              take_chars (current_width + char_width))
        | `End | `Malformed _ | `Await -> ()
    in
    take_chars 0;
    Buffer.contents buf ^ suffix

let pad_string str width =
  let str_width = measure_string str in
  if str_width >= width then str else str ^ String.make (width - str_width) ' '

let expand_tabs ~tab_width ~start_col str =
  (* Expand tabs based on current column position to align to tab stops *)
  let buf = Buffer.create (String.length str * 2) in
  let col = ref start_col in
  String.iter
    (fun ch ->
      if ch = '\t' then (
        (* Calculate spaces to next tab stop *)
        let spaces_to_tab_stop = tab_width - (!col mod tab_width) in
        Buffer.add_string buf (String.make spaces_to_tab_stop ' ');
        col := !col + spaces_to_tab_stop)
      else (
        Buffer.add_char buf ch;
        (* For simplicity, assume single-width chars here - the real width is computed after *)
        incr col))
    str;
  Buffer.contents buf

let draw_border ctx border bounds =
  let {
    Border.tl;
    th;
    tr;
    vl;
    bl;
    bh;
    br;
    vr;
    ml = _;
    mr = _;
    mt = _;
    mb = _;
    mc = _;
  } =
    Border.get_chars (Border.line_style border)
  in
  let row = int_of_float bounds.y in
  let col = int_of_float bounds.x in
  let width = int_of_float bounds.width in
  let height = int_of_float bounds.height in

  let attrs =
    let base_style =
      match Border.color border with
      | Some color -> Style.(fg color)
      | None -> Style.empty
    in
    let final_style =
      match Border.style border with
      | Some s -> Style.merge base_style s
      | None -> base_style
    in
    Style.resolve final_style ~dark:ctx.dark ~pos:(0, 0) ~bounds:(1, 1)
  in

  (* Draw top border *)
  if Border.top border && height > 0 then (
    (* Left corner: use corner if left border enabled, otherwise horizontal *)
    let left_glyph = if Border.left border then tl else th in
    Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row ~col
      ~glyph:left_glyph ~attrs;
    (* Horizontal line *)
    for i = 1 to width - 2 do
      Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row ~col:(col + i)
        ~glyph:th ~attrs
    done;
    (* Right corner: use corner if right border enabled, otherwise horizontal *)
    if width > 1 then
      let right_glyph = if Border.right border then tr else th in
      Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row
        ~col:(col + width - 1)
        ~glyph:right_glyph ~attrs)
  else (
    (* No top border, but draw vertical lines at corners if left/right borders exist *)
    if Border.left border && height > 0 then
      Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row ~col ~glyph:vl
        ~attrs;
    if Border.right border && width > 0 && height > 0 then
      Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row
        ~col:(col + width - 1)
        ~glyph:vr ~attrs);

  (* Draw bottom border *)
  if Border.bottom border && height > 1 then (
    let bottom_row = row + height - 1 in
    (* Left corner: use corner if left border enabled, otherwise horizontal *)
    let left_glyph = if Border.left border then bl else bh in
    Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row:bottom_row ~col
      ~glyph:left_glyph ~attrs;
    (* Horizontal line *)
    for i = 1 to width - 2 do
      Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row:bottom_row
        ~col:(col + i) ~glyph:bh ~attrs
    done;
    (* Right corner: use corner if right border enabled, otherwise horizontal *)
    if width > 1 then
      let right_glyph = if Border.right border then br else bh in
      Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row:bottom_row
        ~col:(col + width - 1)
        ~glyph:right_glyph ~attrs)
  else if
    (* No bottom border, but draw vertical lines at corners if left/right borders exist *)
    Border.left border && height > 1
  then (
    let bottom_row = row + height - 1 in
    Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row:bottom_row ~col
      ~glyph:vl ~attrs;
    if Border.right border && width > 0 && height > 1 then
      let bottom_row = row + height - 1 in
      Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row:bottom_row
        ~col:(col + width - 1)
        ~glyph:vr ~attrs);

  (* Draw left border *)
  (if Border.left border then
     (* Determine vertical range based on top/bottom borders *)
     let start_row = if Border.top border then 1 else 0 in
     let end_row = if Border.bottom border then height - 2 else height - 1 in
     for i = start_row to end_row do
       Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row:(row + i) ~col
         ~glyph:vl ~attrs
     done);

  (* Draw right border *)
  if Border.right border && width > 0 then
    let right_col = col + width - 1 in
    (* Determine vertical range based on top/bottom borders *)
    let start_row = if Border.top border then 1 else 0 in
    let end_row = if Border.bottom border then height - 2 else height - 1 in
    for i = start_row to end_row do
      Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row:(row + i)
        ~col:right_col ~glyph:vr ~attrs
    done

let fill_background ctx style bounds =
  let row = int_of_float bounds.y in
  let col = int_of_float bounds.x in
  let width = int_of_float bounds.width in
  let height = int_of_float bounds.height in

  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let attrs =
        Style.resolve style ~dark:ctx.dark ~pos:(x, y) ~bounds:(width, height)
      in
      Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row:(row + y)
        ~col:(col + x) ~glyph:" " ~attrs
    done
  done

let rec render_node_with_offset ctx (node_id, tree) (parent_x, parent_y) =
  (* Get layout for this node *)
  let layout =
    match Toffee.layout tree node_id with
    | Ok l -> l
    | Error _ -> failwith "Failed to get layout"
  in

  let location = Toffee.Layout.location layout in
  let size = Toffee.Layout.size layout in

  (* Calculate absolute position by adding parent offset *)
  let absolute_x = location.x +. parent_x in
  let absolute_y = location.y +. parent_y in

  let bounds =
    { x = absolute_x; y = absolute_y; width = size.width; height = size.height }
  in

  (* Render this node's renderable *)
  (match Toffee.get_node_context tree node_id with
  | Some renderable -> (
      match renderable with
      | Renderable.Empty -> ()
      | Renderable.Box { border; background } ->
          Option.iter (fun bg -> fill_background ctx bg bounds) background;
          Option.iter (fun b -> draw_border ctx b bounds) border
      | Renderable.Text { content; style; align; tab_width; wrap; _ } ->
          let row = int_of_float bounds.y in
          let col = int_of_float bounds.x in
          let width = int_of_float bounds.width in
          let height = int_of_float bounds.height in

          (* Split content into lines *)
          let lines = String.split_on_char '\n' content in

          let attrs =
            Style.resolve style ~dark:ctx.dark ~pos:(0, 0)
              ~bounds:(width, height)
          in

          (* Process lines based on wrap mode *)
          let processed_lines =
            match wrap with
            | `Wrap ->
                (* Implement word wrapping *)
                List.concat_map
                  (fun line ->
                    let expanded = expand_tabs ~tab_width ~start_col:0 line in
                    if measure_string expanded <= width then [ expanded ]
                    else
                      (* Word-based wrapping with fallback to character wrap *)
                      let rec wrap_line acc current_line =
                        if String.length current_line = 0 then List.rev acc
                        else if measure_string current_line <= width then
                          List.rev (current_line :: acc)
                        else
                          (* Try to find a word boundary to break at *)
                          let find_word_break str max_width =
                            let last_space = ref (-1) in
                            let rec check_width idx =
                              if idx >= String.length str then
                                if !last_space > 0 then !last_space
                                else String.length str
                              else
                                (* Measure the string up to this point *)
                                let substr = String.sub str 0 (idx + 1) in
                                let new_width = measure_string substr in
                                if new_width > max_width then
                                  if !last_space > 0 then !last_space else idx
                                else
                                  let ch = String.get str idx in
                                  if ch = ' ' then last_space := idx + 1;
                                  check_width (idx + 1)
                            in
                            check_width 0
                          in
                          let break_pos = find_word_break current_line width in
                          if break_pos = 0 then
                            (* Can't fit even one word, fall back to character wrap *)
                            let find_char_break str max_width =
                              let rec check_width idx current_width =
                                if idx >= String.length str then
                                  String.length str
                                else
                                  let decoder =
                                    Uutf.decoder ~encoding:`UTF_8
                                      (`String
                                         (String.sub str idx
                                            (String.length str - idx)))
                                  in
                                  match Uutf.decode decoder with
                                  | `Uchar u ->
                                      let char_width = Grid.char_width u in
                                      let new_width =
                                        current_width + char_width
                                      in
                                      if new_width > max_width then idx
                                      else
                                        check_width
                                          (idx + Uchar.utf_8_byte_length u)
                                          new_width
                                  | _ -> idx
                              in
                              check_width 0 0
                            in
                            let char_break =
                              find_char_break current_line width
                            in
                            if char_break = 0 then
                              (* Force at least one character *)
                              let decoder =
                                Uutf.decoder ~encoding:`UTF_8
                                  (`String current_line)
                              in
                              match Uutf.decode decoder with
                              | `Uchar u ->
                                  let char_len = Uchar.utf_8_byte_length u in
                                  let this_line =
                                    String.sub current_line 0 char_len
                                  in
                                  let rest =
                                    String.sub current_line char_len
                                      (String.length current_line - char_len)
                                  in
                                  wrap_line (this_line :: acc) rest
                              | _ -> List.rev acc
                            else
                              let this_line =
                                String.sub current_line 0 char_break
                              in
                              let rest =
                                String.sub current_line char_break
                                  (String.length current_line - char_break)
                              in
                              wrap_line (this_line :: acc) rest
                          else
                            let this_line =
                              String.sub current_line 0 break_pos
                            in
                            let rest =
                              String.sub current_line break_pos
                                (String.length current_line - break_pos)
                            in
                            (* Skip leading spaces on continuation lines *)
                            let rest =
                              let rec skip_spaces s i =
                                if i < String.length s && String.get s i = ' '
                                then skip_spaces s (i + 1)
                                else String.sub s i (String.length s - i)
                              in
                              skip_spaces rest 0
                            in
                            wrap_line (this_line :: acc) rest
                      in
                      wrap_line [] expanded)
                  lines
            | `Truncate ->
                (* Truncate lines that are too wide with ellipsis *)
                List.map
                  (fun line ->
                    let expanded = expand_tabs ~tab_width ~start_col:0 line in
                    if measure_string expanded <= width then expanded
                    else truncate_string_with_ellipsis expanded width "...")
                  lines
            | `Clip ->
                (* Clip lines that are too wide without ellipsis *)
                List.map
                  (fun line ->
                    let expanded = expand_tabs ~tab_width ~start_col:0 line in
                    if measure_string expanded <= width then expanded
                    else truncate_string_with_ellipsis expanded width "")
                  lines
          in

          (* Render processed lines *)
          List.iteri
            (fun line_idx line ->
              if line_idx < height then
                let line_width = measure_string line in
                let start_col =
                  match align with
                  | `Start -> col
                  | `Center -> col + ((width - line_width) / 2)
                  | `End -> col + width - line_width
                  | `Stretch -> col
                in
                let final_line =
                  match align with
                  | `Stretch when line <> "" ->
                      (* Repeat the pattern to fill the width *)
                      let pattern = line in
                      let pattern_width = measure_string pattern in
                      if pattern_width > 0 then
                        let times = width / pattern_width in
                        let remainder = width mod pattern_width in
                        let repeated =
                          String.concat "" (List.init times (fun _ -> pattern))
                        in
                        if remainder > 0 then
                          (* Add partial pattern to fill exact width *)
                          repeated
                          ^ truncate_string_with_ellipsis pattern remainder ""
                        else repeated
                      else line
                  | _ -> line
                in
                let _ =
                  Screen.set_text ctx.screen ~viewport:ctx.viewport
                    ~row:(row + line_idx) ~col:start_col ~text:final_line ~attrs
                in
                ())
            processed_lines
      | Renderable.Canvas { draw } ->
          let row = int_of_float bounds.y in
          let col = int_of_float bounds.x in
          let width = int_of_float bounds.width in
          let height = int_of_float bounds.height in

          let plot ~x ~y ?style glyph =
            if x >= 0 && x < width && y >= 0 && y < height then
              let attrs =
                match style with
                | Some s ->
                    Style.resolve s ~dark:ctx.dark ~pos:(x, y)
                      ~bounds:(width, height)
                | None -> Ansi.Style.default
              in
              Screen.set_grapheme ctx.screen ~viewport:ctx.viewport
                ~row:(row + y) ~col:(col + x) ~glyph ~attrs
          in
          draw plot
      | Renderable.Scroll { h_offset; v_offset } -> (
          (* Clip children to this node's bounds and apply scroll offset *)
          let clip_viewport =
            Screen.Viewport.make ~row:(int_of_float bounds.y)
              ~col:(int_of_float bounds.x)
              ~width:(int_of_float bounds.width)
              ~height:(int_of_float bounds.height)
          in
          (* Use the clip viewport directly for scroll content *)
          let ctx' = { ctx with viewport = clip_viewport } in
          (* Render children with adjusted offset *)
          match Toffee.children tree node_id with
          | Ok children ->
              List.iter
                (fun child_id ->
                  render_node_with_offset ctx' (child_id, tree)
                    ( absolute_x -. float_of_int h_offset,
                      absolute_y -. float_of_int v_offset ))
                children
          | Error _ -> ()
          (* Return early since we've handled children *)))
  | _ -> ());

  (* Render children with this node's absolute position as their parent offset *)
  (* Skip if we already handled children (e.g., for Scroll nodes) *)
  match Toffee.get_node_context tree node_id with
  | Some (Renderable.Scroll _) -> () (* Already handled above *)
  | _ -> (
      (* Check if this node has a border - boxes with borders always clip *)
      let has_border = 
        match Toffee.get_node_context tree node_id with
        | Some (Renderable.Box { border = Some _; _ }) -> true
        | _ -> false
      in
      
      (* Check if this node has overflow:hidden or clip *)
      let has_overflow_hidden = 
        match Toffee.style tree node_id with
        | Ok style ->
            let overflow_point = Toffee.Style.overflow style in
            let open Toffee.Style.Overflow in
            overflow_point.Toffee.Geometry.Point.x = Hidden || 
            overflow_point.Toffee.Geometry.Point.y = Hidden ||
            overflow_point.Toffee.Geometry.Point.x = Clip || 
            overflow_point.Toffee.Geometry.Point.y = Clip
        | Error _ -> false
      in
      
      (* Only clip if overflow is explicitly hidden, not just because there's a border *)
      (* Borders should contain content but not force clipping if overflow is visible *)
      let should_clip = has_overflow_hidden in
      
      match Toffee.children tree node_id with
      | Ok children ->
          if should_clip then (
            (* Apply clipping by setting viewport bounds *)
            
            (* If there's a border, clip to the content area (inside the border) *)
            let (clip_row, clip_col, clip_width, clip_height) =
              if has_border then
                (int_of_float bounds.y + 1,
                 int_of_float bounds.x + 1,
                 int_of_float bounds.width - 2,
                 int_of_float bounds.height - 2)
              else
                (int_of_float bounds.y,
                 int_of_float bounds.x,
                 int_of_float bounds.width,
                 int_of_float bounds.height)
            in
            
            let clip_viewport =
              Screen.Viewport.make ~row:clip_row ~col:clip_col
                ~width:clip_width ~height:clip_height
            in
            let ctx' = { ctx with viewport = clip_viewport } in
            List.iter
              (fun child_id ->
                render_node_with_offset ctx' (child_id, tree)
                  (absolute_x, absolute_y))
              children)
          else
            List.iter
              (fun child_id ->
                render_node_with_offset ctx (child_id, tree)
                  (absolute_x, absolute_y))
              children
      | Error _ -> ())

(* Entry point for rendering - root node has no parent offset *)
let render_node ctx (node_id, tree) =
  render_node_with_offset ctx (node_id, tree) (0.0, 0.0)
