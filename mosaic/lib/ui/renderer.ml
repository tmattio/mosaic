let src = Logs.Src.create "renderer" ~doc:"UI rendering events"

module Log = (val Logs.src_log src : Logs.LOG)

type render_context = {
  screen : Screen.t;
  dark : bool;
  theme : Theme.t;
  viewport : Screen.Viewport.t;
  snapshot : Layout_snapshot.t option;
      (* Optional snapshot to populate during render *)
  mutable z_counter : int; (* Track z-order during rendering *)
  inherited_style : Ansi.Style.t option;
      (* Inherited style from parent for explicit inheritance *)
}

type bounds = { x : float; y : float; width : float; height : float }

let measure_string str =
  (* Use Grid's display width calculation which handles wide chars, emoji, etc. *)
  Grid.string_width str

(* Optimized single-pass truncation with minimal allocations *)
let truncate_string_with_ellipsis str max_width suffix =
  let suffix_width = Grid.string_width suffix in
  let str_width = Grid.string_width str in
  if str_width <= max_width then str
  else
    (* Single-pass truncation using byte positions *)
    let target_width =
      if max_width <= suffix_width then max_width else max_width - suffix_width
    in

    let decoder = Uutf.decoder ~encoding:`UTF_8 (`String str) in
    let current_width = ref 0 in
    let last_byte = ref 0 in
    let continue = ref true in

    while !continue do
      match Uutf.decode decoder with
      | `Uchar u ->
          let char_width = Grid.char_width u in
          let new_width = !current_width + char_width in
          if new_width <= target_width then (
            current_width := new_width;
            last_byte := Uutf.decoder_byte_count decoder)
          else continue := false
      | _ -> continue := false
    done;

    if !last_byte = 0 then ""
    else if max_width <= suffix_width then String.sub str 0 !last_byte
    else String.sub str 0 !last_byte ^ suffix

let pad_string str width =
  let str_width = measure_string str in
  if str_width >= width then str else str ^ String.make (width - str_width) ' '

let expand_tabs ~tab_width ~start_col str =
  let buf = Buffer.create (String.length str * 2) in
  let col = ref start_col in
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String str) in
  let rec loop () =
    match Uutf.decode decoder with
    | `Uchar u ->
        if Uchar.equal u (Uchar.of_char '\t') then (
          let spaces = tab_width - (!col mod tab_width) in
          Buffer.add_string buf (String.make spaces ' ');
          col := !col + spaces)
        else (
          Uutf.Buffer.add_utf_8 buf u;
          col := !col + Grid.char_width u);
        loop ()
    | `End -> ()
    | `Malformed s ->
        Buffer.add_string buf s;
        loop ()
    | `Await -> assert false
  in
  loop ();
  Buffer.contents buf

(* Text wrapping cache for performance optimization *)
module WrapCache = struct
  type key = string * int
  type t = (key, string list) Hashtbl.t

  let create () = Hashtbl.create 128
  let find_opt = Hashtbl.find_opt
  let add = Hashtbl.add
end

let wrap_cache = WrapCache.create ()

(* Optimized iterative text wrapping with caching *)
let wrap_line_to_width line width measure_fn =
  (* Check cache first *)
  let cache_key = (line, width) in
  match WrapCache.find_opt wrap_cache cache_key with
  | Some result -> result
  | None ->
      (* Iterative implementation to avoid stack overflow and reduce allocations *)
      let result = ref [] in
      let current_pos = ref 0 in
      let line_len = String.length line in

      while !current_pos < line_len do
        let remaining =
          String.sub line !current_pos (line_len - !current_pos)
        in

        if measure_fn remaining <= width then (
          (* Rest of line fits, we're done *)
          result := remaining :: !result;
          current_pos := line_len)
        else
          (* Find break point using single pass *)
          let decoder = Uutf.decoder ~encoding:`UTF_8 (`String remaining) in
          let last_space_byte = ref (-1) in
          let current_width = ref 0 in
          let current_byte = ref 0 in
          let break_byte = ref 0 in
          let found_break = ref false in

          while not !found_break do
            match Uutf.decode decoder with
            | `Uchar u ->
                let char_width = Grid.char_width u in
                let new_width = !current_width + char_width in
                let byte_after = Uutf.decoder_byte_count decoder in

                if new_width > width then (
                  (* Found our break point *)
                  break_byte :=
                    if !last_space_byte > 0 then !last_space_byte
                    else if !current_byte > 0 then !current_byte
                    else byte_after;
                  (* At least take one char *)
                  found_break := true)
                else (
                  current_width := new_width;
                  current_byte := byte_after;
                  if Uchar.equal u (Uchar.of_char ' ') then
                    last_space_byte := byte_after)
            | `End ->
                break_byte := String.length remaining;
                found_break := true
            | _ -> found_break := true
          done;

          if !break_byte = 0 then
            (* Can't break, force at least one character *)
            let decoder = Uutf.decoder ~encoding:`UTF_8 (`String remaining) in
            match Uutf.decode decoder with
            | `Uchar _ ->
                let char_len = Uutf.decoder_byte_count decoder in
                result := String.sub remaining 0 char_len :: !result;
                current_pos := !current_pos + char_len
            | _ -> current_pos := line_len (* Give up *)
          else
            let this_line = String.sub remaining 0 !break_byte in
            result := this_line :: !result;
            current_pos := !current_pos + !break_byte;

            (* Skip leading spaces on next line *)
            let remaining_after =
              String.sub line !current_pos (line_len - !current_pos)
            in
            let decoder =
              Uutf.decoder ~encoding:`UTF_8 (`String remaining_after)
            in
            let space_bytes = ref 0 in
            let continue = ref true in
            while !continue do
              match Uutf.decode decoder with
              | `Uchar u ->
                  if Uchar.equal u (Uchar.of_char ' ') then
                    space_bytes := Uutf.decoder_byte_count decoder
                  else continue := false
              | _ -> continue := false
            done;
            current_pos := !current_pos + !space_bytes
      done;

      let final_result = List.rev !result in
      (* Cache the result *)
      WrapCache.add wrap_cache cache_key final_result;
      final_result

let measure_text_content ~known_dimensions ~available_space ~tab_width ~wrap
    content =
  (* Unified text measurement logic using actual wrapping algorithm *)
  let lines = String.split_on_char '\n' content in
  (* Expand tabs before measuring *)
  let expanded_lines =
    List.map (fun line -> expand_tabs ~tab_width ~start_col:0 line) lines
  in

  (* Calculate width and height based on wrap mode *)
  let computed_width, computed_height =
    match wrap with
    | `Wrap -> (
        (* When wrapping, use actual wrap algorithm to count lines *)
        match available_space.Toffee.Geometry.Size.width with
        | Toffee.Available_space.Definite available_w ->
            let available_width = max 1 (int_of_float available_w) in
            (* Ensure at least 1 to avoid issues *)
            (* Count wrapped lines using actual wrapping logic *)
            let wrapped_line_count =
              List.fold_left
                (fun acc line ->
                  if measure_string line <= available_width then acc + 1
                  else
                    (* Use actual wrapping to count lines *)
                    let wrapped =
                      wrap_line_to_width line available_width measure_string
                    in
                    acc + List.length wrapped)
                0 expanded_lines
            in
            (* For wrapped text, width is constrained to available width *)
            let max_line_width =
              List.fold_left
                (fun acc line ->
                  if measure_string line <= available_width then
                    max acc (measure_string line)
                  else
                    (* Check width of wrapped lines *)
                    let wrapped =
                      wrap_line_to_width line available_width measure_string
                    in
                    List.fold_left
                      (fun m l -> max m (measure_string l))
                      acc wrapped)
                0 expanded_lines
            in
            let width =
              match known_dimensions.Toffee.Geometry.Size.width with
              | Some w -> w
              | None -> min available_w (float_of_int max_line_width)
            in
            let height =
              match known_dimensions.Toffee.Geometry.Size.height with
              | Some h -> h
              | None -> float_of_int wrapped_line_count
            in
            (width, height)
        | _ ->
            (* No definite width to wrap to, use natural size *)
            let max_width =
              List.fold_left
                (fun acc line -> max acc (measure_string line))
                0 expanded_lines
            in
            let width =
              match known_dimensions.Toffee.Geometry.Size.width with
              | Some w -> w
              | None -> float_of_int max_width
            in
            let height =
              match known_dimensions.Toffee.Geometry.Size.height with
              | Some h -> h
              | None -> float_of_int (List.length lines)
            in
            (width, height))
    | `Truncate | `Clip ->
        (* Don't clamp to available size - use natural content size *)
        let max_width =
          List.fold_left
            (fun acc line -> max acc (measure_string line))
            0 expanded_lines
        in
        let width =
          match known_dimensions.Toffee.Geometry.Size.width with
          | Some w -> w
          | None -> float_of_int max_width
        in
        let height =
          match known_dimensions.Toffee.Geometry.Size.height with
          | Some h -> h
          | None -> float_of_int (List.length lines)
        in
        (width, height)
  in
  { Toffee.Geometry.Size.width = computed_width; height = computed_height }

let draw_border ctx border bounds =
  let { Border.tl; th; tr; vl; bl; bh; br; vr; ml; mr; mt; mb; mc } =
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

  (* Handle edge cases for small sizes *)
  if width <= 0 || height <= 0 then ()
  else if width = 1 && height = 1 then
    (* Single cell - pick appropriate character based on which borders are enabled *)
    let glyph =
      match
        ( Border.top border,
          Border.bottom border,
          Border.left border,
          Border.right border )
      with
      | true, true, true, true -> mc (* All sides - use cross *)
      | true, true, true, false -> mr (* Top, bottom, left *)
      | true, true, false, true -> ml (* Top, bottom, right *)
      | true, false, true, true -> mb (* Top, left, right *)
      | false, true, true, true -> mt (* Bottom, left, right *)
      | true, true, false, false -> vl (* Top and bottom - vertical *)
      | false, false, true, true -> th (* Left and right - horizontal *)
      | true, false, true, false -> br (* Top and left - bottom-right corner *)
      | true, false, false, true -> bl (* Top and right - bottom-left corner *)
      | false, true, true, false -> tr (* Bottom and left - top-right corner *)
      | false, true, false, true -> tl (* Bottom and right - top-left corner *)
      | true, false, false, false | false, true, false, false ->
          vl (* Single vertical *)
      | false, false, true, false | false, false, false, true ->
          th (* Single horizontal *)
      | false, false, false, false -> " " (* No borders *)
    in
    Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row ~col ~glyph
      ~attrs
  else if height = 1 then (
    if
      (* Single row - only draw horizontal borders *)
      Border.top border || Border.bottom border
    then (
      let left_glyph = if Border.left border then th else th in
      Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row ~col
        ~glyph:left_glyph ~attrs;
      for i = 1 to width - 2 do
        Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row
          ~col:(col + i) ~glyph:th ~attrs
      done;
      if width > 1 then
        let right_glyph = if Border.right border then th else th in
        Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row
          ~col:(col + width - 1)
          ~glyph:right_glyph ~attrs))
  else if width = 1 then (
    if
      (* Single column - only draw vertical borders *)
      Border.left border || Border.right border
    then
      for i = 0 to height - 1 do
        Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row:(row + i)
          ~col ~glyph:vl ~attrs
      done)
  else if
    (* Draw top border *)
    Border.top border && height > 0
  then (
    (* Left corner: use corner if left border enabled, otherwise horizontal *)
    let left_glyph = if Border.left border then tl else th in
    Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row ~col
      ~glyph:left_glyph ~attrs;

    (* Draw horizontal line and text in top border if present *)
    let draw_end = width - 2 in
    (* Stop before right corner *)
    (match border.top_text with
    | Some { text; align; style = text_style } ->
        let padded_text = " " ^ text ^ " " in
        let text_len = Grid.string_width padded_text in
        let available = draw_end in
        (* Space available for text and lines *)
        if text_len <= available then (
          (* Calculate position based on alignment *)
          let text_start =
            match align with
            | `Left -> 1
            | `Center -> 1 + ((available - text_len) / 2)
            | `Right -> draw_end - text_len
          in
          (* Ensure text doesn't overrun into corner *)
          let text_start = min text_start (draw_end - text_len) in
          let text_start = max 1 text_start in

          (* Draw horizontal line before text *)
          for i = 1 to text_start - 1 do
            Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row
              ~col:(col + i) ~glyph:th ~attrs
          done;
          (* Draw the text *)
          let text_attrs =
            match text_style with
            | Some s -> Style.resolve s ~dark:ctx.dark ~pos:(0, 0) ~bounds:(1, 1)
            | None -> attrs
          in
          String.iteri
            (fun idx c ->
              Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row
                ~col:(col + text_start + idx)
                ~glyph:(String.make 1 c) ~attrs:text_attrs)
            padded_text;
          (* Draw horizontal line after text *)
          let line_start = text_start + text_len in
          if line_start <= draw_end then
            for i = line_start to draw_end do
              Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row
                ~col:(col + i) ~glyph:th ~attrs
            done)
        else
          for
            (* Text too long, just draw the border *)
            i = 1 to width - 2
          do
            Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row
              ~col:(col + i) ~glyph:th ~attrs
          done
    | None ->
        (* No text, draw horizontal line normally *)
        for i = 1 to width - 2 do
          Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row
            ~col:(col + i) ~glyph:th ~attrs
        done);

    (* Right corner: use corner if right border enabled, otherwise horizontal *)
    if width > 1 then
      let right_glyph = if Border.right border then tr else th in
      Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row
        ~col:(col + width - 1)
        ~glyph:right_glyph ~attrs)
  else (
    (* No top border, only draw vertical lines if no corner will be drawn later *)
    if
      Border.left border && height > 0
      && not (Border.bottom border && height = 1)
    then
      Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row ~col ~glyph:vl
        ~attrs;
    if
      Border.right border && width > 0 && height > 0
      && not (Border.bottom border && height = 1)
    then
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

    (* Draw horizontal line and text in bottom border if present *)
    let draw_end = width - 2 in
    (* Stop before right corner *)
    (match border.bottom_text with
    | Some { text; align; style = text_style } ->
        let padded_text = " " ^ text ^ " " in
        let text_len = Grid.string_width padded_text in
        let available = draw_end in
        (* Space available for text and lines *)
        if text_len <= available then (
          (* Calculate position based on alignment *)
          let text_start =
            match align with
            | `Left -> 1
            | `Center -> 1 + ((available - text_len) / 2)
            | `Right -> draw_end - text_len
          in
          (* Ensure text doesn't overrun into corner *)
          let text_start = min text_start (draw_end - text_len) in
          let text_start = max 1 text_start in

          (* Draw horizontal line before text *)
          for i = 1 to text_start - 1 do
            Screen.set_grapheme ctx.screen ~viewport:ctx.viewport
              ~row:bottom_row ~col:(col + i) ~glyph:bh ~attrs
          done;
          (* Draw the text *)
          let text_attrs =
            match text_style with
            | Some s -> Style.resolve s ~dark:ctx.dark ~pos:(0, 0) ~bounds:(1, 1)
            | None -> attrs
          in
          String.iteri
            (fun idx c ->
              Screen.set_grapheme ctx.screen ~viewport:ctx.viewport
                ~row:bottom_row
                ~col:(col + text_start + idx)
                ~glyph:(String.make 1 c) ~attrs:text_attrs)
            padded_text;
          (* Draw horizontal line after text *)
          let line_start = text_start + text_len in
          if line_start <= draw_end then
            for i = line_start to draw_end do
              Screen.set_grapheme ctx.screen ~viewport:ctx.viewport
                ~row:bottom_row ~col:(col + i) ~glyph:bh ~attrs
            done)
        else
          for
            (* Text too long, just draw the border *)
            i = 1 to width - 2
          do
            Screen.set_grapheme ctx.screen ~viewport:ctx.viewport
              ~row:bottom_row ~col:(col + i) ~glyph:bh ~attrs
          done
    | None ->
        (* No text, draw horizontal line normally *)
        for i = 1 to width - 2 do
          Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row:bottom_row
            ~col:(col + i) ~glyph:bh ~attrs
        done);

    (* Right corner: use corner if right border enabled, otherwise horizontal *)
    if width > 1 then
      let right_glyph = if Border.right border then br else bh in
      Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row:bottom_row
        ~col:(col + width - 1)
        ~glyph:right_glyph ~attrs)
  else (
    (* No bottom border, only draw vertical lines if not already drawn by top logic *)
    (if Border.left border && height > 1 && not (Border.top border) then
       let bottom_row = row + height - 1 in
       Screen.set_grapheme ctx.screen ~viewport:ctx.viewport ~row:bottom_row
         ~col ~glyph:vl ~attrs);
    if Border.right border && width > 0 && height > 1 && not (Border.top border)
    then
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

let with_clip ctx bounds f =
  (* Encapsulated clipping logic that can be reused *)
  let clip_viewport =
    Screen.Viewport.make ~row:(int_of_float bounds.y)
      ~col:(int_of_float bounds.x)
      ~width:(int_of_float bounds.width)
      ~height:(int_of_float bounds.height)
  in
  let ctx' = { ctx with viewport = clip_viewport } in
  f ctx'

let rec render_node_with_offset ctx (node_id, tree) (parent_x, parent_y) =
  (* Get layout for this node *)
  let layout =
    match Toffee.layout tree node_id with
    | Ok l -> l
    | Error _ -> failwith "Failed to get layout"
  in
  let location = Toffee.Layout.location layout in
  let size = Toffee.Layout.size layout in
  Log.debug (fun m ->
      m "Rendering node at (%.1f, %.1f) size (%.1f, %.1f)"
        (parent_x +. location.x) (parent_y +. location.y) size.width size.height);

  (* Calculate absolute position by adding parent offset *)
  let absolute_x = location.x +. parent_x in
  let absolute_y = location.y +. parent_y in

  let bounds =
    { x = absolute_x; y = absolute_y; width = size.width; height = size.height }
  in

  (* Render this node's renderable *)
  (match Toffee.get_node_context tree node_id with
  | Some renderable -> (
      (* Recursively collect all keys from nested Keyed elements *)
      let rec collect_keys_and_unwrap rend keys =
        match rend with
        | Renderable.Keyed { key; child } ->
            collect_keys_and_unwrap child ((key, bounds) :: keys)
        | r -> (r, keys)
      in
      let renderable, keys = collect_keys_and_unwrap renderable [] in

      (* Record all keyed elements in snapshot *)
      (match ctx.snapshot with
      | Some snapshot ->
          List.iter
            (fun (key, key_bounds) ->
              let rect : Layout_snapshot.rect =
                {
                  Layout_snapshot.x = int_of_float key_bounds.x;
                  Layout_snapshot.y = int_of_float key_bounds.y;
                  Layout_snapshot.w = int_of_float key_bounds.width;
                  Layout_snapshot.h = int_of_float key_bounds.height;
                }
              in
              let entry : Layout_snapshot.entry =
                {
                  Layout_snapshot.rect;
                  Layout_snapshot.z_index = ctx.z_counter;
                  Layout_snapshot.clipping = None;
                  (* TODO: track clipping context *)
                }
              in
              ctx.z_counter <- ctx.z_counter + 1;
              Layout_snapshot.record snapshot (Attr.key key) entry)
            keys
      | None -> ());

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

          (* Resolve style with inheritance *)
          let attrs =
            match ctx.inherited_style with
            | Some inherited ->
                (* Start with inherited style, then apply local style *)
                let base = inherited in
                let local_attrs =
                  Style.resolve style ~dark:ctx.dark ~pos:(0, 0)
                    ~bounds:(width, height)
                in
                Ansi.Style.merge base local_attrs
            | None ->
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
                      (* Use extracted wrapping logic *)
                      wrap_line_to_width expanded width measure_string)
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
                  | `Stretch when line <> "" && measure_string line > 0 ->
                      (* Repeat the pattern to fill the width *)
                      let pattern = line in
                      let pattern_width = measure_string pattern in
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
                  | _ -> line
                in
                let _ =
                  Screen.set_text ctx.screen ~viewport:ctx.viewport
                    ~row:(row + line_idx) ~col:start_col ~text:final_line ~attrs
                in
                ())
            processed_lines
      | Renderable.Keyed _ ->
          (* Should never reach here - handled above *)
          failwith "Unexpected Keyed renderable in render switch"
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
          draw ~width ~height plot
      | Renderable.Scroll { h_offset; v_offset } ->
          (* Ensure scroll offsets are non-negative to prevent out-of-bounds access *)
          let safe_h_offset = max 0 h_offset in
          let safe_v_offset = max 0 v_offset in

          (* Clip children to this node's bounds and apply scroll offset *)
          with_clip ctx bounds (fun ctx' ->
              (* Render children with adjusted offset, passing inherited style *)
              let ctx_with_inherited =
                { ctx' with inherited_style = ctx.inherited_style }
              in
              match Toffee.children tree node_id with
              | Ok children ->
                  List.iter
                    (fun child_id ->
                      render_node_with_offset ctx_with_inherited (child_id, tree)
                        ( absolute_x -. float_of_int safe_h_offset,
                          absolute_y -. float_of_int safe_v_offset ))
                    children
              | Error _ -> ())
      (* Return early since we've handled children *))
  | _ -> ());

  (* Render children with this node's absolute position as their parent offset *)
  (* Skip if we already handled children (e.g., for Scroll nodes) *)
  (* Recursively unwrap keyed elements to check for Scroll *)
  let rec unwrap_to_check_scroll rend =
    match rend with
    | Renderable.Keyed { child; _ } -> unwrap_to_check_scroll child
    | Renderable.Scroll _ -> true
    | _ -> false
  in
  match Toffee.get_node_context tree node_id with
  | Some rend when unwrap_to_check_scroll rend -> () (* Already handled above *)
  | _ -> (
      (* OVERFLOW AND CLIPPING BEHAVIOR:
         
         Clipping is applied to children in the following cases:
         1. When a box has a border (border = Some _) - borders always clip content
         2. When overflow is set to Hidden or Scroll (but not Visible)
         
         Important notes:
         - Content CAN overflow visually when overflow=Visible and no border is present
         - This matches CSS behavior where borders create a clipping context
         - Scroll views handle their own clipping through viewport adjustments
         - Nested clipping contexts are properly composed
      *)

      (* Check if this node has a border - boxes with borders always clip *)
      let has_border =
        match Toffee.get_node_context tree node_id with
        | Some (Renderable.Box { border = Some _; _ }) -> true
        | _ -> false
      in

      (* Determine the complete resolved style for this node to pass to children *)
      let inherited_style_for_children =
        (* Helper to unwrap Keyed wrappers to get to the actual renderable *)
        let rec unwrap_renderable = function
          | Renderable.Keyed { child; _ } -> unwrap_renderable child
          | other -> other
        in
        match Toffee.get_node_context tree node_id with
        | Some rend -> (
            (* Unwrap any Keyed wrappers to get to the actual renderable *)
            let unwrapped = unwrap_renderable rend in
            match unwrapped with
            | Renderable.Box { background; _ } -> (
                match background with
                | Some bg_style -> (
                    (* Resolve this node's style *)
                    let width = int_of_float bounds.width in
                    let height = int_of_float bounds.height in
                    let node_style =
                      Style.resolve bg_style ~dark:ctx.dark ~pos:(0, 0)
                        ~bounds:(width, height)
                    in
                    (* Merge with parent's inherited style if any *)
                    match ctx.inherited_style with
                    | Some parent_style ->
                        Some (Ansi.Style.merge parent_style node_style)
                    | None -> Some node_style)
                | None -> ctx.inherited_style)
            | Renderable.Text { style; _ } -> (
                (* Text nodes also have styles that should be inherited by children if any *)
                let width = int_of_float bounds.width in
                let height = int_of_float bounds.height in
                let node_style =
                  Style.resolve style ~dark:ctx.dark ~pos:(0, 0)
                    ~bounds:(width, height)
                in
                (* Merge with parent's inherited style if any *)
                match ctx.inherited_style with
                | Some parent_style ->
                    Some (Ansi.Style.merge parent_style node_style)
                | None -> Some node_style)
            | _ -> ctx.inherited_style)
        | None -> ctx.inherited_style
      in

      (* Check if this node has overflow:hidden or clip *)
      let has_overflow_hidden =
        match Toffee.style tree node_id with
        | Ok style ->
            let overflow_point = Toffee.Style.overflow style in
            let open Toffee.Style.Overflow in
            overflow_point.Toffee.Geometry.Point.x = Hidden
            || overflow_point.Toffee.Geometry.Point.y = Hidden
            || overflow_point.Toffee.Geometry.Point.x = Clip
            || overflow_point.Toffee.Geometry.Point.y = Clip
        | Error _ -> false
      in

      (* Only clip if overflow is explicitly hidden, not just because there's a border *)
      (* Borders should contain content but not force clipping if overflow is visible *)
      let should_clip = has_overflow_hidden in

      match Toffee.children tree node_id with
      | Ok children ->
          if should_clip then (* Apply clipping by setting viewport bounds *)
            (* If there's a border, clip to the content area (inside the border) *)
            let clip_row, clip_col, clip_width, clip_height =
              if has_border then
                ( int_of_float bounds.y + 1,
                  int_of_float bounds.x + 1,
                  int_of_float bounds.width - 2,
                  int_of_float bounds.height - 2 )
              else
                ( int_of_float bounds.y,
                  int_of_float bounds.x,
                  int_of_float bounds.width,
                  int_of_float bounds.height )
            in

            let clip_viewport =
              Screen.Viewport.make ~row:clip_row ~col:clip_col ~width:clip_width
                ~height:clip_height
            in
            let ctx' =
              {
                ctx with
                viewport = clip_viewport;
                inherited_style = inherited_style_for_children;
              }
            in
            List.iter
              (fun child_id ->
                render_node_with_offset ctx' (child_id, tree)
                  (absolute_x, absolute_y))
              children
          else
            let ctx' =
              { ctx with inherited_style = inherited_style_for_children }
            in
            List.iter
              (fun child_id ->
                render_node_with_offset ctx' (child_id, tree)
                  (absolute_x, absolute_y))
              children
      | Error _ -> ())

(* Entry point for rendering - root node has no parent offset *)
let render_node ctx (node_id, tree) =
  render_node_with_offset ctx (node_id, tree) (0.0, 0.0)
