(* Integration test for FreeType + HarfBuzz *)

let test_integration () =
  try
    (* Find a font *)
    let font_paths =
      [
        "/System/Library/Fonts/Helvetica.ttc";
        "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf";
        "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf";
        "/System/Library/Fonts/Avenir.ttc";
      ]
    in

    let rec find_font = function
      | [] -> failwith "No font found"
      | path :: rest -> if Sys.file_exists path then path else find_font rest
    in

    let font_path = find_font font_paths in
    Printf.printf "Using font: %s\n" font_path;

    (* Create FreeType context *)
    let ft = Freetype.create ~font_path ~pixel_size:24 in

    (* Create HarfBuzz context from FreeType face *)
    let hb = Harfbuzz.create_from_ft_face (Freetype.get_face ft) in

    (* Shape some text *)
    let text = "Hello!" in
    let glyphs = Harfbuzz.shape_text hb text in
    Printf.printf "Shaped '%s' into %d glyphs\n" text (Array.length glyphs);

    (* Create a pixel buffer *)
    let width = 100 in
    let height = 40 in
    let pixels = Bytes.create (width * height * 3) in

    (* Fill with white background *)
    Bytes.fill pixels 0 (Bytes.length pixels) '\255';

    (* Render each glyph using FreeType *)
    let x = ref 10 in
    let y = 25 in
    (* baseline position *)

    Array.iter
      (fun glyph ->
        let open Harfbuzz in
        (* Load and render the glyph *)
        let bitmap_data, metrics, pitch =
          Freetype.load_and_render_glyph ft glyph.codepoint
        in

        (* Calculate position *)
        let px = !x + glyph.x_offset in
        let py = y + glyph.y_offset in

        (* Blit the glyph to our buffer *)
        for row = 0 to metrics.Freetype.height - 1 do
          for col = 0 to metrics.Freetype.width - 1 do
            let bx = px + metrics.Freetype.bitmap_left + col in
            let by = py - metrics.Freetype.bitmap_top + row in

            if bx >= 0 && bx < width && by >= 0 && by < height then
              let alpha = Char.code bitmap_data.[(row * pitch) + col] in
              if alpha > 0 then (
                let idx = ((by * width) + bx) * 3 in
                let a = float_of_int alpha /. 255.0 in
                (* Render black text on white background *)
                let new_val = int_of_float (255.0 *. (1.0 -. a)) in
                Bytes.set pixels idx (char_of_int new_val);
                Bytes.set pixels (idx + 1) (char_of_int new_val);
                Bytes.set pixels (idx + 2) (char_of_int new_val))
          done
        done;

        (* Advance position *)
        x := !x + glyph.x_advance)
      glyphs;

    Printf.printf "Rendered text to %dx%d buffer\n" width height;

    (* Check that pixels were modified *)
    let modified = ref 0 in
    for i = 0 to Bytes.length pixels - 1 do
      if Bytes.get pixels i <> '\255' then incr modified
    done;
    Printf.printf "Modified %d pixels\n" !modified;
    assert (!modified > 0);

    Printf.printf "Integration test passed!\n";
    true
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e);
    false

let () = if test_integration () then exit 0 else exit 1
