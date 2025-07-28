open Harfbuzz

let test_basic_shaping () =
  try
    (* Try to find a font - check common locations *)
    let font_paths =
      [
        "/System/Library/Fonts/Helvetica.ttc";
        (* macOS *)
        "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf";
        (* Linux *)
        "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf";
        (* Linux *)
        "/System/Library/Fonts/Avenir.ttc";
        (* macOS *)
      ]
    in

    let rec find_font = function
      | [] -> failwith "No font found"
      | path :: rest -> if Sys.file_exists path then path else find_font rest
    in

    let font_path = find_font font_paths in
    Printf.printf "Using font: %s\n" font_path;

    (* Create FreeType context first *)
    let ft = Freetype.create ~font_path ~pixel_size:24 in

    (* Create HarfBuzz context from FreeType face *)
    let hb = create_from_ft_face (Freetype.get_face ft) in

    (* Test basic text shaping *)
    let text = "Hello, World!" in
    let glyphs = shape_text hb text in

    Printf.printf "Shaped text '%s' into %d glyphs:\n" text
      (Array.length glyphs);
    Array.iteri
      (fun i glyph ->
        Printf.printf "  [%d] glyph=%d advance=(%d,%d) offset=(%d,%d)\n" i
          glyph.codepoint glyph.x_advance glyph.y_advance glyph.x_offset
          glyph.y_offset)
      glyphs;

    (* Test text width calculation *)
    let width = text_width hb text in
    Printf.printf "Text width: %d units\n" width;

    (* Test font metrics *)
    let metrics = get_metrics hb in
    Printf.printf "Font metrics: ascender=%d descender=%d line_gap=%d\n"
      metrics.ascender metrics.descender metrics.line_gap;

    Printf.printf "Line height: %d units\n" (line_height hb);

    (* Test rendering is now done through FreeType, not HarfBuzz *)
    Printf.printf "Text shaping test completed successfully\n";

    true
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e);
    false

let () =
  if test_basic_shaping () then Printf.printf "All tests passed!\n" else exit 1
