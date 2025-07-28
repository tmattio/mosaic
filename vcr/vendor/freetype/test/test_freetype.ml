open Freetype

let test_basic_rendering () =
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

    (* Create FreeType context *)
    let ft = create ~font_path ~pixel_size:24 in
    Printf.printf "Created FreeType context with pixel size 24\n";

    (* Test character rendering *)
    let test_char = Char.code 'A' in
    let bitmap_data, metrics, pitch = load_and_render_char ft test_char in
    Printf.printf "Rendered character 'A':\n";
    Printf.printf "  Bitmap size: %dx%d\n" metrics.width metrics.height;
    Printf.printf "  Bitmap left: %d, top: %d\n" metrics.bitmap_left
      metrics.bitmap_top;
    Printf.printf "  Advance: (%d, %d)\n" metrics.advance_x metrics.advance_y;
    Printf.printf "  Pitch: %d\n" pitch;
    Printf.printf "  Data length: %d bytes\n" (String.length bitmap_data);

    (* Test glyph index lookup *)
    let glyph_index = get_char_index ft test_char in
    Printf.printf "Glyph index for 'A': %d\n" glyph_index;

    (* Test rendering by glyph index *)
    let _, metrics2, _ = load_and_render_glyph ft glyph_index in
    Printf.printf "Rendered glyph by index:\n";
    Printf.printf "  Bitmap size: %dx%d\n" metrics2.width metrics2.height;
    assert (metrics.width = metrics2.width);
    assert (metrics.height = metrics2.height);

    (* Test font metrics *)
    let font_metrics = get_metrics ft in
    Printf.printf "Font metrics:\n";
    Printf.printf "  Ascender: %d\n" font_metrics.ascender;
    Printf.printf "  Descender: %d\n" font_metrics.descender;
    Printf.printf "  Height: %d\n" font_metrics.height;
    Printf.printf "  Max advance width: %d\n" font_metrics.max_advance_width;
    Printf.printf "  Max advance height: %d\n" font_metrics.max_advance_height;

    (* Test text rendering to pixel buffer *)
    let width = 200 in
    let height = 50 in
    let pixels = Bytes.create (width * height * 3) in

    (* Fill with white background *)
    Bytes.fill pixels 0 (Bytes.length pixels) '\255';

    (* Render black text *)
    let x = 10 in
    let y = 30 in
    let color = (0, 0, 0) in
    render_text ft ~pixels ~width ~height ~x ~y ~color "Hello!";

    Printf.printf "Rendered text to %dx%d pixel buffer\n" width height;

    (* Check that some pixels were modified (text was rendered) *)
    let modified_pixels = ref 0 in
    for i = 0 to Bytes.length pixels - 1 do
      if Bytes.get pixels i <> '\255' then incr modified_pixels
    done;
    Printf.printf "Modified %d pixels\n" !modified_pixels;
    assert (!modified_pixels > 0);

    true
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e);
    false

let test_different_sizes () =
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

    (* Test different pixel sizes *)
    let sizes = [ 12; 16; 24; 32; 48 ] in
    List.iter
      (fun size ->
        let ft = create ~font_path ~pixel_size:size in
        let _, metrics, _ = load_and_render_char ft (Char.code 'A') in
        Printf.printf "Size %d: 'A' has dimensions %dx%d, advance %d\n" size
          metrics.width metrics.height metrics.advance_x)
      sizes;

    true
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e);
    false

let test_special_characters () =
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
    let ft = create ~font_path ~pixel_size:24 in

    (* Test various characters *)
    let test_chars =
      [
        ('A', "uppercase A");
        ('a', "lowercase a");
        ('1', "digit 1");
        (' ', "space");
        ('.', "period");
        ('@', "at sign");
        ('#', "hash");
        ('$', "dollar");
        ('%', "percent");
        ('&', "ampersand");
      ]
    in

    List.iter
      (fun (ch, desc) ->
        let char_code = Char.code ch in
        let glyph_index = get_char_index ft char_code in
        if glyph_index = 0 then
          Printf.printf "Warning: No glyph for %s (char code %d)\n" desc
            char_code
        else
          let _, metrics, _ = load_and_render_char ft char_code in
          Printf.printf "Character '%c' (%s): %dx%d, advance %d\n" ch desc
            metrics.width metrics.height metrics.advance_x)
      test_chars;

    true
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e);
    false

let test_get_face () =
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
    let ft = create ~font_path ~pixel_size:24 in

    (* Test get_face function *)
    let face = get_face ft in
    Printf.printf "Got face from FreeType context\n";

    (* We can use the face with other low-level functions *)
    set_pixel_sizes face 32 32;
    Printf.printf "Successfully changed pixel size using face\n";

    true
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e);
    false

let () =
  let tests =
    [
      ("Basic rendering", test_basic_rendering);
      ("Different sizes", test_different_sizes);
      ("Special characters", test_special_characters);
      ("Get face", test_get_face);
    ]
  in

  let passed = ref 0 in
  let total = List.length tests in

  List.iter
    (fun (name, test_fn) ->
      Printf.printf "\n=== Test: %s ===\n" name;
      if test_fn () then (
        Printf.printf "✓ Test passed\n";
        incr passed)
      else Printf.printf "✗ Test failed\n")
    tests;

  Printf.printf "\n=== Summary ===\n";
  Printf.printf "Passed: %d/%d tests\n" !passed total;

  if !passed < total then exit 1
