let test_png_render () =
  let vte = Vte.create ~rows:10 ~cols:40 () in

  (* Add some test content *)
  let test_text = "Hello, PNG!" in
  Vte.feed vte (Bytes.of_string test_text) 0 (String.length test_text);

  (* Create PNG renderer config *)
  let config =
    Renderer.Png_renderer.
      {
        char_width = 8;
        char_height = 16;
        theme = Renderer.Gif_renderer.default_theme;
        font_path = None;
        font_size = 14;
        target_width = None;
        target_height = None;
        padding = 10;
        cursor_style = `Block;
        cursor_color = None;
      }
  in

  (* Create renderer and capture frame *)
  let renderer = Renderer.Png_renderer.create vte config in
  Renderer.Png_renderer.capture_frame renderer;

  (* Render to PNG *)
  let png_data = Renderer.Png_renderer.render renderer in

  (* Basic validation *)
  if String.length png_data > 0 then print_endline "✓ PNG data generated"
  else print_endline "✗ No PNG data generated";

  if String.length png_data > 4 && String.sub png_data 1 3 = "PNG" then
    print_endline "✓ Valid PNG signature"
  else print_endline "✗ Invalid PNG signature";

  (* Write to file for manual inspection *)
  let oc = open_out_bin "test_output.png" in
  output_string oc png_data;
  close_out oc;
  print_endline "✓ PNG written to test_output.png"

let () = test_png_render ()
