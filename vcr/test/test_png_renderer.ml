let test_png_render () =
  let rows = 10 in
  let cols = 40 in
  let vte = Vte.create ~rows ~cols () in

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
        output_dir = "/tmp/vcr_test_png";
        (* Temporary directory for testing *)
      }
  in

  (* Create renderer *)
  let renderer = Renderer.Png_renderer.create ~rows ~cols config in

  (* Create a frame from the VTE state *)
  let grid = Vte.grid vte in
  let cursor_row, cursor_col = Vte.cursor_pos vte in
  let frame =
    Renderer.Frame.create ~grid ~cursor_row ~cursor_col ~cursor_visible:true
      ~delay_cs:0 ~dirty_regions:[] ~cursor_moved:false
  in

  (* Write frame using writer *)
  let output_buffer = Buffer.create 1024 in
  let writer bytes offset length =
    Buffer.add_subbytes output_buffer bytes offset length
  in

  Renderer.Png_renderer.write_frame renderer frame ~incremental:false ~writer;

  (* Get the PNG data *)
  let png_data = Buffer.contents output_buffer in

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
