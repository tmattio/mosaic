open Gif

let read_file filename =
  let ic = open_in_bin filename in
  let len = in_channel_length ic in
  let data = really_input_string ic len in
  close_in ic;
  data

let write_file filename data =
  let oc = open_out_bin filename in
  output_string oc data;
  close_out oc

let test_decode_encode_cycle () =
  (* Read the test GIF from our source directory *)
  let test_file =
    try
      (* Try from test directory first *)
      let _ = Unix.access "test_image.gif" [ Unix.R_OK ] in
      "test_image.gif"
    with Unix.Unix_error _ ->
      (* Try from project root *)
      "vcr/vendor/gif/test/test_image.gif"
  in

  try
    let input_data = read_file test_file in
    Printf.printf "Read test GIF: %d bytes\n" (String.length input_data);

    (* Decode the GIF *)
    match decode input_data with
    | Error e ->
        Printf.printf "FAIL: Decode error: %s\n"
          (match e with
          | Decode_error msg -> msg
          | Invalid_argument msg -> Printf.sprintf "Invalid argument: %s" msg
          | Palette_too_large n -> Printf.sprintf "Palette too large: %d" n
          | Color_out_of_range (r, g, b) ->
              Printf.sprintf "Color out of range: (%d,%d,%d)" r g b
          | Invalid_dimensions (w, h) ->
              Printf.sprintf "Invalid dimensions: %dx%d" w h
          | Invalid_palette_index idx ->
              Printf.sprintf "Invalid palette index: %d" idx
          | Io_error exn ->
              Printf.sprintf "IO error: %s" (Printexc.to_string exn));
        false
    | Ok gif -> (
        Printf.printf "Successfully decoded GIF\n";
        let frames = gif_frames gif in
        Printf.printf "  Frames: %d\n" (List.length frames);
        Printf.printf "  Dimensions: %dx%d\n" (gif_width gif) (gif_height gif);

        (* Extract palette *)
        let palette = gif_global_palette gif in
        Printf.printf "  Palette size: %d colors\n" (palette_size palette);

        (* Re-encode the GIF *)
        match
          gif_create ~width:(gif_width gif) ~height:(gif_height gif) ~palette
            ~background_index:(gif_background_index gif)
            ?loop_count:(gif_loop_count gif) ~frames ()
        with
        | Error e ->
            Printf.printf "FAIL: Could not create GIF for encoding: %s\n"
              (match e with
              | Palette_too_large n -> Printf.sprintf "Palette too large: %d" n
              | Invalid_dimensions (w, h) ->
                  Printf.sprintf "Invalid dimensions: %dx%d" w h
              | Invalid_palette_index idx ->
                  Printf.sprintf "Invalid palette index: %d" idx
              | _ -> "Unknown error");
            false
        | Ok new_gif -> (
            match encode new_gif with
            | Error e ->
                Printf.printf "FAIL: Encode error: %s\n"
                  (match e with
                  | Io_error exn -> Printexc.to_string exn
                  | _ -> "Unknown error");
                false
            | Ok output_data -> (
                write_file "test_output.gif" output_data;
                Printf.printf "Successfully re-encoded GIF\n";
                Printf.printf "  Input size: %d bytes\n"
                  (String.length input_data);
                Printf.printf "  Output size: %d bytes\n"
                  (String.length output_data);

                (* The files might not be byte-identical due to different encoding choices,
                        but we can verify the output is a valid GIF with the same properties *)
                match decode output_data with
                | Error _ ->
                    Printf.printf "FAIL: Could not decode re-encoded GIF\n";
                    false
                | Ok gif2 ->
                    let frames2 = gif_frames gif2 in
                    let same_frame_count =
                      List.length frames = List.length frames2
                    in
                    let same_dimensions =
                      gif_width gif = gif_width gif2
                      && gif_height gif = gif_height gif2
                    in

                    if same_frame_count && same_dimensions then
                      (* Compare frame content *)
                      let content_matches =
                        try
                          List.for_all2
                            (fun f1 f2 ->
                              (* Compare frame properties *)
                              f1.width = f2.width && f1.height = f2.height
                              && f1.x_offset = f2.x_offset
                              && f1.y_offset = f2.y_offset
                              && f1.disposal = f2.disposal
                              && f1.transparent_index = f2.transparent_index
                              &&
                              (* Compare pixel data *)
                              Bytes.equal f1.pixels f2.pixels)
                            frames frames2
                        with Invalid_argument _ -> false
                      in

                      if content_matches then (
                        Printf.printf "PASS: Round-trip successful!\n";
                        Printf.printf
                          "  Both GIFs have %d frames and %dx%d dimensions\n"
                          (List.length frames) (gif_width gif) (gif_height gif);
                        Printf.printf "  All frame content matches exactly\n";

                        (* Clean up *)
                        Unix.unlink "test_output.gif";
                        true)
                      else (
                        Printf.printf
                          "FAIL: Frame content differs after round-trip\n";

                        (* Find first difference *)
                        (try
                           List.iteri
                             (fun i f1 ->
                               let f2 = List.nth frames2 i in
                               if not (Bytes.equal f1.pixels f2.pixels) then (
                                 Printf.printf
                                   "  Frame %d has different pixel data\n" i;
                                 (* Find first differing pixel *)
                                 for j = 0 to Bytes.length f1.pixels - 1 do
                                   if
                                     Bytes.get f1.pixels j
                                     <> Bytes.get f2.pixels j
                                   then (
                                     Printf.printf
                                       "    First difference at pixel %d: %d \
                                        vs %d\n"
                                       j
                                       (Char.code (Bytes.get f1.pixels j))
                                       (Char.code (Bytes.get f2.pixels j));
                                     raise Exit)
                                 done))
                             frames
                         with Exit -> ());
                        false)
                    else (
                      Printf.printf "FAIL: Round-trip produced different GIF\n";
                      Printf.printf "  Frame count: %d -> %d\n"
                        (List.length frames) (List.length frames2);
                      Printf.printf "  Dimensions: %dx%d -> %dx%d\n"
                        (gif_width gif) (gif_height gif) (gif_width gif2)
                        (gif_height gif2);
                      false))))
  with
  | Sys_error msg ->
      Printf.printf "FAIL: Could not read test file: %s\n" msg;
      false
  | exn ->
      Printf.printf "FAIL: Unexpected error: %s\n" (Printexc.to_string exn);
      false

let () =
  Printf.printf "Running GIF integration test...\n";
  if test_decode_encode_cycle () then exit 0 else exit 1
