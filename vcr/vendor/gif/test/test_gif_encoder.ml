(** Comprehensive test suite for GIF encoder *)

open Alcotest

(* Test helpers *)
let create_test_palette size =
  Array.init size (fun i ->
      let gray = if size = 1 then 0 else i * 255 / (size - 1) in
      Gif.rgb gray gray gray)

let create_solid_frame ~width ~height ~color_idx =
  let pixels = Bytes.create (width * height) in
  Bytes.fill pixels 0 (width * height) (Char.chr color_idx);
  {
    Gif.width;
    height;
    x_offset = 0;
    y_offset = 0;
    delay_cs = 10;
    disposal = Gif.No_disposal;
    transparent_index = None;
    pixels;
    local_palette = None;
  }

(* Palette tests *)
let test_palette_creation () =
  (* Valid palettes *)
  let test_valid size =
    let colors = create_test_palette size in
    match Gif.palette_of_array colors with
    | Ok p ->
        check int "palette size" size (Gif.palette_size p);
        (* Check we can get colors back *)
        for i = 0 to size - 1 do
          match Gif.palette_get p i with
          | Some c -> check int (Printf.sprintf "color %d" i) colors.(i).r c.r
          | None -> fail (Printf.sprintf "Missing color at index %d" i)
        done
    | Error _ -> fail "Valid palette rejected"
  in

  test_valid 1;
  test_valid 16;
  test_valid 256;

  (* Invalid palettes *)
  check bool "empty palette" true
    (match Gif.palette_of_array [||] with
    | Error (Gif.Invalid_argument _) -> true
    | _ -> false);

  check bool "palette too large" true
    (match Gif.palette_of_array (Array.init 257 (fun _ -> Gif.rgb 0 0 0)) with
    | Error (Gif.Palette_too_large 257) -> true
    | _ -> false);

  check bool "invalid color" true
    (match Gif.palette_of_array [| Gif.rgb 256 0 0 |] with
    | Error (Gif.Color_out_of_range (256, 0, 0)) -> true
    | _ -> false)

(* Frame validation tests *)
let test_frame_validation () =
  let palette =
    match Gif.palette_of_array (create_test_palette 16) with
    | Ok p -> p
    | Error _ -> fail "Failed to create test palette"
  in

  (* Test frame validation through gif_create *)
  let valid_frame = create_solid_frame ~width:10 ~height:10 ~color_idx:0 in
  check bool "valid frame" true
    (match
       Gif.gif_create ~width:100 ~height:100 ~palette ~frames:[ valid_frame ] ()
     with
    | Ok _ -> true
    | Error _ -> false);

  (* Invalid delay *)
  let bad_delay = { valid_frame with delay_cs = -1 } in
  check bool "negative delay" true
    (match
       Gif.gif_create ~width:100 ~height:100 ~palette ~frames:[ bad_delay ] ()
     with
    | Error (Gif.Invalid_argument _) -> true
    | _ -> false);

  (* Frame exceeds canvas *)
  let oversized = { valid_frame with x_offset = 95 } in
  check bool "frame exceeds canvas" true
    (match
       Gif.gif_create ~width:100 ~height:100 ~palette ~frames:[ oversized ] ()
     with
    | Error (Gif.Invalid_argument _) -> true
    | _ -> false);

  (* Invalid pixel data length *)
  let wrong_pixels = { valid_frame with pixels = Bytes.create 50 } in
  check bool "wrong pixel data length" true
    (match
       Gif.gif_create ~width:100 ~height:100 ~palette ~frames:[ wrong_pixels ]
         ()
     with
    | Error (Gif.Invalid_argument _) -> true
    | _ -> false);

  (* Invalid transparent index *)
  let bad_trans = { valid_frame with transparent_index = Some 16 } in
  check bool "transparent index out of bounds" true
    (match
       Gif.gif_create ~width:100 ~height:100 ~palette ~frames:[ bad_trans ] ()
     with
    | Error (Gif.Invalid_palette_index _) -> true
    | _ -> false)

(* Basic encoding tests *)
let test_basic_encoding () =
  let palette =
    match Gif.palette_of_array (create_test_palette 4) with
    | Ok p -> p
    | Error _ -> fail "Failed to create test palette"
  in

  (* Single frame GIF *)
  let frame = create_solid_frame ~width:10 ~height:10 ~color_idx:1 in
  let gif =
    match Gif.gif_create ~width:10 ~height:10 ~palette ~frames:[ frame ] () with
    | Ok g -> g
    | Error _ -> fail "Failed to create GIF"
  in

  match Gif.encode gif with
  | Ok data ->
      (* Check header *)
      check string "GIF header" "GIF89a" (String.sub data 0 6);
      (* Check dimensions (little-endian) *)
      check int "width byte 0" 10 (Char.code data.[6]);
      check int "width byte 1" 0 (Char.code data.[7]);
      check int "height byte 0" 10 (Char.code data.[8]);
      check int "height byte 1" 0 (Char.code data.[9]);
      (* Check for trailer *)
      check char "trailer" '\x3B' data.[String.length data - 1]
  | Error _ -> fail "Encoding failed"

(* Round-trip tests *)
let test_round_trip () =
  (* First test a simple single-color frame *)
  let palette =
    match Gif.palette_of_array [| Gif.rgb 0 0 0; Gif.rgb 255 255 255 |] with
    | Ok p -> p
    | Error _ -> fail "Failed to create test palette"
  in

  (* Create a single frame with all zeros *)
  let frame = create_solid_frame ~width:10 ~height:10 ~color_idx:0 in

  let gif =
    match Gif.gif_create ~width:10 ~height:10 ~palette ~frames:[ frame ] () with
    | Ok g -> g
    | Error _ -> fail "Failed to create GIF"
  in

  (* Encode and decode *)
  match Gif.encode gif with
  | Error _ -> fail "Encoding failed"
  | Ok data -> (
      match Gif.decode data with
      | Error e ->
          fail
            (Printf.sprintf "Decoding failed: %s"
               (match e with Gif.Decode_error msg -> msg | _ -> "unknown"))
      | Ok decoded ->
          check int "width preserved" (Gif.gif_width gif)
            (Gif.gif_width decoded);
          check int "height preserved" (Gif.gif_height gif)
            (Gif.gif_height decoded);
          check int "frame count" 1 (List.length (Gif.gif_frames decoded));

          (* Check frame pixels *)
          let decoded_frame = List.hd (Gif.gif_frames decoded) in
          check int "frame width" frame.width decoded_frame.width;
          check int "frame height" frame.height decoded_frame.height;
          check bytes "pixels preserved" frame.pixels decoded_frame.pixels)

(* Color quantization tests *)
let test_quantization () =
  (* Test with various color counts *)
  let test_palette_size num_colors max_colors =
    let rgb_data =
      Array.init num_colors (fun i ->
          let v = i * 255 / (num_colors - 1) in
          (v, v, v))
    in

    match Gif.Quantize.create_palette rgb_data max_colors with
    | Ok p ->
        let size = Gif.palette_size p in
        check bool
          (Printf.sprintf "%d colors -> max %d" num_colors max_colors)
          true
          (size <= max_colors && size >= 1)
    | Error _ -> fail "Palette creation failed"
  in

  test_palette_size 10 16;
  test_palette_size 100 16;
  test_palette_size 256 256;

  (* Test nearest color finding *)
  let palette =
    match
      Gif.palette_of_array
        [| Gif.rgb 0 0 0; Gif.rgb 255 0 0; Gif.rgb 0 255 0; Gif.rgb 0 0 255 |]
    with
    | Ok p -> p
    | Error _ -> fail "Failed to create test palette"
  in

  check int "nearest to black" 0
    (Gif.Quantize.find_nearest_color (0, 0, 0) palette);
  check int "nearest to red" 1
    (Gif.Quantize.find_nearest_color (255, 0, 0) palette);
  check int "nearest to dark red" 1
    (Gif.Quantize.find_nearest_color (128, 0, 0) palette)

(* Dithering tests *)
let test_dithering () =
  let palette =
    match Gif.palette_of_array [| Gif.rgb 0 0 0; Gif.rgb 255 255 255 |] with
    | Ok p -> p
    | Error _ -> fail "Failed to create test palette"
  in

  (* Test gradient with and without dithering *)
  let width = 10 in
  let rgb_data =
    Array.init 100 (fun i ->
        let gray = i * 255 / 99 in
        (gray, gray, gray))
  in

  (* No dithering *)
  match Gif.rgb_to_indexed ~width rgb_data palette with
  | Error _ -> fail "rgb_to_indexed failed"
  | Ok indexed -> (
      (* Check it's mostly split between black and white *)
      let blacks = ref 0 in
      for i = 0 to 49 do
        if Bytes.get indexed i = '\x00' then incr blacks
      done;
      check bool "no dither: first half mostly black" true (!blacks > 40);

      (* With Floyd-Steinberg *)
      match
        Gif.rgb_to_indexed ~dither:Gif.Floyd_steinberg ~width rgb_data palette
      with
      | Error _ -> fail "dithered rgb_to_indexed failed"
      | Ok dithered ->
          (* Should have more mixed pattern *)
          let transitions = ref 0 in
          for i = 1 to 99 do
            if Bytes.get dithered i <> Bytes.get dithered (i - 1) then
              incr transitions
          done;
          check bool "dithered has more transitions" true (!transitions > 10))

(* Edge case tests *)
let test_edge_cases () =
  (* 1x1 GIF *)
  let palette =
    match Gif.palette_of_array [| Gif.rgb 0 0 0 |] with
    | Ok p -> p
    | Error _ -> fail "Failed to create 1-color palette"
  in

  let tiny_frame = create_solid_frame ~width:1 ~height:1 ~color_idx:0 in
  let tiny_gif =
    match
      Gif.gif_create ~width:1 ~height:1 ~palette ~frames:[ tiny_frame ] ()
    with
    | Ok g -> g
    | Error _ -> fail "Failed to create 1x1 GIF"
  in

  check bool "1x1 GIF encodes" true
    (match Gif.encode tiny_gif with Ok _ -> true | Error _ -> false);

  (* Maximum dimensions *)
  let large_frame = create_solid_frame ~width:100 ~height:100 ~color_idx:0 in
  let large_gif =
    match
      Gif.gif_create ~width:65535 ~height:65535 ~palette ~frames:[ large_frame ]
        ()
    with
    | Ok g -> g
    | Error _ -> fail "Failed to create max-size GIF"
  in

  check bool "max-size GIF validates" true
    (match Gif.encode large_gif with Ok _ -> true | Error _ -> false);

  (* No frames - should fail *)
  check bool "no frames fails" true
    (match Gif.gif_create ~width:10 ~height:10 ~palette ~frames:[] () with
    | Error _ -> true
    | Ok _ -> false)

(* Disposal method tests *)
let test_disposal_methods () =
  let palette =
    match Gif.palette_of_array (create_test_palette 4) with
    | Ok p -> p
    | Error _ -> fail "Failed to create test palette"
  in

  let test_disposal disposal =
    let frame =
      { (create_solid_frame ~width:10 ~height:10 ~color_idx:0) with disposal }
    in

    match Gif.gif_create ~width:10 ~height:10 ~palette ~frames:[ frame ] () with
    | Error _ -> fail "Failed to create GIF with disposal"
    | Ok gif -> (
        match Gif.encode gif with
        | Error _ -> fail "Failed to encode"
        | Ok data -> (
            match Gif.decode data with
            | Error _ -> fail "Failed to decode"
            | Ok decoded ->
                let decoded_frame = List.hd (Gif.gif_frames decoded) in
                check bool "disposal preserved" true
                  (decoded_frame.disposal = disposal)))
  in

  test_disposal Gif.No_disposal;
  test_disposal Gif.Do_not_dispose;
  test_disposal Gif.Restore_background;
  test_disposal Gif.Restore_previous

(* Test suite *)
let () =
  run "GIF encoder tests"
    [
      ("palette", [ test_case "palette creation" `Quick test_palette_creation ]);
      ( "validation",
        [ test_case "frame validation" `Quick test_frame_validation ] );
      ( "encoding",
        [
          test_case "basic encoding" `Quick test_basic_encoding;
          test_case "round trip" `Quick test_round_trip;
        ] );
      ( "quantization",
        [
          test_case "color quantization" `Quick test_quantization;
          test_case "dithering" `Quick test_dithering;
        ] );
      ( "edge cases",
        [
          test_case "edge cases" `Quick test_edge_cases;
          test_case "disposal methods" `Quick test_disposal_methods;
        ] );
    ]
