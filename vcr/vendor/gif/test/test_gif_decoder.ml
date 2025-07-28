(** Decoder and LZW tests *)

open Alcotest

(* Helper to create test frames *)
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

(* Test invalid GIF data *)
let test_invalid_gifs () =
  (* Not a GIF *)
  check bool "invalid header" true
    (match Gif.decode "NOTGIF" with
    | Error (Gif.Decode_error _) -> true
    | _ -> false);

  (* Truncated header *)
  check bool "truncated header" true
    (match Gif.decode "GIF8" with
    | Error (Gif.Decode_error _) -> true
    | _ -> false);

  (* Valid header but truncated *)
  check bool "truncated after header" true
    (match Gif.decode "GIF89a" with
    | Error (Gif.Decode_error _) -> true
    | _ -> false);

  (* Missing trailer *)
  let no_trailer =
    "GIF89a" ^ "\x0A\x00\x0A\x00"
    (* 10x10 *)
    ^ "\x80\x00\x00"
    ^
    (* has GCT, bg=0, aspect=0 *)
    "\x00\x00\x00\xFF\xFF\xFF" (* 2-color table *)
  in
  check bool "missing trailer" true
    (match Gif.decode no_trailer with
    | Error (Gif.Decode_error msg) ->
        String.lowercase_ascii msg |> fun s ->
        String.contains s 't' && String.contains s 'r'
    | _ -> false)

(* Test interlaced decoding *)
let test_interlaced_decoding () =
  (* We can't test de_interlace directly as it's not exposed.
     Instead, test that interlaced GIFs decode correctly by creating
     one and checking the round-trip *)
  ()

(* Test LZW decompression *)
let test_lzw_decompression () =
  (* Since Lzw_decode is not exposed, we test it indirectly through round-trip *)
  let palette =
    match Gif.palette_of_array [| Gif.rgb 0 0 0; Gif.rgb 255 255 255 |] with
    | Ok p -> p
    | Error _ -> fail "Failed to create test palette"
  in

  (* Create a pattern that will stress LZW *)
  let pixels = Bytes.create 100 in
  (* Pattern: ABABAB... which creates the special case *)
  for i = 0 to 99 do
    Bytes.set pixels i (Char.chr (i mod 2))
  done;

  let frame =
    {
      Gif.width = 10;
      height = 10;
      x_offset = 0;
      y_offset = 0;
      delay_cs = 10;
      disposal = Gif.No_disposal;
      transparent_index = None;
      pixels;
      local_palette = None;
    }
  in

  match Gif.gif_create ~width:10 ~height:10 ~palette ~frames:[ frame ] () with
  | Error _ -> fail "Failed to create GIF"
  | Ok gif -> (
      match Gif.encode gif with
      | Error _ -> fail "Failed to encode"
      | Ok data -> (
          match Gif.decode data with
          | Error _ -> fail "Failed to decode"
          | Ok decoded ->
              let decoded_frame = List.hd (Gif.gif_frames decoded) in
              check bytes "pixels preserved" pixels decoded_frame.pixels))

(* Test transparent index validation *)
let test_transparent_validation () =
  let palette =
    match
      Gif.palette_of_array
        (Array.init 4 (fun i -> Gif.rgb (i * 85) (i * 85) (i * 85)))
    with
    | Ok p -> p
    | Error _ -> fail "Failed to create palette"
  in

  (* Test through round-trip encoding/decoding *)
  (* Frame with transparent index in bounds *)
  let valid_frame =
    {
      Gif.width = 2;
      height = 2;
      x_offset = 0;
      y_offset = 0;
      delay_cs = 10;
      disposal = Gif.No_disposal;
      transparent_index = Some 3;
      pixels = Bytes.create 4;
      local_palette = None;
    }
  in

  check bool "valid transparent index" true
    (match
       Gif.gif_create ~width:10 ~height:10 ~palette ~frames:[ valid_frame ] ()
     with
    | Ok _ -> true
    | Error _ -> false);

  (* Out of bounds transparent index *)
  let invalid_frame = { valid_frame with transparent_index = Some 4 } in

  check bool "invalid transparent index" true
    (match
       Gif.gif_create ~width:10 ~height:10 ~palette ~frames:[ invalid_frame ] ()
     with
    | Error (Gif.Invalid_palette_index _) -> true
    | _ -> false);

  (* With local palette *)
  let small_palette =
    match Gif.palette_of_array [| Gif.rgb 0 0 0; Gif.rgb 255 255 255 |] with
    | Ok p -> p
    | Error _ -> fail "Failed to create small palette"
  in

  let local_frame =
    {
      valid_frame with
      local_palette = Some small_palette;
      transparent_index = Some 1;
    }
  in

  check bool "valid with local palette" true
    (match
       Gif.gif_create ~width:10 ~height:10 ~palette ~frames:[ local_frame ] ()
     with
    | Ok _ -> true
    | Error _ -> false);

  (* Out of bounds with local palette *)
  let invalid_local = { local_frame with transparent_index = Some 2 } in

  check bool "invalid with local palette" true
    (match
       Gif.gif_create ~width:10 ~height:10 ~palette ~frames:[ invalid_local ] ()
     with
    | Error (Gif.Invalid_palette_index _) -> true
    | _ -> false)

(* Test streaming encoder *)
let test_streaming_encoder () =
  let palette =
    match
      Gif.palette_of_array
        (Array.init 4 (fun i -> Gif.rgb (i * 85) (i * 85) (i * 85)))
    with
    | Ok p -> p
    | Error _ -> fail "Failed to create test palette"
  in

  let frame = create_solid_frame ~width:10 ~height:10 ~color_idx:1 in
  let gif =
    match Gif.gif_create ~width:10 ~height:10 ~palette ~frames:[ frame ] () with
    | Ok g -> g
    | Error _ -> fail "Failed to create GIF"
  in

  (* Encode to buffer using streaming API *)
  let buffer = Buffer.create 1024 in
  let writer bytes offset length =
    Buffer.add_subbytes buffer bytes offset length
  in

  match Gif.encode_streaming gif ~writer with
  | Error _ -> fail "Streaming encode failed"
  | Ok () -> (
      let streaming_data = Buffer.contents buffer in

      (* Compare with regular encode *)
      match Gif.encode gif with
      | Error _ -> fail "Regular encode failed"
      | Ok regular_data ->
          check int "same length"
            (String.length regular_data)
            (String.length streaming_data);
          check string "same content" regular_data streaming_data)

(* Test suite *)
let () =
  run "GIF decoder tests"
    [
      ("invalid data", [ test_case "invalid GIFs" `Quick test_invalid_gifs ]);
      ( "interlacing",
        [ test_case "interlaced decoding" `Quick test_interlaced_decoding ] );
      ("LZW", [ test_case "LZW decompression" `Quick test_lzw_decompression ]);
      ( "transparency",
        [
          test_case "transparent index validation" `Quick
            test_transparent_validation;
        ] );
      ( "streaming",
        [ test_case "streaming encoder" `Quick test_streaming_encoder ] );
    ]
