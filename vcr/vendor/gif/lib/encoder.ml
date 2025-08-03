(** GIF encoder implementation *)

open Types

(** Helper functions *)

let pack_u16 buf value =
  if value < 0 || value > 65535 then
    invalid_arg (Printf.sprintf "Value %d out of range for 16-bit" value);
  Buffer.add_char buf (Char.chr (value land 0xFF));
  Buffer.add_char buf (Char.chr ((value lsr 8) land 0xFF))

let write_byte writer b =
  let buf = Bytes.create 1 in
  Bytes.set buf 0 b;
  writer buf 0 1

let write_string writer s =
  writer (Bytes.unsafe_of_string s) 0 (String.length s)

let write_u16 writer value =
  let buf = Bytes.create 2 in
  Bytes.set buf 0 (Char.chr (value land 0xFF));
  Bytes.set buf 1 (Char.chr ((value lsr 8) land 0xFF));
  writer buf 0 2

let write_color_table buf colors required_size =
  Array.iter
    (fun c ->
      Buffer.add_char buf (Char.chr c.r);
      Buffer.add_char buf (Char.chr c.g);
      Buffer.add_char buf (Char.chr c.b))
    colors;
  (* Pad to required size *)
  for _ = Array.length colors to required_size - 1 do
    Buffer.add_string buf "\000\000\000"
  done

let write_color_table_writer writer colors required_size =
  let buf = Bytes.create (required_size * 3) in
  let pos = ref 0 in
  Array.iter
    (fun c ->
      Bytes.set buf !pos (Char.chr c.r);
      Bytes.set buf (!pos + 1) (Char.chr c.g);
      Bytes.set buf (!pos + 2) (Char.chr c.b);
      pos := !pos + 3)
    colors;
  (* Pad to required size *)
  while !pos < required_size * 3 do
    Bytes.set buf !pos '\000';
    incr pos
  done;
  writer buf 0 (required_size * 3)

let color_bits count =
  let rec loop n b = if n >= count then b else loop (n * 2) (b + 1) in
  max 1 (loop 2 1)

(** Validate frame parameters *)
let validate_frame ~encoder_width ~encoder_height ~global_palette frame =
  if frame.delay_cs < 0 || frame.delay_cs > 65535 then
    Error
      (Invalid_argument
         (Printf.sprintf "Delay %d out of range (0-65535)" frame.delay_cs))
  else if frame.x_offset < 0 || frame.y_offset < 0 then
    Error
      (Invalid_argument
         (Printf.sprintf "Frame position (%d,%d) cannot be negative"
            frame.x_offset frame.y_offset))
  else if
    frame.x_offset + frame.width > encoder_width
    || frame.y_offset + frame.height > encoder_height
  then
    Error
      (Invalid_argument
         (Printf.sprintf
            "Frame at (%d,%d) with size %dx%d exceeds canvas size %dx%d"
            frame.x_offset frame.y_offset frame.width frame.height encoder_width
            encoder_height))
  else if Bytes.length frame.pixels <> frame.width * frame.height then
    Error
      (Invalid_argument
         (Printf.sprintf
            "Pixel data length %d does not match frame dimensions %dx%d \
             (expected %d)"
            (Bytes.length frame.pixels)
            frame.width frame.height
            (frame.width * frame.height)))
  else
    (* Validate transparent index is within palette bounds *)
    let palette_len =
      match frame.local_palette with
      | Some p -> Palette.size p
      | None -> Palette.size global_palette
    in
    match frame.transparent_index with
    | Some idx when idx < 0 || idx >= palette_len ->
        Error (Invalid_palette_index idx)
    | _ -> Ok ()

(** Write GIF header and initial metadata to a buffer *)
let write_gif_header buf gif =
  (* Write header *)
  Buffer.add_bytes buf gif_header;

  (* Write logical screen descriptor *)
  pack_u16 buf gif.width;
  pack_u16 buf gif.height;

  let bits = color_bits (Palette.size gif.global_palette) in
  let table_size = 1 lsl bits in
  let packed_field =
    0x80
    lor
    (* Global color table flag *)
    ((bits - 1) lsl 4)
    lor
    (* Color resolution *)
    (bits - 1)
    (* Global color table size *)
  in
  Buffer.add_char buf (Char.chr packed_field);
  Buffer.add_char buf (Char.chr gif.background_index);
  Buffer.add_char buf '\x00';

  (* Pixel aspect ratio *)

  (* Write global color table with padding *)
  write_color_table buf (Palette.to_array gif.global_palette) table_size;

  (* Add NETSCAPE2.0 extension for looping if specified *)
  match gif.loop_count with
  | None -> ()
  | Some loop_count ->
      Buffer.add_char buf extension_introducer;
      Buffer.add_char buf application_extension_label;
      Buffer.add_char buf '\x0B';
      (* Block size *)
      Buffer.add_bytes buf (Bytes.unsafe_of_string "NETSCAPE2.0");
      Buffer.add_char buf '\x03';
      (* Sub-block size *)
      Buffer.add_char buf '\x01';
      (* Sub-block ID *)
      pack_u16 buf loop_count;
      (* Loop count: 0 = forever *)
      Buffer.add_char buf '\x00' (* Block terminator *)

(** Write a frame to the buffer *)
let write_frame buf frame global_palette =
  (* Graphics Control Extension *)
  Buffer.add_char buf extension_introducer;
  Buffer.add_char buf graphic_control_label;
  Buffer.add_char buf '\x04';

  (* Block size *)
  let disposal_code =
    match frame.disposal with
    | No_disposal -> 0
    | Do_not_dispose -> 1
    | Restore_background -> 2
    | Restore_previous -> 3
  in
  let has_transparent = frame.transparent_index <> None in
  let packed_field =
    (disposal_code lsl 2) lor if has_transparent then 1 else 0
  in
  Buffer.add_char buf (Char.chr packed_field);
  pack_u16 buf frame.delay_cs;
  (* Ensure transparent index is 0 when flag is clear *)
  let trans_byte =
    if has_transparent then Option.get frame.transparent_index else 0
  in
  Buffer.add_char buf (Char.chr trans_byte);
  Buffer.add_char buf '\x00';

  (* Block terminator *)

  (* Image Descriptor *)
  Buffer.add_char buf image_separator;
  pack_u16 buf frame.x_offset;
  (* Left position *)
  pack_u16 buf frame.y_offset;
  (* Top position *)
  pack_u16 buf frame.width;
  pack_u16 buf frame.height;

  (* Handle local color table if present *)
  let _palette, palette_bits =
    match frame.local_palette with
    | Some local_table ->
        let bits = color_bits (Palette.size local_table) in
        let table_size = 1 lsl bits in
        let packed = 0x80 lor ((bits - 1) land 0x07) in
        (* Local color table flag + size, ensure reserved bits are zero *)
        Buffer.add_char buf (Char.chr packed);
        write_color_table buf (Palette.to_array local_table) table_size;
        (local_table, bits)
    | None ->
        Buffer.add_char buf '\x00';
        (* No local color table *)
        (global_palette, color_bits (Palette.size global_palette))
  in

  (* Compress and write image data *)
  let min_code_size = max 2 palette_bits in
  Buffer.add_char buf (Char.chr min_code_size);
  let compressed = Lzw.Encoder.compress frame.pixels palette_bits in
  let packaged = Lzw.Encoder.package_subblocks compressed in
  Buffer.add_string buf packaged

(** Encode a GIF data structure to binary format *)
let encode gif =
  try
    let buf = Buffer.create 1024 in

    (* Write header and metadata *)
    write_gif_header buf gif;

    (* Write all frames *)
    List.iter (fun frame -> write_frame buf frame gif.global_palette) gif.frames;

    (* Write trailer *)
    Buffer.add_char buf trailer;

    Ok (Buffer.contents buf)
  with exn -> Error (Io_error exn)

(** Write GIF header with streaming writer *)
let write_gif_header_streaming writer gif =
  (* Write header *)
  write_string writer "GIF89a";

  (* Write logical screen descriptor *)
  write_u16 writer gif.width;
  write_u16 writer gif.height;

  let bits = color_bits (Palette.size gif.global_palette) in
  let table_size = 1 lsl bits in
  let packed_field = 0x80 lor ((bits - 1) lsl 4) lor (bits - 1) in
  write_byte writer (Char.chr packed_field);
  write_byte writer (Char.chr gif.background_index);
  write_byte writer '\x00';

  (* Pixel aspect ratio *)

  (* Write global color table with padding *)
  write_color_table_writer writer
    (Palette.to_array gif.global_palette)
    table_size;

  (* Add NETSCAPE2.0 extension for looping if specified *)
  match gif.loop_count with
  | None -> ()
  | Some loop_count ->
      write_byte writer extension_introducer;
      write_byte writer application_extension_label;
      write_byte writer '\x0B';
      (* Block size *)
      write_string writer "NETSCAPE2.0";
      write_byte writer '\x03';
      (* Sub-block size *)
      write_byte writer '\x01';
      (* Sub-block ID *)
      write_u16 writer loop_count;
      (* Loop count: 0 = forever *)
      write_byte writer '\x00' (* Block terminator *)

(** Write a frame with streaming writer *)
let write_frame_streaming writer frame global_palette =
  (* Graphics Control Extension *)
  write_byte writer extension_introducer;
  write_byte writer graphic_control_label;
  write_byte writer '\x04';

  (* Block size *)
  let disposal_code =
    match frame.disposal with
    | No_disposal -> 0
    | Do_not_dispose -> 1
    | Restore_background -> 2
    | Restore_previous -> 3
  in
  let has_transparent = frame.transparent_index <> None in
  let packed_field =
    (disposal_code lsl 2) lor if has_transparent then 1 else 0
  in
  write_byte writer (Char.chr packed_field);
  write_u16 writer frame.delay_cs;
  let trans_byte =
    if has_transparent then Option.get frame.transparent_index else 0
  in
  write_byte writer (Char.chr trans_byte);
  write_byte writer '\x00';

  (* Block terminator *)

  (* Image Descriptor *)
  write_byte writer image_separator;
  write_u16 writer frame.x_offset;
  write_u16 writer frame.y_offset;
  write_u16 writer frame.width;
  write_u16 writer frame.height;

  (* Handle local color table if present *)
  let _palette, palette_bits =
    match frame.local_palette with
    | Some local_table ->
        let bits = color_bits (Palette.size local_table) in
        let table_size = 1 lsl bits in
        let packed = 0x80 lor ((bits - 1) land 0x07) in
        write_byte writer (Char.chr packed);
        write_color_table_writer writer
          (Palette.to_array local_table)
          table_size;
        (local_table, bits)
    | None ->
        write_byte writer '\x00';
        (* No local color table *)
        (global_palette, color_bits (Palette.size global_palette))
  in

  (* Compress and write image data *)
  let min_code_size = max 2 palette_bits in
  write_byte writer (Char.chr min_code_size);
  let compressed = Lzw.Encoder.compress frame.pixels palette_bits in
  let packaged = Lzw.Encoder.package_subblocks compressed in
  write_string writer packaged

(** Encode a GIF directly to a writer function *)
let encode_streaming gif ~writer =
  try
    write_gif_header_streaming writer gif;

    (* Write all frames *)
    List.iter
      (fun frame -> write_frame_streaming writer frame gif.global_palette)
      gif.frames;

    (* Write trailer *)
    write_byte writer trailer;

    Ok ()
  with exn -> Error (Io_error exn)

type streaming_encoder = {
  width : int;
  height : int;
  global_palette : palette;
  background_index : int;
  loop_count : int option;
  mutable header_written : bool;
}
(** Streaming encoder state *)

(** Create a new streaming encoder *)
let create_streaming_encoder ~width ~height ~palette ?(background_index = 0)
    ?loop_count () =
  if width <= 0 || width > 65535 then Error (Invalid_dimensions (width, height))
  else if height <= 0 || height > 65535 then
    Error (Invalid_dimensions (width, height))
  else if background_index < 0 || background_index >= Palette.size palette then
    Error (Invalid_palette_index background_index)
  else
    Ok
      {
        width;
        height;
        global_palette = palette;
        background_index;
        loop_count;
        header_written = false;
      }

(** Write a frame using the streaming encoder *)
let write_frame_streaming encoder frame ~writer =
  (* Write header on first frame *)
  if not encoder.header_written then (
    let header_gif =
      {
        width = encoder.width;
        height = encoder.height;
        global_palette = encoder.global_palette;
        background_index = encoder.background_index;
        loop_count = encoder.loop_count;
        frames = [];
        (* Not used for header *)
      }
    in
    write_gif_header_streaming writer header_gif;
    encoder.header_written <- true);

  (* Validate frame *)
  match
    validate_frame ~encoder_width:encoder.width ~encoder_height:encoder.height
      ~global_palette:encoder.global_palette frame
  with
  | Error e -> Error e
  | Ok () -> (
      try
        write_frame_streaming writer frame encoder.global_palette;
        Ok ()
      with exn -> Error (Io_error exn))

(** Finalize the streaming encoder *)
let finalize_streaming_encoder encoder ~writer =
  if not encoder.header_written then
    Error (Invalid_argument "No frames written to streaming encoder")
  else
    try
      write_byte writer trailer;
      Ok ()
    with exn -> Error (Io_error exn)
