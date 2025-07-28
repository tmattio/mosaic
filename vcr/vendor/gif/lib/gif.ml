(* GIF encoder library for creating animated GIFs *)

(* Errors that may be returned by any function in the library. *)
type gif_error =
  | Palette_too_large of int
  | Color_out_of_range of int * int * int
  | Invalid_dimensions of int * int
  | Invalid_palette_index of int
  | Io_error of exn
  | Decode_error of string
  | Invalid_argument of string

(* GIF format constants *)
let gif_header = Bytes.unsafe_of_string "GIF89a"
let trailer = '\x3B'
let image_separator = '\x2C'
let extension_introducer = '\x21'
let graphic_control_label = '\xF9'
let application_extension_label = '\xFF'

type color = { r : int; g : int; b : int }
(* Color type *)

(* Helper to create colors *)
let rgb r g b = { r; g; b }

type palette = color array
(* Opaque palette type - internally just a validated color array *)

(* Validate a color *)
let validate_color c =
  c.r >= 0 && c.r <= 255 && c.g >= 0 && c.g <= 255 && c.b >= 0 && c.b <= 255

(* Create a palette from a color array *)
let palette_of_array colors =
  let len = Array.length colors in
  if len = 0 then Error (Invalid_argument "Palette cannot be empty")
  else if len > 256 then Error (Palette_too_large len)
  else
    let rec check_colors i =
      if i >= Array.length colors then Ok (Array.copy colors)
      else
        let c = colors.(i) in
        if not (validate_color c) then
          Error (Color_out_of_range (c.r, c.g, c.b))
        else check_colors (i + 1)
    in
    check_colors 0

(* Get a copy of palette colors *)
let palette_to_array p = Array.copy p

(* Get palette size *)
let palette_size p = Array.length p

(* Get color at index *)
let palette_get p idx =
  if idx >= 0 && idx < Array.length p then Some p.(idx) else None

(* Frame disposal method *)
type disposal_method =
  | No_disposal (* No disposal specified *)
  | Do_not_dispose (* Leave frame in place *)
  | Restore_background (* Restore to background color *)
  | Restore_previous (* Restore to previous frame *)

type centiseconds = int
(* Time duration in centiseconds (1/100th of a second) *)

type frame = {
  width : int;
  height : int;
  x_offset : int; (* X position of frame *)
  y_offset : int; (* Y position of frame *)
  delay_cs : centiseconds; (* Delay in centiseconds *)
  disposal : disposal_method;
  transparent_index : int option;
  pixels : bytes; (* Indexed pixel data *)
  local_palette : palette option; (* Optional per-frame palette *)
}
(* A single frame in the animation *)

type t = {
  width : int;
  height : int;
  global_palette : palette;
  background_index : int;
  loop_count : int option;
  frames : frame list;
}
(* GIF data structure *)

(* Helper functions for accessing GIF data structure *)
let gif_width gif = gif.width
let gif_height gif = gif.height
let gif_frames gif = gif.frames
let gif_global_palette gif = gif.global_palette
let gif_background_index gif = gif.background_index
let gif_loop_count gif = gif.loop_count

(* Validate frame parameters *)
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
      | Some p -> palette_size p
      | None -> palette_size global_palette
    in
    match frame.transparent_index with
    | Some idx when idx < 0 || idx >= palette_len ->
        Error (Invalid_palette_index idx)
    | _ -> Ok ()

(* Create a GIF data structure *)
let gif_create ~width ~height ~palette ?(background_index = 0) ?loop_count
    ~frames () =
  if width <= 0 || width > 65535 then Error (Invalid_dimensions (width, height))
  else if height <= 0 || height > 65535 then
    Error (Invalid_dimensions (width, height))
  else if background_index < 0 || background_index >= palette_size palette then
    Error (Invalid_palette_index background_index)
  else if frames = [] then
    Error (Invalid_argument "GIF must have at least one frame")
  else
    (* Validate all frames *)
    let rec validate_frames = function
      | [] -> Ok ()
      | frame :: rest -> (
          match
            validate_frame ~encoder_width:width ~encoder_height:height
              ~global_palette:palette frame
          with
          | Error _ as e -> e
          | Ok () -> validate_frames rest)
    in
    match validate_frames frames with
    | Error _ as e -> e
    | Ok () ->
        Ok
          {
            width;
            height;
            global_palette = palette;
            background_index;
            loop_count;
            frames;
          }

(* Pack a 16-bit value into 2 bytes, little-endian *)
let pack_u16 buf value =
  if value < 0 || value > 65535 then
    invalid_arg (Printf.sprintf "Value %d out of range for 16-bit" value);
  Buffer.add_char buf (Char.chr (value land 0xFF));
  Buffer.add_char buf (Char.chr ((value lsr 8) land 0xFF))

(* Write a byte using writer function *)
let write_byte writer b =
  let buf = Bytes.create 1 in
  Bytes.set buf 0 b;
  writer buf 0 1

(* Write a string using writer function *)
let write_string writer s =
  writer (Bytes.unsafe_of_string s) 0 (String.length s)

(* Write a 16-bit value using writer function *)
let write_u16 writer value =
  let buf = Bytes.create 2 in
  Bytes.set buf 0 (Char.chr (value land 0xFF));
  Bytes.set buf 1 (Char.chr ((value lsr 8) land 0xFF));
  writer buf 0 2

(* Write a color table to the buffer with proper padding *)
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

(* Write a color table using writer function with proper padding *)
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

(* Calculate the color resolution bits needed for a color table *)
let color_bits count =
  let rec loop n b = if n >= count then b else loop (n * 2) (b + 1) in
  max 1 (loop 2 1)

(* Validate encoder parameters *)
let _validate_encoder_params ~width ~height ~palette ~background_index =
  if width <= 0 || width > 65535 then Error (Invalid_dimensions (width, height))
  else if height <= 0 || height > 65535 then
    Error (Invalid_dimensions (width, height))
  else if background_index < 0 || background_index >= palette_size palette then
    Error (Invalid_palette_index background_index)
  else Ok ()

(* LZW compression for GIF *)
module Lzw = struct
  type entry = {
    parent : int; (* Parent code, -1 for roots *)
    suffix : char; (* Suffix byte *)
  }
  [@@warning "-69"]
  (* Code table entry Note: Currently unused in encoder-only implementation but
      kept for future decoder support. The parent/suffix fields would be used to
      reconstruct strings when decoding. *)
  (* Suppress unused field warnings *)

  type code_table = {
    entries : entry array; (* Kept for future decoder implementation *)
    mutable next_code : int;
    children : (int * char, int) Hashtbl.t; (* (parent, suffix) -> code *)
  }
  (* Fast trie-based code table *)

  let create_table min_bits =
    let clear = 1 lsl min_bits in
    let entries = Array.make 4096 { parent = -1; suffix = '\000' } in
    let children = Hashtbl.create 4096 in
    (* Initialize single-byte codes *)
    for i = 0 to clear - 1 do
      entries.(i) <- { parent = -1; suffix = Char.chr i };
      Hashtbl.add children (-1, Char.chr i) i
    done;
    {
      entries;
      next_code = clear + 2;
      (* After clear and end codes *)
      children;
    }

  let find_child table parent suffix =
    Hashtbl.find_opt table.children (parent, suffix)

  let add_code table parent suffix =
    if table.next_code < 4096 then (
      table.entries.(table.next_code) <- { parent; suffix };
      Hashtbl.add table.children (parent, suffix) table.next_code;
      table.next_code <- table.next_code + 1;
      true)
    else false

  type bitstream = {
    mutable buffer : int;
    mutable bits_in_buffer : int;
    output : Buffer.t;
  }
  (* Bitstream writer *)

  let create_bitstream () =
    { buffer = 0; bits_in_buffer = 0; output = Buffer.create 1024 }

  let write_bits stream value num_bits =
    stream.buffer <- stream.buffer lor (value lsl stream.bits_in_buffer);
    stream.bits_in_buffer <- stream.bits_in_buffer + num_bits;
    while stream.bits_in_buffer >= 8 do
      Buffer.add_char stream.output (Char.chr (stream.buffer land 0xFF));
      stream.buffer <- stream.buffer lsr 8;
      stream.bits_in_buffer <- stream.bits_in_buffer - 8
    done

  let flush_bits stream =
    if stream.bits_in_buffer > 0 then
      Buffer.add_char stream.output (Char.chr stream.buffer)

  (* Compress data using LZW algorithm *)
  let compress data color_bits =
    let min_code_size = max 2 color_bits in
    let clear = 1 lsl min_code_size in
    let end_of_info = clear + 1 in

    let stream = create_bitstream () in
    let table = create_table min_code_size in
    let code_size = ref (min_code_size + 1) in
    let code_limit = ref (1 lsl !code_size) in

    write_bits stream clear !code_size;

    let len = Bytes.length data in
    if len > 0 then (
      let current_code = ref (Char.code (Bytes.unsafe_get data 0)) in
      let i = ref 1 in

      while !i < len do
        let suffix = Bytes.unsafe_get data !i in
        match find_child table !current_code suffix with
        | Some code ->
            current_code := code;
            incr i
        | None ->
            (* Output current code *)
            write_bits stream !current_code !code_size;

            (* Check if we need to increase code size BEFORE adding new code *)
            if table.next_code = !code_limit && !code_size < 12 then (
              incr code_size;
              code_limit := 1 lsl !code_size);

            (* Add new code if room *)
            if not (add_code table !current_code suffix) then (
              (* Dictionary full - emit clear code and reset *)
              write_bits stream clear !code_size;
              table.next_code <- clear + 2;
              (* Reset to after Clear and End codes *)
              Hashtbl.clear table.children;
              (* Re-initialize single-byte codes *)
              for j = 0 to clear - 1 do
                table.entries.(j) <- { parent = -1; suffix = Char.chr j };
                Hashtbl.add table.children (-1, Char.chr j) j
              done;
              code_size := min_code_size + 1;
              code_limit := 1 lsl !code_size);

            current_code := Char.code suffix;
            incr i
      done;

      (* Output final code *)
      write_bits stream !current_code !code_size);

    write_bits stream end_of_info !code_size;
    flush_bits stream;

    (* Return only the compressed data, not the min_code_size byte *)
    Buffer.contents stream.output

  (* Package data into sub-blocks *)
  let package_subblocks data =
    let result = Buffer.create (String.length data + 256) in
    let len = String.length data in
    let pos = ref 0 in

    while !pos < len do
      let block_size = min 255 (len - !pos) in
      Buffer.add_char result (Char.chr block_size);
      Buffer.add_substring result data !pos block_size;
      pos := !pos + block_size
    done;

    Buffer.add_char result '\x00';
    (* Block terminator *)
    Buffer.contents result
end

(* Write GIF header and initial metadata to a buffer *)
let write_gif_header buf gif =
  (* Write header *)
  Buffer.add_bytes buf gif_header;

  (* Write logical screen descriptor *)
  pack_u16 buf gif.width;
  pack_u16 buf gif.height;

  let bits = color_bits (palette_size gif.global_palette) in
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
  write_color_table buf (palette_to_array gif.global_palette) table_size;

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
      Buffer.add_char buf '\x00'
(* Block terminator *)

(* Write a frame to the buffer *)
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
        let bits = color_bits (palette_size local_table) in
        let table_size = 1 lsl bits in
        let packed = 0x80 lor ((bits - 1) land 0x07) in
        (* Local color table flag + size, ensure reserved bits are zero *)
        Buffer.add_char buf (Char.chr packed);
        write_color_table buf (palette_to_array local_table) table_size;
        (local_table, bits)
    | None ->
        Buffer.add_char buf '\x00';
        (* No local color table *)
        (global_palette, color_bits (palette_size global_palette))
  in

  (* Compress and write image data *)
  let min_code_size = max 2 palette_bits in
  Buffer.add_char buf (Char.chr min_code_size);
  let compressed = Lzw.compress frame.pixels palette_bits in
  let packaged = Lzw.package_subblocks compressed in
  Buffer.add_string buf packaged

(* Encode a GIF data structure to binary format *)
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

(* Encode a GIF directly to a writer function *)
let rec encode_streaming gif ~writer =
  try
    (* Write header *)
    write_string writer "GIF89a";

    (* Write logical screen descriptor *)
    write_u16 writer gif.width;
    write_u16 writer gif.height;

    let bits = color_bits (palette_size gif.global_palette) in
    let table_size = 1 lsl bits in
    let packed_field = 0x80 lor ((bits - 1) lsl 4) lor (bits - 1) in
    write_byte writer (Char.chr packed_field);
    write_byte writer (Char.chr gif.background_index);
    write_byte writer '\x00';

    (* Pixel aspect ratio *)

    (* Write global color table with padding *)
    write_color_table_writer writer
      (palette_to_array gif.global_palette)
      table_size;

    (* Add NETSCAPE2.0 extension for looping if specified *)
    (match gif.loop_count with
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
        write_byte writer '\x00');

    (* Block terminator *)

    (* Write all frames *)
    List.iter
      (fun frame -> write_frame_streaming writer frame gif.global_palette)
      gif.frames;

    (* Write trailer *)
    write_byte writer trailer;

    Ok ()
  with exn -> Error (Io_error exn)

(* Write a frame to the writer function *)
and write_frame_streaming writer frame global_palette =
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
  (* Ensure transparent index is 0 when flag is clear *)
  let trans_byte =
    if has_transparent then Option.get frame.transparent_index else 0
  in
  write_byte writer (Char.chr trans_byte);
  write_byte writer '\x00';

  (* Block terminator *)

  (* Image Descriptor *)
  write_byte writer image_separator;
  write_u16 writer frame.x_offset;
  (* Left position *)
  write_u16 writer frame.y_offset;
  (* Top position *)
  write_u16 writer frame.width;
  write_u16 writer frame.height;

  (* Handle local color table if present *)
  let _palette, palette_bits =
    match frame.local_palette with
    | Some local_table ->
        let bits = color_bits (palette_size local_table) in
        let table_size = 1 lsl bits in
        let packed = 0x80 lor ((bits - 1) land 0x07) in
        write_byte writer (Char.chr packed);
        write_color_table_writer writer
          (palette_to_array local_table)
          table_size;
        (local_table, bits)
    | None ->
        write_byte writer '\x00';
        (* No local color table *)
        (global_palette, color_bits (palette_size global_palette))
  in

  (* Compress and write image data *)
  let min_code_size = max 2 palette_bits in
  write_byte writer (Char.chr min_code_size);
  let compressed = Lzw.compress frame.pixels palette_bits in
  let packaged = Lzw.package_subblocks compressed in
  write_string writer packaged

(* Quantize RGB colors to a palette *)
module Quantize = struct
  let color_distance (r1, g1, b1) (r2, g2, b2) =
    (* Manhattan distance to avoid overflow *)
    abs (r1 - r2) + abs (g1 - g2) + abs (b1 - b2)

  let find_nearest_color color palette =
    let min_dist = ref max_int in
    let best_idx = ref 0 in
    Array.iteri
      (fun i c ->
        let dist = color_distance color (c.r, c.g, c.b) in
        if dist < !min_dist then (
          min_dist := dist;
          best_idx := i))
      palette;
    !best_idx

  (* Count color frequencies *)
  let count_colors rgb_data =
    let freq = Hashtbl.create 256 in
    Array.iter
      (fun color ->
        match Hashtbl.find_opt freq color with
        | Some n -> Hashtbl.replace freq color (n + 1)
        | None -> Hashtbl.add freq color 1)
      rgb_data;
    freq

  type box = {
    colors : (int * int * int * int) array; (* r, g, b, count *)
    start_idx : int;
    end_idx : int;
  }
  (* Simple median-cut algorithm *)

  let box_volume box =
    if box.start_idx >= box.end_idx then 0
    else
      let r_min = ref 255 and r_max = ref 0 in
      let g_min = ref 255 and g_max = ref 0 in
      let b_min = ref 255 and b_max = ref 0 in
      for i = box.start_idx to box.end_idx - 1 do
        let r, g, b, _ = box.colors.(i) in
        r_min := min !r_min r;
        r_max := max !r_max r;
        g_min := min !g_min g;
        g_max := max !g_max g;
        b_min := min !b_min b;
        b_max := max !b_max b
      done;
      (!r_max - !r_min) * (!g_max - !g_min) * (!b_max - !b_min)

  let split_box box =
    (* Find dimension with largest range *)
    let r_min = ref 255 and r_max = ref 0 in
    let g_min = ref 255 and g_max = ref 0 in
    let b_min = ref 255 and b_max = ref 0 in
    for i = box.start_idx to box.end_idx - 1 do
      let r, g, b, _ = box.colors.(i) in
      r_min := min !r_min r;
      r_max := max !r_max r;
      g_min := min !g_min g;
      g_max := max !g_max g;
      b_min := min !b_min b;
      b_max := max !b_max b
    done;

    let r_range = !r_max - !r_min in
    let g_range = !g_max - !g_min in
    let b_range = !b_max - !b_min in

    (* Sort along longest dimension *)
    let cmp =
      if r_range >= g_range && r_range >= b_range then
        fun (r1, _, _, _) (r2, _, _, _) -> compare r1 r2
      else if g_range >= b_range then fun (_, g1, _, _) (_, g2, _, _) ->
        compare g1 g2
      else fun (_, _, b1, _) (_, _, b2, _) -> compare b1 b2
    in

    (* Create an array of indices and sort those instead of copying data *)
    let len = box.end_idx - box.start_idx in
    let indices = Array.init len (fun i -> box.start_idx + i) in
    Array.sort (fun i j -> cmp box.colors.(i) box.colors.(j)) indices;

    (* Create a temporary array for the sorted slice *)
    let sorted = Array.init len (fun i -> box.colors.(indices.(i))) in
    Array.blit sorted 0 box.colors box.start_idx len;

    (* Split at median *)
    let mid = box.start_idx + ((box.end_idx - box.start_idx) / 2) in
    let box1 =
      { colors = box.colors; start_idx = box.start_idx; end_idx = mid }
    in
    let box2 =
      { colors = box.colors; start_idx = mid; end_idx = box.end_idx }
    in
    (box1, box2)

  (* Create palette using median cut *)
  let create_palette rgb_data max_colors =
    if max_colors > 256 then Error (Palette_too_large max_colors)
    else
      let freq = count_colors rgb_data in

      (* Convert to array with frequencies *)
      let weighted_colors =
        Hashtbl.fold
          (fun (r, g, b) count acc -> (r, g, b, count) :: acc)
          freq []
        |> Array.of_list
      in

      if Array.length weighted_colors <= max_colors then
        (* Use all unique colors *)
        Ok (Array.map (fun (r, g, b, _) -> { r; g; b }) weighted_colors)
      else
        (* Median cut algorithm *)
        let initial_box =
          {
            colors = weighted_colors;
            start_idx = 0;
            end_idx = Array.length weighted_colors;
          }
        in

        let boxes = ref [ initial_box ] in

        (* Split boxes until we have enough *)
        let continue = ref true in
        while !continue && List.length !boxes < max_colors do
          (* Find box with largest volume *)
          let sorted =
            List.sort (fun a b -> compare (box_volume b) (box_volume a)) !boxes
          in
          match sorted with
          | [] -> assert false
          | largest :: rest ->
              if box_volume largest > 0 then
                let b1, b2 = split_box largest in
                boxes := b1 :: b2 :: rest
              else
                (* Can't split further *)
                continue := false
        done;

        (* Extract average color from each box *)
        !boxes
        |> List.filter (fun b -> b.start_idx < b.end_idx)
        |> List.map (fun box ->
               let r_sum = ref 0 and g_sum = ref 0 and b_sum = ref 0 in
               let count_sum = ref 0 in
               for i = box.start_idx to box.end_idx - 1 do
                 let r, g, b, count = box.colors.(i) in
                 r_sum := !r_sum + (r * count);
                 g_sum := !g_sum + (g * count);
                 b_sum := !b_sum + (b * count);
                 count_sum := !count_sum + count
               done;
               if !count_sum = 0 then { r = 0; g = 0; b = 0 }
               else
                 {
                   r = !r_sum / !count_sum;
                   g = !g_sum / !count_sum;
                   b = !b_sum / !count_sum;
                 })
        |> Array.of_list
        |> fun result -> Ok result
end

(* Dithering algorithm *)
type dithering = No_dither | Floyd_steinberg

(* Convert RGB data to indexed color data *)
let rgb_to_indexed ?(dither = No_dither) ~width rgb_data palette =
  let len = Array.length rgb_data in
  let height = len / width in
  if len mod width <> 0 then
    Error (Invalid_argument "RGB data length must be a multiple of width")
  else
    let indexed = Bytes.create len in
    match dither with
    | No_dither ->
        Array.iteri
          (fun i color ->
            let idx = Quantize.find_nearest_color color palette in
            Bytes.set indexed i (Char.chr idx))
          rgb_data;
        Ok indexed
    | Floyd_steinberg ->
        (* Create a working copy for error diffusion *)
        let work =
          Array.map
            (fun (r, g, b) -> (float_of_int r, float_of_int g, float_of_int b))
            rgb_data
        in

        for y = 0 to height - 1 do
          for x = 0 to width - 1 do
            let i = (y * width) + x in
            let r, g, b = work.(i) in

            (* Clamp to valid range *)
            let r = max 0.0 (min 255.0 r) in
            let g = max 0.0 (min 255.0 g) in
            let b = max 0.0 (min 255.0 b) in

            (* Find nearest color *)
            let idx =
              Quantize.find_nearest_color
                (int_of_float r, int_of_float g, int_of_float b)
                palette
            in
            Bytes.set indexed i (Char.chr idx);

            (* Calculate error *)
            match palette_get palette idx with
            | None -> () (* Should not happen *)
            | Some c ->
                let er = r -. float c.r in
                let eg = g -. float c.g in
                let eb = b -. float c.b in

                (* Distribute error to neighbors *)
                let distribute dx dy factor =
                  let nx = x + dx and ny = y + dy in
                  if nx >= 0 && nx < width && ny >= 0 && ny < height then
                    let ni = (ny * width) + nx in
                    let nr, ng, nb = work.(ni) in
                    work.(ni) <-
                      ( nr +. (er *. factor),
                        ng +. (eg *. factor),
                        nb +. (eb *. factor) )
                in

                (* Floyd-Steinberg error distribution:
                   X   7/16
                   3/16 5/16 1/16 *)
                if x < width - 1 then distribute 1 0 (7.0 /. 16.0);
                if y < height - 1 then (
                  if x > 0 then distribute (-1) 1 (3.0 /. 16.0);
                  distribute 0 1 (5.0 /. 16.0);
                  if x < width - 1 then distribute 1 1 (1.0 /. 16.0))
          done
        done;
        Ok indexed

(* LZW decompression for GIF *)
module Lzw_decode = struct
  type decoder = {
    min_code_size : int;
    clear_code : int;
    end_code : int;
    mutable code_size : int;
    mutable next_code : int;
    dict : string array;
    mutable old_code : int option;
  }

  let create min_code_size =
    let clear_code = 1 lsl min_code_size in
    let end_code = clear_code + 1 in
    let dict = Array.make 4096 "" in
    (* Initialize single-byte strings *)
    for i = 0 to clear_code - 1 do
      dict.(i) <- String.make 1 (Char.chr i)
    done;
    {
      min_code_size;
      clear_code;
      end_code;
      code_size = min_code_size + 1;
      next_code = end_code + 1;
      dict;
      old_code = None;
    }

  let reset decoder =
    decoder.code_size <- decoder.min_code_size + 1;
    decoder.next_code <- decoder.end_code + 1;
    decoder.old_code <- None

  let add_string decoder str =
    if decoder.next_code < 4096 then (
      decoder.dict.(decoder.next_code) <- str;
      decoder.next_code <- decoder.next_code + 1)

  let decode_string decoder code =
    if code < decoder.next_code then decoder.dict.(code)
    else if code = decoder.next_code && decoder.old_code <> None then
      (* Special case: code = next available code *)
      let old = Option.get decoder.old_code in
      decoder.dict.(old) ^ String.sub decoder.dict.(old) 0 1
    else
      failwith
        (Printf.sprintf "Invalid LZW code: %d (next_code=%d)" code
           decoder.next_code)

  type bitstream_reader = {
    data : string;
    mutable pos : int;
    mutable buffer : int;
    mutable bits_in_buffer : int;
  }

  let create_reader data = { data; pos = 0; buffer = 0; bits_in_buffer = 0 }

  let read_bits reader num_bits =
    while
      reader.bits_in_buffer < num_bits && reader.pos < String.length reader.data
    do
      reader.buffer <-
        reader.buffer
        lor (Char.code reader.data.[reader.pos] lsl reader.bits_in_buffer);
      reader.bits_in_buffer <- reader.bits_in_buffer + 8;
      reader.pos <- reader.pos + 1
    done;

    if reader.bits_in_buffer < num_bits then
      failwith "Not enough bits in stream";

    let mask = (1 lsl num_bits) - 1 in
    let result = reader.buffer land mask in
    reader.buffer <- reader.buffer lsr num_bits;
    reader.bits_in_buffer <- reader.bits_in_buffer - num_bits;
    result

  let decompress compressed_data min_code_size =
    let decoder = create min_code_size in
    let reader = create_reader compressed_data in
    let output = Buffer.create (String.length compressed_data * 4) in

    try
      while true do
        let code =
          try read_bits reader decoder.code_size
          with Failure _ -> raise Exit (* No more data *)
        in

        if code = decoder.end_code then raise Exit
        else if code = decoder.clear_code then (
          reset decoder;
          (* After a clear code, the next code must be a literal *)
          decoder.old_code <- None)
        else
          let str = decode_string decoder code in
          Buffer.add_string output str;

          (match decoder.old_code with
          | Some old ->
              let old_str = decoder.dict.(old) in
              let new_str = old_str ^ String.sub str 0 1 in
              add_string decoder new_str
          | None -> ());
          decoder.old_code <- Some code;

          (* Check if we need to increase code size after processing *)
          if
            decoder.next_code = 1 lsl decoder.code_size
            && decoder.code_size < 12
          then decoder.code_size <- decoder.code_size + 1
      done;
      assert false
    with Exit -> Buffer.contents output
end

type frame_info = { frame : frame; _data_offset : int; _data_length : int }

exception Invalid_gif of string

let read_u16 data pos =
  if pos + 1 >= String.length data then
    raise (Invalid_gif "Unexpected end of data reading u16");
  Char.code data.[pos] lor (Char.code data.[pos + 1] lsl 8)

let read_color_table data pos size =
  if pos + (size * 3) > String.length data then
    raise (Invalid_gif "Unexpected end of data reading color table");
  Array.init size (fun i ->
      let idx = pos + (i * 3) in
      {
        r = Char.code data.[idx];
        g = Char.code data.[idx + 1];
        b = Char.code data.[idx + 2];
      })

let decode_disposal = function
  | 0 -> No_disposal
  | 1 -> Do_not_dispose
  | 2 -> Restore_background
  | 3 -> Restore_previous
  | _ -> No_disposal

type gce_info = {
  gce_delay : int;
  gce_disposal : disposal_method;
  gce_transparent_index : int option;
}

let skip_sub_blocks data pos =
  while !pos < String.length data && data.[!pos] <> '\x00' do
    let sub_size = Char.code data.[!pos] in
    if !pos + 1 + sub_size > String.length data then
      raise (Invalid_gif "Truncated sub-block");
    pos := !pos + 1 + sub_size
  done;
  if !pos < String.length data then pos := !pos + 1
  else raise (Invalid_gif "Missing sub-block terminator")

let read_sub_blocks data pos =
  let buffer = Buffer.create 1024 in
  while !pos < String.length data && data.[!pos] <> '\x00' do
    let sub_size = Char.code data.[!pos] in
    if !pos + 1 + sub_size > String.length data then
      raise (Invalid_gif "Truncated sub-block");
    Buffer.add_substring buffer data (!pos + 1) sub_size;
    pos := !pos + 1 + sub_size
  done;
  if !pos < String.length data then pos := !pos + 1
  else raise (Invalid_gif "Missing sub-block terminator");
  Buffer.contents buffer

let de_interlace data width height =
  (* GIF uses Adam7-style interlacing with 4 passes *)
  let deinterlaced = Bytes.create (width * height) in
  let src_idx = ref 0 in

  (* Pass 1: Lines 0, 8, 16, ... *)
  let y = ref 0 in
  while !y < height do
    for x = 0 to width - 1 do
      if !src_idx < Bytes.length data then (
        Bytes.set deinterlaced ((!y * width) + x) (Bytes.get data !src_idx);
        incr src_idx)
    done;
    y := !y + 8
  done;

  (* Pass 2: Lines 4, 12, 20, ... *)
  y := 4;
  while !y < height do
    for x = 0 to width - 1 do
      if !src_idx < Bytes.length data then (
        Bytes.set deinterlaced ((!y * width) + x) (Bytes.get data !src_idx);
        incr src_idx)
    done;
    y := !y + 8
  done;

  (* Pass 3: Lines 2, 6, 10, 14, ... *)
  y := 2;
  while !y < height do
    for x = 0 to width - 1 do
      if !src_idx < Bytes.length data then (
        Bytes.set deinterlaced ((!y * width) + x) (Bytes.get data !src_idx);
        incr src_idx)
    done;
    y := !y + 4
  done;

  (* Pass 4: Lines 1, 3, 5, 7, ... *)
  y := 1;
  while !y < height do
    for x = 0 to width - 1 do
      if !src_idx < Bytes.length data then (
        Bytes.set deinterlaced ((!y * width) + x) (Bytes.get data !src_idx);
        incr src_idx)
    done;
    y := !y + 2
  done;

  deinterlaced

let decode data =
  try
    (* Check header *)
    if String.length data < 6 then
      raise (Invalid_gif "File too small for GIF header");
    let header = String.sub data 0 6 in
    if header <> "GIF87a" && header <> "GIF89a" then
      raise (Invalid_gif "Invalid GIF header");

    (* Read logical screen descriptor *)
    if String.length data < 13 then
      raise (Invalid_gif "File too small for logical screen descriptor");

    let width = read_u16 data 6 in
    let height = read_u16 data 8 in
    let packed = Char.code data.[10] in
    let has_gct = packed land 0x80 <> 0 in
    let gct_size = if has_gct then 1 lsl ((packed land 0x07) + 1) else 0 in
    let background_index = Char.code data.[11] in
    let _pixel_aspect_ratio = Char.code data.[12] in

    (* Validate dimensions *)
    if width = 0 || height = 0 then
      raise (Invalid_gif "Invalid canvas dimensions");
    if width > 65535 || height > 65535 then
      raise (Invalid_gif "Canvas dimensions exceed maximum");

    let pos = ref 13 in

    (* Read global color table if present *)
    let global_color_table =
      if has_gct then (
        if !pos + (gct_size * 3) > String.length data then
          raise (Invalid_gif "Truncated global color table");
        let table = read_color_table data !pos gct_size in
        pos := !pos + (gct_size * 3);
        Some table)
      else None
    in

    let frames = ref [] in
    let current_gce = ref None in
    let has_trailer = ref false in
    let loop_count = ref None in
    let comments = ref [] in

    (* Parse blocks *)
    while !pos < String.length data && not !has_trailer do
      if !pos >= String.length data then
        raise (Invalid_gif "Unexpected end of data");

      match data.[!pos] with
      | '\x21' -> (
          (* Extension *)
          if !pos + 2 > String.length data then
            raise (Invalid_gif "Truncated extension block");
          let label = data.[!pos + 1] in
          pos := !pos + 2;

          match label with
          | '\xF9' ->
              (* Graphics Control Extension *)
              if !pos + 6 > String.length data then
                raise (Invalid_gif "Truncated graphics control extension");
              let block_size = Char.code data.[!pos] in
              if block_size <> 4 then
                raise
                  (Invalid_gif
                     (Printf.sprintf "Invalid GCE block size: %d" block_size));
              let packed = Char.code data.[!pos + 1] in
              let delay = read_u16 data (!pos + 2) in
              let trans_idx = Char.code data.[!pos + 4] in
              let has_trans = packed land 1 <> 0 in
              let disposal = decode_disposal ((packed lsr 2) land 0x07) in

              current_gce :=
                Some
                  {
                    gce_delay = delay;
                    gce_disposal = disposal;
                    gce_transparent_index =
                      (if has_trans then Some trans_idx else None);
                  };
              pos := !pos + 5;
              (* Skip terminator *)
              if !pos >= String.length data || data.[!pos] <> '\x00' then
                raise (Invalid_gif "Missing GCE terminator");
              pos := !pos + 1
          | '\xFF' ->
              (* Application Extension *)
              if !pos >= String.length data then
                raise (Invalid_gif "Truncated application extension");
              let block_size = Char.code data.[!pos] in
              if block_size <> 11 then (
                (* Not a standard app extension, skip it *)
                if !pos + 1 + block_size > String.length data then
                  raise (Invalid_gif "Truncated application identifier");
                pos := !pos + 1 + block_size;
                skip_sub_blocks data pos)
              else (
                (* Check if it's NETSCAPE2.0 *)
                if !pos + 12 > String.length data then
                  raise (Invalid_gif "Truncated application identifier");
                let app_id = String.sub data (!pos + 1) 11 in
                pos := !pos + 12;

                if app_id = "NETSCAPE2.0" then
                  if
                    (* Read loop count from sub-block *)
                    !pos + 4 <= String.length data
                    && Char.code data.[!pos] = 3
                    && Char.code data.[!pos + 1] = 1
                  then (
                    let loop_val = read_u16 data (!pos + 2) in
                    loop_count := Some loop_val;
                    pos := !pos + 4;
                    (* Skip terminator *)
                    if !pos < String.length data && data.[!pos] = '\x00' then
                      pos := !pos + 1)
                  else skip_sub_blocks data pos
                else skip_sub_blocks data pos)
          | '\xFE' ->
              (* Comment Extension *)
              let comment_text = read_sub_blocks data pos in
              comments := comment_text :: !comments
          | '\x01' ->
              (* Plain Text Extension *)
              if !pos >= String.length data then
                raise (Invalid_gif "Truncated plain text extension");
              let block_size = Char.code data.[!pos] in
              if block_size <> 12 then
                raise (Invalid_gif "Invalid plain text block size");
              if !pos + 1 + block_size > String.length data then
                raise (Invalid_gif "Truncated plain text data");
              pos := !pos + 1 + block_size;
              skip_sub_blocks data pos
          | _ ->
              (* Unknown extension *)
              skip_sub_blocks data pos)
      | '\x2C' ->
          (* Image Separator *)
          if !pos + 10 > String.length data then
            raise (Invalid_gif "Truncated image descriptor");
          pos := !pos + 1;

          let left = read_u16 data !pos in
          let top = read_u16 data (!pos + 2) in
          let frame_width = read_u16 data (!pos + 4) in
          let frame_height = read_u16 data (!pos + 6) in
          let flags = Char.code data.[!pos + 8] in
          pos := !pos + 9;

          (* Validate frame dimensions *)
          if frame_width = 0 || frame_height = 0 then
            raise (Invalid_gif "Invalid frame dimensions");
          if left + frame_width > width || top + frame_height > height then
            raise (Invalid_gif "Frame exceeds canvas bounds");

          let has_lct = flags land 0x80 <> 0 in
          let interlaced = flags land 0x40 <> 0 in
          let _sorted = flags land 0x20 <> 0 in
          let lct_size = if has_lct then 1 lsl ((flags land 0x07) + 1) else 0 in

          let local_color_table =
            if has_lct then (
              if !pos + (lct_size * 3) > String.length data then
                raise (Invalid_gif "Truncated local color table");
              let table = read_color_table data !pos lct_size in
              pos := !pos + (lct_size * 3);
              Some table)
            else None
          in

          (* LZW minimum code size *)
          if !pos >= String.length data then
            raise (Invalid_gif "Missing LZW minimum code size");
          let min_code_size = Char.code data.[!pos] in
          if min_code_size < 2 || min_code_size > 8 then
            raise
              (Invalid_gif
                 (Printf.sprintf "Invalid LZW minimum code size: %d"
                    min_code_size));
          pos := !pos + 1;

          let data_start = !pos in

          (* Read compressed image data *)
          let compressed_data = read_sub_blocks data pos in
          let data_length = String.length compressed_data in

          (* Validate transparent index if present *)
          (match !current_gce with
          | Some gce -> (
              match gce.gce_transparent_index with
              | Some idx ->
                  let palette_size =
                    match local_color_table with
                    | Some lct -> Array.length lct
                    | None -> (
                        match global_color_table with
                        | Some gct -> Array.length gct
                        | None ->
                            raise
                              (Invalid_gif
                                 "Transparent index without color table"))
                  in
                  if idx >= palette_size then
                    raise
                      (Invalid_gif "Transparent index out of palette bounds")
              | None -> ())
          | None -> ());

          (* Decompress the image data *)
          let decompressed_str =
            Lzw_decode.decompress compressed_data min_code_size
          in
          let decompressed = Bytes.of_string decompressed_str in

          (* Handle interlacing if needed *)
          let pixel_data =
            if interlaced then
              de_interlace decompressed frame_width frame_height
            else decompressed
          in

          (* Validate pixel data length *)
          let expected_len = frame_width * frame_height in
          if Bytes.length pixel_data <> expected_len then
            raise
              (Invalid_gif
                 (Printf.sprintf
                    "Decompressed data length %d does not match frame size \
                     %dx%d (expected %d)"
                    (Bytes.length pixel_data) frame_width frame_height
                    expected_len));

          (* Create frame info *)
          let frame =
            {
              width = frame_width;
              height = frame_height;
              x_offset = left;
              y_offset = top;
              delay_cs =
                (match !current_gce with Some g -> g.gce_delay | None -> 0);
              disposal =
                (match !current_gce with
                | Some g -> g.gce_disposal
                | None -> No_disposal);
              transparent_index =
                (match !current_gce with
                | Some g -> g.gce_transparent_index
                | None -> None);
              local_palette =
                (match local_color_table with
                | None -> None
                | Some ct -> (
                    match palette_of_array ct with
                    | Error _ -> raise (Invalid_gif "Invalid local color table")
                    | Ok p -> Some p));
              pixels = pixel_data;
            }
          in
          let frame_info =
            { frame; _data_offset = data_start; _data_length = data_length }
          in

          frames := frame_info :: !frames;
          current_gce := None
      | '\x3B' ->
          (* Trailer *)
          has_trailer := true;
          pos := !pos + 1
      | '\x00' ->
          (* Sometimes encoders add null padding *)
          pos := !pos + 1
      | _ ->
          raise
            (Invalid_gif
               (Printf.sprintf "Unknown block type 0x%02X at position %d"
                  (Char.code data.[!pos])
                  !pos))
    done;

    (* Validate we found the trailer *)
    if not !has_trailer then raise (Invalid_gif "Missing GIF trailer");

    (* Validate we found at least one frame (unless GIF87a) *)
    if !frames = [] && header = "GIF89a" then
      raise (Invalid_gif "No frames found in GIF89a");

    (* Convert global color table to palette *)
    let global_palette =
      match global_color_table with
      | None -> raise (Invalid_gif "No global color table found")
      | Some ct -> (
          match palette_of_array ct with
          | Error _ -> raise (Invalid_gif "Invalid global color table")
          | Ok p -> p)
    in

    Ok
      {
        width;
        height;
        global_palette;
        background_index;
        loop_count = !loop_count;
        frames = List.rev !frames |> List.map (fun fi -> fi.frame);
      }
  with Invalid_gif msg -> Error (Decode_error msg)

(* Decode a GIF and return the extracted frames with actual pixel data *)
let decode_frames gif_data =
  match decode gif_data with Ok info -> Ok info.frames | Error _ as e -> e
