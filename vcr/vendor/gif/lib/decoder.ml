(** GIF decoder implementation *)

open Types

(** {1 Binary reading utilities} *)

(** Monadic bind operator for Result *)
let ( >>= ) r f = match r with Ok v -> f v | Error _ as e -> e

module Reader = struct
  (** Read a 16-bit little-endian integer *)
  let read_u16 data pos =
    if pos + 1 >= String.length data then
      Error (Decode_error "Unexpected end of data reading u16")
    else Ok (Char.code data.[pos] lor (Char.code data.[pos + 1] lsl 8))

  (** Read a color table *)
  let read_color_table data pos size =
    if pos + (size * 3) > String.length data then
      Error (Decode_error "Unexpected end of data reading color table")
    else
      Ok
        (Array.init size (fun i ->
             let idx = pos + (i * 3) in
             {
               r = Char.code data.[idx];
               g = Char.code data.[idx + 1];
               b = Char.code data.[idx + 2];
             }))

  (** Skip sub-blocks structure *)
  let skip_sub_blocks data pos =
    let rec skip () =
      if !pos >= String.length data then
        Error (Decode_error "Missing sub-block terminator")
      else if data.[!pos] = '\x00' then (
        pos := !pos + 1;
        Ok ())
      else
        let sub_size = Char.code data.[!pos] in
        if !pos + 1 + sub_size > String.length data then
          Error (Decode_error "Truncated sub-block")
        else (
          pos := !pos + 1 + sub_size;
          skip ())
    in
    skip ()

  (** Read sub-blocks and concatenate their data *)
  let read_sub_blocks data pos =
    let buffer = Buffer.create 1024 in
    let rec read () =
      if !pos >= String.length data then
        Error (Decode_error "Missing sub-block terminator")
      else if data.[!pos] = '\x00' then (
        pos := !pos + 1;
        Ok (Buffer.contents buffer))
      else
        let sub_size = Char.code data.[!pos] in
        if !pos + 1 + sub_size > String.length data then
          Error (Decode_error "Truncated sub-block")
        else (
          Buffer.add_substring buffer data (!pos + 1) sub_size;
          pos := !pos + 1 + sub_size;
          read ())
    in
    read ()
end

(** {1 GIF-specific decoding} *)

(** Decode disposal method from packed field *)
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
(** Graphics Control Extension data *)

(** De-interlace image data using GIF's interlacing scheme *)
let de_interlace data width height =
  let deinterlaced = Bytes.create (width * height) in
  let src_idx = ref 0 in

  (* GIF uses 4-pass interlacing *)
  let passes =
    [
      (0, 8);
      (* Pass 1: Lines 0, 8, 16, ... *)
      (4, 8);
      (* Pass 2: Lines 4, 12, 20, ... *)
      (2, 4);
      (* Pass 3: Lines 2, 6, 10, 14, ... *)
      (1, 2);
      (* Pass 4: Lines 1, 3, 5, 7, ... *)
    ]
  in

  List.iter
    (fun (start, step) ->
      let y = ref start in
      while !y < height do
        for x = 0 to width - 1 do
          if !src_idx < Bytes.length data then (
            Bytes.set deinterlaced ((!y * width) + x) (Bytes.get data !src_idx);
            incr src_idx)
        done;
        y := !y + step
      done)
    passes;

  deinterlaced

(** {1 Extension parsers} *)

module Extensions = struct
  (** Parse Graphics Control Extension *)
  let parse_gce data pos =
    if !pos + 6 > String.length data then
      Error (Decode_error "Truncated graphics control extension")
    else
      let block_size = Char.code data.[!pos] in
      if block_size <> 4 then
        Error
          (Decode_error (Printf.sprintf "Invalid GCE block size: %d" block_size))
      else
        let packed = Char.code data.[!pos + 1] in
        match Reader.read_u16 data (!pos + 2) with
        | Error e -> Error e
        | Ok delay ->
            let trans_idx = Char.code data.[!pos + 4] in
            let has_trans = packed land 1 <> 0 in
            let disposal = decode_disposal ((packed lsr 2) land 0x07) in
            let gce =
              {
                gce_delay = delay;
                gce_disposal = disposal;
                gce_transparent_index =
                  (if has_trans then Some trans_idx else None);
              }
            in
            pos := !pos + 5;
            if !pos >= String.length data || data.[!pos] <> '\x00' then
              Error (Decode_error "Missing GCE terminator")
            else (
              pos := !pos + 1;
              Ok gce)

  (** Parse NETSCAPE2.0 Application Extension for loop count *)
  let parse_netscape_loop data pos =
    if
      !pos + 4 <= String.length data
      && Char.code data.[!pos] = 3
      && Char.code data.[!pos + 1] = 1
    then (
      match Reader.read_u16 data (!pos + 2) with
      | Error e -> Error e
      | Ok loop_val ->
          pos := !pos + 4;
          if !pos < String.length data && data.[!pos] = '\x00' then
            pos := !pos + 1;
          Ok (Some loop_val))
    else Reader.skip_sub_blocks data pos >>= fun () -> Ok None

  (** Parse Application Extension *)
  let parse_app_extension data pos =
    if !pos >= String.length data then
      Error (Decode_error "Truncated application extension")
    else
      let block_size = Char.code data.[!pos] in
      if block_size <> 11 then
        if !pos + 1 + block_size > String.length data then
          Error (Decode_error "Truncated application identifier")
        else (
          pos := !pos + 1 + block_size;
          Reader.skip_sub_blocks data pos >>= fun () -> Ok None)
      else if !pos + 12 > String.length data then
        Error (Decode_error "Truncated application identifier")
      else
        let app_id = String.sub data (!pos + 1) 11 in
        pos := !pos + 12;
        if app_id = "NETSCAPE2.0" then parse_netscape_loop data pos
        else Reader.skip_sub_blocks data pos >>= fun () -> Ok None

  (** Parse Comment Extension *)
  let parse_comment data pos =
    match Reader.read_sub_blocks data pos with
    | Error e -> Error e
    | Ok comment_text -> Ok (Some comment_text)

  (** Parse Plain Text Extension *)
  let parse_plain_text data pos =
    if !pos >= String.length data then
      Error (Decode_error "Truncated plain text extension")
    else
      let block_size = Char.code data.[!pos] in
      if block_size <> 12 then
        Error (Decode_error "Invalid plain text block size")
      else if !pos + 1 + block_size > String.length data then
        Error (Decode_error "Truncated plain text data")
      else (
        pos := !pos + 1 + block_size;
        Reader.skip_sub_blocks data pos)
end

(** {1 Image data parser} *)

type image_data = {
  left : int;
  top : int;
  frame_width : int;
  frame_height : int;
  local_palette : palette option;
  pixel_data : bytes;
}
(** Intermediate image data structure *)

(** Parse an image descriptor and decompress the frame data *)
let parse_image_data data pos ~canvas_width ~canvas_height =
  if !pos + 10 > String.length data then
    Error (Decode_error "Truncated image descriptor")
  else (
    pos := !pos + 1;

    (* Skip image separator *)

    (* Read frame position and dimensions *)
    Reader.read_u16 data !pos >>= fun left ->
    Reader.read_u16 data (!pos + 2) >>= fun top ->
    Reader.read_u16 data (!pos + 4) >>= fun frame_width ->
    Reader.read_u16 data (!pos + 6) >>= fun frame_height ->
    let flags = Char.code data.[!pos + 8] in
    pos := !pos + 9;

    (* Validate frame dimensions *)
    if frame_width = 0 || frame_height = 0 then
      Error (Decode_error "Invalid frame dimensions")
    else if
      left + frame_width > canvas_width || top + frame_height > canvas_height
    then Error (Decode_error "Frame exceeds canvas bounds")
    else
      let has_lct = flags land 0x80 <> 0 in
      let interlaced = flags land 0x40 <> 0 in
      let lct_size = if has_lct then 1 lsl ((flags land 0x07) + 1) else 0 in

      (* Read local color table if present *)
      let read_lct () =
        if has_lct then (
          if !pos + (lct_size * 3) > String.length data then
            Error (Decode_error "Truncated local color table")
          else
            match Reader.read_color_table data !pos lct_size with
            | Error e -> Error e
            | Ok table ->
                pos := !pos + (lct_size * 3);
                Ok (Some table))
        else Ok None
      in

      read_lct () >>= fun local_color_table ->
      (* Read LZW minimum code size *)
      if !pos >= String.length data then
        Error (Decode_error "Missing LZW minimum code size")
      else
        let min_code_size = Char.code data.[!pos] in
        if min_code_size < 2 || min_code_size > 8 then
          Error
            (Decode_error
               (Printf.sprintf "Invalid LZW minimum code size: %d" min_code_size))
        else (
          pos := !pos + 1;

          (* Read and decompress image data *)
          Reader.read_sub_blocks data pos >>= fun compressed_data ->
          Lzw.Decoder.decompress compressed_data min_code_size
          >>= fun decompressed_str ->
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
            Error
              (Decode_error
                 (Printf.sprintf
                    "Decompressed data length %d does not match frame size \
                     %dx%d (expected %d)"
                    (Bytes.length pixel_data) frame_width frame_height
                    expected_len))
          else
            (* Convert local color table to palette *)
            let convert_lct = function
              | None -> Ok None
              | Some lct -> (
                  match Palette.of_array lct with
                  | Error _ -> Error (Decode_error "Invalid local color table")
                  | Ok p -> Ok (Some p))
            in
            convert_lct local_color_table >>= fun local_palette ->
            Ok
              {
                left;
                top;
                frame_width;
                frame_height;
                local_palette;
                pixel_data;
              }))

(** {1 Main decoder state} *)

type decoder_state = {
  data : string;
  mutable pos : int;
  mutable frames : frame list;
  mutable current_gce : gce_info option;
  mutable loop_count : int option;
  mutable has_trailer : bool;
}

(** Create frame from parsed image data and current GCE *)
let create_frame image_data gce =
  {
    width = image_data.frame_width;
    height = image_data.frame_height;
    x_offset = image_data.left;
    y_offset = image_data.top;
    delay_cs = (match gce with Some g -> g.gce_delay | None -> 0);
    disposal = (match gce with Some g -> g.gce_disposal | None -> No_disposal);
    transparent_index =
      (match gce with Some g -> g.gce_transparent_index | None -> None);
    local_palette = image_data.local_palette;
    pixels = image_data.pixel_data;
  }

(** Parse all blocks in the GIF *)
let rec parse_blocks state ~canvas_width ~canvas_height =
  if state.pos >= String.length state.data || state.has_trailer then Ok ()
  else
    match state.data.[state.pos] with
    | '\x21' -> parse_extension state ~canvas_width ~canvas_height
    | '\x2C' -> parse_image state ~canvas_width ~canvas_height
    | '\x3B' ->
        state.has_trailer <- true;
        state.pos <- state.pos + 1;
        Ok ()
    | '\x00' ->
        (* Sometimes encoders add null padding *)
        state.pos <- state.pos + 1;
        parse_blocks state ~canvas_width ~canvas_height
    | _ ->
        Error
          (Decode_error
             (Printf.sprintf "Unknown block type 0x%02X at position %d"
                (Char.code state.data.[state.pos])
                state.pos))

and parse_extension state ~canvas_width ~canvas_height =
  if state.pos + 2 > String.length state.data then
    Error (Decode_error "Truncated extension block")
  else
    let label = state.data.[state.pos + 1] in
    state.pos <- state.pos + 2;
    let pos_ref = ref state.pos in
    match label with
    | '\xF9' ->
        Extensions.parse_gce state.data pos_ref >>= fun gce ->
        state.pos <- !pos_ref;
        state.current_gce <- Some gce;
        parse_blocks state ~canvas_width ~canvas_height
    | '\xFF' ->
        Extensions.parse_app_extension state.data pos_ref >>= fun loop_opt ->
        state.pos <- !pos_ref;
        (match loop_opt with
        | Some l -> state.loop_count <- Some l
        | None -> ());
        parse_blocks state ~canvas_width ~canvas_height
    | '\xFE' ->
        Extensions.parse_comment state.data pos_ref >>= fun _comment ->
        state.pos <- !pos_ref;
        parse_blocks state ~canvas_width ~canvas_height
    | '\x01' ->
        Extensions.parse_plain_text state.data pos_ref >>= fun () ->
        state.pos <- !pos_ref;
        parse_blocks state ~canvas_width ~canvas_height
    | _ ->
        Reader.skip_sub_blocks state.data pos_ref >>= fun () ->
        state.pos <- !pos_ref;
        parse_blocks state ~canvas_width ~canvas_height

and parse_image state ~canvas_width ~canvas_height =
  let pos_ref = ref state.pos in
  parse_image_data state.data pos_ref ~canvas_width ~canvas_height
  >>= fun image_data ->
  state.pos <- !pos_ref;
  let frame = create_frame image_data state.current_gce in
  state.frames <- frame :: state.frames;
  state.current_gce <- None;
  parse_blocks state ~canvas_width ~canvas_height

(** {1 Main decoder function} *)

type header_info = {
  header : string;
  width : int;
  height : int;
  has_gct : bool;
  gct_size : int;
  background_index : int;
}
(** Header information *)

(** Decode GIF header and logical screen descriptor *)
let decode_header data =
  (* Check header *)
  if String.length data < 6 then
    Error (Decode_error "File too small for GIF header")
  else
    let header = String.sub data 0 6 in
    if header <> "GIF87a" && header <> "GIF89a" then
      Error (Decode_error "Invalid GIF header")
    else if String.length data < 13 then
      Error (Decode_error "File too small for logical screen descriptor")
    else
      (* Read logical screen descriptor *)
      Reader.read_u16 data 6 >>= fun width ->
      Reader.read_u16 data 8 >>= fun height ->
      let packed = Char.code data.[10] in
      let has_gct = packed land 0x80 <> 0 in
      let gct_size = if has_gct then 1 lsl ((packed land 0x07) + 1) else 0 in
      let background_index = Char.code data.[11] in

      (* Validate dimensions *)
      if width = 0 || height = 0 then
        Error (Decode_error "Invalid canvas dimensions")
      else if width > 65535 || height > 65535 then
        Error (Decode_error "Canvas dimensions exceed maximum")
      else Ok { header; width; height; has_gct; gct_size; background_index }

(** Main decode function *)
let decode data =
  decode_header data >>= fun header_info ->
  let pos = ref 13 in

  (* Read global color table if present *)
  let read_gct () =
    if header_info.has_gct then (
      if !pos + (header_info.gct_size * 3) > String.length data then
        Error (Decode_error "Truncated global color table")
      else
        match Reader.read_color_table data !pos header_info.gct_size with
        | Error e -> Error e
        | Ok table ->
            pos := !pos + (header_info.gct_size * 3);
            Ok (Some table))
    else Ok None
  in

  read_gct () >>= fun global_color_table ->
  (* Create decoder state *)
  let state =
    {
      data;
      pos = !pos;
      frames = [];
      current_gce = None;
      loop_count = None;
      has_trailer = false;
    }
  in

  (* Parse all blocks *)
  parse_blocks state ~canvas_width:header_info.width
    ~canvas_height:header_info.height
  >>= fun () ->
  (* Validate results *)
  if not state.has_trailer then Error (Decode_error "Missing GIF trailer")
  else if state.frames = [] && header_info.header = "GIF89a" then
    Error (Decode_error "No frames found in GIF89a")
  else
    (* Convert global color table to palette *)
    match global_color_table with
    | None -> Error (Decode_error "No global color table found")
    | Some ct -> (
        match Palette.of_array ct with
        | Error _ -> Error (Decode_error "Invalid global color table")
        | Ok global_palette ->
            Ok
              {
                width = header_info.width;
                height = header_info.height;
                global_palette;
                background_index = header_info.background_index;
                loop_count = state.loop_count;
                frames = List.rev state.frames;
              })

(** Decode a GIF and return just the frames *)
let decode_frames gif_data =
  match decode gif_data with Ok info -> Ok info.frames | Error _ as e -> e
