(*
 * This file is part of Imagelib.
 *
 * Imagelib is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Imabelib is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Imabelib.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) 2014 Rodolphe Lepigre.
 * Modified to be standalone.
 *)

let debug = ref false
let png_signature = "\137PNG\013\010\026\010"

(** Exceptions *)
exception Corrupted_image of string

(** Basic types *)
type chunk = { chunk_type : string; chunk_data : string }

type ihdr_data = {
  image_size : int * int;
  bit_depth : int;
  colour_type : int;
  compression_method : int;
  filter_method : int;
  interlace_method : int;
}

type pixel = { r : int; g : int; b : int }

(** Image representation *)
type pixel_data =
  | Grey of int array array
  | GreyA of (int * int) array array  
  | RGB of (int * int * int) array array
  | RGBA of (int * int * int * int) array array

type image = {
  width : int;
  height : int;
  max_val : int;
  pixels : pixel_data;
}

(** Chunk reader/writer abstractions *)
type chunk_reader = {
  read_byte : unit -> char;
  read_bytes : int -> string;
  close : unit -> unit;
}

type chunk_writer = {
  write_byte : char -> unit;
  write_bytes : string -> unit;
  close : unit -> unit;
}

(** Helper functions from ImageUtil *)
let int_of_char c = Char.code c

let pow_of_2 n = 1 lsl n

let ones n = (1 lsl n) - 1

let int32_of_str4 s =
  let b0 = Int32.of_int (int_of_char s.[0]) in
  let b1 = Int32.of_int (int_of_char s.[1]) in
  let b2 = Int32.of_int (int_of_char s.[2]) in
  let b3 = Int32.of_int (int_of_char s.[3]) in
  Int32.logor (Int32.shift_left b0 24)
    (Int32.logor (Int32.shift_left b1 16)
       (Int32.logor (Int32.shift_left b2 8) b3))

let int_of_str4 s =
  let b0 = int_of_char s.[0] in
  let b1 = int_of_char s.[1] in
  let b2 = int_of_char s.[2] in
  let b3 = int_of_char s.[3] in
  (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3

let int_of_str2_le s =
  let b0 = int_of_char s.[0] in
  let b1 = int_of_char s.[1] in
  (b0 lsl 8) lor b1

let int_to_str4 i =
  let b0 = (i lsr 24) land 0xff in
  let b1 = (i lsr 16) land 0xff in
  let b2 = (i lsr 8) land 0xff in
  let b3 = i land 0xff in
  Bytes.init 4 (function
    | 0 -> Char.chr b0
    | 1 -> Char.chr b1
    | 2 -> Char.chr b2
    | 3 -> Char.chr b3
    | _ -> assert false)

let get_bytes ich n = ich.read_bytes n
let chunk_byte ich = ich.read_byte ()
let chunk_write och s = och.write_bytes s
let close_chunk_reader (ich : chunk_reader) = ich.close ()
let close_chunk_writer (och : chunk_writer) = och.close ()

(** Create chunk reader from input channel *)
let chunk_reader_of_in_channel ic = {
  read_byte = (fun () -> input_char ic);
  read_bytes = (fun n ->
    let buf = Bytes.create n in
    really_input ic buf 0 n;
    Bytes.to_string buf);
  close = (fun () -> close_in ic);
}

(** Create chunk writer from output channel *)
let chunk_writer_of_out_channel oc = {
  write_byte = (fun c -> output_char oc c);
  write_bytes = (fun s -> output_string oc s);
  close = (fun () -> close_out oc);
}

(** Create chunk writer from buffer *)
let chunk_writer_of_buffer buf = {
  write_byte = (fun c -> Buffer.add_char buf c);
  write_bytes = (fun s -> Buffer.add_string buf s);
  close = (fun () -> ());
}

(** Image creation functions *)
let create_grey ~alpha ~max_val width height =
  let pixels = 
    if alpha then
      GreyA (Array.init height (fun _ -> Array.make width (0, max_val)))
    else
      Grey (Array.init height (fun _ -> Array.make width 0))
  in
  { width; height; max_val; pixels }

let create_rgb ~alpha ~max_val width height =
  let pixels = 
    if alpha then
      RGBA (Array.init height (fun _ -> Array.make width (0, 0, 0, max_val)))
    else
      RGB (Array.init height (fun _ -> Array.make width (0, 0, 0)))
  in
  { width; height; max_val; pixels }

(** Image write functions *)
let write_grey img x y g =
  match img.pixels with
  | Grey arr -> arr.(y).(x) <- g
  | _ -> failwith "write_grey: not a grey image"

let write_greya img x y g a =
  match img.pixels with
  | GreyA arr -> arr.(y).(x) <- (g, a)
  | _ -> failwith "write_greya: not a grey+alpha image"

let write_rgb img x y r g b =
  match img.pixels with
  | RGB arr -> arr.(y).(x) <- (r, g, b)
  | _ -> failwith "write_rgb: not an RGB image"

let write_rgba img x y r g b a =
  match img.pixels with
  | RGBA arr -> arr.(y).(x) <- (r, g, b, a)
  | _ -> failwith "write_rgba: not an RGBA image"

(** Image read functions *)
let read_grey img x y f =
  match img.pixels with
  | Grey arr -> f arr.(y).(x)
  | _ -> failwith "read_grey: not a grey image"

let read_greya img x y f =
  match img.pixels with
  | GreyA arr -> let (g, a) = arr.(y).(x) in f g a
  | _ -> failwith "read_greya: not a grey+alpha image"

let read_rgb img x y f =
  match img.pixels with
  | RGB arr -> let (r, g, b) = arr.(y).(x) in f r g b
  | _ -> failwith "read_rgb: not an RGB image"

let read_rgba img x y f =
  match img.pixels with
  | RGBA arr -> let (r, g, b, a) = arr.(y).(x) in f r g b a
  | _ -> failwith "read_rgba: not an RGBA image"

(****************************************************************************
 * Zlib compression functions using camlzip
 ****************************************************************************)
module PNG_Zlib : sig
  val uncompress_string : string -> string
  val compress_string : string -> string
end = struct
  let uncompress_string input : string =
    (* Use Zlib module from camlzip for decompression *)
    let buffer = Buffer.create (String.length input * 2) in
    let input_pos = ref 0 in
    let refill buf =
      let remaining = String.length input - !input_pos in
      let n = min remaining (Bytes.length buf) in
      Bytes.blit_string input !input_pos buf 0 n;
      input_pos := !input_pos + n;
      n
    in
    let flush buf len =
      Buffer.add_subbytes buffer buf 0 len
    in
    Zlib.uncompress refill flush;
    Buffer.contents buffer

  let compress_string input : string =
    (* Use Zlib module from camlzip for compression *)
    let buffer = Buffer.create (String.length input) in
    let input_pos = ref 0 in
    let refill buf =
      let remaining = String.length input - !input_pos in
      let n = min remaining (Bytes.length buf) in
      Bytes.blit_string input !input_pos buf 0 n;
      input_pos := !input_pos + n;
      n
    in
    let flush buf len =
      Buffer.add_subbytes buffer buf 0 len
    in
    Zlib.compress refill flush;
    Buffer.contents buffer
end

open PNG_Zlib

(****************************************************************************
 * CRC-related things.                                                      *
 ****************************************************************************)
module PNG_CRC = struct
  let ( >> ) = Int32.shift_right_logical
  let ( & ) = Int32.logand
  let ( ^ ) = Int32.logxor

  let crc_table =
    let elem n =
      let c = ref (Int32.of_int n) in
      for _ = 0 to 7 do
        c := (!c >> 1) ^ (0xedb88320l & Int32.succ (Int32.lognot (!c & 1l)))
      done;
      !c
    in
    Array.init 256 elem

  let update_crc crc buf len =
    let c = ref crc in
    for n = 0 to len - 1 do
      let e = Int32.of_int (int_of_char buf.[n]) in
      c := crc_table.(Int32.to_int ((!c ^ e) & 0xffl)) ^ (!c >> 8)
    done;
    !c

  let png_crc buf len = Int32.lognot (update_crc 0xffffffffl buf len)
end

open PNG_CRC

(****************************************************************************
 * Core PNG function                                                        *
 ****************************************************************************)
module ReadPNG = struct
  let extensions = [ "png" ]

  (* Checks for the PNG signature
   * Arguments:
   *   - ich : input channel.
   *)
  let read_signature (ich : chunk_reader) =
    let hdr = get_bytes ich 8 in
    if String.sub hdr 1 3 = "PNG" then (
      if hdr <> png_signature then raise (Corrupted_image "Corrupted header..."))
    else raise (Corrupted_image "Invalid PNG header...")

  (* Read one PNG chunk, and check the CRC.
   * Arguments:
   *   - ich : input channel
   * Returns chunk data.
   *)
  let read_chunk ich =
    let length = int32_of_str4 (get_bytes ich 4) in
    let real_length_read = Int32.add length 4l in
    if
      real_length_read < 4l (* <-- check for overflow*)
      (* check that it's safe to cast to int: *)
      || Int32.(of_int (to_int real_length_read)) <> real_length_read
      (* so on x86 we can't read strings > 16 MB (...): *)
      || Int32.to_int real_length_read > Sys.max_string_length
    then
      raise (Corrupted_image "Size of chunk greater than OCaml can handle...");
    let length = Int32.to_int length in
    (* FIXME unsafe for large chunks *)
    try
      let data = get_bytes ich (length + 4) in
      let str_crc = get_bytes ich 4 in
      let expected_crc = int32_of_str4 str_crc in
      let crc = png_crc data (length + 4) in
      if expected_crc <> crc then raise (Corrupted_image "CRC error...");
      {
        chunk_type = String.sub data 0 4;
        chunk_data = String.sub data 4 length;
      }
    with End_of_file ->
      raise
        (Corrupted_image "Reached end of file while looking for end of chunk")

  (* Read data form the IHDR header.
   * Arguments:
   *   - s : string containing the data of the IHDR chunk.
   * Returns IHDR data.
   *)
  let data_from_ihdr s =
    if String.length s < 13 then
      raise (Corrupted_image "IHDR chunk is too small");
    (* FIXME problem with very wide images (more that 2^30 - 1 pixels) *)
    let image_width = int_of_str4 (String.sub s 0 4) in
    let image_height = int_of_str4 (String.sub s 4 4) in
    if image_height < 1 || image_width < 1 then
      raise
        (Corrupted_image
           (Printf.sprintf "PNG IHDR chunk with invalid dimensions %dx%d"
              image_width image_height));
    let bit_depth = int_of_char s.[8] in
    let colour_type = int_of_char s.[9] in
    let valid =
      match (colour_type, bit_depth) with
      | 0, 1
      | 0, 2
      | 0, 4
      | 0, 8
      | 0, 16
      | 2, 8
      | 2, 16
      | 3, 1
      | 3, 2
      | 3, 4
      | 3, 8
      | 4, 8
      | 4, 16
      | 6, 8
      | 6, 16 ->
          true
      | _ -> false
    in
    (if not valid then
       let msg =
         Printf.sprintf
           "Unsupported combination of colour type %X with bit depth %X..."
           colour_type bit_depth
       in
       raise (Corrupted_image msg));
    let compression_method = int_of_char s.[10] in
    (if compression_method <> 0 then
       let msg =
         Printf.sprintf
           "Unsupported compression method %X (only code %X is standard)..."
           compression_method 0
       in
       raise (Corrupted_image msg));
    let filter_method = int_of_char s.[11] in
    (if filter_method <> 0 then
       let msg =
         Printf.sprintf
           "Unsupported filter method %X (only code %X is standard)..."
           compression_method 0
       in
       raise (Corrupted_image msg));
    let interlace_method = int_of_char s.[12] in
    (if interlace_method <> 0 && interlace_method <> 1 then
       let msg =
         Printf.sprintf
           "Unsupported interlace method %X (only %X and %X are standard)..."
           interlace_method 0 1
       in
       raise (Corrupted_image msg));
    {
      image_size = (image_width, image_height);
      bit_depth;
      colour_type;
      compression_method;
      filter_method;
      interlace_method;
    }

  (* Read the size of a PNG image.
   * Arguments:
   *   - fn : filename.
   * Returns a couble (width, height).
   * Note: the image is not checked for inconsistency, only the signature and
   * header are checked.
   *)
  let size ich =
    read_signature ich;
    let ihdr_chunk = read_chunk ich in
    if ihdr_chunk.chunk_type <> "IHDR" then
      raise (Corrupted_image "First chunk should be of type IHDR...");
    let ihdr = data_from_ihdr ihdr_chunk.chunk_data in
    close_chunk_reader ich;
    ihdr.image_size

  (* Removes the filter on a scanline.
   * Arguments:
   *   - ftype : filter type.
   *   - bpp : number of bytes per pixels.
   *   - scanline : the scanline to filter.
   *   - prev_scanline : previous scanline with filter removed.
   * Returns the unfiltered scanline.
   *)
  let unfilter ftype bpp scanline prev_scanline : string =
    let paeth_predictor a b c =
      let p = a + b - c in
      let pa = abs (p - a) in
      let pb = abs (p - b) in
      let pc = abs (p - c) in
      if pa <= pb && pa <= pc then a else if pb <= pc then b else c
    in

    let slen = String.length scanline in
    let unfiltered = Bytes.create slen in

    for x = 0 to slen - 1 do
      let filtx = int_of_char scanline.[x] in
      let recona =
        let j = x - bpp in
        if j < 0 then 0 else int_of_char (Bytes.get unfiltered j)
      in
      let reconb =
        match prev_scanline with None -> 0 | Some psl -> int_of_char psl.[x]
      in
      let reconc =
        let j = x - bpp in
        if j < 0 then 0
        else
          match prev_scanline with None -> 0 | Some psl -> int_of_char psl.[j]
      in
      let recon =
        match ftype with
        | 0 -> filtx
        | 1 -> (filtx + recona) mod 256
        | 2 -> (filtx + reconb) mod 256
        | 3 -> (filtx + ((recona + reconb) / 2)) mod 256
        | 4 -> (filtx + paeth_predictor recona reconb reconc) mod 256
        | _ ->
            let msg = Printf.sprintf "Unknown filter type (%i)..." ftype in
            raise (Corrupted_image msg)
      in
      Bytes.set unfiltered x (char_of_int recon)
    done;
    Bytes.to_string unfiltered

  (*
   * Pass extraction function.
   * Arguments :
   *   - s : the string containing the data.
   *   - pl_bit : number of bits per pixel.
   *   - w, h : width and height of the image in pixels.
   * Returns an array of strings containing image rows.
   *)
  let extract_pass s pl_bit w h =
    let starting_row = [| 0; 0; 4; 0; 2; 0; 1 |] in
    let starting_col = [| 0; 4; 0; 2; 0; 1; 0 |] in
    let row_increment = [| 8; 8; 8; 4; 4; 2; 2 |] in
    let col_increment = [| 8; 8; 4; 4; 2; 2; 1 |] in
    (*let block_height  = [| 8; 8; 4; 4; 2; 2; 1 |] in*)
    (*let block_width   = [| 8; 4; 4; 2; 2; 1; 1 |] in*)

    let rowsize_bit = w * pl_bit in
    let rowsize = (rowsize_bit / 8) + if rowsize_bit mod 8 <> 0 then 1 else 0 in
    let zchar = char_of_int 0 in
    let output = Array.init h (fun _ -> Bytes.make rowsize zchar) in

    let input_byte = ref 0 in
    let input_bit = ref 0 in

    let read_byte () =
      if !input_bit = 0 then
        raise (Corrupted_image "PNG extract_pass: input_bit is 0");
      let c = String.get s !input_byte in
      incr input_byte;
      int_of_char c
    in

    let read_pix str pixnum =
      if pl_bit mod 8 = 0 then
        let bpp = pl_bit / 8 in
        let offset = pixnum * bpp in
        String.sub str offset bpp
      else
        let bitoffset = pl_bit * pixnum in
        let byte =
          if bitoffset / 8 >= String.length str then (
            if !debug then
              Printf.eprintf "Warning: read_pix: bitoffset out of bound...";
            255)
          else int_of_char str.[bitoffset / 8]
        in
        let bitpos = bitoffset mod 8 in
        let mask = ones pl_bit lsl (8 - pl_bit) in
        let pix = (byte lsl bitpos) land mask in
        let res = String.make 1 (char_of_int pix) in
        res
    in

    let read_pixel () =
      if pl_bit mod 8 = 0 then (
        let bpp = pl_bit / 8 in
        let res = String.sub s !input_byte bpp in
        input_byte := !input_byte + bpp;
        res)
      else
        let byte = int_of_char s.[!input_byte] in
        let mask = ones pl_bit lsl (8 - pl_bit) in
        let pix = (byte lsl !input_bit) land mask in
        input_bit := !input_bit + pl_bit;
        if !input_bit > 7 then (
          input_bit := 0;
          incr input_byte);
        String.make 1 (char_of_int pix)
    in

    let flush_end_of_byte () =
      if !input_bit <> 0 then (
        input_bit := 0;
        incr input_byte)
    in

    (* Writes the pixel pix at pixel position pos in the `bytes` str. *)
    let output_pixel pix pos str =
      if pl_bit mod 8 = 0 then
        let bpp = pl_bit / 8 in
        let offset = pos * bpp in
        String.blit pix 0 str offset bpp
      else
        let pixv = int_of_char pix.[0] in
        let bitpos = pos * pl_bit in
        let byte = bitpos / 8 in
        let bit = bitpos mod 8 in
        let content = int_of_char @@ Bytes.get str byte in
        let mask = lnot ((ones pl_bit lsl (8 - pl_bit)) lsr bit) in
        let newcontent = content land mask lor (pixv lsr bit) in
        Bytes.set str byte (char_of_int newcontent)
    in

    let sl = Bytes.make (w * 8) zchar in
    (* ugly... (2bytes x 4 component) *)
    let slpos = ref 0 in

    for pass = 0 to 6 do
      let prevsl = ref None in

      let row = ref starting_row.(pass) in
      while !row < h do
        let ft = ref (-1) in

        slpos := 0;
        let col = ref starting_col.(pass) in
        while !col < w do
          if !ft < 0 then ft := read_byte ();

          let pix = read_pixel () in

          output_pixel pix !slpos sl;
          incr slpos;

          col := !col + col_increment.(pass)
        done;
        flush_end_of_byte ();

        if !ft >= 0 then (
          let bitlen = !slpos * pl_bit in
          let sllen = (bitlen / 8) + if bitlen mod 8 = 0 then 0 else 1 in
          (if bitlen mod 8 <> 0 then
             let nbbits = bitlen mod 8 in
             let mask = ones nbbits lsl (8 - nbbits) in
             let last = int_of_char @@ Bytes.get sl (sllen - 1) in
             Bytes.set sl (sllen - 1) (char_of_int (last land mask)));
          let sl = Bytes.sub sl 0 sllen in
          let bpp = max (pl_bit / 8) 1 in
          let slunfilt = unfilter !ft bpp (Bytes.to_string sl) !prevsl in
          prevsl := Some slunfilt;

          col := starting_col.(pass);
          slpos := 0;
          while !col < w do
            let pix = read_pix slunfilt !slpos in
            incr slpos;
            output_pixel pix !col output.(!row);

            col := !col + col_increment.(pass)
          done);

        row := !row + row_increment.(pass)
      done
    done;
    output

  let parsefile ich =
    read_signature ich;

    let curr_chunk = ref (read_chunk ich) in
    let read_chunks = ref [] in

    let only_once ctype =
      if List.mem ctype !read_chunks then (
        close_chunk_reader ich;
        raise
          (Corrupted_image
             (Printf.sprintf "Chunk %s should not appear more than once..."
                ctype)))
    in

    let only_before ctype ctype' =
      if List.mem ctype' !read_chunks then (
        close_chunk_reader ich;
        raise
          (Corrupted_image
             (Printf.sprintf "Chunk %s should appear before chunk %s..." ctype
                ctype')))
    in

    let only_after ctype' ctype =
      if not (List.mem ctype' !read_chunks) then (
        close_chunk_reader ich;
        raise
          (Corrupted_image
             (Printf.sprintf "Chunk %s should appear after chunk %s..." ctype
                ctype')))
    in

    let is_first_chunk ctype =
      if [] <> !read_chunks then (
        close_chunk_reader ich;
        raise
          (Corrupted_image
             (Printf.sprintf "Chunk %s can only be the first chunk..." ctype)))
    in

    let is_not_first_chunk ctype =
      if [] = !read_chunks then (
        close_chunk_reader ich;
        raise
          (Corrupted_image
             (Printf.sprintf "Chunk %s cannot be the first chunk..." ctype)))
    in

    let is_not_compatible_with ctype ctype' =
      if List.mem ctype' !read_chunks then (
        close_chunk_reader ich;
        raise
          (Corrupted_image
             (Printf.sprintf "Chunk %s is not compatible with chunk %s..." ctype
                ctype')))
    in

    let last_chunk () = match !read_chunks with [] -> "NONE" | x :: _ -> x in

    let has_read_chunk ctype = List.mem ctype !read_chunks in

    let not_after ctype' ctype =
      if List.mem ctype' !read_chunks then (
        close_chunk_reader ich;
        raise
          (Corrupted_image
             (Printf.sprintf "Chunk %s cannot appear after chunk %s..." ctype
                ctype')))
    in

    let empty_ihdr =
      {
        image_size = (-1, -1);
        bit_depth = -1;
        colour_type = -1;
        compression_method = -1;
        filter_method = -1;
        interlace_method = -1;
      }
    in
    let ihdr = ref empty_ihdr in
    let palette = ref [||] in
    let raw_idat = ref "" in
    let aspect_ratio = ref None in
    let pixel_size = ref None in

    (* Alpha values specified by tRNS chunk (simple transparency). *)
    let palette_alpha = ref [||] in
    (* TODO check that the values are not corrupted. *)
    let grey_level_alpha : int option ref = ref None in
    let true_color_alpha : pixel option ref = ref None in

    (try
       while !curr_chunk.chunk_type <> "IEND" do
         let curr_ctype = !curr_chunk.chunk_type in
         (match curr_ctype with
         (* Critical chunks *)
         | "IHDR" ->
             only_once curr_ctype;
             is_first_chunk curr_ctype;
             ihdr := data_from_ihdr !curr_chunk.chunk_data;
             if !debug then (
               Printf.fprintf stderr "IHDR content:\n%!";
               let w, h = !ihdr.image_size in
               Printf.fprintf stderr "  - image size:         %ix%i\n%!" w h;
               Printf.fprintf stderr "  - bit depth:          %i\n%!"
                 !ihdr.bit_depth;
               Printf.fprintf stderr "  - colour type:        %i\n%!"
                 !ihdr.colour_type;
               Printf.fprintf stderr "  - compression_method: %i\n%!"
                 !ihdr.compression_method;
               Printf.fprintf stderr "  - filter_method:      %i\n%!"
                 !ihdr.filter_method;
               Printf.fprintf stderr "  - interlace_method:   %i\n%!"
                 !ihdr.interlace_method)
         | "PLTE" ->
             is_not_first_chunk curr_ctype;
             only_before curr_ctype "IDAT";
             only_once curr_ctype;
             not_after "tRNS" curr_ctype;
             not_after "bKGD" curr_ctype;

             let ct = !ihdr.colour_type in
             (if ct = 0 || ct = 4 then
                let msg =
                  Printf.sprintf
                    "Chunk PLTE is forbiden for greyscale mode (%i)..." ct
                in
                raise (Corrupted_image msg));

             let bytes_palette = String.length !curr_chunk.chunk_data in
             if bytes_palette mod 3 <> 0 then
               raise (Corrupted_image "Invalid palette size...");
             let palette_length = bytes_palette / 3 in
             (if palette_length > pow_of_2 !ihdr.bit_depth then
                let msg =
                  Printf.sprintf
                    "Palette length %d exceeds maximum palette size %i for bit \
                     depth %i.\n\
                     %!"
                    palette_length (pow_of_2 !ihdr.bit_depth) !ihdr.bit_depth
                in
                raise (Corrupted_image msg));
             palette :=
               Array.init palette_length (fun i ->
                   {
                     r = int_of_char !curr_chunk.chunk_data.[i * 3];
                     g = int_of_char !curr_chunk.chunk_data.[(i * 3) + 1];
                     b = int_of_char !curr_chunk.chunk_data.[(i * 3) + 2];
                   });
             if !debug then
               Printf.fprintf stderr "PLTE with %i lines\n%!" palette_length
         | "IDAT" ->
             is_not_first_chunk curr_ctype;
             if has_read_chunk curr_ctype && last_chunk () <> curr_ctype then
               raise (Corrupted_image "Chunks IDAT should be consecutive...");
             raw_idat := String.concat "" [ !raw_idat; !curr_chunk.chunk_data ];
             if !debug then
               Printf.fprintf stderr "IDAT (raw data is now %i bytes long)\n%!"
                 (String.length !raw_idat)
         (* IEND cannot occur (end condition of the loop) *)
         (* | "IEND" -> *)

         (* Ancillary chunks *)
         | "cHRM" ->
             only_once curr_ctype;
             is_not_first_chunk curr_ctype;
             only_before curr_ctype "PLTE";
             only_before curr_ctype "IDAT";
             (* TODO *)
             if !debug then Printf.fprintf stderr "cHRM chunk ignored\n%!"
         | "gAMA" ->
             only_once curr_ctype;
             is_not_first_chunk curr_ctype;
             only_before curr_ctype "PLTE";
             only_before curr_ctype "IDAT";
             (* TODO *)
             if !debug then Printf.fprintf stderr "gAMA chunk ignored\n%!"
         | "iCCP" ->
             only_once curr_ctype;
             is_not_first_chunk curr_ctype;
             only_before curr_ctype "PLTE";
             only_before curr_ctype "IDAT";
             is_not_compatible_with curr_ctype "sRGB";
             (* TODO *)
             if !debug then Printf.fprintf stderr "iCPP chunk ignored\n%!"
         | "sBIT" ->
             only_once curr_ctype;
             is_not_first_chunk curr_ctype;
             only_before curr_ctype "PLTE";
             only_before curr_ctype "IDAT";
             (* TODO *)
             if !debug then Printf.fprintf stderr "sBIT chunk ignored\n%!"
         | "sRGB" ->
             only_once curr_ctype;
             is_not_first_chunk curr_ctype;
             only_before curr_ctype "PLTE";
             only_before curr_ctype "IDAT";
             is_not_compatible_with curr_ctype "iCPP";
             (* TODO *)
             if !debug then Printf.fprintf stderr "sRGB chunk ignored\n%!"
         | "bKGD" ->
             only_once curr_ctype;
             only_before curr_ctype "IDAT";
             (* TODO *)
             if !debug then Printf.fprintf stderr "bKGD chunk ignored\n%!"
         | "hIST" ->
             only_once curr_ctype;
             only_after "PLTE" curr_ctype;
             only_before curr_ctype "IDAT";
             (* TODO *)
             if !debug then Printf.fprintf stderr "hIST chunk ignored\n%!"
         | "tRNS" -> (
             only_once curr_ctype;
             only_before curr_ctype "IDAT";

             let data = !curr_chunk.chunk_data in
             let data_len = String.length data in
             match !ihdr.colour_type with
             | 0 ->
                 if data_len <> 2 then
                   Printf.eprintf "Corrupted tRNS chunk (ignored).\n%!"
                 else
                   let alpha = int_of_str2_le data in
                   grey_level_alpha := Some alpha;
                   if !debug then
                     Printf.eprintf "tRNS with alpha value %i.\n%!" alpha
             | 2 ->
                 if data_len <> 6 then
                   Printf.eprintf "Corrupted tRNS chunk (ignored).\n%!"
                 else
                   let r = int_of_str2_le (String.sub data 0 2) in
                   let g = int_of_str2_le (String.sub data 2 2) in
                   let b = int_of_str2_le (String.sub data 4 2) in
                   true_color_alpha := Some { r; g; b };
                   if !debug then
                     Printf.eprintf "tRNS with alpha value (%i,%i,%i).\n%!" r g
                       b
             | 3 ->
                 if data_len > Array.length !palette then
                   Printf.eprintf "Corrupted tRNS chunk (ignored).\n%!"
                 else
                   let init i = int_of_char data.[i] in
                   palette_alpha := Array.init data_len init;
                   if !debug then (
                     Printf.eprintf "tRNS with alpha values for palette:";
                     Array.iter (Printf.eprintf " %i") !palette_alpha;
                     Printf.eprintf ".\n%!")
             | c ->
                 if !debug then (
                   Printf.eprintf "tRNS chunk is prohibited for ";
                   Printf.eprintf "color type %i (ignored).\n%!" c))
         | "pHYs" -> (
             only_once curr_ctype;
             is_not_first_chunk curr_ctype;
             only_before curr_ctype "IDAT";
             let data = !curr_chunk.chunk_data in
             if String.length data <> 9 then
               raise (Corrupted_image "Invalid pHYs chunk size");
             let sx = int_of_str4 (String.sub data 0 4) in
             let sy = int_of_str4 (String.sub data 4 4) in
             match int_of_char (String.get data 8) with
             | 0 ->
                 if !debug then Printf.eprintf "Aspect ratio is %i / %i\n" sx sy;
                 aspect_ratio := Some (sx, sy) (* Unknown unit *)
             | 1 ->
                 if !debug then (
                   Printf.eprintf "Pixel size X axis: %i px/m\n" sx;
                   Printf.eprintf "Pixel size Y axis: %i px/m\n" sy);
                 pixel_size := Some (sx, sy) (* Unit is pixel / metre *)
             | _ -> raise (Corrupted_image "Bad unit in pHYs chunk..."))
         | "sPLT" ->
             is_not_first_chunk curr_ctype;
             only_before curr_ctype "IDAT";
             (* TODO *)
             if !debug then Printf.fprintf stderr "sPLT chunk ignored\n%!"
         | "tIME" ->
             only_once curr_ctype;
             is_not_first_chunk curr_ctype;
             (* TODO *)
             if !debug then Printf.fprintf stderr "tIME chunk ignored\n%!"
         | "iTXt" ->
             is_not_first_chunk curr_ctype;
             (* TODO *)
             if !debug then Printf.fprintf stderr "iTXt chunk ignored\n%!"
         | "tEXt" ->
             is_not_first_chunk curr_ctype;
             (* TODO *)
             if !debug then Printf.fprintf stderr "tEXt chunk ignored\n%!"
               (* Printf.fprintf stderr "  \"%s\"\n%!" !curr_chunk.chunk_data *)
         | "zTXt" ->
             is_not_first_chunk curr_ctype;
             (* TODO *)
             if !debug then Printf.fprintf stderr "zTXt chunk ignored\n%!"
         (* Registered extension chunks *)
         | "sCAL" ->
             only_before curr_ctype "IDAT";
             only_once curr_ctype;
             (* TODO *)
             if !debug then Printf.fprintf stderr "sCAL chunk ignored\n%!"
         | "oFFs" ->
             only_before curr_ctype "IDAT";
             only_once curr_ctype;
             (* TODO *)
             if !debug then Printf.fprintf stderr "oFFs chunk ignored\n%!"
         | "pCAL" ->
             is_not_first_chunk curr_ctype;
             only_after curr_ctype "PLTE";
             only_before curr_ctype "IDAT";
             only_once curr_ctype;
             (* TODO *)
             if !debug then Printf.fprintf stderr "pCAL chunk ignored\n%!"
         | "sTER" ->
             only_before curr_ctype "IDAT";
             only_once curr_ctype;
             (* TODO *)
             if !debug then Printf.fprintf stderr "sTER chunk ignored\n%!"
         | s when String.length s > 1 && Char.code s.[0] land 0x20 = 0x20 ->
             (* Starts with lowercase aka a private chunk,
                  which can be ignored. *)
             if !debug then Printf.fprintf stderr "%s chunk ignored\n%!" s
         | s ->
             let msg = Printf.sprintf "Unknown chunk type \"%s\"..." s in
             raise (Corrupted_image msg));
         read_chunks := !curr_chunk.chunk_type :: !read_chunks;
         curr_chunk := read_chunk ich
       done
     with End_of_file ->
       close_chunk_reader ich;
       raise (Corrupted_image "End of file reached before chunk end..."));
    read_chunks := "IEND" :: !read_chunks;
    if !debug then Printf.eprintf "IEND reached\n%!";

    (* Check for trailing bytes... *)
    if
      try
        let _ = chunk_byte ich in
        true
      with End_of_file -> false
    then raise (Corrupted_image "Data after the IEND chunk...");

    close_chunk_reader ich;

    let uncomp_idat = uncompress_string !raw_idat in

    let w, h = !ihdr.image_size in
    let bd = !ihdr.bit_depth in
    let ct = !ihdr.colour_type in
    let im = !ihdr.interlace_method in

    if w < 0 || h < 0 then
      raise (Corrupted_image "One or more dimensions are negative");

    (* Computing number of component and byte per pixel *)
    let nb_comp =
      match ct with
      | 0 -> 1
      | 2 -> 3
      | 3 -> 1
      | 4 -> 2
      | 6 -> 4
      | _ -> assert false
    in
    let bpp = match bd with 8 -> nb_comp | 16 -> 2 * nb_comp | _ -> 1 in

    let unfiltered =
      match im with
      | 0 ->
          (* Finding the lenght of the scanline *)
          let sl_bit = w * nb_comp * bd in
          let slen = (sl_bit / 8) + if sl_bit mod 8 <> 0 then 1 else 0 in
          if !debug then
            Printf.fprintf stderr "No interlace, scanline length = %i\n%!" slen;

          (* Building the scanlines *)
          let scanlines =
            Array.init h (fun y ->
                let ind = y * (slen + 1) in
                let ft = int_of_char uncomp_idat.[ind] in
                let sl = String.sub uncomp_idat (ind + 1) slen in
                (ft, sl))
          in

          (* Removing the filter on the scanlines *)
          let prev_scanline = ref None in
          Array.map
            (fun (ftype, scanline) ->
              let output = unfilter ftype bpp scanline !prev_scanline in
              prev_scanline := Some output;
              output)
            scanlines
      | 1 ->
          if !debug then Printf.fprintf stderr "Interlace method 1.\n%!";
          let pixlen_bit = nb_comp * bd in
          extract_pass uncomp_idat pixlen_bit w h |> Array.map Bytes.to_string
      | _ ->
          raise (Corrupted_image "PNG: parse_file: unfiltered -> im <> (2;1)")
    in

    (* Conversion of the array of string into an array of array of int *)
    let unfiltered_int = Array.init h (fun _ -> Array.make (w * nb_comp) 0) in

    for y = 0 to h - 1 do
      for x = 0 to (w * nb_comp) - 1 do
        match bd with
        | 16 ->
            let b1 = int_of_char unfiltered.(y).[2 * x] in
            let b2 = int_of_char unfiltered.(y).[(2 * x) + 1] in
            let v = (b1 lsl 8) + b2 in
            Array.set unfiltered_int.(y) x v
        | 8 -> Array.set unfiltered_int.(y) x (int_of_char unfiltered.(y).[x])
        | 4 ->
            let ind = x / 2 in
            let partb = x mod 2 in
            let b = int_of_char unfiltered.(y).[ind] in
            let v = if partb == 0 then b lsr 4 else b mod 16 in
            Array.set unfiltered_int.(y) x v
        | 2 ->
            let ind = x / 4 in
            let partb = x mod 4 in
            let b = int_of_char unfiltered.(y).[ind] in
            let v = (b mod pow_of_2 (2 * (4 - partb))) lsr (2 * (3 - partb)) in
            Array.set unfiltered_int.(y) x v
        | 1 ->
            let ind = x / 8 in
            let partb = x mod 8 in
            let b = int_of_char unfiltered.(y).[ind] in
            let v = (b mod pow_of_2 (8 - partb)) lsr (7 - partb) in
            Array.set unfiltered_int.(y) x v
        | _ -> raise (Corrupted_image "PNG: parse_file: bd <> [16;8;4;2;1]")
      done
    done;

    (* Output *)
    if !debug then Printf.fprintf stderr "Building image structure...\n%!";
    match ct with
    | 0 ->
        let alpha, alpha_val =
          match !grey_level_alpha with
          | Some alpha -> (true, alpha)
          | None -> (false, -1)
          (* Cannot occur. *)
        in
        let max_val = ones bd in
        let image = create_grey ~alpha ~max_val w h in
        for y = 0 to h - 1 do
          for x = 0 to w - 1 do
            let g = unfiltered_int.(y).(x) in
            if alpha then
              let alpha = if g = alpha_val then 0 else max_val in
              write_greya image x y g alpha
            else write_grey image x y g
          done
        done;
        image
    | 2 ->
        let alpha, alpha_val =
          match !true_color_alpha with
          | Some alpha -> (true, alpha)
          | None -> (false, { r = -1; g = -1; b = -1 })
          (* Cannot occur. *)
        in
        let max_val = ones bd in
        let image = create_rgb ~alpha ~max_val w h in
        for y = 0 to h - 1 do
          for x = 0 to w - 1 do
            let r = unfiltered_int.(y).(3 * x) in
            let g = unfiltered_int.(y).((3 * x) + 1) in
            let b = unfiltered_int.(y).((3 * x) + 2) in
            if alpha then
              let alpha = if { r; g; b } = alpha_val then 0 else max_val in
              write_rgba image x y r g b alpha
            else write_rgb image x y r g b
          done
        done;
        image
    | 3 ->
        (* Check for alpha channel specified by a tRNS chunk. *)
        let alpha = Array.length !palette_alpha <> 0 in
        let image = create_rgb ~alpha ~max_val:255 w h in
        for y = 0 to h - 1 do
          for x = 0 to w - 1 do
            let index = unfiltered_int.(y).(x) in
            let index =
              (* FIXME *)
              if index >= Array.length !palette then (
                Printf.fprintf stderr "Palette index too big...\n%!";
                0)
              else index
            in
            let p = !palette.(index) in
            if alpha then
              let alpha =
                try !palette_alpha.(index) with Invalid_argument _ -> 255
              in
              write_rgba image x y p.r p.g p.b alpha
            else write_rgb image x y p.r p.g p.b
          done
        done;
        image
    | 4 ->
        let image = create_grey ~alpha:true ~max_val:(ones bd) w h in
        for y = 0 to h - 1 do
          for x = 0 to w - 1 do
            write_greya image x y
              unfiltered_int.(y).(2 * x)
              unfiltered_int.(y).((2 * x) + 1)
          done
        done;
        image
    | 6 ->
        let image = create_rgb ~alpha:true ~max_val:(ones bd) w h in
        for y = 0 to h - 1 do
          for x = 0 to w - 1 do
            let r = unfiltered_int.(y).(4 * x) in
            let g = unfiltered_int.(y).((4 * x) + 1) in
            let b = unfiltered_int.(y).((4 * x) + 2) in
            let a = unfiltered_int.(y).((4 * x) + 3) in
            write_rgba image x y r g b a
          done
        done;
        image
    | _ -> raise (Corrupted_image "PNG: ct <> [0;2;3;4;6]")
end

module PngWriter = struct
  (****************************************************************************
   * PNG writing function                                                     *
   ****************************************************************************)
  let write_signature och = chunk_write och png_signature

  let write_chunk (och : chunk_writer) chunk =
    let len = String.length chunk.chunk_data in
    chunk_write och (int_to_str4 len |> Bytes.to_string);
    chunk_write och chunk.chunk_type;
    chunk_write och chunk.chunk_data;
    let type_and_data =
      String.concat "" [ chunk.chunk_type; chunk.chunk_data ]
    in
    let crc = png_crc type_and_data (len + 4) in
    let conv x = Int32.to_int x |> char_of_int in
    let crc3 = crc >> 24 & 0xFFl in
    let crc2 = crc >> 16 & 0xFFl in
    let crc1 = crc >> 8 & 0xFFl in
    let crc0 = crc & 0xFFl in
    let crc_s =
      Printf.sprintf "%c%c%c%c" (conv crc3) (conv crc2) (conv crc1) (conv crc0)
    in
    chunk_write och crc_s

  let ihdr_to_string ihdr =
    let s = Bytes.create 13 in
    Bytes.blit (int_to_str4 (fst ihdr.image_size)) 0 s 0 4;
    Bytes.blit (int_to_str4 (snd ihdr.image_size)) 0 s 4 4;
    Bytes.set s 8 (char_of_int ihdr.bit_depth);
    Bytes.set s 9 (char_of_int ihdr.colour_type);
    Bytes.set s 10 (char_of_int ihdr.compression_method);
    Bytes.set s 11 (char_of_int ihdr.filter_method);
    Bytes.set s 12 (char_of_int ihdr.interlace_method);
    Bytes.to_string s

  let output_png img (och : chunk_writer) =
    write_signature och;

    let maxv = img.max_val in
    let bd =
      let rec find_bd i = if ones i >= maxv then i else find_bd (i + 1) in
      find_bd 1
    in

    let ct =
      match img.pixels with
      | Grey _ -> 0
      | GreyA _ -> 4
      | RGB _ -> 2
      | RGBA _ -> 6
    in

    let w = img.width and h = img.height in

    let ihdr =
      {
        image_size = (w, h);
        bit_depth = bd;
        colour_type = ct;
        compression_method = 0;
        filter_method = 0;
        interlace_method = 0;
      }
    in
    let ihdr = { chunk_type = "IHDR"; chunk_data = ihdr_to_string ihdr } in
    write_chunk och ihdr;

    let buf = Buffer.create 4096 in
    let byte0 () = Buffer.add_char buf (char_of_int 0) in
    let add_byte i = Buffer.add_char buf (char_of_int i) in
    (match (img.pixels, bd) with
    | Grey _, 1 ->
        let byte = ref 0 in
        let numb = ref 0 in
        let add_bit i =
          (match !numb with
          | 0 -> byte := !byte lor (i lsl 7)
          | 1 -> byte := !byte lor (i lsl 6)
          | 2 -> byte := !byte lor (i lsl 5)
          | 3 -> byte := !byte lor (i lsl 4)
          | 4 -> byte := !byte lor (i lsl 3)
          | 5 -> byte := !byte lor (i lsl 2)
          | 6 -> byte := !byte lor (i lsl 1)
          | 7 -> byte := !byte lor i
          | _ -> raise (Corrupted_image "PNG: !numb <> [0..7]"));
          incr numb;
          if !numb = 8 then (
            add_byte !byte;
            byte := 0;
            numb := 0)
        in
        for y = 0 to h - 1 do
          byte0 ();
          (* Scanline with no filter *)
          for x = 0 to w - 1 do
            read_grey img x y (fun g -> add_bit g)
          done
        done;
        if !numb <> 0 then add_byte !byte
    | Grey _, 2 ->
        let byte = ref 0 in
        let numb = ref 0 in
        let add_quarter_byte i =
          (match !numb with
          | 0 -> byte := !byte lor (i lsl 6)
          | 1 -> byte := !byte lor (i lsl 4)
          | 2 -> byte := !byte lor (i lsl 2)
          | 3 -> byte := !byte lor i
          | _ -> raise (Corrupted_image "PNG: !numb <> [0..4]"));
          incr numb;
          if !numb = 4 then (
            add_byte !byte;
            byte := 0;
            numb := 0)
        in
        for y = 0 to h - 1 do
          byte0 ();
          (* Scanline with no filter *)
          for x = 0 to w - 1 do
            read_grey img x y (fun g -> add_quarter_byte g)
          done
        done;
        if !numb <> 0 then add_byte !byte
    | Grey _, 4 ->
        let byte = ref 0 in
        let numb = ref 0 in
        let add_half_byte i =
          (match !numb with
          | 0 -> byte := i lsl 4
          | 1 -> byte := !byte lor i
          | _ -> raise (Corrupted_image "PNG: numb <> [0;1]"));
          incr numb;
          if !numb = 2 then (
            add_byte !byte;
            byte := 0;
            numb := 0)
        in
        for y = 0 to h - 1 do
          byte0 ();
          (* Scanline with no filter *)
          for x = 0 to w - 1 do
            read_grey img x y (fun g -> add_half_byte g)
          done
        done;
        if !numb <> 0 then add_byte !byte
    | Grey _, 8 ->
        for y = 0 to h - 1 do
          byte0 ();
          (* Scanline with no filter *)
          for x = 0 to w - 1 do
            read_grey img x y (fun g -> add_byte g)
          done
        done
    | Grey _, 16 ->
        let mask = ones 8 in
        for y = 0 to h - 1 do
          byte0 ();
          (* Scanline with no filter *)
          for x = 0 to w - 1 do
            read_grey img x y (fun g ->
                add_byte (g lsr 8);
                add_byte (g land mask))
          done
        done
    | GreyA _, 8 ->
        for y = 0 to h - 1 do
          byte0 ();
          (* Scanline with no filter *)
          for x = 0 to w - 1 do
            read_greya img x y (fun g a ->
                add_byte g;
                add_byte a)
          done
        done
    | GreyA _, 16 ->
        let mask = ones 8 in
        for y = 0 to h - 1 do
          byte0 ();
          (* Scanline with no filter *)
          for x = 0 to w - 1 do
            read_greya img x y (fun g a ->
                add_byte (g lsr 8);
                add_byte (g land mask);
                add_byte (a lsr 8);
                add_byte (a land mask))
          done
        done
    | RGB _, 8 ->
        for y = 0 to h - 1 do
          byte0 ();
          (* Scanline with no filter *)
          for x = 0 to w - 1 do
            read_rgb img x y (fun r g b ->
                add_byte r;
                add_byte g;
                add_byte b)
          done
        done
    | RGB _, 16 ->
        let mask = ones 8 in
        for y = 0 to h - 1 do
          byte0 ();
          (* Scanline with no filter *)
          for x = 0 to w - 1 do
            read_rgb img x y (fun r g b ->
                add_byte (r lsr 8);
                add_byte (r land mask);
                add_byte (g lsr 8);
                add_byte (g land mask);
                add_byte (b lsr 8);
                add_byte (b land mask))
          done
        done
    | RGBA _, 8 ->
        for y = 0 to h - 1 do
          byte0 ();
          (* Scanline with no filter *)
          for x = 0 to w - 1 do
            read_rgba img x y (fun r g b a ->
                add_byte r;
                add_byte g;
                add_byte b;
                add_byte a)
          done
        done
    | RGBA _, 16 ->
        let mask = ones 8 in
        for y = 0 to h - 1 do
          byte0 ();
          (* Scanline with no filter *)
          for x = 0 to w - 1 do
            read_rgba img x y (fun r g b a ->
                add_byte (r lsr 8);
                add_byte (r land mask);
                add_byte (g lsr 8);
                add_byte (g land mask);
                add_byte (b lsr 8);
                add_byte (b land mask);
                add_byte (a lsr 8);
                add_byte (a land mask))
          done
        done
    | _ ->
        if !debug then Printf.fprintf stderr "bd: %i\n%!" bd;
        raise (Corrupted_image "PNG: img.pixels * bd (bitdepth) mismatch"));
    let data = Buffer.contents buf in
    let data = compress_string data in

    let datalen = String.length data in
    let max_idat_len = 1048576 in
    (* 2^20 should be enough *)
    (* FIXME constant too big for 32 bit architectures...
     maybe Sys.max_string_length
  *)
    (*let max_idat_len = 2147483647 - 4 in (* 2^31 - 1 - 4 *)*)
    let rec output_idat_from pos =
      if datalen - pos < max_idat_len then
        let idat = String.sub data pos (datalen - pos) in
        let idat = { chunk_type = "IDAT"; chunk_data = idat } in
        write_chunk och idat
      else
        let idat = String.sub data pos max_idat_len in
        let idat = { chunk_type = "IDAT"; chunk_data = idat } in
        write_chunk och idat;
        output_idat_from (pos + max_idat_len)
    in
    output_idat_from 0;

    let iend = { chunk_type = "IEND"; chunk_data = "" } in
    write_chunk och iend;
    close_chunk_writer och
end

let write_png (och : chunk_writer) img = PngWriter.output_png img och

let bytes_of_png img =
  let approx_size = img.width * img.height in
  let buf = Buffer.create approx_size in
  let och = chunk_writer_of_buffer buf in
  PngWriter.output_png img och;
  close_chunk_writer och;
  (* Do this instead of Buffer.to_bytes to avoid copying
     since the underlying string backing the Buffer.to_bytes
     does not escape this scope: *)
  Bytes.unsafe_of_string (Buffer.contents buf)

include ReadPNG

let write = write_png

(* Convenience functions for file I/O *)
let read_png_file filename =
  let ic = open_in_bin filename in
  let ich = chunk_reader_of_in_channel ic in
  try
    let img = parsefile ich in
    close_chunk_reader ich;
    img
  with e ->
    close_chunk_reader ich;
    raise e

let write_png_file filename img =
  let oc = open_out_bin filename in
  let och = chunk_writer_of_out_channel oc in
  try
    write_png och img;
    close_chunk_writer och
  with e ->
    close_chunk_writer och;
    raise e

(* Create RGB image from raw pixel data *)
let rgb_of_pixels ~width ~height pixels =
  let image = create_rgb ~alpha:false ~max_val:255 width height in
  Array.iteri (fun y row ->
    Array.iteri (fun x (r, g, b) ->
      write_rgb image x y r g b
    ) row
  ) pixels;
  image

(* Create RGBA image from raw pixel data *)
let rgba_of_pixels ~width ~height pixels =
  let image = create_rgb ~alpha:true ~max_val:255 width height in
  Array.iteri (fun y row ->
    Array.iteri (fun x (r, g, b, a) ->
      write_rgba image x y r g b a
    ) row
  ) pixels;
  image