(** LZW compression and decompression for GIF encoding/decoding *)

open Types

(** {1 LZW Encoder} *)

module Encoder = struct
  type entry = {
    parent : int; [@warning "-69"]  (** Parent code, -1 for roots *)
    suffix : char; [@warning "-69"]  (** Suffix byte *)
  }
  (** Code table entry *)

  type code_table = {
    entries : entry array;  (** Array of code entries (for future decoder) *)
    mutable next_code : int;  (** Next available code *)
    children : (int * char, int) Hashtbl.t;  (** (parent, suffix) -> code *)
  }
  (** Fast trie-based code table *)

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
  (** Bitstream writer *)

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

  (** Compress data using LZW algorithm *)
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

  (** Package data into sub-blocks *)
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

(** {1 LZW Decoder} *)

module Decoder = struct
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
    if code < decoder.next_code then Ok decoder.dict.(code)
    else if code = decoder.next_code && decoder.old_code <> None then
      (* Special case: code = next available code *)
      let old = Option.get decoder.old_code in
      Ok (decoder.dict.(old) ^ String.sub decoder.dict.(old) 0 1)
    else
      Error
        (Decode_error
           (Printf.sprintf "Invalid LZW code: %d (next_code=%d)" code
              decoder.next_code))

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
      Error (Decode_error "Not enough bits in stream")
    else
      let mask = (1 lsl num_bits) - 1 in
      let result = reader.buffer land mask in
      reader.buffer <- reader.buffer lsr num_bits;
      reader.bits_in_buffer <- reader.bits_in_buffer - num_bits;
      Ok result

  let decompress compressed_data min_code_size =
    let decoder = create min_code_size in
    let reader = create_reader compressed_data in
    let output = Buffer.create (String.length compressed_data * 4) in

    let rec decode_loop () =
      match read_bits reader decoder.code_size with
      | Error _ -> Ok (Buffer.contents output) (* No more data *)
      | Ok code -> (
          if code = decoder.end_code then Ok (Buffer.contents output)
          else if code = decoder.clear_code then (
            reset decoder;
            (* After a clear code, the next code must be a literal *)
            decoder.old_code <- None;
            decode_loop ())
          else
            match decode_string decoder code with
            | Error e -> Error e
            | Ok str ->
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
                then decoder.code_size <- decoder.code_size + 1;

                decode_loop ())
    in
    decode_loop ()
end
