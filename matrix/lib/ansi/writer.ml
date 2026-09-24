type t = { mutable bytes : bytes; mutable pos : int }

let create capacity = { bytes = Bytes.create capacity; pos = 0 }
let[@inline] len w = w.pos
let[@inline] reset_pos w = w.pos <- 0
let[@inline] buffer w = w.bytes
let contents w = Bytes.sub_string w.bytes 0 w.pos

let grow w additional =
  if additional > Sys.max_string_length - w.pos then
    invalid_arg "Ansi.Writer: output too large";
  let capacity = Bytes.length w.bytes in
  let doubled =
    if capacity > Sys.max_string_length / 2 then Sys.max_string_length
    else max 16 (capacity * 2)
  in
  let bytes = Bytes.create (max (w.pos + additional) doubled) in
  Bytes.blit w.bytes 0 bytes 0 w.pos;
  w.bytes <- bytes

let[@inline] write_char w c =
  if w.pos = Bytes.length w.bytes then grow w 1;
  Bytes.unsafe_set w.bytes w.pos c;
  w.pos <- w.pos + 1

let[@inline] write_string w s =
  let len = String.length s in
  if len > Bytes.length w.bytes - w.pos then grow w len;
  Bytes.blit_string s 0 w.bytes w.pos len;
  w.pos <- w.pos + len

let[@inline] write_subbytes w bytes off len =
  if len < 0 || off < 0 || off > Bytes.length bytes - len then
    invalid_arg "Ansi.Writer: invalid source slice";
  if len > Bytes.length w.bytes - w.pos then grow w len;
  Bytes.blit bytes off w.bytes w.pos len;
  w.pos <- w.pos + len
