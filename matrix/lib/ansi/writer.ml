type t = { bytes : Bytes.t; cap : int; mutable pos : int }

exception Buffer_full

let make bytes = { bytes; cap = Bytes.length bytes; pos = 0 }
let make_counting () = { bytes = Bytes.create 0; cap = -1; pos = 0 }
let[@inline] len w = w.pos
let[@inline] pos w = w.pos
let[@inline] reset_pos w = w.pos <- 0
let slice w = Bytes.sub w.bytes 0 w.pos

let[@inline] count w len =
  if len > Sys.max_string_length - w.pos then raise_notrace Buffer_full;
  w.pos <- w.pos + len

let[@inline] write_char w c =
  if w.cap < 0 then count w 1
  else (
    if w.pos >= w.cap then raise_notrace Buffer_full;
    Bytes.unsafe_set w.bytes w.pos c;
    w.pos <- w.pos + 1)

let[@inline] write_string w s =
  let slen = String.length s in
  if slen = 0 then ()
  else if w.cap < 0 then count w slen
  else (
    if slen > w.cap - w.pos then raise_notrace Buffer_full;
    Bytes.blit_string s 0 w.bytes w.pos slen;
    w.pos <- w.pos + slen)

let[@inline] write_subbytes w bytes off blen =
  if blen < 0 || off < 0 || off + blen > Bytes.length bytes then
    invalid_arg "Writer: invalid slice";
  if blen = 0 then ()
  else if w.cap < 0 then count w blen
  else (
    if blen > w.cap - w.pos then raise_notrace Buffer_full;
    Bytes.blit bytes off w.bytes w.pos blen;
    w.pos <- w.pos + blen)
