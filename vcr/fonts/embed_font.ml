#!/usr/bin/env ocaml

(* Helper to properly embed binary font data *)

let read_file path =
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let bytes = Bytes.create len in
  really_input ic bytes 0 len;
  close_in ic;
  Bytes.to_string bytes

let string_to_ocaml_string_literal s =
  let buf = Buffer.create (String.length s * 2) in
  String.iter
    (fun c -> Buffer.add_string buf (Printf.sprintf "\\x%02x" (Char.code c)))
    s;
  Buffer.contents buf

let () =
  let font_data = read_file Sys.argv.(1) in
  Printf.printf "let %s = \"%s\"\n" Sys.argv.(2)
    (string_to_ocaml_string_literal font_data)
