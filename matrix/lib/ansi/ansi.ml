include Escape
module Writer = Writer
module Color = Color
module Attr = Attr
module Style = Style
module Parser = Parser
module Sgr_state = Sgr_state

let render ?hyperlinks_enabled segments =
  Segment.render ?hyperlinks_enabled segments

(* A string without ESC, DEL or a non-ASCII byte has nothing to remove. *)
let rec is_plain s i =
  i >= String.length s
  ||
  let c = String.unsafe_get s i in
  c <> '\x1b' && c < '\x7f' && is_plain s (i + 1)

let strip s =
  if is_plain s 0 then s
  else
    let b = Buffer.create (String.length s) in
    let keep = function
      | Parser.Text text -> Buffer.add_string b text
      | Parser.Control c when c < ' ' -> Buffer.add_char b c
      | _ -> ()
    in
    let p = Parser.create () in
    Parser.feed p (Bytes.unsafe_of_string s) ~off:0 ~len:(String.length s) keep;
    Parser.finish p keep;
    Buffer.contents b
