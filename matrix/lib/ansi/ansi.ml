include Escape
module Writer = Writer
module Color = Color
module Attr = Attr
module Style = Style
module Parser = Parser
module Sgr_state = Sgr_state

let render ?hyperlinks_enabled segments =
  Segment.render ?hyperlinks_enabled segments

let parse = Parser.parse

let strip str =
  match String.index_from_opt str 0 '\x1b' with
  | None -> str
  | Some _ ->
      let buf = Buffer.create (String.length str) in
      let p = Parser.create () in
      let collect = function
        | Parser.Text s -> Buffer.add_string buf s
        | Parser.SGR _ | Parser.Control _ -> ()
      in
      Parser.feed p
        (Bytes.unsafe_of_string str)
        ~off:0 ~len:(String.length str) collect;
      Parser.feed p Bytes.empty ~off:0 ~len:0 collect;
      Buffer.contents buf
