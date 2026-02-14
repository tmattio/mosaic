include Escape

module Color = Color
module Attr = Attr
module Style = Style
module Parser = Parser
module Sgr_state = Sgr_state

let styled ?(reset = false) ?fg ?bg ?(bold = false) ?(dim = false)
    ?(italic = false) ?(underline = false) ?(blink = false) ?(inverse = false)
    ?(hidden = false) ?(strikethrough = false) ?(overline = false)
    ?(double_underline = false) ?(framed = false) ?(encircled = false) ?link str
    =
  let style =
    Style.make ?fg ?bg ~bold ~dim ~italic ~underline ~blink ~inverse ~hidden
      ~strikethrough ~overline ~double_underline ~framed ~encircled ?link ()
  in
  match link with
  | None -> Style.styled ~reset style str
  | Some url ->
      to_string (fun w ->
          hyperlink_open w url;
          Style.emit style w;
          literal str w;
          hyperlink_close w;
          if reset then Escape.reset w)

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
      Parser.feed p (Bytes.unsafe_of_string str) ~off:0
        ~len:(String.length str) collect;
      Parser.feed p Bytes.empty ~off:0 ~len:0 collect;
      Buffer.contents buf
