type align = [ `Start | `Center | `End | `Stretch ]

type t =
  | Empty
  | Box of { border : Border.t option; background : Style.t option }
  | Text of {
      content : string;
      style : Style.t;
      align : align;
      tab_width : int;
      wrap : [ `Wrap | `Truncate | `Clip ];
    }
  | Canvas of {
      draw : (x:int -> y:int -> ?style:Style.t -> string -> unit) -> unit;
    }
  | Scroll of { h_offset : int; v_offset : int }

let box ?border ?background () = Box { border; background }

let text ?(style = Style.empty) ?(align = `Start) ?(tab_width = 4)
    ?(wrap = `Clip) content =
  Text { content; style; align; tab_width; wrap }

let canvas draw = Canvas { draw }
let scroll ?(h_offset = 0) ?(v_offset = 0) () = Scroll { h_offset; v_offset }
let empty = Empty
