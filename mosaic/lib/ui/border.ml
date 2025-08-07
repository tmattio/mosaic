type line_style = Solid | Rounded | Double | Thick | ASCII

type text_align = [ `Left | `Center | `Right ]

type border_text = {
  text : string;
  align : text_align;
  style : Style.t option; (* Optional style override for the text *)
}

type t = {
  top : bool;
  bottom : bool;
  left : bool;
  right : bool;
  line_style : line_style;
  color : Ansi.color option;
  style : Style.t option;
      (* Style attributes (bold, italic, etc.) for the border *)
  top_text : border_text option;  (* Text to embed in top border *)
  bottom_text : border_text option; (* Text to embed in bottom border *)
}

let make ?(top = true) ?(bottom = true) ?(left = true) ?(right = true)
    ?(line_style = Solid) ?color ?style ?top_text ?bottom_text () =
  { top; bottom; left; right; line_style; color; style; top_text; bottom_text }

let normal = make ~line_style:Solid ()
let rounded = make ~line_style:Rounded ()
let double = make ~line_style:Double ()
let thick = make ~line_style:Thick ()
let ascii = make ~line_style:ASCII ()
let line_style t = t.line_style
let top t = t.top
let bottom t = t.bottom
let left t = t.left
let right t = t.right
let color t = t.color
let style t = t.style
let with_style t style = { t with style = Some style }
let with_top_text t text ?(align = `Center) ?style () = 
  { t with top_text = Some { text; align; style } }
let with_bottom_text t text ?(align = `Center) ?style () = 
  { t with bottom_text = Some { text; align; style } }
let space_h b = (if left b then 1 else 0) + if right b then 1 else 0
let space_v b = (if top b then 1 else 0) + if bottom b then 1 else 0

type border_chars = {
  tl : string;
  th : string;
  tr : string;
  vl : string;
  bl : string;
  bh : string;
  br : string;
  vr : string;
  ml : string;
  mr : string;
  mt : string;
  mb : string;
  mc : string;
}

let get_chars = function
  | Solid ->
      {
        tl = "┌";
        th = "─";
        tr = "┐";
        vl = "│";
        bl = "└";
        bh = "─";
        br = "┘";
        vr = "│";
        ml = "├";
        mr = "┤";
        mt = "┬";
        mb = "┴";
        mc = "┼";
      }
  | Rounded ->
      {
        tl = "╭";
        th = "─";
        tr = "╮";
        vl = "│";
        bl = "╰";
        bh = "─";
        br = "╯";
        vr = "│";
        ml = "├";
        mr = "┤";
        mt = "┬";
        mb = "┴";
        mc = "┼";
      }
  | Double ->
      {
        tl = "╔";
        th = "═";
        tr = "╗";
        vl = "║";
        bl = "╚";
        bh = "═";
        br = "╝";
        vr = "║";
        ml = "╠";
        mr = "╣";
        mt = "╦";
        mb = "╩";
        mc = "╬";
      }
  | Thick ->
      {
        tl = "┏";
        th = "━";
        tr = "┓";
        vl = "┃";
        bl = "┗";
        bh = "━";
        br = "┛";
        vr = "┃";
        ml = "┣";
        mr = "┫";
        mt = "┳";
        mb = "┻";
        mc = "╋";
      }
  | ASCII ->
      {
        tl = "+";
        th = "-";
        tr = "+";
        vl = "|";
        bl = "+";
        bh = "-";
        br = "+";
        vr = "|";
        ml = "+";
        mr = "+";
        mt = "+";
        mb = "+";
        mc = "+";
      }
