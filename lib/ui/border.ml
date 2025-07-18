type style = Solid | Rounded | Double | Thick | ASCII

type t = {
  top : bool;
  bottom : bool;
  left : bool;
  right : bool;
  style : style;
  color : Ansi.color option;
}

let make ?(top = true) ?(bottom = true) ?(left = true) ?(right = true)
    ?(style = Solid) ?color () =
  { top; bottom; left; right; style; color }

let normal = make ~style:Solid ()
let rounded = make ~style:Rounded ()
let double = make ~style:Double ()
let thick = make ~style:Thick ()
let ascii = make ~style:ASCII ()
let style t = t.style
let top t = t.top
let bottom t = t.bottom
let left t = t.left
let right t = t.right
let color t = t.color
let space_h b = (if left b then 1 else 0) + if right b then 1 else 0
let space_v b = (if top b then 1 else 0) + if bottom b then 1 else 0
