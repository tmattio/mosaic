(** Box-drawing characters, the sets {!Matrix_grid.draw_box} draws borders with.

    A {e border} gives the 11 scalars that draw a box and the lines that divide
    it. These are the four corners, the horizontal and vertical edges, the four
    T-junctions, and the cross. Each must be one cell wide, since a box gives
    each one cell. *)

(** {1:borders Borders} *)

type t = {
  top_left : Uchar.t;
  top_right : Uchar.t;
  bottom_left : Uchar.t;
  bottom_right : Uchar.t;
  horizontal : Uchar.t;
  vertical : Uchar.t;
  top_t : Uchar.t;
  bottom_t : Uchar.t;
  left_t : Uchar.t;
  right_t : Uchar.t;
  cross : Uchar.t;
}
(** The type for borders. *)

(** {1:presets Presets} *)

val single : t
(** [single] is the light border ┌ ┐ └ ┘ ─ │ ┬ ┴ ├ ┤ ┼. *)

val double : t
(** [double] is the double border ╔ ╗ ╚ ╝ ═ ║ ╦ ╩ ╠ ╣ ╬. *)

val rounded : t
(** [rounded] is the light border with round corners ╭ ╮ ╰ ╯ ─ │ ┬ ┴ ├ ┤ ┼. *)

val heavy : t
(** [heavy] is the heavy border ┏ ┓ ┗ ┛ ━ ┃ ┳ ┻ ┣ ┫ ╋. *)

val ascii : t
(** [ascii] is the border of ASCII characters [+ + + + - | + + + + +], for
    terminals without box-drawing characters. *)

val empty : t
(** [empty] is the border of spaces. *)

(** {1:customizing Customizing} *)

val modify :
  ?top_left:Uchar.t ->
  ?top_right:Uchar.t ->
  ?bottom_left:Uchar.t ->
  ?bottom_right:Uchar.t ->
  ?horizontal:Uchar.t ->
  ?vertical:Uchar.t ->
  ?top_t:Uchar.t ->
  ?bottom_t:Uchar.t ->
  ?left_t:Uchar.t ->
  ?right_t:Uchar.t ->
  ?cross:Uchar.t ->
  t ->
  t
(** [modify ?top_left ... base] is [base] with the characters given in place of
    its own. *)

(** {1:sides Sides} *)

type side = [ `Top | `Right | `Bottom | `Left ]
(** The type for box sides. *)

val all : side list
(** [all] is [[\`Top; \`Right; \`Bottom; \`Left]]. *)
