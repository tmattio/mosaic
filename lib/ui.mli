(* High-level rendering API *)

module Style : sig
  type t

  (** Style attributes for building styles from lists *)
  type attr =
    | Fg of Ansi.color
    | Bg of Ansi.color
    | Bold
    | Dim
    | Italic
    | Underline
    | Blink
    | Reverse
    | Strikethrough
    | Link of string

  val empty : t
  val fg : Ansi.color -> t
  val bg : Ansi.color -> t
  val bold : t
  val dim : t
  val italic : t
  val underline : t
  val blink : t
  val reverse : t
  val strikethrough : t

  val link : string -> t
  (** Create a hyperlink style *)

  val of_list : attr list -> t
  (** Create a style from a list of attributes *)

  val ( ++ ) : t -> t -> t
  (** Combine two styles, with the second taking precedence *)

  type color = Ansi.color =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
    | Default
    | Bright_black
    | Bright_red
    | Bright_green
    | Bright_yellow
    | Bright_blue
    | Bright_magenta
    | Bright_cyan
    | Bright_white
    | Index of int  (** 256-color palette (0-255) *)
    | RGB of int * int * int  (** 24-bit color (0-255 each) *)

  val ansi256 : int -> color
  (** Create a 256-color ANSI color *)

  val rgb : int -> int -> int -> color
  (** Create an RGB color *)

  val gray : int -> color
  (** [gray n] creates a grayscale color (0-23) *)

  val rgb_hex : int -> color
  (** [rgb_hex 0xFF00FF] creates an RGB color from hex *)
end

(** {2 Layout Types} *)

type padding = { top : int; right : int; bottom : int; left : int }
(** Padding specification for elements *)

val padding :
  ?top:int -> ?right:int -> ?bottom:int -> ?left:int -> unit -> padding
(** [padding ?top ?right ?bottom ?left ()] creates a padding specification. All
    values default to 0. *)

val pad :
  ?all:int ->
  ?x:int ->
  ?y:int ->
  ?top:int ->
  ?right:int ->
  ?bottom:int ->
  ?left:int ->
  unit ->
  padding
(** Create padding. [all] sets all sides. [x]/[y] set horizontal/vertical.
    Individual sides override [all]/[x]/[y]. All default to 0. *)

val padding_all : int -> padding
(** [padding_all n] creates uniform padding on all sides *)

val padding_xy : int -> int -> padding
(** [padding_xy x y] creates padding with x for left/right, y for top/bottom *)

type border_style =
  | Solid
  | Rounded
  | Double
  | Thick
  | ASCII  (** Border drawing styles *)

type border = { style : border_style; color : Ansi.color option }
(** Border specification *)

val border : ?style:border_style -> ?color:Ansi.color -> unit -> border
(** [border ?style ?color ()] creates a border specification. Default style is
    Solid. *)

type align =
  | Start
  | Center
  | End
  | Stretch  (** Alignment options for elements *)

type element

val text : ?style:Style.t -> string -> element
(** Create a text element *)

val hbox :
  ?gap:int ->
  ?width:int ->
  ?height:int ->
  ?padding:padding ->
  ?border:border ->
  ?align_items:align ->
  ?justify_content:align ->
  element list ->
  element
(** Create a horizontal box.
    - [gap]: Space between elements
    - [width], [height]: Fixed dimensions
    - [padding]: Inner padding
    - [border]: Border style
    - [align_items]: How items are aligned on the cross-axis (vertically)
      (default: Start)
    - [justify_content]: How items are distributed on the main-axis
      (horizontally) (default: Start) *)

val vbox :
  ?gap:int ->
  ?width:int ->
  ?height:int ->
  ?padding:padding ->
  ?border:border ->
  ?align_items:align ->
  ?justify_content:align ->
  element list ->
  element
(** Create a vertical box.
    - [gap]: Space between elements
    - [width], [height]: Fixed dimensions
    - [padding]: Inner padding
    - [border]: Border style
    - [align_items]: How items are aligned on the cross-axis (horizontally)
      (default: Start)
    - [justify_content]: How items are distributed on the main-axis (vertically)
      (default: Start) *)

val spacer : int -> element
(** Create a spacer element that consumes space without rendering *)

val space : int -> element
(** Empty space. In hbox: width. In vbox: height. Alias for spacer. *)

val expand : element -> element
(** Make an element expand to fill available space in its parent container *)

val render : Render.buffer -> element -> unit
(** Render an element to a Render buffer with proper layout support *)
