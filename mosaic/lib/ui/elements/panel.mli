(** Panel element with Rich-like API for drawing borders around content *)

type align = [ `Left | `Center | `Right ]
(** Alignment options for title and subtitle *)

val panel :
  ?box_style:Border.line_style ->
  ?title:string ->
  ?title_align:align ->
  ?subtitle:string ->
  ?subtitle_align:align ->
  ?expand:bool ->
  ?style:Style.t ->
  ?border_style:Style.t ->
  ?width:int ->
  ?height:int ->
  ?padding:Element.length_percentage Element.sides ->
  ?highlight:bool ->
  Element.t ->
  Element.t
(** Create a panel with a border around its contents.

    @param child The content to display inside the panel
    @param box_style The border style (default: Rounded)
    @param title Optional title displayed in panel header
    @param title_align Alignment of title (default: Center)
    @param subtitle Optional subtitle displayed in panel footer
    @param subtitle_align Alignment of subtitle (default: Center)
    @param expand
      If true, panel stretches to fill available width (default: true)
    @param style The style of the panel contents
    @param border_style The style of the border
    @param width Optional fixed width of panel
    @param height Optional fixed height of panel
    @param padding Padding around content (top, right, bottom, left)
    @param highlight Enable automatic highlighting of title/subtitle if string
*)
