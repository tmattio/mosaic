(** A table component for displaying structured, tabular data with row selection
    and scrolling support. *)

open Mosaic

(** {2 Types} *)

type model
(** The internal state of the table *)

type msg
(** Messages the table can handle *)

type column = { title : string; width : int }
(** Column definition with title and width *)

type row = string list
(** A row is a list of cell values *)

val component : (model, msg) Mosaic.app
(** The component definition *)

(** {2 Initialization} *)

val init :
  ?columns:column list ->
  ?rows:row list ->
  ?height:int ->
  ?focused:bool ->
  unit ->
  model * msg Cmd.t
(** [init ?columns ?rows ?height ?focused ()] creates a new table.

    @param columns Column definitions (default: empty)
    @param rows Table data (default: empty)
    @param height Viewport height in rows (default: 10)
    @param focused Whether the table starts focused (default: false) *)

(** {2 Accessors} *)

val selected_row : model -> row option
(** Get the currently selected row, if any *)

val cursor : model -> int
(** Get the cursor position (selected row index) *)

val rows : model -> row list
(** Get all rows *)

val columns : model -> column list
(** Get column definitions *)

val is_focused : model -> bool
(** Check if the table is focused *)

val row_count : model -> int
(** Get the total number of rows *)

val visible_rows : model -> row list
(** Get the currently visible rows *)

(** {2 Actions} *)

val focus : model -> model * msg Cmd.t
(** Focus the table, enabling keyboard navigation *)

val blur : model -> model
(** Remove focus from the table *)

val set_columns : column list -> model -> model
(** Update the column definitions *)

val set_rows : row list -> model -> model
(** Update the table data *)

val set_cursor : int -> model -> model
(** Set the cursor to a specific row index *)

val move_up : int -> model -> model
(** Move the cursor up by the given number of rows *)

val move_down : int -> model -> model
(** Move the cursor down by the given number of rows *)

val go_to_start : model -> model
(** Move the cursor to the first row *)

val go_to_end : model -> model
(** Move the cursor to the last row *)

val set_height : int -> model -> model
(** Set the viewport height *)

val request_focus : model -> model * msg Cmd.t
(** Request focus for keyboard navigation *)

val request_blur : model -> model * msg Cmd.t
(** Release focus *)

(** {2 Theming} *)

type theme = {
  header_style : Style.t;
  cell_style : Style.t;
  selected_style : Style.t;
  border : Ui.border option;
}
(** Theme configuration for the table *)

val default_theme : theme
(** Default table theme *)

val with_theme : theme -> model -> model
(** Apply a custom theme to the table *)

(** {2 Component Interface} *)

val update : msg -> model -> model * msg Cmd.t
(** Handle messages and update the table state *)

val view : model -> Ui.element
(** Render the table *)

val subscriptions : model -> msg Sub.t
(** Subscribe to keyboard events when focused *)
