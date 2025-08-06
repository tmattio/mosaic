(** A table component for displaying and navigating structured tabular data with
    row selection and scrolling support.

    This component provides a scrollable table view with keyboard navigation,
    row selection, and customizable column widths. It efficiently handles large
    datasets by only rendering visible rows.

    {2 Architecture}

    State tracks row data, cursor position, and viewport offset. Renders only
    visible rows for performance. Column widths are fixed at initialization.

    {2 Key Invariants}

    - Cursor position is always within valid row bounds
    - Only visible rows are rendered for performance
    - Column count must match cell count in each row
    - Selected row updates as cursor moves
    - Viewport scrolls to keep cursor visible

    {2 Example}

    {[
      (* Define table structure *)
      let columns = [
        { Table.title = "Name"; width = 20 };
        { Table.title = "Age"; width = 10 };
        { Table.title = "City"; width = 25 };
      ]

      let rows = [
        ["Alice"; "30"; "New York"];
        ["Bob"; "25"; "San Francisco"];
        ["Charlie"; "35"; "London"];
        ["David"; "28"; "Tokyo"];
      ]

      (* Initialize table *)
      let table_model, table_cmd =
        Table.init
          ~columns
          ~rows
          ~height:5
          ~focused:true
          ()

      (* In your update function *)
      | Table_msg msg ->
          let new_table, cmd = Table.update msg model.table in
          ({ model with table = new_table },
           Cmd.map (fun m -> Table_msg m) cmd)

      (* In your view *)
      let open Ui in
      vbox ~gap:(`Cells 1) [
        text "User Directory:";
        Table.view model.table;
        (match Table.selected_row model.table with
         | Some row ->
             text (Printf.sprintf "Selected: %s" (List.hd row))
         | None ->
             text "No selection");
      ]

      (* Navigate the table *)
      let model = Table.move_down 1 model
      let model = Table.go_to_end model

      (* Update data *)
      let model = Table.set_rows new_rows model
    ]} *)

open Mosaic

(** {2 Types} *)

type model
(** The internal state of the table containing data, cursor position, viewport
    settings, and theme configuration. *)

type msg
(** Messages that the table can handle, including navigation and focus events.
*)

type column = { title : string; width : int }
(** Column definition specifying the header title and fixed character width. *)

type row = string list
(** A table row represented as a list of cell values. The number of cells should
    match the number of columns. *)

val component : (model, msg) Mosaic.app
(** The table component definition following The Elm Architecture. *)

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
(** [selected_row model] returns the currently selected row data, if any. *)

val cursor : model -> int
(** [cursor model] returns the current cursor position as a zero-based row
    index. *)

val rows : model -> row list
(** [rows model] returns all table rows including those not currently visible.
*)

val columns : model -> column list
(** [columns model] returns the list of column definitions with titles and
    widths. *)

val is_focused : model -> bool
(** [is_focused model] returns whether the table has keyboard focus for
    navigation. *)

val row_count : model -> int
(** [row_count model] returns the total number of rows in the table. *)

val visible_rows : model -> row list
(** [visible_rows model] returns only the rows currently visible in the
    viewport. *)

(** {2 Actions} *)

val focus : model -> model * msg Cmd.t
(** [focus model] sets keyboard focus on the table for navigation. Returns the
    model immediately as focus is handled internally. *)

val blur : model -> model
(** [blur model] removes keyboard focus from the table. *)

val set_columns : column list -> model -> model
(** [set_columns columns model] updates the column definitions. This should be
    called before setting rows to ensure proper alignment. *)

val set_rows : row list -> model -> model
(** [set_rows rows model] replaces all table data with new rows. The cursor is
    reset to the first row if it exceeds the new row count. *)

val set_cursor : int -> model -> model
(** [set_cursor idx model] moves the cursor to row at index [idx], clamping to
    valid bounds. *)

val move_up : int -> model -> model
(** [move_up n model] moves the cursor up by [n] rows, stopping at the first
    row. *)

val move_down : int -> model -> model
(** [move_down n model] moves the cursor down by [n] rows, stopping at the last
    row. *)

val go_to_start : model -> model
(** [go_to_start model] moves the cursor to the first row. *)

val go_to_end : model -> model
(** [go_to_end model] moves the cursor to the last row. *)

val set_height : int -> model -> model
(** [set_height h model] sets the viewport height to [h] rows. *)

val request_focus : model -> model * msg Cmd.t
(** [request_focus model] requests keyboard focus for the table, returning a
    focus command. *)

val request_blur : model -> model * msg Cmd.t
(** [request_blur model] releases keyboard focus from the table, returning a
    blur command. *)

(** {2 Theming} *)

type theme = {
  header_style : Style.t;
  cell_style : Style.t;
  selected_style : Style.t;
  border : Ui.Border.t option;
}
(** Theme configuration for customizing the table appearance.

    - [header_style] - Style for column headers
    - [cell_style] - Style for regular cells
    - [selected_style] - Style for the selected row
    - [border] - Optional border configuration for the table *)

val default_theme : theme
(** The default theme with standard table styling. *)

val with_theme : theme -> model -> model
(** [with_theme theme model] applies a custom theme to the table. *)

(** {2 Component Interface} *)

val update : msg -> model -> model * msg Cmd.t
(** [update msg model] handles messages and updates the table state, including
    cursor movement and viewport scrolling. *)

val view : model -> Ui.element
(** [view model] renders the table with headers, visible rows, and selection
    highlighting. *)

val subscriptions : model -> msg Sub.t
(** [subscriptions model] returns keyboard event subscriptions when the table
    has focus. *)
