(** A pagination component for managing and displaying page navigation.

    This is a foundational component that handles pagination logic and can
    render various styles of page indicators. It can be used by other components
    like Select, Table, and List for consistent pagination. *)

open Mosaic

(** {2 Types} *)

type model
(** The internal state of the paginator *)

type msg
(** Messages the paginator can handle *)

(** Different pagination display styles *)
type style =
  | Dots  (** Display as dots: ● ○ ○ ○ ○ *)
  | Numbers  (** Display as numbers: Page 1 of 5 *)
  | Compact  (** Display as compact: 1/5 *)

val component : (model, msg) Mosaic.app
(** The component definition *)

(** {2 Initialization} *)

val init :
  ?total_items:int ->
  ?items_per_page:int ->
  ?current_page:int ->
  ?style:style ->
  unit ->
  model * msg Cmd.t
(** [init ?total_items ?items_per_page ?current_page ?style ()] creates a new
    paginator.

    @param total_items Total number of items to paginate (default: 0)
    @param items_per_page Number of items shown per page (default: 10)
    @param current_page Initial page (0-based, default: 0)
    @param style Display style (default: Dots) *)

(** {2 Accessors} *)

val current_page : model -> int
(** Get the current page index (0-based) *)

val total_pages : model -> int
(** Get the total number of pages *)

val items_per_page : model -> int
(** Get the number of items per page *)

val total_items : model -> int
(** Get the total number of items *)

val on_first_page : model -> bool
(** Check if currently on the first page *)

val on_last_page : model -> bool
(** Check if currently on the last page *)

val slice_bounds : model -> int * int
(** Get the start and end indices for the current page. Returns
    (start_inclusive, end_exclusive) for slicing arrays/lists. *)

val items_on_page : model -> int
(** Get the actual number of items on the current page (may be less than
    items_per_page on the last page) *)

(** {2 Actions} *)

val next_page : model -> model
(** Go to the next page (does nothing if on last page) *)

val prev_page : model -> model
(** Go to the previous page (does nothing if on first page) *)

val go_to_page : int -> model -> model
(** Go to a specific page (clamped to valid range) *)

val first_page : model -> model
(** Go to the first page *)

val last_page : model -> model
(** Go to the last page *)

val set_total_items : int -> model -> model
(** Update the total number of items (may adjust current page) *)

val set_items_per_page : int -> model -> model
(** Update items per page (may adjust current page) *)

val set_style : style -> model -> model
(** Change the display style *)

(** {2 Theming} *)

type theme = {
  active_dot_style : Style.t;
  inactive_dot_style : Style.t;
  number_style : Style.t;
  active_dot : string;
  inactive_dot : string;
}
(** Theme configuration for the paginator *)

val default_theme : theme
(** Default paginator theme *)

val with_theme : theme -> model -> model
(** Apply a custom theme to the paginator *)

(** {2 Component Interface} *)

val update : msg -> model -> model * msg Cmd.t
(** Handle messages and update the paginator state *)

val view : model -> Ui.element
(** Render the paginator *)

val subscriptions : model -> msg Sub.t
(** Paginators don't need subscriptions *)
