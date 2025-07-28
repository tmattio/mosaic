(** Pagination component for navigating through paged content.

    This component manages page state and renders navigation indicators. Features
    include multiple display styles, page bounds checking, and efficient slicing
    calculations for displaying subsets of large datasets.

    {2 Architecture}

    State tracks page index, total items, items per page, and display style. No
    keyboard subscriptions - navigation triggered by parent components via API
    calls.

    {2 Key Invariants}

    Current page is always within valid bounds [0, total_pages). Items per page
    is always positive. Total pages are calculated as ceiling(total_items /
    items_per_page). Page navigation is clamped to prevent out-of-bounds access.

    {2 Example}

    Creates a paginator for search results.
    {[
      (* Initialize for 95 results, 20 per page *)
      let model, cmd = Paginator.init
        ~total_items:95
        ~items_per_page:20
        ~style:Numbers
        ()

      (* Get items for current page *)
      let start_idx, end_idx = Paginator.slice_bounds model in
      let page_items = List.filteri
        (fun i _ -> i >= start_idx && i < end_idx)
        all_results

      (* Handle pagination in update *)
      | Paginator_msg msg ->
          let model', cmd = Paginator.update msg model.paginator in
          { model with paginator = model' }, Cmd.map (fun m -> Paginator_msg m) cmd

      (* Show page info *)
      Printf.printf "Page %d of %d"
        (Paginator.current_page model + 1)
        (Paginator.total_pages model)
    ]} *)

open Mosaic

(** {2 Types} *)

type model
(** [model] represents the internal state of a paginator component.

    The model tracks current page index, total items, items per page, and
    display style. Models are immutable and updated through navigation actions.
*)

type msg
(** [msg] represents internal messages processed by the paginator component.

    Messages include page navigation events. Use [update] to process messages
    and produce new states. *)

(** [style] determines how pagination indicators are rendered.

    Dots show filled/empty circles for visual page position. Numbers show
    human-readable page counts. Compact shows minimal fraction notation. *)
type style =
  | Dots  (** Display as dots: ● ○ ○ ○ ○ *)
  | Numbers  (** Display as numbers: Page 1 of 5 *)
  | Compact  (** Display as compact: 1/5 *)

val component : (model, msg) Mosaic.app
(** [component] provides the complete application interface for the paginator.

    Bundles the [init], [update], [view], and [subscriptions] functions into a
    single record for use with the Mosaic framework. *)

(** {2 Initialization} *)

val init :
  ?total_items:int ->
  ?items_per_page:int ->
  ?current_page:int ->
  ?style:style ->
  unit ->
  model * msg Cmd.t
(** [init ?total_items ?items_per_page ?current_page ?style ()] creates a new
    paginator component.

    The component starts at the specified page or first page. Current page is
    clamped to valid range based on total pages. Returns initial model and
    startup command.

    @param total_items
      Total number of items to paginate, must be non-negative (default: 0)
    @param items_per_page
      Number of items shown per page, must be positive (default: 10)
    @param current_page
      Initial page index (0-based), clamped to valid range (default: 0)
    @param style Display style for page indicators (default: Dots)

    @raise Invalid_argument if [items_per_page <= 0]

    Example: Creates a paginator for a list view.
    {[
      let model, cmd =
        Paginator.init ~total_items:150 ~items_per_page:25 ~current_page:2
          ~style:Numbers ()
    ]} *)

(** {2 Accessors} *)

val current_page : model -> int
(** [current_page model] returns the current page index.

    Pages are 0-indexed. Always within range [0, max(0, total_pages - 1)]. *)

val total_pages : model -> int
(** [total_pages model] returns the total number of pages.

    Calculated as ceiling(total_items / items_per_page). Returns 1 when no items
    exist to show empty state. *)

val items_per_page : model -> int
(** [items_per_page model] returns the maximum items shown per page.

    Always positive. Actual items on last page may be fewer. *)

val total_items : model -> int
(** [total_items model] returns the total number of items being paginated.

    Can be zero for empty datasets. Used to calculate total pages. *)

val on_first_page : model -> bool
(** [on_first_page model] checks whether currently viewing the first page.

    Returns true when current_page is 0. Useful for disabling "previous"
    navigation. *)

val on_last_page : model -> bool
(** [on_last_page model] checks whether currently viewing the last page.

    Returns true when current_page equals total_pages - 1. Useful for disabling
    "next" navigation. *)

val slice_bounds : model -> int * int
(** [slice_bounds model] returns the item range for the current page.

    Returns [(start_inclusive, end_exclusive)] suitable for array slicing or
    list filtering. End index is clamped to total_items. Empty range when no
    items exist.

    Example: Extracts current page items.
    {[
      let start_idx, end_idx = Paginator.slice_bounds paginator in
      let page_items = Array.sub all_items start_idx (end_idx - start_idx)
    ]} *)

val items_on_page : model -> int
(** [items_on_page model] returns the actual item count on the current page.

    Usually equals items_per_page except on the last page which may have fewer.
    Returns 0 when no items exist. *)

(** {2 Actions} *)

val next_page : model -> model
(** [next_page model] advances to the next page.

    No effect if already on last page. Page index increases by 1. Slice bounds
    update accordingly. *)

val prev_page : model -> model
(** [prev_page model] moves to the previous page.

    No effect if already on first page. Page index decreases by 1. Slice bounds
    update accordingly. *)

val go_to_page : int -> model -> model
(** [go_to_page page_idx model] jumps to a specific page.

    Page index is 0-based and clamped to [0, max(0, total_pages - 1)]. Negative
    values become 0. Values beyond last page become last page. *)

val first_page : model -> model
(** [first_page model] jumps to the first page.

    Sets current page to 0. Equivalent to [go_to_page 0 model]. *)

val last_page : model -> model
(** [last_page model] jumps to the last page.

    Sets current page to total_pages - 1. Shows final items in dataset. *)

val set_total_items : int -> model -> model
(** [set_total_items count model] updates the total item count.

    Recalculates total pages. Current page is clamped if now out of bounds.
    Count must be non-negative. *)

val set_items_per_page : int -> model -> model
(** [set_items_per_page count model] updates items shown per page.

    Recalculates total pages and current page bounds. Maintains approximate
    position in dataset. Count must be positive.

    @raise Invalid_argument if [count <= 0] *)

val set_style : style -> model -> model
(** [set_style style model] changes the pagination display style.

    Updates visual representation without affecting page state. Takes effect in
    next render. *)

(** {2 Theming} *)

type theme = {
  active_dot_style : Style.t;
  inactive_dot_style : Style.t;
  number_style : Style.t;
  active_dot : string;
  inactive_dot : string;
}
(** [theme] controls the visual appearance of the paginator component.

    Styles apply to different elements: active_dot_style for current page
    indicator, inactive_dot_style for other pages, number_style for text
    displays. The dot strings customize the characters used for dot style. *)

val default_theme : theme
(** [default_theme] provides a standard appearance.

    Uses filled circle (●) for active page, empty circle (○) for inactive pages.
    Applies bold style to active indicators. *)

val with_theme : theme -> model -> model
(** [with_theme theme model] applies a custom theme to the paginator.

    Updates all visual styles. Changes take effect immediately in next render.
*)

(** {2 Component Interface} *)

val update : msg -> model -> model * msg Cmd.t
(** [update msg model] processes a message to produce a new model state.

    Handles page navigation events. Returns updated model and any commands to
    execute. Currently no commands are generated.

    Example: Integrates with parent update function.
    {[
      | Paginator_msg msg ->
          let model', cmd = Paginator.update msg model.paginator in
          { model with paginator = model' }, Cmd.map (fun m -> Paginator_msg m) cmd
    ]} *)

val view : model -> Ui.element
(** [view model] renders the paginator as a UI element.

    Displays page indicators according to the configured style. Shows dots,
    numbers, or compact notation. Applies theme styles to active/inactive
    elements.

    The rendered element is a single line showing the current page position. *)

val subscriptions : model -> msg Sub.t
(** [subscriptions model] returns event subscriptions for the paginator.

    Always returns [Sub.none] as pagination is driven by external events, not
    keyboard input. Parent components handle navigation triggers. *)
