(** A note/message display component for displaying informational messages,
    warnings, and alerts to users.

    This component displays various types of notes with customizable styling and
    optional dismissal functionality. It supports different note kinds (info,
    success, warning, error) with appropriate icons and colors.

    {2 Architecture}

    State tracks note content, kind (info/success/warning/error), and dismissed
    state. Width can be fixed or automatic.

    {2 Key Invariants}

    - A dismissed note will not be rendered in the view
    - Note width is either fixed or automatically sized to content
    - Each note kind has a distinct visual style and icon
    - The dismiss action only works if the note is dismissible

    {2 Example}

    {[
      (* Initialize a dismissible warning note *)
      let note_model, note_cmd =
        Note.init
          ~title:"Important"
          ~text:"Remember to save your work frequently!"
          ~kind:`Warning
          ~dismissible:true
          ()

      (* In your update function *)
      | Note_msg msg ->
          let new_note, cmd = Note.update msg model.note in
          ({ model with note = new_note },
           Cmd.map (fun m -> Note_msg m) cmd)

      (* In your view *)
      Note.view model.note

      (* Check if note was dismissed *)
      if Note.is_dismissed model.note then
        print_endline "Note was dismissed"
    ]} *)

open Mosaic

type model
(** The internal state of the note containing content, kind, display state, and
    theme configuration. *)

type msg
(** Messages that the note component can handle, including dismiss actions. *)

val component : (model, msg) Mosaic.app
(** The note component definition following The Elm Architecture. *)

(** {2 Note Types} *)

type kind =
  [ `Info
  | `Success
  | `Warning
  | `Error
  | `Custom of Style.color * string (* color and icon *) ]
(** Different types of notes with associated styling.

    - [`Info] - Informational note with default blue styling
    - [`Success] - Success message with green styling and checkmark icon
    - [`Warning] - Warning message with yellow styling and warning icon
    - [`Error] - Error message with red styling and error icon
    - [`Custom (color, icon)] - Custom note with specified color and icon *)

(** {2 Initialization} *)

val init :
  ?title:string ->
  ?text:string ->
  ?kind:kind ->
  ?dismissible:bool ->
  ?width:int ->
  unit ->
  model * msg Cmd.t
(** [init ?title ?text ?kind ?dismissible ?width ()] creates a new note.

    @param title Optional title for the note
    @param text Main content of the note
    @param kind Type of note (default: `Info)
    @param dismissible Whether the note can be dismissed (default: false)
    @param width Fixed width for the note (default: auto) *)

(** {2 Accessors} *)

val is_dismissed : model -> bool
(** [is_dismissed model] returns whether the note has been dismissed by the
    user. *)

val kind : model -> kind
(** [kind model] returns the current kind of the note. *)

val title : model -> string option
(** [title model] returns the optional title of the note. *)

val text : model -> string
(** [text model] returns the main text content of the note. *)

(** {2 Actions} *)

val dismiss : model -> model * msg Cmd.t
(** [dismiss model] dismisses the note if it is dismissible. Returns the model
    unchanged if the note is not dismissible. *)

val show : model -> model * msg Cmd.t
(** [show model] makes a previously dismissed note visible again. *)

val set_content : ?title:string -> ?text:string -> model -> model
(** [set_content ?title ?text model] updates the note's title and/or text
    content.

    @param title Optional new title for the note
    @param text Optional new text content for the note *)

val set_kind : kind -> model -> model
(** [set_kind kind model] changes the note's kind, updating its visual style
    accordingly. *)

(** {2 Theming} *)

type theme = {
  border_style : Ui.Border.line_style;
  title_style : Style.t;
  info_style : Style.t;
  success_style : Style.t;
  warning_style : Style.t;
  error_style : Style.t;
  dismiss_style : Style.t;
}
(** Theme configuration for customizing the note's appearance.

    - [border_style] - Border style for the note container
    - [title_style] - Style for the note title
    - [info_style] - Style for info notes
    - [success_style] - Style for success notes
    - [warning_style] - Style for warning notes
    - [error_style] - Style for error notes
    - [dismiss_style] - Style for the dismiss button *)

val default_theme : theme
(** The default theme with standard colors for each note kind. *)

val with_theme : theme -> model -> model
(** [with_theme theme model] applies a custom theme to the note. *)

(** {2 Component Interface} *)

val update : msg -> model -> model * msg Cmd.t
(** [update msg model] handles messages and updates the note state. *)

val view : model -> Ui.element
(** [view model] renders the note component. Returns an empty element if the
    note has been dismissed. *)

val subscriptions : model -> msg Sub.t
(** [subscriptions model] returns subscriptions for the note component.
    Currently returns no subscriptions as notes are not interactive beyond
    dismissal. *)
