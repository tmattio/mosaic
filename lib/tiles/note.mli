(** A note/message display component

    This component displays informational messages, warnings, or any kind of
    note to the user. Similar to huh's Note field.

    Example:
    {[
      (* Initialize a note *)
      let note_model, note_cmd =
        Note.init ~title:"Important"
          ~text:"Remember to save your work frequently!" ~kind:`Warning ()
          (* In your view *)
          Note.view model.note
    ]} *)

open Mosaic

type model
(** The internal state of the note *)

type msg
(** Messages the note can handle *)

val component : (model, msg) Mosaic.app
(** The component definition *)

(** {2 Note Types} *)

type kind =
  [ `Info
  | `Success
  | `Warning
  | `Error
  | `Custom of Style.color * string (* color and icon *) ]
(** Different types of notes with associated styling *)

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
(** Check if the note has been dismissed *)

val kind : model -> kind
(** Get the note's kind *)

val title : model -> string option
(** Get the note's title *)

val text : model -> string
(** Get the note's text content *)

(** {2 Actions} *)

val dismiss : model -> model * msg Cmd.t
(** Dismiss the note (if dismissible) *)

val show : model -> model * msg Cmd.t
(** Show a previously dismissed note *)

val set_content : ?title:string -> ?text:string -> model -> model
(** Update the note's content *)

val set_kind : kind -> model -> model
(** Change the note's kind *)

(** {2 Theming} *)

type theme = {
  border_style : Ui.border_style;
  title_style : Style.t;
  info_style : Style.t;
  success_style : Style.t;
  warning_style : Style.t;
  error_style : Style.t;
  dismiss_style : Style.t;
}

val default_theme : theme
val with_theme : theme -> model -> model

(** {2 Component Interface} *)

val update : msg -> model -> model * msg Cmd.t
(** Update function for the component *)

val view : model -> Ui.element
(** View function for the component *)

val subscriptions : model -> msg Sub.t
(** Subscriptions for the component *)
