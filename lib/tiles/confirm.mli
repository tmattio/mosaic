(** A yes/no confirmation component

    This component provides a simple confirmation dialog with customizable
    affirmative and negative responses. Similar to huh's Confirm field.

    Example:
    {[
      (* Initialize a confirmation *)
      let confirm_model, confirm_cmd =
        Confirm.init
          ~title:"Delete file?"
          ~affirmative:"Yes, delete it"
          ~negative:"No, keep it"
          ()

      (* In your update function *)
      | ConfirmMsg msg ->
          let new_confirm, cmd = Confirm.update msg model.confirm in
          ({ model with confirm = new_confirm },
           Cmd.map (fun m -> ConfirmMsg m) cmd)

      (* In your view *)
      Confirm.view model.confirm

      (* Check if confirmed *)
      match Confirm.value model.confirm with
      | Some true -> (* User confirmed *)
      | Some false -> (* User denied *)
      | None -> (* Not yet answered *)
    ]} *)

open Mosaic

type model
(** The internal state of the confirmation *)

type msg
(** Messages the confirmation can handle *)

val component : (model, msg) Mosaic.app
(** The component definition *)

(** {2 Initialization} *)

val init :
  ?affirmative:string ->
  ?negative:string ->
  ?default:bool ->
  ?inline:bool ->
  unit ->
  model * msg Cmd.t
(** [init ?affirmative ?negative ?default ?inline ()] creates a new
    confirmation.

    @param affirmative Text for positive response (default: "Yes")
    @param negative Text for negative response (default: "No")
    @param default Initial selection (default: true for affirmative)
    @param inline Display options horizontally (default: false) *)

(** {2 Accessors} *)

val value : model -> bool option
(** Get the current selection: [Some true] for affirmative, [Some false] for
    negative, [None] if not yet submitted *)

val is_confirmed : model -> bool
(** Returns true if the user has confirmed (submitted with affirmative) *)

val is_denied : model -> bool
(** Returns true if the user has denied (submitted with negative) *)

val is_submitted : model -> bool
(** Returns true if the user has made a choice *)

val is_focused : model -> bool
(** Check if the confirmation is currently focused *)

val current_selection : model -> bool
(** Get the current selection (true for affirmative, false for negative) *)

(** {2 Actions} *)

val focus : model -> model * msg Cmd.t
(** Focus the confirmation *)

val blur : model -> model * msg Cmd.t
(** Remove focus from the confirmation *)

val reset : model -> model
(** Reset to initial state (unsubmitted) *)

val select_affirmative : model -> model * msg Cmd.t
(** Select the affirmative option *)

val select_negative : model -> model * msg Cmd.t
(** Select the negative option *)

val submit : model -> model * msg Cmd.t
(** Submit the current selection *)

(** {2 Theming} *)

type theme = {
  focused_style : Style.t;
  blurred_style : Style.t;
  selected_style : Style.t;
  unselected_style : Style.t;
  submitted_style : Style.t;
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
