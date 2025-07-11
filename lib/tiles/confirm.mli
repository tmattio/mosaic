(** Binary choice confirmation component with keyboard navigation.

    This component presents a yes/no decision to users. Features include
    customizable labels, keyboard navigation, visual feedback for selection, and
    submission handling.

    {2 Architecture}

    State tracks binary selection, submission status, and focus. Submission is
    one-way: once submitted, becomes read-only. Inline layout switches keyboard
    navigation from vertical to horizontal arrows.

    {2 Key Invariants}

    Selection always points to either affirmative or negative option. Submission
    locks the choice and prevents further changes. Focus enables keyboard
    navigation between options.

    {2 Example}

    Creates a deletion confirmation.
    {[
      (* Initialize with custom labels *)
      let model, cmd = Confirm.init
        ~affirmative:"Yes, delete it"
        ~negative:"No, keep it"
        ~default:false
        ()

      (* Handle messages in update *)
      | Confirm_msg msg ->
          let model', cmd = Confirm.update msg model.confirm in
          { model with confirm = model' }, Cmd.map (fun m -> Confirm_msg m) cmd

      (* Render in view *)
      Confirm.view model.confirm

      (* Process the decision *)
      match Confirm.value model.confirm with
      | Some true -> delete_file ()
      | Some false -> Printf.printf "File kept"
      | None -> () (* Still deciding *)
    ]} *)

open Mosaic

type model
(** [model] represents the internal state of a confirmation component.

    The model tracks current selection, submission status, and focus state.
    Models are immutable and updated through the [update] function. *)

type msg
(** [msg] represents internal messages processed by the confirmation component.

    Messages include keyboard events, selection changes, and submission actions.
    Use [update] to process messages and produce new states. *)

val component : (model, msg) Mosaic.app
(** [component] provides the complete application interface for the
    confirmation.

    Bundles the [init], [update], [view], and [subscriptions] functions into a
    single record for use with the Mosaic framework. *)

(** {2 Initialization} *)

val init :
  ?affirmative:string ->
  ?negative:string ->
  ?default:bool ->
  ?inline:bool ->
  unit ->
  model * msg Cmd.t
(** [init ?affirmative ?negative ?default ?inline ()] creates a new confirmation
    component.

    The component starts unsubmitted with the default selection highlighted.
    Returns initial model and startup command.

    @param affirmative Label for the positive response (default: "Yes")
    @param negative Label for the negative response (default: "No")
    @param default Initial selection, true for affirmative (default: true)
    @param inline
      Display options horizontally instead of vertically (default: false)

    Example: Creates a save confirmation with custom labels.
    {[
      let model, cmd =
        Confirm.init ~affirmative:"Save changes" ~negative:"Discard"
          ~default:true ~inline:true ()
    ]} *)

(** {2 Accessors} *)

val value : model -> bool option
(** [value model] returns the submitted choice.

    Returns [Some true] if affirmative was submitted, [Some false] if negative
    was submitted, or [None] if not yet submitted. Once submitted, the value is
    locked. *)

val is_confirmed : model -> bool
(** [is_confirmed model] checks whether affirmative was submitted.

    Returns true only after submission with affirmative selected. Useful for
    conditional logic based on user confirmation. *)

val is_denied : model -> bool
(** [is_denied model] checks whether negative was submitted.

    Returns true only after submission with negative selected. Useful for
    conditional logic based on user denial. *)

val is_submitted : model -> bool
(** [is_submitted model] checks whether any choice has been submitted.

    Returns true after submission regardless of selection. Once submitted, the
    component becomes read-only. *)

val is_focused : model -> bool
(** [is_focused model] checks whether the confirmation has keyboard focus.

    Focus affects visual styling and enables keyboard navigation. Focus is
    gained through [focus] action or user interaction. *)

val current_selection : model -> bool
(** [current_selection model] returns which option is currently selected.

    Returns true when affirmative is selected, false when negative is selected.
    Selection can change until submission. *)

(** {2 Actions} *)

val focus : model -> model * msg Cmd.t
(** [focus model] gives keyboard focus to the confirmation.

    Updates visual styling to focused state. Enables keyboard navigation between
    options. Returns updated model and no command. *)

val blur : model -> model * msg Cmd.t
(** [blur model] removes keyboard focus from the confirmation.

    Updates visual styling to blurred state. Disables keyboard navigation.
    Returns updated model and no command. *)

val reset : model -> model
(** [reset model] restores the initial unsubmitted state.

    Clears submission status and returns to default selection. Useful for
    reusing the component for multiple confirmations. *)

val select_affirmative : model -> model * msg Cmd.t
(** [select_affirmative model] selects the affirmative option.

    No effect if already submitted. Updates visual highlight. Returns updated
    model and no command. *)

val select_negative : model -> model * msg Cmd.t
(** [select_negative model] selects the negative option.

    No effect if already submitted. Updates visual highlight. Returns updated
    model and no command. *)

val submit : model -> model * msg Cmd.t
(** [submit model] submits the current selection.

    Locks the choice and prevents further changes. Updates visual state to show
    submission. Returns updated model and no command. *)

(** {2 Theming} *)

type theme = {
  focused_style : Style.t;
  blurred_style : Style.t;
  selected_style : Style.t;
  unselected_style : Style.t;
  submitted_style : Style.t;
}
(** [theme] controls the visual appearance of the confirmation component.

    Each style applies to different states: focused_style for active component,
    blurred_style for inactive state, selected_style for current choice,
    unselected_style for other option, and submitted_style for locked choices.
*)

val default_theme : theme
(** [default_theme] provides a standard color scheme.

    Uses blue for focus, gray for blur, bold for selection, and green for
    submitted choices. *)

val with_theme : theme -> model -> model
(** [with_theme theme model] applies a custom theme to the confirmation.

    Updates all visual styles. Changes take effect immediately in next render.
*)

(** {2 Component Interface} *)

val update : msg -> model -> model * msg Cmd.t
(** [update msg model] processes a message to produce a new model state.

    Handles keyboard navigation (arrows, tab), selection (space, enter), and
    focus changes. Prevents changes after submission. Returns updated model and
    any commands to execute.

    Example: Integrates with parent update function.
    {[
      | Confirm_msg msg ->
          let model', cmd = Confirm.update msg model.confirm in
          { model with confirm = model' }, Cmd.map (fun m -> Confirm_msg m) cmd
    ]} *)

val view : model -> Ui.element
(** [view model] renders the confirmation as a UI element.

    Displays both options with visual indicators for selection and focus. Shows
    submitted state with locked appearance. Layout depends on inline setting.

    The rendered element shows the two options with appropriate styling. *)

val subscriptions : model -> msg Sub.t
(** [subscriptions model] returns event subscriptions based on current state.

    Subscribes to keyboard events when focused for navigation (arrows, tab,
    enter, space). No subscriptions when blurred or submitted. Automatically
    managed based on state.

    Example: Combines with other subscriptions.
    {[
      Sub.batch
        [
          Confirm.subscriptions model.confirm
          |> Sub.map (fun m -> Confirm_msg m);
          Sub.on_key Escape `Cancel;
        ]
    ]} *)
