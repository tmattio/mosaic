(** Mosaic Tiles provides a comprehensive collection of reusable terminal user
    interface components.

    This module serves as the main entry point for all tile components in the
    Mosaic framework. Each tile is a self-contained component following The Elm
    Architecture, providing consistent APIs for initialization, updates, and
    rendering.

    {2 Architecture}

    Each tile is self-contained with model, messages, and subscriptions.
    Viewport-based rendering for large datasets. All tiles support custom
    theming.

    {2 Component Categories}

    The tiles are organized into several categories:
    - {b Form Components}: Input fields, selection lists, and confirmation
      dialogs
    - {b Display Components}: Tables, progress bars, and informational notes
    - {b Navigation Components}: File pickers and paginated content
    - {b Utility Components}: Spinners, timers, and generic viewports
    - {b Helper Components}: Help displays and other supporting elements

    {2 Example}

    {[
      open Mosaic_tiles.Tiles

      (* Create a form with multiple components *)
      let name_input, name_cmd =
        Input.init ~placeholder:"Enter your name" ()

      let language_select, lang_cmd =
        Select.init
          ~options:[
            ("ocaml", "OCaml");
            ("rust", "Rust");
            ("go", "Go");
          ]
          ~title:"Favorite language"
          ()

      let confirm, confirm_cmd =
        Confirm.init ~prompt:"Save changes?" ()

      (* Compose in your view *)
      let open Ui in
      vbox ~gap:2 [
        text "User Profile";
        Input.view name_input;
        Select.view language_select;
        Confirm.view confirm;
      ]

      (* Handle updates *)
      match msg with
      | Name_msg m ->
          let model, cmd = Input.update m model.name_input in
          update_name model cmd
      | Lang_msg m ->
          let model, cmd = Select.update m model.language_select in
          update_language model cmd
      | Confirm_msg m ->
          let model, cmd = Confirm.update m model.confirm in
          if Confirm.is_confirmed model then
            save_profile ()
    ]} *)

(** {2 Common Types} *)

module Key_binding = Key_binding
(** Common key binding configuration for text input components. See
    {!Key_binding}. *)

(** {2 Basic Components} *)

module Spinner = Spinner
(** Animated loading spinner with various styles. See {!Spinner}. *)

(** {2 Form Components} *)

module Input = Input
(** Advanced single-line text input with validation and suggestions. See
    {!Input}. *)

module Textarea = Textarea
(** Multi-line text area with scrolling support. See {!Text}. *)

module Confirm = Confirm
(** Yes/no confirmation dialog. See {!Confirm}. *)

module Select = Select
(** Single-choice selection from a list. See {!Select}. *)

module Multi_select = Multi_select
(** Multiple-choice selection from a list. See {!Multi_select}. *)

(** {2 Display Components} *)

module Note = Note
(** Informational messages and alerts. See {!Note}. *)

module Table = Table
(** Table for displaying structured, tabular data. See {!Table}. *)

module Progress = Progress
(** Progress bar for long-running operations. See {!Progress}. *)

(** {2 File System Components} *)

module File_picker = File_picker
(** File browser and selector. See {!File_picker}. *)

(** {2 Time Components} *)

module Timer = Timer
(** Countdown timer component. See {!Timer}. *)

module Stopwatch = Stopwatch
(** Stopwatch for measuring elapsed time. See {!Stopwatch}. *)

(** {2 Helper Components} *)

module Viewport = Viewport
(** Generic scrollable viewport for displaying large content. See {!Viewport}.
*)

module Paginator = Paginator
(** Pagination logic and display component. See {!Paginator}. *)

module Help = Help
(** Help display for keyboard shortcuts and commands. See {!Help}. *)
