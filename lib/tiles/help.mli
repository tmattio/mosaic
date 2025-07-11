(** A help component for displaying keyboard shortcuts and command help in a
    consistent format.

    This component renders help information based on key bindings, supporting
    both short inline help and full help views with grouped key bindings. It is
    designed to be used by other components to provide consistent help displays
    across the application.

    {2 Architecture}

    State tracks display mode (short/full) and width. Key bindings organized in
    groups with optional titles. Only enabled bindings are displayed.

    {2 Key Invariants}

    - Only enabled key bindings are displayed in the help view
    - Groups with no enabled bindings are automatically hidden
    - Short help displays bindings in a single line with separators
    - Full help displays bindings in columnar format with groups

    {2 Example}

    {[
      (* Define key bindings *)
      let navigation_bindings =
        [
          Help.make_binding ~key:"↑/k" ~description:"Move up" ();
          Help.make_binding ~key:"↓/j" ~description:"Move down" ();
          Help.make_binding ~key:"←/h" ~description:"Move left" ~enabled:false
            ();
          Help.make_binding ~key:"→/l" ~description:"Move right" ();
        ]

      (* Create key groups for full help *)
      let key_groups =
        [
          Help.make_group ~title:"Navigation" navigation_bindings;
          Help.make_group ~title:"Actions"
            [
              Help.make_binding ~key:"Enter" ~description:"Select" ();
              Help.make_binding ~key:"Esc" ~description:"Cancel" ();
            ];
        ]

      (* Initialize help component *)
      let help_model, help_cmd = Help.init ~show_full:false ()

      (* Render short help *)
      let short_help = Help.view_short navigation_bindings model.help

      (* Render full help *)
      let full_help = Help.view_full key_groups model.help

      (* Toggle between short and full help *)
      let new_help, cmd = Help.toggle model.help
    ]} *)

open Mosaic

(** {2 Types} *)

type model
(** The internal state of the help component containing display mode and width
    settings. *)

type msg
(** Messages that the help component can handle, primarily for toggling display
    modes. *)

type key_binding = {
  key : string;  (** Key representation (e.g., "↑/k", "ctrl+c") *)
  description : string;  (** What the key does *)
  enabled : bool;  (** Whether this binding is currently active *)
}
(** A single key binding with its description and enabled state. *)

type key_group = {
  title : string option;  (** Optional group title *)
  bindings : key_binding list;  (** Bindings in this group *)
}
(** A group of related key bindings, optionally titled for organization. *)

val component : (model, msg) Mosaic.app
(** The help component definition following The Elm Architecture. *)

(** {2 Initialization} *)

val init : ?show_full:bool -> ?width:int -> unit -> model * msg Cmd.t
(** [init ?show_full ?width ()] creates a new help component.

    @param show_full Start with full help displayed (default: false)
    @param width Maximum width for help display (default: 80) *)

(** {2 Accessors} *)

val is_showing_full : model -> bool
(** [is_showing_full model] returns whether the help is in full display mode. *)

val width : model -> int
(** [width model] returns the maximum width for help display. *)

(** {2 Actions} *)

val toggle_full : model -> model
(** [toggle_full model] toggles between short and full help display modes. *)

val toggle : model -> model * msg Cmd.t
(** [toggle model] toggles between short and full help views, returning a
    message for component updates. *)

val show_full : bool -> model -> model
(** [show_full enabled model] sets the help display mode to full if [enabled] is
    true, otherwise short. *)

val set_width : int -> model -> model
(** [set_width w model] sets the maximum width for help display. *)

(** {2 Rendering} *)

val view_short : key_binding list -> model -> Ui.element
(** [view_short bindings model] renders a short inline help view from a list of
    key bindings. Only enabled bindings are shown, separated by the theme's
    short separator. *)

val view_full : key_group list -> model -> Ui.element
(** [view_full groups model] renders a full help view from grouped key bindings.
    Groups are displayed in columns with optional titles. Only groups with at
    least one enabled binding are shown. *)

(** {2 Theming} *)

type theme = {
  key_style : Style.t;
  desc_style : Style.t;
  separator_style : Style.t;
  group_title_style : Style.t;
  short_separator : string;
  group_separator : string;
  ellipsis : string;
}
(** Theme configuration for customizing the help display appearance.

    - [key_style] - Style for key representations
    - [desc_style] - Style for key descriptions
    - [separator_style] - Style for separators between items
    - [group_title_style] - Style for group titles in full help
    - [short_separator] - Separator string for short help (default: " • ")
    - [group_separator] - Separator between groups in full help
    - [ellipsis] - String shown when content is truncated *)

val default_theme : theme
(** The default theme with standard styling for help displays. *)

val with_theme : theme -> model -> model
(** [with_theme theme model] applies a custom theme to the help component. *)

(** {2 Component Interface} *)

val update : msg -> model -> model * msg Cmd.t
(** [update msg model] handles messages and updates the help state. *)

val view : model -> Ui.element
(** [view model] renders the help component. Note: this returns an empty
    element; use [view_short] or [view_full] for actual rendering. *)

val subscriptions : model -> msg Sub.t
(** [subscriptions model] returns subscriptions for the help component.
    Currently returns no subscriptions as help display is not interactive. *)

(** {2 Utility Functions} *)

val make_binding :
  key:string -> description:string -> ?enabled:bool -> unit -> key_binding
(** [make_binding ~key ~description ?enabled ()] creates a key binding.

    @param key The key representation (e.g., "↑/k", "ctrl+c")
    @param description What the key does
    @param enabled Whether this binding is active (default: true) *)

val make_group : ?title:string -> key_binding list -> key_group
(** [make_group ?title bindings] creates a group of related key bindings.

    @param title Optional title for the group
    @param bindings List of key bindings in this group *)
