(** A help component for displaying keyboard shortcuts and command help.

    This component renders help information based on key bindings. It supports
    both short and full help views and can be used by other components to
    provide consistent help displays. *)

open Mosaic

(** {2 Types} *)

type model
(** The internal state of the help component *)

type msg
(** Messages the help component can handle *)

type key_binding = {
  key : string;  (** Key representation (e.g., "↑/k", "ctrl+c") *)
  description : string;  (** What the key does *)
  enabled : bool;  (** Whether this binding is currently active *)
}
(** A single key binding *)

type key_group = {
  title : string option;  (** Optional group title *)
  bindings : key_binding list;  (** Bindings in this group *)
}
(** A group of related key bindings *)

val component : (model, msg) Mosaic.app
(** The component definition *)

(** {2 Initialization} *)

val init : ?show_full:bool -> ?width:int -> unit -> model * msg Cmd.t
(** [init ?show_full ?width ()] creates a new help component.

    @param show_full Start with full help displayed (default: false)
    @param width Maximum width for help display (default: 80) *)

(** {2 Accessors} *)

val is_showing_full : model -> bool
(** Check if full help is currently displayed *)

val width : model -> int
(** Get the current width setting *)

(** {2 Actions} *)

val toggle_full : model -> model
(** Toggle between short and full help views *)

val toggle : model -> model * msg Cmd.t
(** Toggle between short and full help views with message *)

val show_full : bool -> model -> model
(** Set whether to show full help *)

val set_width : int -> model -> model
(** Update the maximum width *)

(** {2 Rendering} *)

val view_short : key_binding list -> model -> Ui.element
(** Render a short help view from a list of key bindings. Only enabled bindings
    are shown. *)

val view_full : key_group list -> model -> Ui.element
(** Render a full help view from grouped key bindings. Groups are displayed in
    columns. Only groups with at least one enabled binding are shown. *)

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
(** Theme configuration for the help display *)

val default_theme : theme
(** Default help theme *)

val with_theme : theme -> model -> model
(** Apply a custom theme to the help component *)

(** {2 Component Interface} *)

val update : msg -> model -> model * msg Cmd.t
(** Handle messages and update the help state *)

val view : model -> Ui.element
(** Render the help component (empty - use view_short or view_full) *)

val subscriptions : model -> msg Sub.t
(** Help component doesn't need subscriptions *)

(** {2 Utility Functions} *)

val make_binding :
  key:string -> description:string -> ?enabled:bool -> unit -> key_binding
(** Create a key binding. Default enabled is true. *)

val make_group : ?title:string -> key_binding list -> key_group
(** Create a key group with optional title *)
