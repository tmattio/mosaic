(** Flexible form components inspired by modern web component libraries

    This module provides form components with flexible label positioning,
    validation states, helper text, and other features common in modern web UI
    libraries like Material UI, Ant Design, and Chakra UI. *)

open Ui

type size = [ `Small | `Medium | `Large ]
(** Size variants for form components *)

(** Label positioning options for form fields *)
type label_position =
  [ `Top  (** Label above input *)
  | `Left  (** Label to the left of input *)
  | `Right  (** Label to the right of input *)
  | `Inline  (** Label inline with input, no spacing *)
  | `Hidden ]
(** Label hidden but still accessible *)

(** {2 Form Components} *)

val input :
  ?label_position:label_position ->
  ?label_width:int option ->
  ?required:bool ->
  ?helper_text:string option ->
  ?error:string option ->
  ?disabled:bool ->
  ?size:size ->
  ?style:Style.t ->
  ?border:Border.t ->
  ?border_style:Style.t ->
  ?password:bool ->
  ?tab_index:int ->
  ?placeholder:string ->
  ?value:string ->
  ?on_change:(string -> unit) ->
  ?on_submit:(string -> unit) ->
  label:string ->
  unit ->
  element
(** Input field with flexible configuration

    @param label_position Where to position the label (default: `Left)
    @param label_width Fixed width for label alignment in forms
    @param required Show required indicator after label
    @param helper_text Helper text shown below the field
    @param error Error message (shown in red, overrides helper_text)
    @param disabled Whether the field is disabled
    @param size Size variant (affects padding)
    @param style Additional styles for the input
    @param border Optional border for the input field
    @param border_style Style for the border
    @param password Mask input as password
    @param tab_index Tab order index
    @param placeholder Placeholder text when empty
    @param value Controlled value (if provided, component is controlled)
    @param on_change Called when value changes
    @param on_submit Called when Enter is pressed
    @param label Field label text

    Example:
    {[
      input
        ~label_position:`Top
        ~required:true
        ~helper_text:(Some "Enter your full name")
        ~placeholder:"John Doe"
        ~on_change:(fun value -> ...)
        ~label:"Name"
        ()
    ]} *)

val select :
  ?label_position:label_position ->
  ?label_width:int option ->
  ?required:bool ->
  ?helper_text:string option ->
  ?error:string option ->
  ?disabled:bool ->
  ?size:size ->
  ?style:Style.t ->
  ?border:Border.t ->
  ?border_style:Style.t ->
  ?tab_index:int ->
  ?selected:int ->
  ?on_change:(int -> string -> unit) ->
  label:string ->
  options:string list ->
  unit ->
  element

val radio_group :
  ?style:Style.t ->
  ?disabled:bool ->
  ?tab_index:int ->
  ?selected:int ->
  ?on_change:(int -> string -> unit) ->
  label:string ->
  options:string list ->
  unit ->
  element

val textarea :
  ?style:Style.t ->
  ?border:Border.t ->
  ?border_style:Style.t ->
  ?rows:int ->
  ?disabled:bool ->
  ?tab_index:int ->
  ?placeholder:string ->
  ?value:string ->
  ?on_change:(string -> unit) ->
  ?on_submit:(string -> unit) ->
  label:string ->
  unit ->
  element
(** Select/dropdown with flexible configuration

    @param label_position Where to position the label (default: `Left)
    @param label_width Fixed width for label alignment in forms
    @param required Show required indicator after label
    @param helper_text Helper text shown below the field
    @param error Error message (shown in red, overrides helper_text)
    @param disabled Whether the field is disabled
    @param size Size variant (affects padding)
    @param style Additional styles for the select
    @param border Optional border for the select field
    @param border_style Style for the border
    @param tab_index Tab order index
    @param selected
      Controlled selected index (if provided, component is controlled)
    @param on_change Called when selection changes with (index, value)
    @param label Field label text
    @param options List of options to choose from

    Example:
    {[
      select
        ~label_position:`Left
        ~label_width:10
        ~error:validation_error
        ~options:["Option 1"; "Option 2"; "Option 3"]
        ~on_change:(fun idx value -> ...)
        ~label:"Choose"
        ()
    ]} *)

(** {2 Form Layout Helpers} *)

val form : ?spacing:Ui.length_percentage -> element list -> element
(** Form container with consistent spacing

    @param spacing Gap between form fields (default: `Cells 1)

    Example:
    {[
      form ~spacing:(`Cells 2)
        [
          input ~label:"Name" ();
          input ~label:"Email" ();
          select ~label:"Country" ~options:countries ();
        ]
    ]} *)

val field_group :
  ?label:string ->
  ?direction:[ `Vertical | `Horizontal ] ->
  ?spacing:Ui.length_percentage ->
  element list ->
  element
(** Group related fields together

    @param label Optional group label
    @param direction Layout direction (default: `Vertical)
    @param spacing Gap between fields (default: `Cells 1)

    Example:
    {[
      field_group ~label:"Address" ~direction:`Vertical
        [
          input ~label:"Street" ();
          input ~label:"City" ();
          input ~label:"Zip" ();
        ]
    ]} *)

val form_section :
  ?title:string ->
  ?border:Border.t option ->
  ?spacing:Ui.length_percentage ->
  element list ->
  element
(** Form section with optional title and border

    @param title Optional section title
    @param border Optional border around the section
    @param spacing Gap between fields (default: `Cells 1)

    Example:
    {[
      form_section ~title:"Personal Information" ~border:(Some Border.rounded)
        [
          input ~label:"First Name" ();
          input ~label:"Last Name" ();
          input ~label:"Email" ();
        ]
    ]} *)
