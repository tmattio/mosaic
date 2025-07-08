open Mosaic

(** Markdown rendering for Mosaic applications.

    This library provides a way to render markdown text into structured
    {!Mosaic.Ui.element} values, allowing for easy integration into Mosaic TUIs.
    Styling is highly configurable through the {!Style} module. *)

(** {1 Styling} *)

module Style : module type of Markdown_style

(** {1 Rendering} *)

val render : ?style:Style.t -> ?width:int -> string -> Ui.element
(** [render ?style ?width markdown_text] parses and renders a markdown string
    into a Mosaic UI element.

    @param style The style configuration to use. Defaults to {!Style.default}.
    @param width
      The maximum width for rendering and word-wrapping. Defaults to 80.
    @param markdown_text The markdown string to render.
    @return A {!Ui.element} representing the rendered markdown. *)
