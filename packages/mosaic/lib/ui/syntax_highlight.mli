(** Source highlight ranges.

    [Syntax_highlight] is the stable bridge between syntax highlighters and code
    renderables. It describes styled byte ranges over a UTF-8 source string
    without depending on a concrete highlighter such as Tree-sitter: a producer
    names each range with a scope (see {!Syntax_style} for the vocabulary), and
    a {!Syntax_style.t} chosen by the view turns scopes into terminal styles.

    Captures are bound to the source they describe. A range means nothing
    without its bytes, so a consumer shows captures only over exactly those
    bytes: captures computed for another source, even one of the same length,
    are rejected rather than coloring text they were not computed for.

    Ranges can carry metadata used by source renderables for behaviours such as
    concealment and injection-container suppression. *)

type meta = {
  is_injection : bool;
      (** [true] when the range belongs to injected source code. *)
  contains_injection : bool;
      (** [true] when the range contains injected source code. *)
  conceal : string option;
      (** Replacement text when concealment is enabled. [Some ""] hides the
          range. *)
  conceal_lines : bool;
      (** [true] when the line break following the range should be hidden when
          concealment is enabled. *)
}
(** Metadata attached to a highlight range. *)

val default_meta : meta
(** [default_meta] has all flags disabled and no conceal replacement. *)

type range
(** A highlighted source byte range. *)

type t
(** Captures bound to the exact source bytes they describe. *)

val range :
  ?meta:meta -> start_byte:int -> end_byte:int -> scope:string -> unit -> range
(** [range ~start_byte ~end_byte ~scope ()] highlights bytes from [start_byte]
    inclusive to [end_byte] exclusive with capture scope [scope]. Offsets count
    bytes of the UTF-8 source, not characters or graphemes, as tree-sitter's
    captures do. An empty range highlights nothing.

    Raises [Invalid_argument] if [start_byte < 0] or [end_byte < start_byte]. *)

val make : content:string -> range list -> t
(** [make ~content ranges] binds [ranges] to [content]. Raises
    [Invalid_argument] if a range exceeds the source byte length. *)

val of_triples : content:string -> (int * int * string) list -> t
(** [of_triples ~content ranges] binds [(start_byte, end_byte, scope)] ranges
    with {!default_meta} to [content]. Validates bounds as {!make}. *)

val content : t -> string
(** [content t] is the original source, including concealed bytes. *)

val ranges : t -> range list
(** [ranges t] is the capture list in declaration order. *)

val start_byte : range -> int
(** [start_byte r] is the inclusive byte start of [r]. *)

val end_byte : range -> int
(** [end_byte r] is the exclusive byte end of [r]. *)

val scope : range -> string
(** [scope r] is the capture scope of [r]. *)

val meta : range -> meta
(** [meta r] is the metadata attached to [r]. *)

val conceals : range -> bool
(** [conceals r] is [true] iff concealment changes the bytes [r] shows. That
    holds when [r] has a [conceal] replacement, hides the line break after it
    ([conceal_lines]), or has a scope that is [conceal] or starts with
    [conceal.]. Such a scope without a replacement hides the range, and
    [conceal.with.space] shows one space in its place. *)

val to_annotations :
  ?conceal:bool -> style:Syntax_style.t -> t -> Text_buffer.Annotations.t
(** [to_annotations ~style t] resolves captures over their source. Overlaps
    merge by scope specificity, least-specific first, then declaration order.
    Uncaptured text receives the syntax base style. Offsets count bytes,
    including line breaks; grapheme rendering follows
    {!Text_buffer.Annotations.make}.

    [conceal] defaults to [false], preserving the original source for editable
    text. When [true], conceal metadata projects a visible source; annotations
    then belong to that projected source. The source and conceal projection are
    shared across theme changes without copying the text. *)

val to_spans :
  ?conceal:bool -> style:Syntax_style.t -> t -> Text_buffer.span list
(** [to_spans ~style t] materializes the same result as {!to_annotations},
    copying styled substrings. [conceal] defaults to [true]. Rendering
    annotations directly avoids these copies. *)
