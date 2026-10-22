(** Syntax styles: maps capture-group names to terminal styles.

    A syntax style resolves dot-separated group names with hierarchical
    fallback: ["keyword.control.flow"] tries ["keyword.control.flow"], then
    ["keyword.control"], then ["keyword"]. This follows the TextMate/tree-sitter
    scope convention.

    {1:scopes Scope vocabulary}

    Portable highlighters emit scopes whose first segment is one of: [comment],
    [string], [number], [constant], [keyword], [operator], [punctuation],
    [function], [type], [variable], [module], [attribute], [label], [embedded],
    or [tag]. Dotted refinements, such as [variable.member], [function.builtin],
    and [keyword.control], retain the corresponding base category. [embedded]
    identifies an embedded-language region; its contents may carry more specific
    scopes.

    This vocabulary is the backend/theme contract, not a language registry.
    Producers should normalize backend-specific captures into these categories.
    Applications may define additional scopes in a custom style. Unmapped scopes
    are retained and resolve to the base style; they are never dropped. Language
    names remain opaque application-defined strings.

    See doc/highlighting.md for integration recipes. *)

(** {1:types Types} *)

type t
(** The type for syntax styles. *)

(** {1:constructors Constructors} *)

val make : base:Ansi.Style.t -> (string * Ansi.Style.t) list -> t
(** [make ~base mappings] is a syntax style with [base] as the default style for
    unstyled text and [mappings] associating capture-group names to overlay
    styles. When a group name appears more than once, its last mapping wins.

    Group names follow the tree-sitter/TextMate dot-separated convention:
    ["keyword"], ["keyword.control"], ["string.special.regex"], etc.

    See also {!val-default}. *)

val default : t
(** [default] is the built-in dark syntax style with mappings for common capture
    groups. *)

(** {1:predicates Predicates} *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] have equal base styles and map the
    same group names to equal overlays. Styles are values: two styles built by
    separate calls from the same mappings are equal, so a view may rebuild its
    style on every render without restyling anything. *)

(** {1:resolving Resolving} *)

val base : t -> Ansi.Style.t
(** [base style] is the default style for text outside highlighted ranges. *)

val resolve_overlay : t -> string -> Ansi.Style.t
(** [resolve_overlay style group] is the raw overlay style for [group], using
    hierarchical fallback. Returns {!Ansi.Style.default} when no match is found.

    Unlike {!val-resolve}, the result is {e not} merged with the base style,
    which is useful for composing multiple overlays manually. *)

val resolve : t -> string -> Ansi.Style.t
(** [resolve style group] is the complete style for [group]: the group's overlay
    merged on top of the base style, using hierarchical fallback. Returns the
    base style when no match is found.

    See also {!val-resolve_overlay}. *)
