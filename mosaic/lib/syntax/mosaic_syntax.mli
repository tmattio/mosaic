(** Incremental syntax highlighting built on Tree-sitter.

    This library provides Tree-sitter-based syntax highlighting for OCaml
    applications. It parses code incrementally, returning byte-range highlights
    with capture group names (e.g., [@keyword], [@string]). Styling is left to
    the consumer.

    {1 Overview}

    Mosaic_syntax follows a three-tier architecture:

    - {!Language.t} bundles a Tree-sitter language, highlight queries, and
      optional injection queries.
    - {!Set.t} is an immutable collection of languages, indexed by
      {!type-filetype}.
    - {!Session.t} is a stateful parser for a single filetype, supporting
      incremental updates.

    Typical usage:

    {[
      let languages = Mosaic_syntax.builtins () in
      let session =
        Mosaic_syntax.Session.create_exn languages ~filetype:"ocaml"
      in
      let highlights =
        Mosaic_syntax.Session.highlight session ~content:"let x = 42"
      in
      Array.iter
        (fun h -> Printf.printf "%d-%d: %s\n" h.start_byte h.end_byte h.group)
        highlights
    ]}

    {1 Key Concepts}

    {2 Capture Groups}

    Highlight queries use Tree-sitter captures (e.g., [@keyword], [@string]).
    The library returns these group names uninterpreted. Capture names starting
    with [_] are excluded by default unless [include_private:true] is passed to
    {!Session.highlight}.

    {2 Injections}

    Languages can define injection queries to highlight nested languages (e.g.,
    OCaml code inside Markdown fenced blocks). Injection queries must use these
    captures:

    - [@injection.content]: the node containing the injected content.
    - [@injection.language]: a node whose text is a selector string resolved by
      {!Set.resolve_injection}.

    Injections are applied recursively up to a depth limit (2) to prevent
    cycles.

    {2 Incremental Parsing}

    Sessions maintain a parse tree and reuse it across {!Session.highlight}
    calls. Call {!Session.reset} to discard the tree and reparse from scratch.
    Close sessions with {!Session.close} to release Tree-sitter resources.

    {1 Performance and Constraints}

    - {!Session.highlight} is O(content length) for the initial parse and
      O(changed regions) for incremental updates.
    - Injection resolution is limited to 2 levels of nesting.
    - All operations are non-blocking and run on the calling thread. *)

type filetype = string
(** A language identifier (e.g., ["ocaml"], ["json"]).

    Filetype strings are case-sensitive and must match the filetype specified in
    {!Language.make}. The {!Set.default_resolve_injection} function normalizes
    selectors to lowercase. *)

type error =
  | Unknown_filetype of filetype
      (** The requested filetype is not present in the {!Set.t}. *)
  | Query_compile_failed of {
      filetype : filetype;
      query : [ `Highlights | `Injections ];
      message : string;
    }
      (** Tree-sitter query compilation failed.

          The [message] field contains the error from Tree-sitter. Common causes
          include invalid capture names or syntax errors in the query string. *)

module Highlight : sig
  (** Syntax highlight capture ranges. *)

  type origin = [ `Host | `Injection of filetype ]
  (** The source of a highlight.

      - [`Host]: from the primary language's highlight query.
      - [`Injection ft]: from an injected language with filetype [ft]. *)

  type t = {
    start_byte : int;
        (** Start byte offset (inclusive) in the content string. *)
    end_byte : int;  (** End byte offset (exclusive) in the content string. *)
    group : string;
        (** Capture group name from the Tree-sitter query (e.g., ["keyword"]).
        *)
    origin : origin;
        (** Whether this highlight comes from the host language or an injection.
        *)
  }
  (** A highlight range with metadata.

      Invariant: [start_byte < end_byte]. Empty ranges are excluded by
      {!Session.highlight}.

      Byte offsets are valid indices into the UTF-8 encoded content string
      passed to {!Session.highlight}. *)

  val compare : t -> t -> int
  (** [compare a b] orders highlights by [start_byte], then [end_byte].

      Used internally to sort highlight arrays. Returns a negative integer if
      [a] comes before [b], zero if equal, and positive if [a] comes after [b].
  *)
end

module Language : sig
  (** Language definitions with Tree-sitter queries.

      A language bundles a Tree-sitter grammar, a highlight query, and an
      optional injection query. *)

  type t
  (** An opaque language definition. *)

  val filetype : t -> filetype
  (** [filetype lang] returns the filetype identifier. *)

  val ts_language : t -> Tree_sitter.Language.t
  (** [ts_language lang] returns the underlying Tree-sitter language. *)

  val capture_groups : t -> string list
  (** [capture_groups lang] returns all capture names defined in the highlight
      query, sorted lexicographically.

      Private captures (starting with [_]) are included. Use this to discover
      available groups for styling. *)

  val make :
    filetype:filetype ->
    ts_language:Tree_sitter.Language.t ->
    highlights:string ->
    ?injections:string ->
    unit ->
    (t, error) result
  (** [make ~filetype ~ts_language ~highlights ?injections ()] compiles a
      language from query strings.

      [highlights] is the Tree-sitter query string for syntax highlighting. It
      must contain captures like [@keyword], [@string], etc.

      [injections] is an optional query string for language injections. If
      provided, it must use these capture names:
      - [@injection.content]: the node containing the injected code.
      - [@injection.language]: a node whose text is a selector string (e.g.,
        ["ocaml"], ["json"]).

      The selector string is resolved by {!Set.resolve_injection}.

      Returns [Error (Query_compile_failed _)] if Tree-sitter cannot parse a
      query.

      {4 Example}

      {[
        let ocaml_lang =
          Mosaic_syntax.Language.make ~filetype:"ocaml"
            ~ts_language:Tree_sitter_ocaml.ocaml ()
            ~highlights:
              {|
              (comment) @comment
              ["let" "in"] @keyword
              (value_name) @variable
            |}
            ()
      ]} *)

  val make_exn :
    filetype:filetype ->
    ts_language:Tree_sitter.Language.t ->
    highlights:string ->
    ?injections:string ->
    unit ->
    t
  (** [make_exn ~filetype ~ts_language ~highlights ?injections ()] is like
      {!make} but raises [Invalid_argument] on error. *)
end

module Set : sig
  (** Immutable collections of languages.

      A set indexes languages by filetype and provides injection resolution. *)

  type t
  (** An immutable language set. *)

  type resolve_injection = host:filetype -> selector:string -> filetype option
  (** A function that resolves injection selectors to filetypes.

      [host] is the filetype of the language containing the injection.
      [selector] is the text from the [@injection.language] capture. Return
      [None] to skip the injection. *)

  val create : ?resolve_injection:resolve_injection -> Language.t list -> t
  (** [create ?resolve_injection langs] builds a language set.

      If multiple languages share the same filetype, the last one in [langs]
      wins. The resulting {!languages} list contains unique filetypes in reverse
      order of appearance.

      [resolve_injection] defaults to {!default_resolve_injection}.

      {4 Example}

      {[
        let ocaml_lang = (* ... *) in
        let json_lang = (* ... *) in
        let set = Mosaic_syntax.Set.create [ocaml_lang; json_lang]
      ]} *)

  val default_resolve_injection : resolve_injection
  (** [default_resolve_injection ~host ~selector] is a Markdown-style resolver.

      Algorithm: 1. Trim leading and trailing whitespace from [selector]. 2.
      Take the first whitespace-separated token. 3. Convert to lowercase. 4.
      Return [None] for empty strings.

      Example: ["  OCaml stuff"] resolves to [Some "ocaml"]. *)

  val languages : t -> Language.t list
  (** [languages set] returns the list of unique languages, ordered by last
      occurrence in the input to {!create}. *)

  val find : t -> filetype -> Language.t option
  (** [find set ft] looks up a language by filetype.

      Returns [None] if [ft] is not in [set]. *)
end

module Session : sig
  (** Stateful parsing sessions for incremental highlighting.

      A session maintains a parse tree and provides incremental updates. Create
      one session per widget or consumer that needs highlighting for a specific
      filetype. *)

  type t
  (** A stateful parsing session.

      Sessions hold Tree-sitter parsers, cursors, and parse trees. They are not
      thread-safe; concurrent calls to {!highlight} from multiple threads must
      be synchronized externally. *)

  val create : Set.t -> filetype:filetype -> (t, error) result
  (** [create set ~filetype] creates a session for [filetype].

      Returns [Error (Unknown_filetype ft)] if [ft] is not in [set].

      Sessions allocate Tree-sitter resources. Call {!close} when done to
      release them. *)

  val create_exn : Set.t -> filetype:filetype -> t
  (** [create_exn set ~filetype] is like {!create} but raises [Invalid_argument]
      on error. *)

  val filetype : t -> filetype
  (** [filetype session] returns the session's filetype. *)

  val reset : t -> unit
  (** [reset session] discards the incremental parse tree.

      The next call to {!highlight} will parse from scratch. Use this after
      large content changes where incremental parsing overhead exceeds a full
      reparse. *)

  val close : t -> unit
  (** [close session] releases Tree-sitter resources.

      After [close], [session] must not be used. Calling {!highlight}, {!reset},
      or {!close} again results in [Invalid_argument]. *)

  val highlight :
    ?include_private:bool -> t -> content:string -> Highlight.t array
  (** [highlight ?include_private session ~content] parses [content] and returns
      sorted highlights.

      [include_private] controls whether capture groups starting with [_] are
      included. Default is [false].

      The session reuses the previous parse tree for incremental parsing. If the
      language defines injection queries, injected regions are highlighted
      recursively (up to depth 2) and merged into the result.

      Invariants:
      - The returned array is sorted by {!Highlight.compare}.
      - All highlights have [start_byte < end_byte].
      - Byte offsets are valid indices into [content].

      @raise Invalid_argument if [session] is closed.

      {4 Example}

      {[
        let session =
          Mosaic_syntax.Session.create_exn languages ~filetype:"ocaml"
        in
        let highlights =
          Mosaic_syntax.Session.highlight session ~content:"let x = 42"
        in
        (* First call: full parse *)
        let highlights' =
          Mosaic_syntax.Session.highlight session ~content:"let x = 43"
        in
        (* Second call: incremental update *)
        Mosaic_syntax.Session.close session
      ]} *)
end

val builtins : unit -> Set.t
(** [builtins ()] returns a language set with built-in OCaml and JSON support.

    The set includes:
    - ["ocaml"]: OCaml syntax with captures for comments, strings, keywords,
      types, modules, and variables.
    - ["json"]: JSON syntax with captures for strings, numbers, booleans, null,
      and object keys.

    This function allocates a new set on each call. For performance, call once
    and reuse the result.

    {4 Example}

    {[
      let languages = Mosaic_syntax.builtins () in
      let session = Mosaic_syntax.Session.create_exn languages ~filetype:"ocaml"
    ]} *)
