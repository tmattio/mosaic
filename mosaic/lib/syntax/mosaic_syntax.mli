(** Syntax highlighting support built on Tree-sitter.

    This library provides a small, focused abstraction over the [tree-sitter]
    bindings, suitable for driving Mosaic's [Code] renderable and future
    editor-like widgets. It does not perform any styling itself; instead it
    reports structured highlight ranges that higher-level code can map to
    [Ansi.Style.t] values. *)

type filetype = string
(** Language identifier, typically a short string such as ["ocaml"], ["json"],
    or ["markdown"]. *)

module Injection_mapping : sig
  type t = {
    node_types : (string, filetype) Hashtbl.t;
        (** Map from syntax node kinds (e.g. ["code_fence_content"]) to target
            filetypes. *)
    info_string_map : (string, filetype) Hashtbl.t;
        (** Map from info strings (e.g. markdown code fence tags) to target
            filetypes. *)
  }
end

module Language : sig
  type t = {
    filetype : filetype;
    ts_language : Tree_sitter.Language.t;
    highlight_query : Tree_sitter.Query.t;
    injection_query : Tree_sitter.Query.t option;
    injection_mapping : Injection_mapping.t option;
  }
  (** Description of a Tree-sitter language and its highlight queries for a
      given [filetype]. *)
end

module Highlight : sig
  type meta = {
    is_injection : bool;
    injection_lang : filetype option;
    contains_injection : bool;
    conceal : string option;
    conceal_lines : string option;
  }
  (** Optional metadata for a highlight range.

      Initially left unpopulated; this structure exists so callers and future
      implementations can reason about injections and concealment without
      changing the public API.

      - [is_injection]: whether this range comes from an injected language
      - [injection_lang]: the language of the injection if applicable
      - [contains_injection]: whether this range contains nested injections
      - [conceal]: replacement text to display instead of the source
      - [conceal_lines]: replacement text for multi-line concealment *)

  type t = {
    start_offset : int;
    end_offset : int;
    group : string;
    meta : meta option;
  }
  (** A single highlight range over the original source content.

      Offsets are byte offsets into the UTF-8 source string, with [end_offset]
      exclusive. [group] is the capture name from the highlight query (e.g.
      ["keyword"], ["string.special"]). *)
end

type options = { parsers : Language.t list }
(** Initial configuration for a Tree-sitter client.

    [parsers] provides the set of languages known at creation time. Additional
    languages can be registered later. *)

type t
(** A Tree-sitter client instance.

    Clients own Tree-sitter parsers and trees and can be shared across multiple
    widgets. *)

val create : options -> t
(** [create opts] constructs a new client and registers initial languages from
    [opts.parsers]. *)

val register_language : t -> Language.t -> unit
(** [register_language client lang] adds or replaces the parser associated with
    [lang.filetype]. *)

val has_language : t -> filetype -> bool
(** [has_language client filetype] reports whether a language has been
    registered for [filetype]. *)

val get_language : t -> filetype -> Language.t option
(** [get_language client filetype] returns the language registered for
    [filetype], or [None] if no language is registered. *)

val highlight_once :
  t -> filetype:filetype -> content:string -> (Highlight.t array, string) result
(** [highlight_once client ~filetype ~content] parses [content] using the
    language registered for [filetype] and returns raw highlight captures.

    On success, the result is a dense array of capture ranges sorted by
    ascending [start_offset]. On error (e.g. unknown [filetype]), returns
    [Error msg]. *)

val default_client : unit -> t
(** [default_client ()] returns a process-wide client preconfigured with a small
    set of built-in languages (currently OCaml and JSON).

    The same client instance is returned on every call. *)
