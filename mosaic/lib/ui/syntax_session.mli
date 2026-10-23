(** The highlighting of a view's content.

    A session keeps a view's captures up to date with the content it shows: it
    requests captures from a highlighter, at most one request at a time,
    coalesces the changes made meanwhile, keeps the latest result and the
    highlighter's state for the next request, and says which captures describe
    the content shown. {!Code} and the editable surface of {!Textarea} each hold
    one. {!Code} documents highlighters and syntax settings. *)

(** {1:highlighters Highlighters} *)

module Highlighter : sig
  type edit = { start : int; removed : int; inserted : int }
  type 'state previous = { content : string; state : 'state; edit : edit }

  type 'state request = {
    content : string;
    language : string;
    previous : 'state previous option;
  }

  type 'state result = (Syntax_highlight.t * 'state, exn) Stdlib.result
  type 'state job
  type t

  val job :
    poll:(unit -> 'state result option) -> cancel:(unit -> unit) -> 'state job

  val sync : ('state request -> Syntax_highlight.t * 'state) -> t
  val threaded : ('state request -> Syntax_highlight.t * 'state) -> t
  val async : ('state request -> notify:(unit -> unit) -> 'state job) -> t
end

(** {1:syntax Syntax settings} *)

type syntax
(** Where a view's captures come from, and the style that presents them. *)

val syntax :
  ?language:string -> ?style:Syntax_style.t -> Syntax_highlight.t -> syntax

val with_highlighter :
  language:string -> ?style:Syntax_style.t -> Highlighter.t -> syntax

val language : syntax -> string option
val style : syntax -> Syntax_style.t

val equal : syntax -> syntax -> bool
(** [equal a b] compares by value, except highlighters, by identity. *)

(** {1:sessions Sessions} *)

type t
(** The type for a view's highlighting. *)

val create : notify:(unit -> unit) -> t
(** [create ~notify] shows nothing yet. [notify] schedules a render of the view;
    a highlighter may call it from any thread. *)

val set : t -> content:string -> syntax option -> unit
(** [set t ~content syntax] makes [t] describe [content] under [syntax]. A
    change of content, highlighter or language requests captures; a change of
    style alone requests nothing. *)

val poll : t -> bool
(** [poll t] takes the results of completed requests and starts the request that
    waited for them. It is [true] iff {!shown} may have changed. *)

val shown : t -> conceal:bool -> Syntax_highlight.t option
(** [shown t ~conceal] is the captures that describe the content shown, bound to
    it, or [None] when it shows plain. While a request is pending they are the
    latest result's, moved onto the content: a capture keeps its bytes the
    content shares with the result's source as a common prefix or suffix, and
    the other bytes are plain. When [conceal] holds, a concealing capture that
    the change cuts through is dropped, since its replacement stood for all its
    bytes. *)

val is_highlighting : t -> bool
(** [is_highlighting t] is [true] while a result for the content shown is
    pending. *)

val cancel : t -> unit
(** [cancel t] cancels the request in flight and drops the one waiting. *)
