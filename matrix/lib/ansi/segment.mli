(** Styled segments written with the fewest style and hyperlink changes, the
    implementation of {!Ansi.render}.

    A segment is a style and the text it styles. {!emit} writes segments one
    after the other, with the OSC 8 and the transition ({!Style.emit}) from the
    segment before. *)

(** {1:segments Segments} *)

type segment = Style.t * string
(** The type for segments. *)

(** {1:state Render state} *)

type state = {
  style : Style.t;  (** The style the terminal is in. *)
  link_open : bool;  (** [true] iff a hyperlink is open. *)
}
(** The type for the state of the terminal {!emit} writes to. The state given to
    {!emit} must be the terminal's, or the hyperlinks it writes are unbalanced.
*)

val initial_state : state
(** [initial_state] is {!Style.default} without an open hyperlink. *)

(** {1:rendering Rendering} *)

val emit :
  ?state:state -> ?hyperlinks_enabled:bool -> Writer.t -> segment list -> state
(** [emit ~state ~hyperlinks_enabled w segs] writes [segs] to [w] from [state]
    and is the state after them. Where the link changes, it closes the open
    hyperlink and opens the new one, unless [hyperlinks_enabled] is [false].
    [state] defaults to {!initial_state} and [hyperlinks_enabled] to [true]. *)

val render : ?state:state -> ?hyperlinks_enabled:bool -> segment list -> string
(** [render ~state ~hyperlinks_enabled segs] is the bytes {!emit} writes,
    followed by the OSC 8 that closes an open hyperlink and [CSI 0 m]. *)
