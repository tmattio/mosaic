(** The style a terminal is in, tracked so that only changes are written.

    A renderer writes a frame cell by cell, and most cells have the style of the
    cell before them. A {e tracker} ({!type-t}) records the colors, attributes
    and hyperlink it last wrote to the terminal, its {e state}, and {!update}
    writes only the codes that change that state into a cell's style. The state
    is right only while everything written to the terminal goes through the
    tracker. Output that bypasses it, such as a subprocess's, leaves the
    terminal in a state the tracker does not know. A tracker in the {e unknown}
    state writes [CSI 0 m] and the whole style at its next {!update}.

    A tracker writes colors at a {e color depth} ({!Color.type-depth}). It first
    reduces each color with {!Color.to_depth}. At [`Ansi16] and [`Ansi8] it
    writes the palette's first 16 entries with the codes 30..37, 90..97, 40..47
    and 100..107. {!update} writes with the writer's functions and builds no
    closure, list or string.

    The life cycle of a tracker:
    + {!create} makes one in the unknown state, at depth [`Truecolor].
    + {!set_color_depth} sets the depth the terminal reports.
    + For each cell of a frame, {!update} writes the change to the cell's style.
    + At the end of a frame, {!close_link} closes an open hyperlink.
    + After output that bypassed the tracker, {!reset} puts it in the unknown
      state.

    A tracker is mutable and must not be used by two domains in parallel. *)

(** {1:state State} *)

type t
(** The type for trackers. *)

type color_depth = Color.depth
(** The type for color depths, {!Color.type-depth}. *)

(** {1:lifecycle Life cycle} *)

val create : unit -> t
(** [create ()] is a tracker in the unknown state, at depth [`Truecolor]. *)

val reset : t -> unit
(** [reset s] puts [s] in the unknown state. It forgets an open hyperlink
    without closing it, so a caller that wants the link closed calls
    {!close_link} first. *)

val set_color_depth : t -> color_depth -> unit
(** [set_color_depth s d] sets the depth of [s] to [d]. If [d] differs from the
    depth [s] had, the colors and attributes of [s] become unknown, so the next
    {!update} writes them whole. *)

(** {1:operations Writing} *)

val update :
  t -> Writer.t -> fg:Color.t -> bg:Color.t -> attrs:int -> link:string -> unit
(** [update s w ~fg ~bg ~attrs ~link] writes to [w] what changes the terminal
    from the state of [s] to the style [fg], [bg], [attrs] and [link], and makes
    that style the state of [s]. [attrs] is a set packed by {!Attr.pack}, and
    [link] is a URL, or [""] for none.

    It first writes the OSC 8 that closes the open hyperlink and opens [link],
    if the link changed. Then, if the colors or attributes changed, it writes
    one SGR sequence. The sequence is [CSI 0 m] and the whole style if the state
    was unknown or if the foreground, background and attributes all changed, and
    the codes of what changed otherwise. *)

val close_link : t -> Writer.t -> unit
(** [close_link s w] writes the OSC 8 that closes the open hyperlink of [s], if
    one is open. *)
