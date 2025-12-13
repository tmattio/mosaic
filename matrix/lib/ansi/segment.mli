(** Styled text segment rendering.

    Renders styled text segments to ANSI escape sequences with automatic style
    transitions and hyperlink management.

    {1 Overview}

    A {!segment} is a [(Style.t, string)] pair representing styled text. The
    renderer tracks terminal state to emit only necessary escape sequences when
    transitioning between segments.

    {1 Usage}

    Render styled segments to a string:
    {[
      let segments =
        [
          (Style.make ~fg:Color.red ~bold:true (), "Error:");
          (Style.default, " ");
          (Style.make ~fg:Color.white (), "File not found");
        ]
      in
      let output = Segment.render segments
    ]}

    For streaming rendering with explicit state tracking:
    {[
      let output =
        Escape.to_string (fun w ->
            let state = Segment.emit w segments1 in
            let state = Segment.emit ~state w segments2 in
            if state.link_open then Escape.hyperlink_end w;
            Escape.reset w)
    ]}

    {1 Hyperlink Management}

    Hyperlinks (OSC 8 sequences) are tracked via the {!state.link_open} field:

    - Opening a new hyperlink automatically closes any active hyperlink
    - {!render} closes hyperlinks and resets style at the end
    - {!emit} returns the current state for manual management

    {b Important}: The [state] parameter must accurately reflect the terminal's
    current escape state. If [state.link_open] is [true], the terminal must have
    an open hyperlink. Mismatches cause unbalanced OSC 8 sequences. *)

type segment = Style.t * string
(** A styled text segment: [(style, text)]. *)

type state = {
  style : Style.t;  (** Current style applied to the terminal. *)
  link_open : bool;  (** Whether a hyperlink is currently open. *)
}
(** Render state tracking the terminal's current escape state.

    This separates "style configuration" from "terminal state", fixing the
    ambiguity of the previous API where passing a style with a hyperlink URL
    implied the hyperlink was already open. *)

val initial_state : state
(** [initial_state] is the default state: {!Style.default} with no open link.

    Use this when starting fresh rendering. *)

val emit :
  ?state:state ->
  ?hyperlinks_enabled:bool ->
  Escape.writer ->
  segment list ->
  state
(** [emit ?state ?hyperlinks_enabled writer segments] renders [segments].

    @param state
      Current terminal state. Defaults to {!initial_state}. Must accurately
      reflect the terminal's escape state for correct output.
    @param hyperlinks_enabled
      Whether to emit OSC 8 hyperlink sequences. Defaults to [true]. When
      [false], hyperlinks in styles are ignored.
    @param writer The escape sequence writer.
    @param segments List of [(style, text)] pairs to render.
    @return
      Updated state reflecting the terminal's escape state after rendering.

    Style transitions emit only changed SGR components. Hyperlinks are managed
    automatically: changing to a different URL closes the previous link.

    {b Example}: Chaining multiple emit calls:
    {[
      let state1 = Segment.emit w segments1 in
      let state2 = Segment.emit ~state:state1 w segments2 in
      (* state2 reflects terminal state after both segment lists *)
    ]} *)

val render : ?state:state -> ?hyperlinks_enabled:bool -> segment list -> string
(** [render ?state ?hyperlinks_enabled segments] renders to a string.

    @param state Initial terminal state. Defaults to {!initial_state}.
    @param hyperlinks_enabled Whether to emit hyperlinks. Defaults to [true].

    Automatically closes any active hyperlink and appends a reset sequence
    ([\027\[0m]) at the end. Equivalent to {!emit} followed by cleanup:
    {[
      Escape.to_string (fun w ->
          let state = emit ~state w segments in
          if state.link_open then Escape.hyperlink_end w;
          Escape.reset w)
    ]} *)
