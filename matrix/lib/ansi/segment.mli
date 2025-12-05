(** Styled text segment rendering.

    Provides efficient rendering of styled text segments with optimized style
    transitions and hyperlink management. Segments are [(style, text)] pairs
    rendered sequentially with minimal escape sequences.

    {1 Rendering Model}

    Segments are rendered by:

    1. Computing minimal style transitions between consecutive segments 2.
    Managing hyperlink boundaries (OSC 8 sequences) 3. Emitting only changed
    style components to reduce output size

    The renderer tracks current style and hyperlink state to avoid redundant
    escape sequences.

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

    For buffer-based rendering with state tracking:
    {[
      let buf = Buffer.create 256 in
      let final_style, active_link = Segment.emit buf segments in
      (* final_style contains the last active style *)
      (* active_link contains URL if a hyperlink is still open *)
    ]}

    {1 Hyperlink Management}

    Hyperlinks are tracked across segments:

    - Opening a new hyperlink closes any active hyperlink
    - {!render} automatically closes hyperlinks at the end
    - {!emit} returns the active hyperlink for manual management *)

val emit :
  ?prev:Style.t ->
  ?hyperlinks_enabled:bool ->
  Escape.writer ->
  (Style.t * string) list ->
  Style.t * string option
(** [emit ?prev buf segments] renders styled [segments] to [buf].

    @param prev
      Previous style state, defaults to {!Style.default}. Use to continue
      rendering from a known state.
    @param buf Output buffer.
    @param segments List of [(style, text)] pairs to render.
    @return
      [(final_style, active_link)] where [final_style] is the last style applied
      and [active_link] is [Some url] if a hyperlink is still open, [None]
      otherwise.

    Style transitions are optimized to emit only changed components. Hyperlinks
    are managed automatically, closing previous links when new ones open.

    Use the returned state to chain multiple [emit] calls:
    {[
      let style1, link1 = Segment.emit buf segments1 in
      let style2, link2 = Segment.emit ~prev:style1 buf segments2 in
      (* Continues from the style state of segments1 *)
    ]}

    Hyperlinks remain open if the final segment contains one; use the returned
    [active_link] to decide whether to emit {!Escape.hyperlink_end} or to
    continue streaming. *)

val render :
  ?prev:Style.t -> ?hyperlinks_enabled:bool -> (Style.t * string) list -> string
(** [render ?prev segments] renders styled [segments] to a string.

    @param prev Previous style state, defaults to {!Style.default}.

    Automatically closes any active hyperlink at the end and appends a final
    reset [\[ESC[0m]]. Equivalent to {!emit} followed by extracting the buffer
    contents, closing hyperlinks, and resetting style.

    {[
      let output =
        Segment.render
          [
            (Style.make ~fg:Color.red ~bold:true (), "Error:");
            (Style.default, " File not found");
          ]
    ]} *)
