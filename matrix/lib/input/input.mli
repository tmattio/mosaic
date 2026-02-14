(** Terminal input event handling with keyboard, mouse, and capability tracking.

    Input provides a unified event system for terminal applications, parsing raw
    terminal byte streams into structured events (keyboard, mouse, scroll,
    paste, resize, focus) and capability responses (device attributes, mode
    reports, cursor position). The module supports both legacy terminal
    protocols and modern extended protocols (Kitty keyboard protocol, SGR mouse
    tracking, bracketed paste).

    {1 Overview}

    Terminal input arrives as escape sequences and raw bytes. This module
    handles the complexities of:
    - Parsing ANSI/VT escape sequences into structured events
    - Tracking modifier keys (Ctrl, Alt, Shift, Super) across protocols
    - Normalizing mouse events from different tracking modes (X10, SGR, URXVT)
    - Detecting terminal capabilities via response sequences
    - Buffering incomplete sequences across reads
    - Timing out ambiguous escape sequences

    The event model distinguishes user-facing events ({!t}) from capability
    responses ({!Caps.event}), allowing applications to process input and
    capability detection separately.

    {1 Usage Basics}

    Create a parser and feed raw terminal bytes:
    {[
      let parser = Input.Parser.create () in
      let now = Unix.gettimeofday () in
      Input.Parser.feed parser buffer 0 bytes_read ~now ~on_event:handle_event
        ~on_caps:handle_caps
    ]}

    Pattern match on events:
    {[
      match event with
      | Input.Key { key = Input.Key.Char c; modifier; _ } ->
          if modifier.ctrl then handle_ctrl_char c else handle_char c
      | Input.Mouse (Input.Mouse.Button_press (x, y, Input.Mouse.Left, _)) ->
          handle_click x y
      | Input.Resize (width, height) -> handle_resize width height
      | Input.Paste text -> insert_text text
      | _ -> ()
    ]}

    Use keymaps for key binding:
    {[
      let map =
        Input.Keymap.empty
        |> Input.Keymap.add_char ~ctrl:true 'q' `Quit
        |> Input.Keymap.add Input.Key.Enter `Submit
      in
      match Input.Keymap.find map event with
      | Some `Quit -> exit 0
      | Some `Submit -> submit_form ()
      | None -> ()
    ]}

    {1 Key Concepts}

    {2 Event Types}

    The module provides three core event categories:

    - User-facing events ({!t}): Key presses, mouse actions, scroll, paste,
      resize, focus changes. These are what applications typically handle in
      their event loop.
    - Capability responses ({!Caps.event}): Terminal feature reports like device
      attributes, mode status, pixel dimensions, cursor position. Used for
      runtime capability detection.
    - Key binding events ({!Keymap}): Pattern matching helpers that map key
      combinations to application commands.

    {2 Keyboard Events}

    Keyboard events include the key ({!Key.t}), modifier state
    ({!type-Key.modifier}), and optional metadata from the Kitty protocol (associated
    text, shifted/base keys, event type). Legacy terminals report only press
    events; modern terminals may report press, repeat, and release.

    Modifier keys (Ctrl, Alt, Shift, Super, Hyper, Meta) plus lock states (Caps
    Lock, Num Lock) are tracked per event. Unspecified modifiers in keymap
    patterns match any state, enabling flexible binding.

    {2 Mouse Events}

    Mouse events support multiple tracking modes transparently:
    - Button presses and releases with coordinates
    - Motion events with button state
    - Scroll wheel normalized to direction + delta

    Coordinates are 0-based with origin at the top-left corner. In basic
    tracking modes (X10/Normal, URXVT), button release events report [Button 0]
    because the terminal protocol does not encode which button was released.

    {2 Parser State}

    The parser maintains internal state to handle incomplete sequences. Escape
    sequences may arrive fragmented across multiple {!Parser.feed} calls; the
    parser buffers partial sequences until complete or until they time out.
    Ambiguous sequences (like a lone Escape key vs. Alt+key) use a 50ms timeout;
    clearly incomplete sequences (CSI, OSC, etc.) use a longer 100ms timeout.
    Call {!Parser.drain} after {!Parser.deadline} has passed to emit pending
    events.

    The parser is not thread-safe. Use separate parser instances per terminal or
    synchronize access externally.

    {2 Protocol Support}

    The module handles multiple terminal protocols automatically:
    - Kitty keyboard protocol: Full key event lifecycle (press/repeat/release),
      associated text, base/shifted key representation
    - SGR mouse tracking: Extended coordinates, button release identification
    - URXVT mouse tracking: UTF-8 coordinate encoding
    - X10/Normal mouse tracking: Basic button events
    - Bracketed paste: Strips ANSI sequences from pasted content
    - OSC 52: Clipboard responses with base64 decoding

    {1 Advanced Features}

    {2 Capability Detection}

    Use {!Caps.event} responses to detect terminal features:
    {[
      match caps_event with
      | Input.Caps.Device_attributes attrs -> detect_terminal_type attrs
      | Input.Caps.Mode_report { modes; _ } -> check_feature_support modes
      | Input.Caps.Pixel_resolution (w, h) -> calculate_cell_size w h
      | Input.Caps.Kitty_keyboard { level; _ } ->
          if level > 0 then enable_kitty_keyboard ()
      | _ -> ()
    ]}

    Capability responses arrive asynchronously after querying the terminal with
    control sequences. Pair this with the terminal runtime's query functions.

    {2 Custom Key Bindings}

    Build keymaps incrementally and query them in your event loop:
    {[
      let bindings =
        Keymap.empty
        |> Keymap.add_char ~ctrl:true 'c' `Copy
        |> Keymap.add_char ~ctrl:true 'v' `Paste
        |> Keymap.add ~ctrl:true ~shift:true Key.Tab `Prev
      in
      match Keymap.find bindings event with
      | Some action -> handle_action action
      | None -> default_handler event
    ]}

    Later bindings take precedence over earlier ones. Unspecified modifiers
    match any state. Add general bindings first, then more specific ones to
    override.

    {1 Performance Considerations}

    - Parser buffering is O(sequence_length) with small constants for typical
      escape sequences (< 100 bytes)
    - Event construction allocates; reuse keymaps to avoid rebuilding
    - Mouse motion events can arrive at high rates during drag operations
    - Flush timeouts require external timer integration for responsiveness

    {1 Invariants}

    - Parser state is consistent across {!Parser.feed} calls; partial sequences
      combine with subsequent reads
    - Capability events never appear in the user-facing event list
    - Keymaps are immutable; {!Keymap.add} returns a new map
    - Coordinates in mouse events are 0-based (top-left = 0, 0) *)

include module type of Event
(** @inline *)

module Keymap = Keymap
module Parser = Parser
