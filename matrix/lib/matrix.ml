module Ansi = Ansi
module Grid = Matrix_grid
module Input = Matrix_input
module Screen = Matrix_screen
module Terminal = Matrix_terminal
module Text = Matrix_text

(* Types *)

type kitty_keyboard = [ `Auto | `Off ]
type mode = [ `Alt | `Primary ]
type debug_overlay_corner = Debug_overlay.corner

type event =
  | Key of Input.Keystroke.t
  | Key_release of Input.Keystroke.release
  | Text of string
  | Mouse of Input.Mouse_report.t
  | Paste_start
  | Paste_data of string
  | Paste_end of [ `Complete | `Cut ]
  | Focus_in
  | Focus_out
  | Resize of int * int
  | Color_scheme of [ `Dark | `Light ]
  | Invalid of string
  | Reply of Input.Reply.t

let pp_event ppf = function
  | Key k -> Format.fprintf ppf "Key %a" Input.Keystroke.pp k
  | Key_release r ->
      Format.fprintf ppf "Key_release %a" Input.Keystroke.pp_release r
  | Text s -> Format.fprintf ppf "Text %S" s
  | Mouse m -> Format.fprintf ppf "Mouse %a" Input.Mouse_report.pp m
  | Paste_start -> Format.pp_print_string ppf "Paste_start"
  | Paste_data s -> Format.fprintf ppf "Paste_data %S" s
  | Paste_end `Complete -> Format.pp_print_string ppf "Paste_end Complete"
  | Paste_end `Cut -> Format.pp_print_string ppf "Paste_end Cut"
  | Focus_in -> Format.pp_print_string ppf "Focus_in"
  | Focus_out -> Format.pp_print_string ppf "Focus_out"
  | Resize (cols, rows) -> Format.fprintf ppf "Resize %dx%d" cols rows
  | Color_scheme `Dark -> Format.pp_print_string ppf "Color_scheme Dark"
  | Color_scheme `Light -> Format.pp_print_string ppf "Color_scheme Light"
  | Invalid s -> Format.fprintf ppf "Invalid %S" s
  | Reply r -> Format.fprintf ppf "Reply %a" Input.Reply.pp r

module Backend = struct
  type t = {
    now : unit -> float;
    wake : unit -> unit;
    write_output : bytes -> int -> int -> unit;
    read_input : bytes -> int -> int -> int;
    wait : timeout:float option -> on_event:(event -> unit) -> bool;
    input_is_tty : bool;
    output_is_tty : bool;
    terminal_size : unit -> int * int;
    set_raw_mode : bool -> unit;
    flush_input : unit -> unit;
    protect : (unit -> unit) -> unit;
    cleanup : unit -> unit;
    env : Terminal.Caps.env;
  }
end

type demand = [ `Auto | `Continuous | `Paused ]
type lifecycle = [ `Ready | `Suspended | `Closed ]

type config = {
  mode : mode;
  raw_mode : bool;
  mouse_mode : Terminal.mouse_mode option;
  bracketed_paste : bool;
  kitty_keyboard : kitty_keyboard;
  key_releases : bool;
  exit_on_ctrl_c : bool;
  target_fps : float option;
  explicit_width : bool;
  input_timeout : float option;
  resize_debounce : float option;
  debug_overlay_capacity : int;
  min_tui_height : int;
  pace_redraws : bool;
}

(* Queries sent together, which the terminal answers in order. A batch of
   capability queries, of the runtime's other queries or of the
   application's ends with DA1; a cursor batch is one position report, its
   own answer. *)
type question =
  | Capabilities of Terminal.Caps.batch
  | Cursor
  | Own  (** Queries whose replies the runtime takes as they come. *)
  | Application

type batch = {
  question : question;
  owed : Input.Decoder.owed;
  sent : float;
  mutable reports : int;  (** Cursor reports received. *)
  mutable cursor : (int * int) option;  (** A cursor batch's answer. *)
  mutable answered : bool;
}

type app = {
  terminal : Terminal.t;
  decoder : Input.Decoder.t;
  input_buffer : bytes;
  mutable input_ended : bool;
  (* When input last arrived, from which silence is measured. *)
  mutable last_input : float;
  (* The round trip of the startup probe, 0 until measured. *)
  mutable rtt : float;
  mutable evidence : Terminal.Caps.evidence;
  (* Batches still owed, oldest first. *)
  mutable batches : batch list;
  (* Cursor reports owed by batches given up, which a late report answers
     first. *)
  mutable lost_reports : int;
  (* Events decoded and not yet delivered. *)
  events : event Queue.t;
  (* The size the last resize event reported. *)
  mutable reported_size : int * int;
  config : config;
  backend : Backend.t;
  screen : Screen.t;
  (* One retained writer owns the complete terminal frame. *)
  submit_buf : Ansi.Writer.t;
  mutable running : bool;
  redraw_state : [ `Disabled | `Clean | `Dirty ] Atomic.t;
  mutable width : int;
  mutable height : int;
  (* Primary mode layout *)
  mutable primary : Primary.t;
  (* Layout operations [primary] already counts, sent with the next frame. *)
  mutable layout_ops : Primary.terminal_op list;
  (* The live row the last frame left the cursor on, in [`Primary] mode:
     after a resize, where the cursor went shows where the live region went.
     [None] when unknown. *)
  mutable cursor_live_row : int option;
  (* A resize moved the live region somewhere a cursor report can tell. *)
  mutable reanchor : bool;
  (* Scroll optimisation *)
  mutable scroll_hint : Screen.scroll_hint option;
  (* Cursor state tracking — visual caches are valid only while the cursor is
     visible. [None] means the terminal state is unknown (startup, hidden, or
     after a suspend handed the terminal to another program), so the next
     visible frame must emit. *)
  mutable last_cursor_position : (int * int) option;
  mutable last_cursor_visible : bool option;
  mutable last_cursor_shape : Ansi.cursor_shape option;
  mutable last_cursor_color : (int * int * int) option option;
  (* Resize and frame timing *)
  mutable last_resize_apply_time : float;
  mutable pending_resize : (int * int) option;
  mutable force_full_next_frame : bool;
  mutable next_frame_deadline : float option;
  mutable wakeup_deadline : float option;
  mutable last_render_time : float;
  (* Diagnostics *)
  mutable debug_overlay_enabled : bool;
  mutable debug_overlay_cb : Screen.t -> unit;
  mutable frame_dump_every : int;
  mutable frame_dump_dir : string option;
  mutable frame_dump_pattern : string option;
  mutable frame_dump_hits : bool;
  mutable frame_dump_counter : int;
  mutable lifecycle : lifecycle;
  mutable demand : demand;
  mutable frame_modes_armed : bool;
  mutable live_requests : int;
  mutable restore_modes_on_next_focus : bool;
  (* Whether the current suspend left the alternate-screen buffer, so [resume]
     re-enters it. [false] outside a [~leave_alt:true] suspend. *)
  mutable suspend_left_alt : bool;
}

let with_rollback ~rollback fn =
  try fn ()
  with exn ->
    let backtrace = Printexc.get_raw_backtrace () in
    (try rollback () with _ -> ());
    Printexc.raise_with_backtrace exn backtrace

(* Unix I/O helpers *)

let write_all fd buf off len =
  let rec wait_writable () =
    try ignore (Unix.select [] [ fd ] [] (-1.))
    with Unix.Unix_error (Unix.EINTR, _, _) -> wait_writable ()
  in
  let rec go off remaining =
    if remaining > 0 then
      let n =
        try Unix.write fd buf off remaining with
        | Unix.Unix_error (Unix.EINTR, _, _) -> 0
        | Unix.Unix_error (Unix.EAGAIN, _, _) ->
            wait_writable ();
            0
      in
      go (off + n) (remaining - n)
  in
  go off len

let wake_byte = Bytes.of_string "w"

let wake_fd fd =
  let (_ : int) =
    try Unix.write fd wake_byte 0 1 with Unix.Unix_error _ -> 0
  in
  ()

let drain_wakeup_fd fd =
  let buf = Bytes.create 64 in
  let rec go () =
    match Unix.read fd buf 0 64 with
    | n when n > 0 -> go ()
    | _ -> ()
    | exception Unix.Unix_error _ -> ()
  in
  go ()

(* Process lifecycle *)

let shutdown_handlers : (unit -> unit) list ref = ref []

let run_shutdown_handlers () =
  List.iter (fun f -> try f () with _ -> ()) !shutdown_handlers

let register_shutdown_handler fn = shutdown_handlers := fn :: !shutdown_handlers

let deregister_shutdown_handler fn =
  shutdown_handlers := List.filter (fun f -> f != fn) !shutdown_handlers

let shutdown_signal_handler signum =
  run_shutdown_handlers ();
  exit (128 + signum)

let acquire_signal_dispositions handlers =
  let acquired = ref [] in
  let restore () =
    List.iter
      (fun (signal, previous) ->
        try ignore (Sys.signal signal previous : Sys.signal_behavior)
        with _ -> ())
      !acquired
  in
  try
    List.iter
      (fun (signal, behavior) ->
        match Sys.signal signal behavior with
        | previous -> acquired := (signal, previous) :: !acquired
        | exception Invalid_argument _ -> ())
      handlers;
    let active = ref true in
    fun () ->
      if !active then (
        active := false;
        restore ())
  with exn ->
    let backtrace = Printexc.get_raw_backtrace () in
    restore ();
    Printexc.raise_with_backtrace exn backtrace

let own_process_lifecycle ~signal_handlers ~additional_signals shutdown_fn =
  register_shutdown_handler shutdown_fn;
  try
    let termination_signals =
      if signal_handlers then
        List.map
          (fun signal -> (signal, Sys.Signal_handle shutdown_signal_handler))
          [ Sys.sigterm; Sys.sigint; Sys.sigquit; Sys.sigabrt; Sys.sighup ]
      else []
    in
    let restore_signals =
      acquire_signal_dispositions (termination_signals @ additional_signals)
    in
    let active = ref true in
    fun () ->
      if !active then (
        active := false;
        restore_signals ();
        deregister_shutdown_handler shutdown_fn)
  with exn ->
    let backtrace = Printexc.get_raw_backtrace () in
    deregister_shutdown_handler shutdown_fn;
    Printexc.raise_with_backtrace exn backtrace

let () = at_exit run_shutdown_handlers

(* SIGWINCH: the active Unix app's flag + wakeup pipe fd *)

let winch_received = ref false

(* Unix low-level I/O *)

let wait_input_unix ~input_fd ~wakeup_r ~output_fd =
  let fds = [ input_fd; wakeup_r ] in
  fun ~timeout ~on_event ->
    let readable, _, _ =
      try Unix.select fds [] [] (Option.value ~default:(-1.) timeout)
      with Unix.Unix_error (Unix.EINTR, _, _) -> ([], [], [])
    in
    if List.mem wakeup_r readable then drain_wakeup_fd wakeup_r;
    if !winch_received then (
      winch_received := false;
      let cols, rows = Terminal.size output_fd in
      on_event (Resize (cols, rows)));
    List.mem input_fd readable

let rec read_input_unix ~input_fd buf off len =
  match Unix.read input_fd buf off len with
  | n -> n
  | exception Unix.Unix_error (Unix.EINTR, _, _) ->
      read_input_unix ~input_fd buf off len
  | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> -1

(* Small helpers *)

let clamp lo hi v = max lo (min hi v)

(* Layout *)

let sync_primary_layout t ~resize =
  let region = Primary.live_region t.primary in
  Screen.set_row_offset t.screen region.row_offset;
  if resize then Screen.resize t.screen ~width:t.width ~height:region.height

let force_full_redraw t = t.force_full_next_frame <- true

(* Accessors *)

let size t =
  match t.config.mode with
  | `Alt -> (t.width, t.height)
  | `Primary -> (t.width, (Primary.live_region t.primary).height)

let full_size t = (t.width, t.height)
let mode t = t.config.mode
let pixel_resolution t = Terminal.pixel_resolution t.terminal
let terminal t = t.terminal
let capabilities t = Terminal.capabilities t.terminal
let running t = t.running
let render_eligible t = t.running && t.lifecycle = `Ready

(* A reply needs a terminal on the input side, in raw mode. *)
let can_query t =
  t.backend.input_is_tty && t.config.raw_mode && not t.input_ended

let cadence_active t =
  render_eligible t
  &&
  match t.demand with
  | `Continuous -> true
  | `Auto -> t.live_requests > 0
  | `Paused -> false

let now t = t.backend.now ()

let rec request_redraw t =
  match Atomic.get t.redraw_state with
  | `Disabled -> ()
  | (`Clean | `Dirty) as state ->
      if Atomic.compare_and_set t.redraw_state state `Dirty then
        t.backend.wake ()
      else request_redraw t

let schedule_wakeup t deadline =
  if t.lifecycle <> `Closed then (
    t.wakeup_deadline <- deadline;
    t.backend.wake ())

let redraw_requested t = Atomic.get t.redraw_state = `Dirty

let request_immediate_redraw t =
  if t.lifecycle = `Closed then ()
  else if t.lifecycle <> `Suspended then (
    Atomic.set t.redraw_state `Dirty;
    if cadence_active t then t.next_frame_deadline <- Some (t.backend.now ())
    else t.last_render_time <- Float.neg_infinity;
    t.backend.wake ())

(* The terminal cuts what it receives into cells by the width method the
   runtime sets, and the screen lays out its cells by the same, so a cell is
   as wide on the screen as in the terminal. A change of method voids the
   cells the terminal shows, laid out by the old one. *)
let width_method t =
  if t.config.explicit_width then `Wcwidth
  else (Terminal.capabilities t.terminal).unicode_width

let set_width_method t m =
  Terminal.set_unicode_width t.terminal m;
  if Grid.width_method (Screen.current_grid t.screen) <> m then (
    Screen.set_width_method t.screen m;
    force_full_redraw t)

let refresh_capabilities t =
  let caps = Terminal.capabilities t.terminal in
  let color_depth = if caps.rgb then `Truecolor else `Ansi256 in
  Screen.apply_capabilities t.screen ~explicit_width:caps.explicit_width
    ~explicit_cursor_positioning:caps.explicit_cursor_positioning
    ~hyperlinks:caps.hyperlinks ~color_depth;
  let m = width_method t in
  if m <> Grid.width_method (Screen.current_grid t.screen) then
    set_width_method t m

(* A batch's queries are sent, and what they are owed is added to the
   decoder's counts until the batch is answered or given up. *)
let send_batch t question (bytes, owed) =
  Terminal.send t.terminal bytes;
  Input.Decoder.expect t.decoder owed;
  let b =
    {
      question;
      owed;
      sent = now t;
      reports = 0;
      cursor = None;
      answered = false;
    }
  in
  t.batches <- t.batches @ [ b ];
  b

let da1 = Ansi.(to_string (query Device_attributes))

(* A query whose reply the runtime takes as it comes, such as the pixel size,
   is a batch closed by DA1 too, so that its reply is never taken for the
   application's. *)
let ask_own t q =
  if can_query t then
    ignore
      (send_batch t Own
         ( Ansi.(to_string (query q)) ^ da1,
           { Input.Decoder.cursor_reports = 0; string_replies = 0 } )
        : batch)

(* Input modes follow the configuration and the capabilities, and are set
   again whenever the capabilities change: setting a mode a terminal has is
   idempotent, so this is the difference between what the modes were and what
   they must be. Focus, color scheme and resize reports are set without asking:
   no reference says what a terminal does with a private mode it does not
   know, but xterm (charproc.c, dpmodes: "unknown private code"), kitty
   (screen.c, set_mode_from_const: "Unsupported screen mode"), foot (csi.c,
   decset_decrst: UNHANDLED) and tmux (input.c,
   input_csi_dispatch_sm_private: "unknown") log it and do nothing else. The
   kitty keyboard flags are
   pushed once the terminal has answered its query, then asked for again, so
   that the capabilities say which flags hold. *)

let kitty_flags t = if t.config.key_releases then 0b11111 else 0b00101

let apply_modes t =
  let terminal = t.terminal in
  let caps = Terminal.capabilities terminal in
  Terminal.set_mouse_mode terminal
    (Option.value t.config.mouse_mode ~default:`Off);
  Terminal.enable_bracketed_paste terminal t.config.bracketed_paste;
  Terminal.enable_focus_reporting terminal true;
  Terminal.enable_color_scheme_reports terminal true;
  Terminal.enable_resize_reports terminal true;
  Terminal.enable_meta_sends_escape terminal
    (caps.meta_sends_escape = Some false);
  let kitty =
    t.config.kitty_keyboard = `Auto && Option.is_some caps.kitty_keyboard
  in
  if kitty && not (Terminal.kitty_keyboard_enabled terminal) then (
    Terminal.enable_kitty_keyboard ~flags:(kitty_flags t) terminal true;
    ask_own t Csi_u_support)
  else if not kitty then Terminal.enable_kitty_keyboard terminal false;
  Terminal.enable_modify_other_keys terminal (not kitty);
  set_width_method t (width_method t)

let update_capabilities t =
  let caps = Terminal.Caps.derive t.evidence in
  if caps <> Terminal.capabilities t.terminal then (
    Terminal.set_capabilities t.terminal caps;
    if t.lifecycle = `Ready then (
      apply_modes t;
      refresh_capabilities t;
      force_full_redraw t;
      request_redraw t))

(* Input

   The decoder holds no clock: the runtime says when the terminal has been
   silent. Silence is measured from the last bytes read, and only a wait that
   read nothing ends it; how long a wait must be scales with the round trip
   of the startup probe, the time a terminal takes to write what it began. An
   ESC that may begin a sequence waits max(50 ms, 2 RTT), the 50 ms that
   libtermkey and neovim wait; a sequence begun, which a terminal writes
   whole, waits max(500 ms, 4 RTT).

   A batch of queries is owed until it is answered, by its DA1 or, for a
   cursor batch, its report, and while it is owed the decoder reads its
   replies as replies: a reply is never typed for arriving late. The runtime
   waits for an answer only max(500 ms, 4 RTT), then goes on without it, and
   the batch stays owed. A terminal that never answers would then keep
   [ESC P], [ESC \]] and [ESC _] from being Alt keys, and a modified F3 from
   being F3, for the whole session, so a batch is given up after max(5 s,
   4 RTT). No reference bounds it: it must exceed any real answer's delay,
   since a reply after it decodes as input, and it costs only those keys while
   it lasts. 5 s is ten times the wait for an answer, and 4 RTT keeps it above
   a slow link's round trip as the sequence wait does. The cursor reports a
   given-up batch was owed are counted: a report that comes later answers
   them first, so that it never answers a later batch, such as the one that
   anchors the live region. *)

let silence t =
  match Input.Decoder.pending t.decoder with
  | Nothing -> None
  | Ambiguous -> Some (Float.max 0.05 (2. *. t.rtt))
  | Sequence -> Some (Float.max 0.5 (4. *. t.rtt))

let answer_wait t = Float.max 0.5 (4. *. t.rtt)
let owed_bound t = Float.max 5. (4. *. t.rtt)

let push t event =
  (match event with
  | Resize (cols, rows) -> t.reported_size <- (cols, rows)
  | _ -> ());
  Queue.push event t.events

(* A batch leaves the decoder's counts once, answered or given up. *)
let close t b =
  if List.memq b t.batches then (
    Input.Decoder.settle t.decoder b.owed;
    t.batches <- List.filter (fun b' -> b' != b) t.batches)

let give_up t b =
  t.lost_reports <-
    t.lost_reports + b.owed.Input.Decoder.cursor_reports - b.reports;
  close t b

let ask t batch =
  ignore
    (send_batch t (Capabilities batch) (Terminal.Caps.ask t.evidence batch)
      : batch)

let ask_cursor t =
  send_batch t Cursor
    ( Ansi.(to_string (query Cursor_position)),
      { Input.Decoder.cursor_reports = 1; string_replies = 0 } )

(* A cursor report answers the oldest query still owed one, a given-up one
   first. *)
let cursor_report t ~row ~col =
  if t.lost_reports > 0 then t.lost_reports <- t.lost_reports - 1
  else
    match
      List.find_opt
        (fun b -> b.reports < b.owed.Input.Decoder.cursor_reports)
        t.batches
    with
    | None -> ()
    | Some b -> (
        let index = b.reports in
        b.reports <- index + 1;
        match b.question with
        | Capabilities q ->
            t.evidence <- Terminal.Caps.cursor t.evidence q ~index ~row ~col
        | Cursor ->
            b.cursor <- Some (row, col);
            b.answered <- true;
            close t b
        | Application -> push t (Reply (Cursor { row; col }))
        | Own -> ())

(* The oldest batch that ends with DA1, which the replies that are not cursor
   reports answer. *)
let replying t =
  List.find_opt
    (fun b ->
      match b.question with
      | Capabilities _ | Own | Application -> true
      | Cursor -> false)
    t.batches

(* A DA1 reply answers the oldest batch that asked for one. The terminal
   answers in order, so every query sent before it has been answered or never
   will be. *)
let attributes_report t attributes =
  match replying t with
  | None -> ()
  | Some b -> (
      let rec before = function
        | b' :: rest when b' != b -> b' :: before rest
        | _ -> []
      in
      List.iter (close t) (before t.batches);
      t.lost_reports <- 0;
      b.answered <- true;
      close t b;
      match b.question with
      | Capabilities q ->
          if q = Probe then t.rtt <- now t -. b.sent;
          t.evidence <- Terminal.Caps.answered t.evidence q ~attributes;
          Option.iter (ask t) (Terminal.Caps.next t.evidence q)
      | Application -> push t (Reply (Primary_attributes attributes))
      | Own | Cursor -> ())

(* A reply is the runtime's to act on, and the application's too while its
   batch is the one being answered. The reports a terminal sends on its own
   that are application facts are events. *)
let reply t (r : Input.Reply.t) =
  (match r with
  | Cursor { row; col } -> cursor_report t ~row ~col
  | Primary_attributes attributes -> attributes_report t attributes
  | Color_scheme scheme -> push t (Color_scheme scheme)
  | Resize { rows; cols; height; width } ->
      Terminal.set_pixel_resolution t.terminal (Some (width, height));
      (* The report often follows the window-size signal of the same resize,
         and comes once when the mode is set. *)
      if t.reported_size <> (cols, rows) then push t (Resize (cols, rows))
  | r -> (
      (match r with
      | Text_area_pixels { height; width } ->
          Terminal.set_pixel_resolution t.terminal (Some (width, height))
      | r -> t.evidence <- Terminal.Caps.add t.evidence r);
      match replying t with
      | Some { question = Application; _ } -> push t (Reply r)
      | _ -> ()));
  update_capabilities t

let decoded t : Input.Decoder.event -> unit = function
  | Reply r -> reply t r
  | Key k -> push t (Key k)
  | Key_release r -> push t (Key_release r)
  | Text s -> push t (Text s)
  | Mouse m -> push t (Mouse m)
  | Focus_in -> push t Focus_in
  | Focus_out -> push t Focus_out
  | Paste_start -> push t Paste_start
  | Paste_data s -> push t (Paste_data s)
  | Paste_end ending -> push t (Paste_end ending)
  | Invalid s -> push t (Invalid s)

let expire t =
  let now = now t and bound = owed_bound t in
  List.iter (fun b -> if now >= b.sent +. bound then give_up t b) t.batches

(* The time until the input needs attention, as a timeout. *)
let input_timeout t =
  let now = now t in
  let after deadline = Some (Float.max 0. (deadline -. now)) in
  let silence =
    Option.bind (silence t) (fun wait -> after (t.last_input +. wait))
  in
  match t.batches with
  | [] -> silence
  | b :: _ -> (
      let bound = after (b.sent +. owed_bound t) in
      match (silence, bound) with
      | Some a, Some b -> Some (Float.min a b)
      | None, x | x, None -> x)

(* Waits for input at most [timeout], passing the backend's own events to
   [native], and decodes what came. A wait that read nothing is silence,
   which decides what the decoder holds once it has lasted. *)
let read_input t ~timeout ~native =
  let ready = t.backend.wait ~timeout ~on_event:native in
  let n =
    if ready && (not t.input_ended) && t.lifecycle <> `Closed then
      t.backend.read_input t.input_buffer 0 (Bytes.length t.input_buffer)
    else -1
  in
  (if n = 0 then (
     t.input_ended <- true;
     Input.Decoder.finish t.decoder (decoded t))
   else if n > 0 then (
     t.last_input <- now t;
     Input.Decoder.feed t.decoder t.input_buffer ~off:0 ~len:n (decoded t))
   else
     match silence t with
     | Some wait when now t >= t.last_input +. wait ->
         Input.Decoder.idle t.decoder (decoded t)
     | _ -> ());
  expire t

(* Reads input until [b] is answered, the wait for its answer is over, or the
   input ends. [b] stays owed after the wait. *)
let await t b =
  let until () = b.sent +. answer_wait t in
  while (not b.answered) && (not t.input_ended) && now t < until () do
    let wait = until () -. now t in
    let timeout =
      match input_timeout t with
      | Some timeout -> Float.min timeout wait
      | None -> wait
    in
    read_input t ~timeout:(Some timeout) ~native:(push t)
  done

let abort_paste t = Input.Decoder.cut_paste t.decoder (decoded t)

let query t ~owed bytes =
  if t.lifecycle = `Ready && can_query t then
    ignore (send_batch t Application (bytes ^ da1, owed) : batch)

(* Adopts a primary layout transition: its operations go out with the next
   frame, and a frame after operations or a move of the live region redraws
   it whole. *)
let relayout t (primary, ops) =
  if ops <> [] || Primary.live_region primary <> Primary.live_region t.primary
  then force_full_redraw t;
  t.primary <- primary;
  t.layout_ops <- t.layout_ops @ ops

let refresh_render_region t =
  match t.config.mode with
  | `Alt ->
      Screen.set_row_offset t.screen 0;
      Screen.resize t.screen ~width:t.width ~height:t.height
  | `Primary ->
      relayout t (Primary.resize t.primary ~terminal_height:(max 1 t.height));
      sync_primary_layout t ~resize:true

(* Frame timing *)

let compute_loop_interval t =
  match t.config.target_fps with
  | Some fps when fps > 0. -> Some (1. /. fps)
  | _ -> None

let update_cadence t =
  if cadence_active t then (
    if t.next_frame_deadline = None then
      t.next_frame_deadline <- Some (t.backend.now ()))
  else t.next_frame_deadline <- None

(* Static output (primary mode only) *)

let erase_entire_line = Ansi.(to_string (erase_line ~mode:`All))
let sgr_reset = Ansi.(to_string reset)
let sync_output_on = Ansi.(to_string (enable Sync_output))
let sync_output_off = Ansi.(to_string (disable Sync_output))
let cursor_hide_seq = Ansi.(to_string (disable Cursor_visible))
let cursor_show_seq = Ansi.(to_string (enable Cursor_visible))
let erase_below_seq = Ansi.(to_string erase_below_cursor)
let clear_and_home_seq = Ansi.(to_string clear_and_home)
let clear_scrollback_seq = Ansi.(to_string (erase_display ~mode:`Scrollback))
let reset_scroll_region_seq = Ansi.(to_string reset_scrolling_region)

(* Helpers for buffered frame output. All frame content is accumulated into
   an ANSI writer and flushed as a single write in submit. *)

let buf_cursor_position buf ~row ~col =
  Ansi.cursor_position ~row:(max 1 row) ~col:(max 1 col) buf

let apply_primary_op buf = function
  | Primary.Move_cursor { row; col } -> buf_cursor_position buf ~row ~col
  | Primary.Reset_sgr -> Ansi.Writer.write_string buf sgr_reset
  | Primary.Write s -> Ansi.Writer.write_string buf s
  | Primary.Clear_and_home -> Ansi.Writer.write_string buf clear_and_home_seq
  | Primary.Clear_scrollback ->
      Ansi.Writer.write_string buf clear_scrollback_seq

let effective_size t =
  match t.config.mode with
  | `Alt -> (t.width, t.height)
  | `Primary -> (t.width, (Primary.effective_region t.primary).height)

(* Static rows never wrap: a row wider than a terminal narrowed since the
   snapshot overwrites its last column. *)
let static_snapshot t ?rows grid =
  if Grid.width grid > t.width then
    invalid_arg "Matrix.static_write: grid is wider than the terminal";
  let rows = Option.value rows ~default:(Grid.height grid) in
  let text = Grid.to_ansi ~reset:false ~complete_rows:true ~rows grid in
  (rows, "\027[?7l" ^ text ^ "\027[?7h")

let static_write ?(preserve_live_region = false) ?rows t grid =
  if t.config.mode <> `Alt then
    let rows, text = static_snapshot t ?rows grid in
    if rows > 0 then (
      t.primary <-
        Primary.enqueue_static t.primary { text; rows; preserve_live_region };
      request_redraw t)

let static_replace ?(preserve_live_region = false) ?rows t grid =
  if t.config.mode <> `Alt then (
    let rows, text = static_snapshot t ?rows grid in
    t.primary <-
      Primary.replace_static t.primary { text; rows; preserve_live_region };
    request_redraw t)

(* Immediate action, not part of the frame pipeline: the plan's terminal ops
   are encoded and sent directly, not queued into the frame buffer, so the
   primary screen clears before the next frame renders. The plan's flags are
   still routed like any other plan; [region_changed] is covered by
   [refresh_render_region], which also folds in the terminal resize. *)
let static_clear t =
  if t.config.mode = `Alt then ()
  else
    let primary, ops = Primary.clear_static t.primary in
    t.primary <- primary;
    (* The erase supersedes the layout operations not yet sent. *)
    t.layout_ops <- [];
    let buf = Ansi.Writer.create 16 in
    List.iter (apply_primary_op buf) ops;
    Terminal.send t.terminal (Ansi.Writer.contents buf);
    t.last_cursor_position <- Some (1, 1);
    t.cursor_live_row <- Some 0;
    force_full_redraw t;
    let cols, rows = t.backend.terminal_size () in
    t.width <- max 1 cols;
    t.height <- max 1 rows;
    refresh_render_region t;
    request_redraw t

let set_scroll_hint t hint = t.scroll_hint <- Some hint

(* Protocol configuration *)

let apply_config t =
  refresh_capabilities t;
  let terminal = t.terminal in
  t.backend.set_raw_mode t.config.raw_mode;
  if t.config.mode = `Alt then (
    Terminal.enter_alternate_screen terminal;
    let cursor = Screen.cursor t.screen in
    Screen.set_cursor t.screen { cursor with visible = false })
  else Terminal.leave_alternate_screen terminal;
  t.force_full_next_frame <- true;
  apply_modes t;
  refresh_render_region t

(* Lifecycle: Prepare / Grid / Submit *)

(* No eager buffer clear here: Screen defers the post-present clear of the
   next grid and hit grid to their first observation ([Screen.next_grid],
   [Screen.build], ...), so both direct-draw apps reading {!grid} and
   renderer hosts building through [Screen.build] pay for exactly one clear
   per frame instead of clearing here and again downstream. *)
let prepare ?primary_required_rows t =
  refresh_capabilities t;
  ignore (Atomic.compare_and_set t.redraw_state `Dirty `Clean);
  if t.config.mode = `Primary then begin
    relayout t (Primary.flush_static t.primary);
    Option.iter
      (fun rows -> relayout t (Primary.require_rows t.primary ~rows))
      primary_required_rows
  end;
  refresh_render_region t

let grid t = Screen.next_grid t.screen
let current_grid t = Screen.current_grid t.screen
let hits t = Screen.next_hit_grid t.screen
let screen t = t.screen

(* Apply cursor position, style, and color into the frame buffer.
   Style and color are only emitted when they differ from the last frame. *)
let cursor_shape (cursor : Screen.cursor) : Ansi.cursor_shape =
  Ansi.cursor_shape_of_style ~style:cursor.style ~blinking:cursor.blinking

(* In [`Primary] mode the cursor always ends a frame at a known place, so
   that after a resize, where the terminal took it shows where the live
   region went. Hidden, it waits at the top of the live region: a terminal
   that shrinks drops the rows below its cursor first, which there are only
   live rows. *)
let cursor_output_position t ~(cursor : Screen.cursor) ~cursor_max_row =
  match (cursor.position, t.config.mode) with
  | Some (x, y), `Primary ->
      Some (Primary.terminal_cursor_row t.primary ~live_row:y, max 1 (x + 1))
  | Some (x, y), `Alt -> Some (clamp 0 (cursor_max_row - 1) y + 1, max 1 (x + 1))
  | None, `Primary ->
      let live_row = if cursor.visible then cursor_max_row - 1 else 0 in
      Some (Primary.terminal_cursor_row t.primary ~live_row, 1)
  | None, `Alt -> if cursor.visible then Some (cursor_max_row, 1) else None

(* Allocation-free "changed since last frame" checks: comparing through the
   stored options directly avoids wrapping the current value in [Some]. *)
let[@inline] visible_dirty t visible =
  match t.last_cursor_visible with Some v -> v <> visible | None -> true

let[@inline] shape_dirty t shape =
  match t.last_cursor_shape with Some s -> s <> shape | None -> true

let[@inline] color_dirty t color =
  match t.last_cursor_color with Some c -> c <> color | None -> true

let cursor_dirty t ~(cursor : Screen.cursor) ~position =
  position <> t.last_cursor_position
  || visible_dirty t cursor.visible
  || cursor.visible
     && (shape_dirty t (cursor_shape cursor) || color_dirty t cursor.color)

let apply_cursor_state t ~buf ~(cursor : Screen.cursor) ~position =
  (match position with
  | Some (row, col) ->
      buf_cursor_position buf ~row ~col;
      if cursor.visible then Ansi.Writer.write_string buf cursor_show_seq
  | None -> ());
  if cursor.visible then begin
    let shape = cursor_shape cursor in
    if shape_dirty t shape then begin
      Terminal.write_cursor_style t.terminal ~shape buf;
      t.last_cursor_shape <- Some shape
    end;
    if color_dirty t cursor.color then begin
      Terminal.write_cursor_color t.terminal cursor.color buf;
      t.last_cursor_color <- Some cursor.color
    end
  end
  else begin
    t.last_cursor_shape <- None;
    t.last_cursor_color <- None
  end;
  t.last_cursor_position <- position;
  t.last_cursor_visible <- Some cursor.visible

(* All frame output is accumulated into a single buffer and flushed as one
   write. Skips the write entirely when nothing changed. *)
let submit t =
  if not (render_eligible t) then ()
  else begin
    if t.debug_overlay_enabled then t.debug_overlay_cb t.screen;
    let cursor = Screen.cursor t.screen in
    let caps = Terminal.capabilities t.terminal in
    let use_sync = Terminal.tty t.terminal && caps.sync in
    let buf = t.submit_buf in
    Ansi.Writer.reset_pos buf;

    (* Preamble: BSU + cursor hide. *)
    if use_sync then Ansi.Writer.write_string buf sync_output_on;
    Ansi.Writer.write_string buf cursor_hide_seq;
    let preamble_len = Ansi.Writer.len buf in

    (* No cursor homing here: the diff renderer emits an absolute
       cursor_position for every run it touches, so anything written between
       the preamble capture and the skip check would defeat the
       unchanged-frame write skip. *)
    List.iter (apply_primary_op buf) t.layout_ops;
    t.layout_ops <- [];
    let forced_full =
      let ff = t.force_full_next_frame in
      if ff then t.force_full_next_frame <- false;
      ff
    in
    let scroll_hint =
      let h = t.scroll_hint in
      t.scroll_hint <- None;
      match t.config.mode with `Alt -> h | `Primary -> None
    in
    (* In [`Primary] mode the frame paints the live region. A full redraw
       paints it down to its last row that shows anything, and erases the
       rest. *)
    let viewport, erase_from =
      match t.config.mode with
      | `Alt -> (None, None)
      | `Primary ->
          let y = Primary.render_offset t.primary in
          let live = (Primary.live_region t.primary).height in
          if forced_full then
            let height = min live (max 1 (Screen.active_height t.screen)) in
            ( Some { Screen.y; height },
              if height < live then Some (y + height + 1) else None )
          else (Some { Screen.y; height = live }, None)
    in
    Screen.render_into ~full:forced_full ?scroll_hint ?viewport t.screen buf;
    Option.iter
      (fun row ->
        buf_cursor_position buf ~row ~col:1;
        Ansi.Writer.write_string buf erase_below_seq)
      erase_from;

    (* Skip the write when nothing beyond the preamble was produced and cursor
       state is unchanged. The cursor lives in the live region, whichever of
       its rows this frame painted. *)
    let cursor_max_row =
      match t.config.mode with
      | `Primary -> (Primary.live_region t.primary).height
      | `Alt -> t.height
    in
    let position = cursor_output_position t ~cursor ~cursor_max_row in
    if Ansi.Writer.len buf > preamble_len || cursor_dirty t ~cursor ~position
    then begin
      if t.config.mode = `Alt then Ansi.cursor_position ~row:t.height ~col:1 buf;
      apply_cursor_state t ~buf ~cursor ~position;
      (match (t.config.mode, position) with
      | `Primary, Some (row, _) ->
          t.cursor_live_row <- Some (row - 1 - Primary.render_offset t.primary)
      | _ -> ());
      if use_sync then Ansi.Writer.write_string buf sync_output_off;

      t.frame_modes_armed <- true;
      t.backend.write_output (Ansi.Writer.buffer buf) 0 (Ansi.Writer.len buf)
    end;

    if t.frame_dump_every > 0 then (
      t.frame_dump_counter <- t.frame_dump_counter + 1;
      if t.frame_dump_counter mod t.frame_dump_every = 0 then
        Frame_dump.snapshot ?dir:t.frame_dump_dir ?pattern:t.frame_dump_pattern
          ~hits:t.frame_dump_hits t.screen)
  end

(* Control state *)

let stop t =
  if t.lifecycle <> `Closed then (
    t.running <- false;
    update_cadence t;
    if t.lifecycle = `Ready then Atomic.set t.redraw_state `Clean;
    t.backend.wake ())

let request_live t =
  if t.lifecycle <> `Closed then (
    let active = cadence_active t in
    t.live_requests <- t.live_requests + 1;
    update_cadence t;
    if (not active) && cadence_active t then request_redraw t)

let drop_live t =
  if t.lifecycle <> `Closed then (
    t.live_requests <- max 0 (t.live_requests - 1);
    update_cadence t)

let start t =
  if t.lifecycle <> `Closed then (
    t.demand <- `Continuous;
    t.running <- true;
    update_cadence t;
    if t.lifecycle = `Ready then request_redraw t)

let pause t =
  if t.lifecycle <> `Closed then (
    t.demand <- `Paused;
    update_cadence t)

(* Frame writes bypass the terminal handle and may stop between mode changes.
   Attempt every reset independently, before handing the terminal back. *)
let restore_frame_modes t =
  if t.frame_modes_armed then (
    (try Terminal.send t.terminal sync_output_off with _ -> ());
    (try Terminal.send t.terminal reset_scroll_region_seq with _ -> ());
    try Terminal.send t.terminal "\027[?7h" with _ -> ())

(* Re-anchors the live region after a resize from where the cursor went: the
   last frame left it on a known live row. Input read while waiting for the
   report is replayed. Without a report the layout stays as {!Primary.resize}
   planned it. *)
let reanchor t =
  t.reanchor <- false;
  match t.cursor_live_row with
  | None -> ()
  | Some live_row -> (
      let b = ask_cursor t in
      await t b;
      match b.cursor with
      | Some (row, _) ->
          (* The planned operations assumed the rows had not moved. *)
          t.layout_ops <- [];
          relayout t (Primary.anchor t.primary ~row:(row + 1 - live_row) ~col:1);
          sync_primary_layout t ~resize:true
      | None -> ())

(* A suspended application leaves the screen as a program run in its place
   should find it. In [`Primary] mode that is the transcript, everything a
   frame planned for it sent, and below it the live region, erased, with the
   cursor at its start: the program's output goes on from the transcript. In
   [`Alt] mode it is an erased screen with the cursor at its top left, or,
   when [leave_alt], the primary screen as the shell left it. The cursor is
   shown. *)
let release_screen t ~leave_alt =
  let w = Ansi.Writer.create 64 in
  List.iter (apply_primary_op w) t.layout_ops;
  t.layout_ops <- [];
  (match t.config.mode with
  | `Primary ->
      buf_cursor_position w ~row:(Primary.render_offset t.primary + 1) ~col:1;
      Ansi.Writer.write_string w sgr_reset;
      Ansi.Writer.write_string w erase_below_seq
  | `Alt ->
      if not leave_alt then begin
        Ansi.Writer.write_string w sgr_reset;
        Ansi.Writer.write_string w clear_and_home_seq
      end);
  Ansi.Writer.write_string w cursor_show_seq;
  Terminal.send t.terminal (Ansi.Writer.contents w)

let suspend ?(leave_alt = false) t =
  if t.lifecycle <> `Ready then ()
  else (
    (* After a resize the live region is where the terminal took it. *)
    (if t.reanchor then try reanchor t with _ -> ());
    Atomic.set t.redraw_state `Disabled;
    t.lifecycle <- `Suspended;
    update_cadence t;
    (* A paste the application has begun to receive ends. *)
    Input.Decoder.cut_paste t.decoder (decoded t);
    Input.Decoder.reset t.decoder;
    force_full_redraw t;
    (* The suspended program owns the terminal; whatever cursor state it leaves
       behind is unknown, so the first frame after resume must re-emit. *)
    t.last_cursor_position <- None;
    t.cursor_live_row <- None;
    t.reanchor <- false;
    t.last_cursor_visible <- None;
    t.last_cursor_shape <- None;
    t.last_cursor_color <- None;
    t.backend.protect (fun () -> restore_frame_modes t);
    (try release_screen t ~leave_alt with _ -> ());
    (try Terminal.set_mouse_mode t.terminal `Off with _ -> ());
    (try Terminal.enable_bracketed_paste t.terminal false with _ -> ());
    (try Terminal.enable_focus_reporting t.terminal false with _ -> ());
    (try Terminal.enable_kitty_keyboard t.terminal false with _ -> ());
    (try Terminal.enable_modify_other_keys t.terminal false with _ -> ());
    (try Terminal.enable_color_scheme_reports t.terminal false with _ -> ());
    (try Terminal.enable_resize_reports t.terminal false with _ -> ());
    (try Terminal.enable_meta_sends_escape t.terminal false with _ -> ());
    (try t.backend.set_raw_mode false with _ -> ());
    t.suspend_left_alt <- leave_alt;
    (if leave_alt then
       try Terminal.leave_alternate_screen t.terminal with _ -> ());
    try t.backend.flush_input () with _ -> ())

let resume t =
  if t.lifecycle <> `Suspended then ()
  else (
    (if t.config.raw_mode then try t.backend.set_raw_mode true with _ -> ());
    (try t.backend.flush_input () with _ -> ());
    Input.Decoder.reset t.decoder;
    if t.suspend_left_alt then (
      t.suspend_left_alt <- false;
      try Terminal.enter_alternate_screen t.terminal with _ -> ());
    if t.config.mode = `Primary && can_query t then (
      (* Input read while waiting is stale, but for resizes. *)
      let kept = Queue.length t.events in
      let b = ask_cursor t in
      await t b;
      let stale = Queue.create () in
      Queue.transfer t.events stale;
      ignore
        (Queue.fold
           (fun i event ->
             (match event with
             | Resize _ -> Queue.push event t.events
             | _ -> if i < kept then Queue.push event t.events);
             i + 1)
           0 stale
          : int);
      match b.cursor with
      | Some (row, col) ->
          (* What the suspended program left on the screen voids the
              operations planned before it ran. *)
          t.layout_ops <- [];
          relayout t (Primary.anchor t.primary ~row:(row + 1) ~col:(col + 1))
      | None -> ());
    force_full_redraw t;
    apply_config t;
    Grid.clear (Screen.next_grid t.screen);
    Screen.Hit_grid.clear (Screen.next_hit_grid t.screen);
    t.lifecycle <- `Ready;
    update_cadence t;
    Atomic.set t.redraw_state (if t.running then `Dirty else `Clean);
    t.backend.wake ())

(* Cursor *)

(* Cursor setters update Screen state only. The actual terminal output
   is deferred to submit's apply_cursor_state, which writes everything
   into the frame buffer as a single write. *)

let set_cursor ?visible ?style t =
  let cursor = Screen.cursor t.screen in
  let visible = Option.value visible ~default:cursor.visible in
  let style = Option.value style ~default:cursor.style in
  Screen.set_cursor t.screen { cursor with visible; style }

let set_cursor_style t ~style ~blinking =
  let cursor = Screen.cursor t.screen in
  Screen.set_cursor t.screen { cursor with style; blinking }

let set_cursor_position t ~row ~col =
  let max_row =
    if t.config.mode = `Primary then (Primary.live_region t.primary).height
    else max 1 t.height
  in
  let row = clamp 1 max_row row in
  let col = max 1 col in
  let cursor = Screen.cursor t.screen in
  Screen.set_cursor t.screen { cursor with position = Some (col - 1, row - 1) }

let set_cursor_color t ~r ~g ~b =
  let clamp_01 f = Float.max 0. (Float.min 1. f) in
  let to_byte f = int_of_float (Float.round (f *. 255.)) |> clamp 0 255 in
  let cursor = Screen.cursor t.screen in
  Screen.set_cursor t.screen
    {
      cursor with
      color =
        Some (to_byte (clamp_01 r), to_byte (clamp_01 g), to_byte (clamp_01 b));
    }

(* Resize *)

let apply_resize t cols rows now =
  (* Reaching a resize decision consumes any pending debounced resize in every
     branch, including the no-ops for an unchanged or degenerate size. A
     pending entry left behind keeps its debounce window elapsed, which pins
     [compute_timeout] at zero and spins the loop — starving input — until a
     differently-sized resize clears it. *)
  t.pending_resize <- None;
  if cols <= 0 || rows <= 0 then ()
  else if t.width = cols && t.height = rows then ()
  else (
    t.width <- cols;
    t.height <- rows;
    ask_own t Pixel_size;
    force_full_redraw t;
    refresh_render_region t;
    (* A terminal moves its rows as it resizes, by rules of its own; the
       cursor moves with its row. The loop asks where it went before the next
       frame. *)
    if t.config.mode = `Primary && can_query t then t.reanchor <- true;
    t.last_resize_apply_time <- now;
    request_redraw t)

let handle_resize t cols rows =
  if t.lifecycle <> `Ready then t.pending_resize <- Some (cols, rows)
  else
    let now = t.backend.now () in
    match t.config.resize_debounce with
    | None -> apply_resize t cols rows now
    | Some window_s ->
        if
          t.last_resize_apply_time = 0.
          || now -. t.last_resize_apply_time >= window_s
        then apply_resize t cols rows now
        else t.pending_resize <- Some (cols, rows)

let maybe_apply_pending_resize t =
  if t.lifecycle = `Ready then
    match t.pending_resize with
    | Some (cols, rows) ->
        let now = t.backend.now () in
        let window_s = Option.value t.config.resize_debounce ~default:0. in
        if now -. t.last_resize_apply_time >= window_s then
          apply_resize t cols rows now
    | None -> ()

(* Event dispatch *)

let adjust_event_for_offset t =
  if t.config.mode = `Alt then Fun.id
  else
    let map_y = Primary.map_mouse_y t.primary in
    fun event ->
      match event with
      | Mouse mouse -> Mouse { mouse with y = map_y mouse.y }
      | event -> event

(* Ctrl+C, pressed: a release never closes, and Shift is consumed into the
   key under the kitty protocol's alternate keys. *)
let is_ctrl_c (k : Input.Keystroke.t) =
  k.mods.ctrl
  && (k.key = Char (Uchar.of_char 'c') || k.key = Char (Uchar.of_char 'C'))

let classify_event t evt =
  match evt with
  | Resize (cols, rows) ->
      handle_resize t cols rows;
      Some (`Resize (cols, rows))
  | Focus_in ->
      if t.restore_modes_on_next_focus && t.lifecycle = `Ready then (
        t.restore_modes_on_next_focus <- false;
        Terminal.restore_modes t.terminal);
      Some (`Input evt)
  | Focus_out ->
      t.restore_modes_on_next_focus <- true;
      Some (`Input evt)
  | Key k when t.config.exit_on_ctrl_c && is_ctrl_c k -> None
  | evt -> Some (`Input (adjust_event_for_offset t evt))

(* Close *)

let close t =
  if t.lifecycle = `Closed then ()
  else
    t.backend.protect (fun () ->
        Atomic.set t.redraw_state `Disabled;
        t.lifecycle <- `Closed;
        t.running <- false;
        update_cadence t;
        restore_frame_modes t;
        (try
           let is_tty = Terminal.tty t.terminal in
           if t.config.mode = `Primary && is_tty then (
             let height = max 1 t.height in
             let render_offset =
               clamp 0 (height - 1) (Primary.render_offset t.primary)
             in
             let start_row = render_offset + 1 in
             for row = start_row to height do
               Terminal.move_cursor t.terminal ~row ~col:1
                 ~visible:(Terminal.cursor_visible t.terminal);
               Terminal.send t.terminal erase_entire_line
             done;
             Terminal.move_cursor t.terminal ~row:start_row ~col:1 ~visible:true);
           (* Flush pending mouse/input bytes both before and after mode teardown.
          This avoids leaking trailing SGR mouse payloads back to the shell. *)
           (try t.backend.flush_input () with _ -> ());
           Terminal.close t.terminal
         with _ -> ());
        (try t.backend.flush_input () with _ -> ());
        (try t.backend.set_raw_mode false with _ -> ());
        (try t.backend.flush_input () with _ -> ());
        try t.backend.cleanup () with _ -> ())

(* Construct the runtime over backend I/O. *)

let attach ?(mode = `Alt) ?(raw_mode = true) ?(target_fps = Some 30.)
    ?(mouse_enabled = true) ?(mouse = None) ?(bracketed_paste = true)
    ?(kitty_keyboard = `Auto) ?(key_releases = false) ?(exit_on_ctrl_c = true)
    ?(debug_overlay = false) ?(debug_overlay_corner = `Bottom_right)
    ?(debug_overlay_capacity = 120) ?(frame_dump_every = 0) ?frame_dump_dir
    ?frame_dump_pattern ?(frame_dump_hits = false)
    ?(cursor_visible = mode = `Alt) ?(explicit_width = false)
    ?(input_timeout = None) ?(resize_debounce = Some 0.1) ?(min_tui_height = 1)
    ?(start_idle = false) ?(pace_redraws = true) ?caps ~(backend : Backend.t) ()
    =
  let config =
    let effective_mouse_mode =
      if mouse_enabled then Some (Option.value ~default:`Sgr_any mouse)
      else None
    in
    {
      mode;
      raw_mode;
      mouse_mode = effective_mouse_mode;
      bracketed_paste;
      kitty_keyboard;
      key_releases;
      exit_on_ctrl_c;
      target_fps;
      explicit_width;
      input_timeout;
      resize_debounce;
      debug_overlay_capacity;
      min_tui_height;
      pace_redraws;
    }
  in
  let evidence =
    match caps with
    | Some caps -> Terminal.Caps.assume caps
    | None -> Terminal.Caps.evidence backend.env
  in
  let terminal =
    with_rollback
      ~rollback:(fun () -> backend.protect backend.cleanup)
      (fun () ->
        Terminal.make ~tty:backend.output_is_tty
          ~output:(fun s ->
            backend.write_output (Bytes.unsafe_of_string s) 0 (String.length s))
          (Terminal.Caps.derive evidence))
  in
  let app_ref = ref None in
  let rollback () =
    match !app_ref with
    | Some app -> close app
    | None ->
        backend.protect (fun () ->
            (try Terminal.close terminal with _ -> ());
            (try backend.set_raw_mode false with _ -> ());
            backend.cleanup ())
  in
  with_rollback ~rollback (fun () ->
      let cols, rows = backend.terminal_size () in
      let width = max 1 cols and height = max 1 rows in
      let screen =
        Screen.create ~cursor_visible ~explicit_width:config.explicit_width
          ~clock:backend.now ()
      in
      let app =
        {
          terminal;
          decoder = Input.Decoder.create ();
          input_buffer = Bytes.create 4096;
          input_ended = false;
          last_input = backend.now ();
          rtt = 0.;
          evidence;
          batches = [];
          lost_reports = 0;
          events = Queue.create ();
          reported_size = (width, height);
          config;
          backend;
          screen;
          submit_buf = Ansi.Writer.create 4096;
          running = true;
          redraw_state = Atomic.make `Clean;
          width;
          height;
          primary =
            Primary.create ~terminal_height:height
              ~min_live_height:config.min_tui_height;
          layout_ops = [];
          cursor_live_row = None;
          reanchor = false;
          scroll_hint = None;
          last_cursor_position = None;
          last_cursor_visible = None;
          last_cursor_shape = None;
          last_cursor_color = None;
          last_resize_apply_time = 0.;
          pending_resize = None;
          force_full_next_frame = true;
          next_frame_deadline = None;
          wakeup_deadline = None;
          last_render_time = Float.neg_infinity;
          debug_overlay_enabled = debug_overlay;
          debug_overlay_cb =
            Debug_overlay.on_frame ~corner:debug_overlay_corner
              ~capacity:config.debug_overlay_capacity ();
          frame_dump_every = max 0 frame_dump_every;
          frame_dump_dir;
          frame_dump_pattern;
          frame_dump_hits;
          frame_dump_counter = 0;
          (* Startup applies no mode until the configuration is applied. *)
          lifecycle = `Suspended;
          frame_modes_armed = false;
          demand =
            (if start_idle then `Auto
             else
               match config.target_fps with
               | Some fps when fps > 0. -> `Continuous
               | _ -> `Auto);
          live_requests = 0;
          restore_modes_on_next_focus = false;
          suspend_left_alt = false;
        }
      in
      app_ref := Some app;
      (* A terminal answers queries only on a raw input: the probe waits for
         its answers, input that comes meanwhile is kept for the run. *)
      if backend.input_is_tty && raw_mode then (
        backend.set_raw_mode true;
        let probe =
          send_batch app (Capabilities Probe)
            (Terminal.Caps.ask app.evidence Probe)
        in
        await app probe;
        let cols, rows = backend.terminal_size () in
        app.width <- max 1 cols;
        app.height <- max 1 rows;
        app.reported_size <- (app.width, app.height);
        app.primary <-
          Primary.create ~terminal_height:app.height
            ~min_live_height:config.min_tui_height;
        if mode = `Primary && not app.input_ended then (
          let b = ask_cursor app in
          await app b;
          Option.iter
            (fun (row, col) ->
              let primary, ops =
                Primary.anchor app.primary ~row:(row + 1) ~col:(col + 1)
              in
              app.primary <- primary;
              app.layout_ops <- ops)
            b.cursor));
      app.lifecycle <- `Ready;
      apply_config app;
      ask_own app Pixel_size;
      app)

(* Constructor: create a live app with Unix I/O *)

let create ?(mode = `Alt) ?(raw_mode = true) ?(target_fps = Some 30.)
    ?(mouse_enabled = true) ?(mouse = None) ?(bracketed_paste = true)
    ?(kitty_keyboard = `Auto) ?(key_releases = false) ?(exit_on_ctrl_c = true)
    ?(debug_overlay = false) ?(debug_overlay_corner = `Bottom_right)
    ?(debug_overlay_capacity = 120) ?(frame_dump_every = 0) ?frame_dump_dir
    ?frame_dump_pattern ?(frame_dump_hits = false)
    ?(cursor_visible = mode = `Alt) ?(explicit_width = false)
    ?(input_timeout = None) ?(resize_debounce = Some 0.1) ?(output = `Stdout)
    ?(signal_handlers = true) ?caps ?(min_tui_height = 1) ?(start_idle = false)
    ?(pace_redraws = true) () =
  let output_fd = match output with `Stdout -> Unix.stdout | `Fd fd -> fd in
  let input_fd = Unix.stdin in
  let output_is_tty = Terminal.is_tty output_fd in
  let input_is_tty = Terminal.is_tty input_fd in
  let wakeup_r, wakeup_w = Unix.pipe ~cloexec:true () in
  let release_lifecycle = ref ignore in
  let backend_cleaned = ref false in
  let wakeup_lock = Mutex.create () in
  (* A request published before close may reach wake afterwards. Serialize
     wake with pipe disposal so a reused descriptor can never receive it. *)
  let wake_locked () = if not !backend_cleaned then wake_fd wakeup_w in
  let wake () = Mutex.protect wakeup_lock wake_locked in
  let cleanup_backend () =
    (* Signal handlers cannot take [wakeup_lock]. Revoke their access before
       disposing of the pipe; worker wakeups use the lock below. *)
    !release_lifecycle ();
    let cleaned =
      Mutex.protect wakeup_lock (fun () ->
          if !backend_cleaned then false
          else (
            backend_cleaned := true;
            (try Unix.close wakeup_r with _ -> ());
            (try Unix.close wakeup_w with _ -> ());
            true))
    in
    if cleaned then winch_received := false
  in
  let original_termios = ref None in
  let set_raw_mode enabled =
    if enabled then (
      match !original_termios with
      | Some _ -> ()
      | None ->
          if input_is_tty then
            original_termios := Some (Terminal.set_raw input_fd))
    else
      match !original_termios with
      | Some saved ->
          Terminal.restore input_fd saved;
          original_termios := None
      | None -> ()
  in
  let app_ref = ref None in
  (* Before [attach] returns, the constructor owns raw mode and the wakeup
     pipe directly. Afterwards the app's idempotent close path owns both. *)
  let rollback () =
    match !app_ref with
    | Some app -> close app
    | None ->
        (try set_raw_mode false with _ -> ());
        cleanup_backend ()
  in
  with_rollback ~rollback (fun () ->
      Unix.set_nonblock wakeup_r;
      Unix.set_nonblock wakeup_w;
      let backend : Backend.t =
        {
          now = (fun () -> Int64.to_float (Mtime_clock.elapsed_ns ()) *. 1e-9);
          wake;
          write_output = write_all output_fd;
          read_input = read_input_unix ~input_fd;
          wait = wait_input_unix ~input_fd ~wakeup_r ~output_fd;
          input_is_tty;
          output_is_tty;
          terminal_size = (fun () -> Terminal.size output_fd);
          set_raw_mode;
          flush_input =
            (fun () -> if input_is_tty then Terminal.flush_input input_fd);
          protect = (fun f -> f ());
          cleanup = cleanup_backend;
          env = Terminal.Caps.env Sys.getenv_opt ~win32:Sys.win32;
        }
      in
      let app =
        attach ~mode ~raw_mode ~target_fps ~mouse_enabled ~mouse
          ~bracketed_paste ~kitty_keyboard ~key_releases ~exit_on_ctrl_c
          ~debug_overlay ~debug_overlay_corner ~debug_overlay_capacity
          ~frame_dump_every ?frame_dump_dir ?frame_dump_pattern ~frame_dump_hits
          ~cursor_visible ~explicit_width ~input_timeout ~resize_debounce ?caps
          ~min_tui_height ~start_idle ~pace_redraws ~backend ()
      in
      app_ref := Some app;
      let shutdown_fn () = close app in
      let winch_handler _ =
        winch_received := true;
        wake_fd wakeup_w
      in
      winch_received := false;
      release_lifecycle :=
        own_process_lifecycle ~signal_handlers
          ~additional_signals:
            [ (Sys.sigwinch, Sys.Signal_handle winch_handler) ]
          shutdown_fn;
      app)

(* Event loop helpers *)

let min_timeout a b =
  match (a, b) with
  | None, x | x, None -> x
  | Some x, Some y -> Some (Float.min x y)

(* A one-shot redraw renders as soon as the frame pacing allows. Outside live
   mode there is no frame deadline, so pacing hangs off the last render time:
   an event storm (streaming deltas, rapid dispatches) coalesces to the target
   fps instead of rendering back-to-back. With [pace_redraws] off — a virtual
   clock where coalescing buys nothing and "waiting for the frame" would move
   time that timers read — a one-shot renders immediately at the current
   instant. Live animation still paces off [target_fps] via the frame
   deadline; only one-shots skip the wait. *)
let one_shot_render_due t ~now =
  redraw_requested t
  && (not (cadence_active t))
  && ((not t.config.pace_redraws)
     ||
     match compute_loop_interval t with
     | None -> true
     | Some interval -> now -. t.last_render_time >= interval)

let compute_timeout t ~now =
  let pending_timeout =
    match (t.pending_resize, t.config.resize_debounce) with
    | Some _, Some window_s when t.lifecycle = `Ready ->
        let remaining = t.last_resize_apply_time +. window_s -. now in
        if remaining > 0. then Some remaining else Some 0.
    | _ -> None
  in
  let deadline_timeout =
    match t.next_frame_deadline with
    | Some dl ->
        let dt = dl -. now in
        if dt <= 0. then Some 0. else Some dt
    | None -> None
  in
  let wakeup_timeout =
    match t.wakeup_deadline with
    | Some dl ->
        let dt = dl -. now in
        if dt <= 0. then Some 0. else Some dt
    | None -> None
  in
  let input_timeout = input_timeout t in
  let redraw_timeout =
    if redraw_requested t && not (cadence_active t) then
      if not t.config.pace_redraws then Some 0.
      else
        match compute_loop_interval t with
        | None -> Some 0.
        | Some interval ->
            Some (Float.max 0. (t.last_render_time +. interval -. now))
    else None
  in
  min_timeout redraw_timeout
    (min_timeout wakeup_timeout
       (min_timeout
          (min_timeout deadline_timeout t.config.input_timeout)
          (min_timeout pending_timeout input_timeout)))

let should_render_now t ~now =
  render_eligible t
  && (one_shot_render_due t ~now
     || cadence_active t
        &&
        match t.next_frame_deadline with
        | Some dl -> now >= dl
        | None -> false)

(* Event loop *)

let run ?on_frame ?on_input ?on_resize ?primary_required_rows ~on_render t =
  update_cadence t;
  if render_eligible t then request_redraw t;
  (* [on_resize] always reports the usable surface — {!size}, the live
     viewport in [`Primary] mode — never the raw terminal dimensions of the
     resize event, so the first callback and later ones agree. It fires once
     before the first render and then whenever the applied size changes,
     including debounced resizes applied after their event. *)
  let last_reported_size = ref None in
  let notify_resize ~initial =
    Option.iter
      (fun f ->
        let ((cols, rows) as reported) = size t in
        if initial || !last_reported_size <> Some reported then (
          last_reported_size := Some reported;
          f t ~cols ~rows))
      on_resize
  in
  if t.running then notify_resize ~initial:true;

  let render_cycle ~now ~last_time =
    Option.iter (fun f -> f t ~dt:(now -. last_time)) on_frame;
    if render_eligible t then (
      let primary_required_rows =
        Option.bind primary_required_rows (fun f -> f t)
      in
      prepare ?primary_required_rows t;
      on_render t);
    let user_end = t.backend.now () in
    if render_eligible t then submit t;
    user_end
  in

  let handle_event evt =
    if not t.running then ()
    else
      match classify_event t evt with
      | None -> close t
      | Some (`Resize (cols, rows)) ->
          notify_resize ~initial:false;
          Option.iter (fun f -> f t (Resize (cols, rows))) on_input;
          request_redraw t
      | Some (`Input event) ->
          Option.iter (fun f -> f t event) on_input;
          request_redraw t
  in
  (* Events decoded before the run, during startup, are delivered first. *)
  let read_events ~timeout =
    if Queue.is_empty t.events && not t.input_ended then
      read_input t ~timeout ~native:handle_event;
    while not (Queue.is_empty t.events) do
      handle_event (Queue.pop t.events)
    done;
    if t.input_ended && t.lifecycle <> `Closed then close t
  in

  let rec loop last_time =
    if not (running t) then ()
    else (
      if t.pending_resize <> None then (
        maybe_apply_pending_resize t;
        if t.pending_resize = None then notify_resize ~initial:false);
      if t.reanchor && render_eligible t then (
        reanchor t;
        notify_resize ~initial:false);
      let now = t.backend.now () in
      (* A due one-shot wakeup runs the frame callback without a render: it
         exists so time-based work (timers) can advance off the clock while
         the loop is otherwise idle. Whatever the callback dispatches requests
         its own redraw, which the pacing below then honors. *)
      let last_time =
        match t.wakeup_deadline with
        | Some dl when now >= dl ->
            t.wakeup_deadline <- None;
            Option.iter (fun f -> f t ~dt:(now -. last_time)) on_frame;
            now
        | Some _ | None -> last_time
      in
      if should_render_now t ~now then (
        let frame_interval = compute_loop_interval t in
        let last_time = render_cycle ~now ~last_time in
        (* Schedule next frame deadline after rendering: delay =
           target_frame_time - frame_elapsed. The deadline drives only the
           live cadence — left armed while idle it would read as perpetually
           due and spin the loop; one-shot pacing hangs off
           [last_render_time] instead. *)
        let render_end = t.backend.now () in
        t.last_render_time <- render_end;
        t.next_frame_deadline <-
          (if cadence_active t then
             match frame_interval with
             | Some iv ->
                 let elapsed = render_end -. now in
                 let delay = Float.max 0. (iv -. elapsed) in
                 Some (render_end +. delay)
             | None -> Some render_end
           else None);
        let timeout = compute_timeout t ~now:render_end in
        if t.running then read_events ~timeout;
        loop last_time)
      else
        let timeout = compute_timeout t ~now in
        if t.running then read_events ~timeout;
        loop last_time)
  in
  let start_time = t.backend.now () in
  Fun.protect
    (fun () -> loop start_time)
    ~finally:(fun () -> if t.lifecycle <> `Closed then close t)

(* Diagnostics *)

let set_debug_overlay ?corner t ~enabled =
  let previous = t.debug_overlay_enabled in
  t.debug_overlay_enabled <- enabled;
  Option.iter
    (fun c ->
      t.debug_overlay_cb <-
        Debug_overlay.on_frame ~corner:c
          ~capacity:t.config.debug_overlay_capacity ())
    corner;
  if previous <> enabled || Option.is_some corner then request_redraw t

let toggle_debug_overlay ?corner t =
  set_debug_overlay ?corner t ~enabled:(not t.debug_overlay_enabled)

let configure_frame_dump ?every ?dir ?pattern ?hits t =
  Option.iter
    (fun ev ->
      t.frame_dump_every <- max 0 ev;
      t.frame_dump_counter <- 0)
    every;
  Option.iter (fun d -> t.frame_dump_dir <- Some d) dir;
  Option.iter (fun p -> t.frame_dump_pattern <- Some p) pattern;
  Option.iter (fun h -> t.frame_dump_hits <- h) hits

let dump_frame ?hits ?dir ?pattern t =
  let or_default a b = match a with Some _ -> a | None -> b in
  Frame_dump.snapshot
    ?dir:(or_default dir t.frame_dump_dir)
    ?pattern:(or_default pattern t.frame_dump_pattern)
    ~hits:(Option.value ~default:t.frame_dump_hits hits)
    t.screen
