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

(* The signal handlers, in C (matrix_stubs.c). *)

(* [signal_take s always restart] takes [s] if its action is the default,
   or whatever it is when [always], and says whether it did. *)
external signal_take : int -> bool -> bool -> bool = "matrix_signal_take"
external signal_restore : int -> unit = "matrix_signal_restore"
external signal_ignored : int -> bool = "matrix_signal_ignored"
external signal_raise : int -> unit = "matrix_signal_raise"
external signal_stop_group : unit -> unit = "matrix_signal_stop_group"

(* The runs the handlers know: [run_register controlling] is a run's id and
   its wake pipe's read and write ends. *)
external run_register : bool -> int * Unix.file_descr * Unix.file_descr
  = "matrix_run_register"

external run_unregister : int -> unit = "matrix_run_unregister"
external run_pending : int -> int -> bool = "matrix_run_pending"
external run_terminating : int -> bool = "matrix_run_terminating"

external run_settings : int -> Unix.file_descr -> Unix.file_descr -> unit
  = "matrix_run_settings"

external run_forget_settings : int -> unit = "matrix_run_forget_settings"
external run_reset_bytes : int -> string -> unit = "matrix_run_reset_bytes"
external fatal_restore : unit -> unit = "matrix_fatal_restore"

module Backend = struct
  type signal = Winch | Cont | Tstp | Terminate of int

  type t = {
    now : unit -> int;
    wait : until:int option -> reading:bool -> bool;
    wake : unit -> unit;
    take_signals : unit -> signal list;
    read_input : bytes -> int -> int -> int;
    write_output : bytes -> int -> int -> unit;
    terminal_size : unit -> int * int;
    set_raw_mode : bool -> unit;
    flush_input : unit -> unit;
    input_is_tty : bool;
    output_is_tty : bool;
    controlling : bool;
    on_fatal : string -> unit;
    protect : (unit -> unit) -> unit;
    cleanup : unit -> unit;
    env : Terminal.Caps.env;
  }

  (* The wake pipes of the backends [unix] made, by their [wake], which a
     backend built from one keeps. *)
  let pipes : ((unit -> unit) * Unix.file_descr) list ref = ref []
  let pipes_lock = Mutex.create ()

  let wake_fd b =
    match Mutex.protect pipes_lock (fun () -> List.assq_opt b.wake !pipes) with
    | Some fd -> fd
    | None -> invalid_arg "Matrix.Backend.wake_fd: not a backend unix made"

  let wake_byte = Bytes.of_string "w"

  let drain fd =
    let buf = Bytes.create 64 in
    let rec go () =
      match Unix.read fd buf 0 64 with
      | 64 -> go ()
      | _ -> ()
      | exception Unix.Unix_error _ -> ()
    in
    go ()

  (* The signals the runtime's handlers recorded for run [id], in the order
     of this list. *)
  let signals_from_handlers id () =
    let take acc (s, signal) =
      if run_pending id s then signal :: acc else acc
    in
    List.rev
      (List.fold_left take []
         [
           (Sys.sigwinch, Winch);
           (Sys.sigcont, Cont);
           (Sys.sigtstp, Tstp);
           (Sys.sigint, Terminate Sys.sigint);
           (Sys.sigterm, Terminate Sys.sigterm);
           (Sys.sighup, Terminate Sys.sighup);
           (Sys.sigquit, Terminate Sys.sigquit);
         ])

  (* [Unix.write] goes on past its first [write(2)] and, interrupted there,
     raises without saying how much went: each call here is one
     [write(2)], and the write goes on from where it stopped. *)
  let rec write_all id fd buf off len =
    if len > 0 then
      match Unix.single_write fd buf off len with
      | n -> write_all id fd buf (off + n) (len - n)
      | exception Unix.Unix_error (Unix.EAGAIN, _, _) ->
          (try ignore (Unix.select [] [ fd ] [] (-1.))
           with Unix.Unix_error (Unix.EINTR, _, _) -> ());
          write_all id fd buf off len
      | exception Unix.Unix_error (Unix.EINTR, _, _) ->
          (* A termination signal interrupts a write the terminal is not
             taking: the rest is dropped, and the loop acts on the signal. *)
          if not (run_terminating id) then write_all id fd buf off len

  let rec read_input fd buf off len =
    match Unix.read fd buf off len with
    | n -> n
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> read_input fd buf off len
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> -1

  let unix ?(input = Unix.stdin) ?(output = Unix.stdout) () =
    let input_is_tty = Matrix_tty.is_tty input in
    let output_is_tty = Matrix_tty.is_tty output in
    let controlling =
      Matrix_tty.is_controlling input || Matrix_tty.is_controlling output
    in
    let id, wake_r, wake_w = run_register controlling in
    let wake () =
      try ignore (Unix.write wake_w wake_byte 0 1 : int)
      with Unix.Unix_error _ -> ()
    in
    Mutex.protect pipes_lock (fun () -> pipes := (wake, wake_r) :: !pipes);
    let saved = ref None in
    let set_raw_mode raw =
      match (raw, !saved) with
      | true, None when input_is_tty ->
          run_settings id input output;
          saved := Some (Matrix_tty.raw input)
      | false, Some termios ->
          saved := None;
          (* Restoring from the background would stop the process with
             SIGTTOU, unless the signal is blocked. *)
          let mask = Thread.sigmask SIG_BLOCK [ Sys.sigttou ] in
          Fun.protect
            ~finally:(fun () -> ignore (Thread.sigmask SIG_SETMASK mask))
            (fun () -> Matrix_tty.restore input termios);
          run_forget_settings id
      | _ -> ()
    in
    let wait ~until ~reading =
      let fds = if reading then [ input; wake_r ] else [ wake_r ] in
      let timeout =
        match until with
        | None -> -1.
        | Some until ->
            Float.max 0. (Float.of_int (until - Matrix_tty.now ()) *. 1e-9)
      in
      let readable, _, _ =
        try Unix.select fds [] [] timeout
        with Unix.Unix_error (Unix.EINTR, _, _) -> ([], [], [])
      in
      if List.mem wake_r readable then drain wake_r;
      reading && List.mem input readable
    in
    {
      now = Matrix_tty.now;
      wait;
      wake;
      take_signals = signals_from_handlers id;
      read_input = read_input input;
      write_output = write_all id output;
      (* A descriptor with no window, a pipe or a file, is given the size
         of a classic terminal. *)
      terminal_size =
        (fun () -> Option.value (Matrix_tty.window output) ~default:(80, 24));
      set_raw_mode;
      flush_input =
        (fun () -> if input_is_tty then Matrix_tty.flush_input input);
      input_is_tty;
      output_is_tty;
      controlling;
      on_fatal = run_reset_bytes id;
      protect = (fun f -> f ());
      cleanup =
        (fun () ->
          Fun.protect
            (fun () -> set_raw_mode false)
            ~finally:(fun () ->
              run_unregister id;
              Mutex.protect pipes_lock (fun () ->
                  pipes := List.filter (fun (w, _) -> w != wake) !pipes)));
      env = Terminal.Caps.env Sys.getenv_opt ~win32:Sys.win32;
    }
end

(* Configuration *)

type config = {
  mode : mode;
  raw_mode : bool;
  mouse : Terminal.mouse_mode;
  bracketed_paste : bool;
  kitty_keyboard : kitty_keyboard;
  key_releases : bool;
  fps : float;
  cursor_visible : bool option;
  explicit_width : bool;
  min_tui_height : int;
  caps : Terminal.capabilities option;
  debug_overlay : bool;
  debug_overlay_corner : debug_overlay_corner;
  debug_overlay_capacity : int;
  frame_dump_every : int;
  frame_dump_dir : string option;
  frame_dump_pattern : string option;
  frame_dump_hits : bool;
}

let config ?(mode = `Alt) ?(raw_mode = true) ?(mouse = `Sgr_any)
    ?(bracketed_paste = true) ?(kitty_keyboard = `Auto) ?(key_releases = false)
    ?(fps = 60.) ?cursor_visible ?(explicit_width = false) ?(min_tui_height = 1)
    ?caps ?(debug_overlay = false) ?(debug_overlay_corner = `Bottom_right)
    ?(debug_overlay_capacity = 120) ?(frame_dump_every = 0) ?frame_dump_dir
    ?frame_dump_pattern ?(frame_dump_hits = false) () =
  if (not (Float.is_finite fps)) || fps <= 0. then
    invalid_arg "Matrix.config: fps must be positive and finite";
  {
    mode;
    raw_mode;
    mouse;
    bracketed_paste;
    kitty_keyboard;
    key_releases;
    fps;
    cursor_visible;
    explicit_width;
    min_tui_height;
    caps;
    debug_overlay;
    debug_overlay_corner;
    debug_overlay_capacity;
    frame_dump_every = max 0 frame_dump_every;
    frame_dump_dir;
    frame_dump_pattern;
    frame_dump_hits;
  }

(* Queries sent together, which the terminal answers in order. A batch of
   capability queries, of the runtime's other queries or of the
   application's ends with DA1; a cursor batch is one position report, its
   own answer, which says where the live region is. *)
type question =
  | Capabilities of Terminal.Caps.batch
  | Cursor of anchor
  | Own  (** Queries whose replies the runtime takes as they come. *)
  | Application

(* What a cursor report places: the live region at the cursor, when the run
   begins or takes the terminal back; or, after a resize, the live region
   whose row [live_row] the cursor was left on. *)
and anchor = At_cursor | Moved of int

type batch = {
  question : question;
  owed : Input.Decoder.owed;
  sent : int;
  mutable reports : int;  (** Cursor reports received. *)
  mutable answered : bool;
}

(* Deadlines, in a binary heap ordered by time, then by the order they were
   set, which a timer's number is. *)
type entry = { deadline : int; id : int; mutable index : int }

type heap = {
  mutable entries : entry array;
  mutable length : int;
  by_id : (int, entry) Hashtbl.t;
  mutable next_id : int;
}

type timer = int

type 'msg step =
  | Event of event
  | Timer of timer
  | Posted of 'msg
  | Frame
  | Stopped

(* The size the terminal reported and the runtime has yet to apply. *)
type size_report = Applied | Read | Reported of int * int

type 'msg app = {
  serial : int;
  terminal : Terminal.t;
  decoder : Input.Decoder.t;
  input_buffer : bytes;
  mutable input_ended : bool;
  (* When input last arrived, from which silence is measured. *)
  mutable last_input : int;
  (* The round trip of the startup probe, 0 until measured. *)
  mutable rtt : int;
  mutable evidence : Terminal.Caps.evidence;
  (* Batches still owed, oldest first. *)
  mutable batches : batch list;
  (* Cursor reports owed by batches given up, which a late report answers
     first. *)
  mutable lost_reports : int;
  (* Batches that end with DA1 and wait for the one in flight, oldest
     first. *)
  mutable waiting : (question * (string * Input.Decoder.owed)) list;
  (* Events decoded and not yet returned. *)
  events : event Queue.t;
  config : config;
  backend : Backend.t;
  owner : int;
  screen : Screen.t;
  (* One retained writer owns the complete terminal frame. *)
  submit_buf : Ansi.Writer.t;
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
  (* The cursor report that says where the live region is, which frames wait
     for. *)
  mutable anchor : batch option;
  mutable scroll_hint : Screen.scroll_hint option;
  (* Cursor state tracking — visual caches are valid only while the cursor is
     visible. [None] means the terminal state is unknown (startup, hidden, or
     after the terminal was given to another program), so the next visible
     frame must emit. *)
  mutable last_cursor_position : (int * int) option;
  mutable last_cursor_visible : bool option;
  mutable last_cursor_shape : Ansi.cursor_shape option;
  mutable last_cursor_color : (int * int * int) option option;
  (* Time *)
  mutable now : int;
  heap : heap;
  (* Functions posted from any thread, newest first, and those taken by the
     current turn. *)
  mailbox : 'msg list Atomic.t;
  accepting : bool Atomic.t;
  posted : 'msg Queue.t;
  (* Whether a turn has begun, and whether each kind of work still owes the
     current turn the step every turn gives it. *)
  mutable begun : bool;
  mutable owe_event : bool;
  mutable owe_timer : bool;
  mutable owe_post : bool;
  (* Frames *)
  interval : int;
  mutable dirty : bool;
  mutable drawing : bool;
  (* The frame being drawn was drawn across a release. *)
  mutable void : bool;
  mutable last_frame : int;
  mutable force_full_next_frame : bool;
  mutable size_report : size_report;
  (* The usable size last returned as [Resize]. *)
  mutable usable : int * int;
  (* A [Resize] was returned for the frame that follows it. *)
  mutable frame_follows : bool;
  mutable frame_modes_armed : bool;
  mutable restore_modes_on_next_focus : bool;
  (* While [wait] runs, the application must not call in: an Eio fiber on
     the loop's thread could. *)
  mutable waiting_backend : bool;
  mutable released : bool;
  (* Signals the run took, to restore when it ends. *)
  mutable taken : int list;
  (* Signals received and not yet acted on, oldest first. *)
  mutable kept : Backend.signal list;
  mutable tstp_ignored : bool;
  (* Undos of the steps that took the terminal, latest first. *)
  mutable undo : (exiting:bool -> unit) list;
  (* Diagnostics *)
  mutable debug_overlay_enabled : bool;
  mutable debug_overlay_cb :
    Screen.t -> render_start:int -> render_ns:int -> unit;
  (* The last render of the screen: when it started and how long it took,
     on the backend's clock. The screen reads no clock. *)
  mutable render_start : int;
  mutable render_ns : int;
  mutable frame_dump_every : int;
  mutable frame_dump_dir : string option;
  mutable frame_dump_pattern : string option;
  mutable frame_dump_hits : bool;
  mutable frame_dump_counter : int;
}

(* The runs, latest first, under [runs_lock]: runs may start on several
   domains. A run on the controlling terminal holds it; one started inside
   [release] holds it while the outer one waits. *)
type held = Held : 'msg app -> held

let runs : held list ref = ref []
let runs_lock = Mutex.create ()
let serials = Atomic.make 0
let thread_id () = Thread.id (Thread.self ())

let check t =
  if thread_id () <> t.owner then
    invalid_arg "Matrix: called off the thread that runs the application";
  if t.waiting_backend then
    invalid_arg "Matrix: called while the loop waits in its backend"

(* Heap *)

let heap_create () =
  { entries = [||]; length = 0; by_id = Hashtbl.create 8; next_id = 0 }

let before a b =
  a.deadline < b.deadline || (a.deadline = b.deadline && a.id < b.id)

let heap_set h i e =
  h.entries.(i) <- e;
  e.index <- i

let rec sift_up h i =
  if i > 0 then
    let parent = (i - 1) / 2 in
    let e = h.entries.(i) and p = h.entries.(parent) in
    if before e p then (
      heap_set h parent e;
      heap_set h i p;
      sift_up h parent)

let rec sift_down h i =
  let l = (2 * i) + 1 in
  if l < h.length then
    let r = l + 1 in
    let child =
      if r < h.length && before h.entries.(r) h.entries.(l) then r else l
    in
    let e = h.entries.(i) and c = h.entries.(child) in
    if before c e then (
      heap_set h i c;
      heap_set h child e;
      sift_down h child)

let heap_add h deadline =
  let id = h.next_id in
  h.next_id <- id + 1;
  let e = { deadline; id; index = h.length } in
  if h.length = Array.length h.entries then (
    let grown = Array.make (max 8 (2 * h.length)) e in
    Array.blit h.entries 0 grown 0 h.length;
    h.entries <- grown);
  heap_set h h.length e;
  h.length <- h.length + 1;
  Hashtbl.replace h.by_id id e;
  sift_up h e.index;
  id

let heap_remove h e =
  Hashtbl.remove h.by_id e.id;
  let last = h.entries.(h.length - 1) in
  h.length <- h.length - 1;
  if e.index < h.length then (
    heap_set h e.index last;
    sift_down h last.index;
    sift_up h last.index)

(* The earliest deadline, [max_int] when there is none. *)
let heap_min h = if h.length = 0 then max_int else h.entries.(0).deadline

(* Small helpers *)

let clamp lo hi v = max lo (min hi v)
let ms n = n * 1_000_000

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
let now t = t.now

(* A reply needs a terminal on the input side, in raw mode. *)
let can_query t =
  t.backend.input_is_tty && t.config.raw_mode && not t.input_ended

let request_redraw t =
  check t;
  t.dirty <- true

let frame_owed t = t.dirty

let at t time =
  check t;
  heap_add t.heap time

let cancel t timer =
  check t;
  match Hashtbl.find_opt t.heap.by_id timer with
  | Some e -> heap_remove t.heap e
  | None -> ()

let post t msg =
  if Atomic.get t.accepting then
    let rec push () =
      let posts = Atomic.get t.mailbox in
      if not (Atomic.compare_and_set t.mailbox posts (msg :: posts)) then
        push ()
      else if posts = [] then t.backend.wake ()
    in
    push ()

(* The terminal cuts what it receives into cells by the width method the
   runtime sets, and the screen lays out its cells by the same, so a cell is
   as wide on the screen as in the terminal. A change of method voids the
   cells the terminal shows, laid out by the old one. *)
let width_method t =
  if t.config.explicit_width then `Wcwidth
  else (Terminal.capabilities t.terminal).unicode_width

(* The runtime sets the terminal's grapheme mode only where the terminal
   answered it reset, and resets it when it gives the terminal back: a
   terminal is left with the mode as it was found. *)
let set_width_method t m =
  let caps = Terminal.capabilities t.terminal in
  Terminal.enable_grapheme_mode t.terminal
    (m <> `Wcwidth && caps.grapheme_mode = Some false);
  if Grid.width_method (Screen.current_grid t.screen) <> m then (
    Screen.set_width_method t.screen m;
    force_full_redraw t)

let refresh_capabilities t =
  let caps = Terminal.capabilities t.terminal in
  let color_depth = if caps.rgb then `Truecolor else `Ansi256 in
  Screen.apply_capabilities t.screen ~explicit_width:caps.explicit_width
    ~explicit_cursor_positioning:caps.explicit_cursor_positioning
    ~hyperlinks:caps.hyperlinks ~color_depth;
  set_width_method t (width_method t)

(* A batch's queries are sent, and what they are owed is added to the
   decoder's counts until the batch is answered or given up. *)
let send_batch t question (bytes, owed) =
  Terminal.send t.terminal bytes;
  Input.Decoder.expect t.decoder owed;
  let b = { question; owed; sent = t.now; reports = 0; answered = false } in
  t.batches <- t.batches @ [ b ];
  b

let ends_with_da1 = function
  | Capabilities _ | Own | Application -> true
  | Cursor _ -> false

(* A batch that ends with DA1 is sent once no other is in flight. Every DA1
   reply is alike, so a reply is told to answer a batch by the order of the
   batches alone, and a batch whose DA1 never comes, such as a passthrough
   check that tmux drops, would otherwise take the next batch's. *)
let ask_batch t question bytes_owed =
  if List.exists (fun b -> ends_with_da1 b.question) t.batches then (
    if
      (* The runtime's own query, already waiting, would be answered the
       same. *)
      not (question = Own && List.mem (question, bytes_owed) t.waiting)
    then t.waiting <- t.waiting @ [ (question, bytes_owed) ])
  else ignore (send_batch t question bytes_owed : batch)

let da1 = Ansi.(to_string (query Device_attributes))

(* A query whose reply the runtime takes as it comes, such as the pixel size,
   is a batch closed by DA1 too, so that its reply is never taken for the
   application's. *)
let ask_own t q =
  if can_query t then
    ask_batch t Own
      ( Ansi.(to_string (query q)) ^ da1,
        { Input.Decoder.cursor_reports = 0; string_replies = 0 } )

(* Input modes follow the configuration and the capabilities, and are set
   again whenever the capabilities change: setting a mode a terminal has is
   idempotent, so this is the difference between what the modes were and what
   they must be. Focus, color scheme and resize reports are set without asking:
   no reference says what a terminal does with a private mode it does not
   know, but xterm (charproc.c, dpmodes: "unknown private code"), kitty
   (screen.c, set_mode_from_const: "Unsupported screen mode"), foot (csi.c,
   decset_decrst: UNHANDLED) and tmux (input.c,
   input_csi_dispatch_sm_private: "unknown") log it and do nothing else. The
   kitty keyboard flags are pushed once the terminal has answered its query,
   then asked for again, so that the capabilities say which flags hold. *)

let kitty_flags t = if t.config.key_releases then 0b11111 else 0b00101

let apply_modes t =
  let terminal = t.terminal in
  let caps = Terminal.capabilities terminal in
  Terminal.set_mouse_mode terminal t.config.mouse;
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
    if not t.released then (
      apply_modes t;
      refresh_capabilities t;
      force_full_redraw t;
      t.dirty <- true))

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
  | Ambiguous -> Some (max (ms 50) (2 * t.rtt))
  | Sequence -> Some (max (ms 500) (4 * t.rtt))

let answer_wait t = max (ms 500) (4 * t.rtt)
let owed_bound t = max (ms 5000) (4 * t.rtt)
let push t event = Queue.push event t.events
let inject = push

(* A batch leaves the decoder's counts once, answered or given up. *)
let close t b =
  if List.memq b t.batches then (
    Input.Decoder.settle t.decoder b.owed;
    t.batches <- List.filter (fun b' -> b' != b) t.batches;
    match t.waiting with
    | (question, bytes_owed) :: rest when ends_with_da1 b.question ->
        t.waiting <- rest;
        ignore (send_batch t question bytes_owed : batch)
    | _ -> ())

let give_up t b =
  t.lost_reports <-
    t.lost_reports + b.owed.Input.Decoder.cursor_reports - b.reports;
  close t b

let ask t batch =
  ask_batch t (Capabilities batch) (Terminal.Caps.ask t.evidence batch)

let ask_cursor t anchor =
  send_batch t (Cursor anchor)
    ( Ansi.(to_string (query Cursor_position)),
      { Input.Decoder.cursor_reports = 1; string_replies = 0 } )

(* Adopts a primary layout transition: its operations go out with the next
   frame, and a frame after operations or a move of the live region redraws
   it whole. *)
let relayout t (primary, ops) =
  if ops <> [] || Primary.live_region primary <> Primary.live_region t.primary
  then force_full_redraw t;
  t.primary <- primary;
  t.layout_ops <- t.layout_ops @ ops

(* The live region starts where the cursor report says. When the run begins
   or takes the terminal back, it starts at the cursor; after a resize, the
   cursor was left on a known live row. What the terminal did meanwhile voids
   the operations planned before. *)
let anchored t anchor ~row ~col =
  match anchor with
  | At_cursor ->
      t.layout_ops <- [];
      relayout t (Primary.anchor t.primary ~row:(row + 1) ~col:(col + 1))
  | Moved live_row ->
      t.layout_ops <- [];
      relayout t (Primary.anchor t.primary ~row:(row + 1 - live_row) ~col:1);
      sync_primary_layout t ~resize:true

(* A cursor report answers the oldest query still owed one, a given-up one
   first. *)
let cursor_report t ~row ~col ~page =
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
        | Cursor anchor -> (
            b.answered <- true;
            close t b;
            (* An answer later than the wait for it places nothing: the
               live region stays where it was planned. *)
            match t.anchor with
            | Some a when a == b && t.now < b.sent + answer_wait t ->
                t.anchor <- None;
                if not t.released then anchored t anchor ~row ~col;
                t.dirty <- true
            | _ -> ())
        | Application -> push t (Reply (Cursor { row; col; page }))
        | Own -> ())

(* The batch that ends with DA1 in flight, which the replies that are not
   cursor reports answer. *)
let replying t = List.find_opt (fun b -> ends_with_da1 b.question) t.batches

(* A DA1 reply answers the batch in flight that asked for one. The terminal
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
          if q = Probe then t.rtt <- t.backend.now () - b.sent;
          t.evidence <- Terminal.Caps.answered t.evidence q ~attributes;
          Option.iter (ask t) (Terminal.Caps.next t.evidence q)
      | Application -> push t (Reply (Primary_attributes attributes))
      | Own | Cursor _ -> ())

(* A reply is the runtime's to act on, and the application's too while its
   batch is the one being answered. The reports a terminal sends on its own
   that are application facts are events. *)
let reply t (r : Input.Reply.t) =
  (match r with
  | Cursor { row; col; page } -> cursor_report t ~row ~col ~page
  | Primary_attributes attributes -> attributes_report t attributes
  | Color_scheme scheme -> push t (Color_scheme scheme)
  | Resize { rows; cols; height; width } ->
      Terminal.set_pixel_resolution t.terminal (Some (width, height));
      if (cols, rows) <> (t.width, t.height) then (
        t.size_report <- Reported (cols, rows);
        t.dirty <- true)
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
  (* What was cut or over the budget is in error, and dropped. *)
  | Cut _ | Too_long _ -> ()

let rec expire_batches t = function
  | [] -> ()
  | b :: rest ->
      if t.now >= b.sent + owed_bound t then give_up t b;
      expire_batches t rest

let expire t = expire_batches t t.batches

(* The time the input next needs attention, [max_int] when it needs none:
   a silence that resolves what the decoder holds, or a batch given up. *)
let input_deadline t =
  let silence =
    match silence t with Some wait -> t.last_input + wait | None -> max_int
  in
  match t.batches with
  | [] -> silence
  | b :: _ -> min silence (b.sent + owed_bound t)

(* Reads what the input has, once, if [ready]; a wait that read nothing is
   silence, which decides what the decoder holds once it has lasted. *)
let read_input t ~ready =
  let n =
    if ready && not t.input_ended then
      t.backend.read_input t.input_buffer 0 (Bytes.length t.input_buffer)
    else -1
  in
  (if n = 0 then (
     t.input_ended <- true;
     Input.Decoder.finish t.decoder (decoded t))
   else if n > 0 then (
     t.last_input <- t.now;
     Input.Decoder.feed t.decoder t.input_buffer ~off:0 ~len:n (decoded t))
   else
     match silence t with
     | Some wait when t.now >= t.last_input + wait ->
         Input.Decoder.idle t.decoder (decoded t)
     | _ -> ());
  expire t

let wait t ~until =
  t.waiting_backend <- true;
  match t.backend.wait ~until ~reading:(not t.input_ended) with
  | ready ->
      t.waiting_backend <- false;
      t.now <- t.backend.now ();
      ready
  | exception exn ->
      t.waiting_backend <- false;
      raise exn

(* Reads input until every batch in [batches] is answered, the wait for an
   answer since [from] is over, or the input ends. The wait grows if the
   round trip is measured meanwhile. Events decoded meanwhile are kept. *)
let await t ~from batches =
  let pending () = List.exists (fun b -> not b.answered) batches in
  while pending () && (not t.input_ended) && t.now < from + answer_wait t do
    let until = min (from + answer_wait t) (input_deadline t) in
    let ready = wait t ~until:(Some until) in
    read_input t ~ready
  done

let abort_paste t =
  check t;
  Input.Decoder.cut_paste t.decoder (decoded t)

let query t ~owed bytes =
  check t;
  if can_query t then ask_batch t Application (bytes ^ da1, owed)

let refresh_render_region t =
  match t.config.mode with
  | `Alt ->
      Screen.set_row_offset t.screen 0;
      Screen.resize t.screen ~width:t.width ~height:t.height
  | `Primary ->
      relayout t (Primary.resize t.primary ~terminal_height:(max 1 t.height));
      sync_primary_layout t ~resize:true

(* Static output (primary mode only) *)

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
   an ANSI writer and flushed as a single write in [draw]. *)

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
  check t;
  if t.config.mode <> `Alt then
    let rows, text = static_snapshot t ?rows grid in
    if rows > 0 then (
      t.primary <-
        Primary.enqueue_static t.primary { text; rows; preserve_live_region };
      t.dirty <- true)

let static_replace ?(preserve_live_region = false) ?rows t grid =
  check t;
  if t.config.mode <> `Alt then (
    let rows, text = static_snapshot t ?rows grid in
    t.primary <-
      Primary.replace_static t.primary { text; rows; preserve_live_region };
    t.dirty <- true)

(* Immediate action, not part of the frame pipeline: the plan's terminal ops
   are encoded and sent directly, not queued into the frame buffer, so the
   primary screen clears before the next frame renders. *)
let static_clear t =
  check t;
  if t.config.mode = `Alt then ()
  else
    let primary, ops = Primary.clear_static t.primary in
    t.primary <- primary;
    (* The erase supersedes the layout operations not yet sent, and the
       place a cursor report owed would give the live region. *)
    t.layout_ops <- [];
    t.anchor <- None;
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
    t.dirty <- true

let set_scroll_hint t hint =
  check t;
  t.scroll_hint <- Some hint

(* Frames *)

(* No eager buffer clear here: Screen defers the post-present clear of the
   next grid and hit grid to their first observation ([Screen.next_grid],
   [Screen.build], ...), so both direct-draw apps given the grid and
   renderer hosts building through [Screen.build] pay for exactly one clear
   per frame instead of clearing here and again downstream. *)
let prepare ?primary_required_rows t =
  refresh_capabilities t;
  if t.config.mode = `Primary then begin
    relayout t (Primary.flush_static t.primary);
    Option.iter
      (fun rows -> relayout t (Primary.require_rows t.primary ~rows))
      primary_required_rows
  end;
  refresh_render_region t

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
  if t.debug_overlay_enabled then
    t.debug_overlay_cb t.screen ~render_start:t.render_start
      ~render_ns:t.render_ns;
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
  let start = t.backend.now () in
  Screen.render_into ~full:forced_full ?scroll_hint ?viewport t.screen buf;
  t.render_start <- start;
  t.render_ns <- t.backend.now () - start;
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
  if Ansi.Writer.len buf > preamble_len || cursor_dirty t ~cursor ~position then begin
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

(* A frame drawn across a release is not presented: what the screen holds
   is unknown, and the next frame is drawn whole. *)
let draw t ?primary_required_rows f =
  check t;
  if not t.drawing then invalid_arg "Matrix.draw: no frame is being drawn";
  prepare ?primary_required_rows t;
  f (Screen.next_grid t.screen);
  t.drawing <- false;
  if t.void then (
    t.void <- false;
    t.dirty <- true)
  else submit t

(* Cursor *)

(* Cursor setters update Screen state only. The actual terminal output
   is deferred to [draw], which writes everything into the frame buffer
   as a single write. *)

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

(* The terminal's size, applied when a frame is next drawn: a storm of
   reports costs one layout. A terminal moves its rows as it resizes, by
   rules of its own, and the cursor moves with its row, so in [`Primary]
   mode frames wait for a cursor report that says where the live region
   went: from the live row the last frame left the cursor on, or, before any
   frame since the region was placed, from the cursor, which is still where
   the region starts. The layout operations planned for the old size were
   not sent, and the report voids them. *)
let apply_size t =
  let cols, rows =
    match t.size_report with
    | Applied -> (t.width, t.height)
    | Read -> t.backend.terminal_size ()
    | Reported (cols, rows) -> (cols, rows)
  in
  t.size_report <- Applied;
  if cols > 0 && rows > 0 && (cols, rows) <> (t.width, t.height) then (
    t.width <- cols;
    t.height <- rows;
    ask_own t Pixel_size;
    force_full_redraw t;
    refresh_render_region t;
    if t.config.mode = `Primary && can_query t then
      let anchor =
        match t.cursor_live_row with
        | Some live_row -> Moved live_row
        | None -> At_cursor
      in
      t.anchor <- Some (ask_cursor t anchor))

(* Event delivery *)

let adjust_event_for_offset t event =
  match (t.config.mode, event) with
  | `Primary, Mouse mouse ->
      Mouse { mouse with y = Primary.map_mouse_y t.primary mouse.y }
  | _ -> event

(* A Focus_in after a Focus_out sets the modes again, once: a multiplexer
   that was detached meanwhile may have lost them. *)
let deliver t event =
  (match event with
  | Focus_in ->
      if t.restore_modes_on_next_focus then (
        t.restore_modes_on_next_focus <- false;
        Terminal.restore_modes t.terminal)
  | Focus_out -> t.restore_modes_on_next_focus <- true
  | _ -> ());
  Event (adjust_event_for_offset t event)

(* Taking the terminal and giving it back

   Each step that takes something pushes the undo that gives it back; giving
   the terminal back runs the undos, latest first, each even if another
   raises. [exiting] tells the end of the run from a release, which hands the
   terminal to another program and takes it again. *)

let push_undo t undo = t.undo <- undo :: t.undo

let run_undos t ~exiting =
  let undos = t.undo in
  t.undo <- [];
  let first = ref None in
  t.backend.protect (fun () ->
      List.iter
        (fun undo ->
          try undo ~exiting
          with exn ->
            if !first = None then
              first := Some (exn, Printexc.get_raw_backtrace ()))
        undos);
  Option.iter (fun (exn, bt) -> Printexc.raise_with_backtrace exn bt) !first

(* Signals. The runtime takes the signals that end or stop the process only
   at their default action, as X/Open Curses allows, and the window and
   continue signals always, chaining a handler installed before. A signal
   that ends the process does not resume a system call it interrupts: a
   write the terminal is not taking would otherwise keep the run from
   ending. *)

let stopping_signals = [ Sys.sigtstp; Sys.sigterm; Sys.sighup ]
let interrupting_signals = [ Sys.sigint; Sys.sigquit ]

let take_signal t s ~always ~restart =
  if signal_take s always restart then t.taken <- s :: t.taken

(* The signals that end the process concern every run on a terminal; the
   window and job-control signals come from the controlling terminal
   only. *)
let take_signals t =
  t.tstp_ignored <- signal_ignored Sys.sigtstp;
  if t.backend.controlling then (
    take_signal t Sys.sigwinch ~always:true ~restart:true;
    take_signal t Sys.sigcont ~always:true ~restart:true;
    take_signal t Sys.sigtstp ~always:false ~restart:true);
  if t.backend.input_is_tty || t.backend.output_is_tty || t.backend.controlling
  then (
    take_signal t Sys.sigabrt ~always:false ~restart:true;
    List.iter
      (fun s -> take_signal t s ~always:false ~restart:false)
      [ Sys.sigint; Sys.sigterm; Sys.sighup; Sys.sigquit ]);
  push_undo t (fun ~exiting ->
      if exiting then (
        List.iter signal_restore t.taken;
        t.taken <- []))

(* Raw mode. Leaving it waits for the output to be sent and keeps input not
   yet read for whoever reads next; at the end of the run that is the shell,
   which must not read the runtime's mouse reports. *)
let take_raw_mode t =
  if t.backend.input_is_tty && t.config.raw_mode then (
    t.backend.set_raw_mode true;
    push_undo t (fun ~exiting ->
        (if exiting then try t.backend.flush_input () with _ -> ());
        t.backend.set_raw_mode false;
        if exiting then t.backend.flush_input ()))

(* Frame writes bypass the terminal handle and may stop between mode changes.
   Attempt every reset independently, before handing the terminal back. *)
let restore_frame_modes t =
  if t.frame_modes_armed then (
    t.frame_modes_armed <- false;
    (try Terminal.send t.terminal sync_output_off with _ -> ());
    (try Terminal.send t.terminal reset_scroll_region_seq with _ -> ());
    try Terminal.send t.terminal "\027[?7h" with _ -> ())

(* The screen a program run in the application's place finds. In [`Primary]
   mode that is the transcript, the static rows queued for it included, and
   below it the live region, erased, with the cursor at its start: the
   program's output goes on from the transcript. In [`Alt] mode it is the
   primary screen as the shell left it. The cursor is shown. *)
let give_screen t =
  match t.config.mode with
  | `Alt -> Terminal.leave_alternate_screen t.terminal
  | `Primary ->
      relayout t (Primary.flush_static t.primary);
      let w = Ansi.Writer.create 64 in
      List.iter (apply_primary_op w) t.layout_ops;
      t.layout_ops <- [];
      buf_cursor_position w ~row:(Primary.render_offset t.primary + 1) ~col:1;
      Ansi.Writer.write_string w sgr_reset;
      Ansi.Writer.write_string w erase_below_seq;
      Ansi.Writer.write_string w cursor_show_seq;
      Terminal.send t.terminal (Ansi.Writer.contents w)

(* What the screen held is unknown once another program had it: the next
   frame is drawn whole and sets the cursor again. *)
let forget_screen t =
  force_full_redraw t;
  t.last_cursor_position <- None;
  t.cursor_live_row <- None;
  t.last_cursor_visible <- None;
  t.last_cursor_shape <- None;
  t.last_cursor_color <- None;
  Grid.clear (Screen.next_grid t.screen);
  Screen.Hit_grid.clear (Screen.next_hit_grid t.screen)

(* A step that writes may fail halfway: its undo is pushed first, and undoes
   whatever part of it was done. *)
let take_screen t =
  push_undo t (fun ~exiting:_ -> give_screen t);
  match t.config.mode with
  | `Alt ->
      Terminal.enter_alternate_screen t.terminal;
      let cursor = Screen.cursor t.screen in
      Screen.set_cursor t.screen { cursor with visible = false }
  | `Primary -> Terminal.leave_alternate_screen t.terminal

let reset_modes t ~exiting =
  restore_frame_modes t;
  if exiting then Terminal.close t.terminal
  else
    List.iter
      (fun reset -> try reset () with _ -> ())
      [
        (fun () -> Terminal.set_mouse_mode t.terminal `Off);
        (fun () -> Terminal.enable_bracketed_paste t.terminal false);
        (fun () -> Terminal.enable_focus_reporting t.terminal false);
        (fun () -> Terminal.enable_kitty_keyboard t.terminal false);
        (fun () -> Terminal.enable_modify_other_keys t.terminal false);
        (fun () -> Terminal.enable_color_scheme_reports t.terminal false);
        (fun () -> Terminal.enable_resize_reports t.terminal false);
        (fun () -> Terminal.enable_meta_sends_escape t.terminal false);
        (fun () -> Terminal.enable_grapheme_mode t.terminal false);
        (fun () -> Terminal.send t.terminal cursor_show_seq);
      ]

let take_modes t =
  push_undo t (fun ~exiting -> reset_modes t ~exiting);
  refresh_capabilities t;
  apply_modes t;
  refresh_render_region t;
  force_full_redraw t

(* The bytes that reset what a run may set, which the SIGABRT handler writes
   itself. *)
let abort_bytes mode =
  String.concat ""
    [
      sync_output_off;
      reset_scroll_region_seq;
      "\027[?7h";
      "\027[?1000l\027[?1002l\027[?1003l\027[?1006l";
      "\027[?2004l\027[?1004l\027[?2031l\027[?2048l\027[?1036r";
      "\027[<u\027[>4;0m";
      sgr_reset;
      cursor_show_seq;
      (match mode with `Alt -> "\027[?1049l" | `Primary -> "\r\n");
    ]

(* What restores the terminal if the process ends where no OCaml code runs:
   the backend writes these bytes itself, and they go once the terminal is
   given back. *)
let take_fatal_reset t =
  t.backend.on_fatal (abort_bytes t.config.mode);
  push_undo t (fun ~exiting:_ -> t.backend.on_fatal "")

(* Waits, when a reply is owed, for the terminal to answer what was sent:
   the replies that come after the terminal is given away would reach the
   shell, or the program run in the application's place, as typing. Every
   event decoded and not yet returned, meanwhile or before, is dropped, but
   for replies, and a paste in progress is cut ([release] says why). Batches
   still owed are dropped: their answers went to whoever has the terminal. *)
let settle t =
  if t.batches <> [] && can_query t then await t ~from:t.now t.batches;
  let kept = Queue.create () in
  Queue.iter (function Reply _ as e -> Queue.push e kept | _ -> ()) t.events;
  Queue.clear t.events;
  Queue.transfer kept t.events;
  List.iter (fun b -> Input.Decoder.settle t.decoder b.owed) t.batches;
  t.batches <- [];
  t.lost_reports <- 0;
  t.anchor <- None;
  (* A paste the application began to receive ends. *)
  Input.Decoder.cut_paste t.decoder (decoded t);
  Input.Decoder.reset t.decoder

(* The screen is given back where the terminal has it: its size is read,
   which the loop may not have seen change, and in [`Primary] mode the cursor
   report that says where the live region went is waited for with the
   replies owed. *)
let give_back t ~exiting =
  t.backend.protect (fun () ->
      try
        t.size_report <- Read;
        apply_size t;
        settle t
      with _ -> ());
  run_undos t ~exiting

(* The batch that waited for the ones dropped is sent. *)
let send_waiting t =
  match t.waiting with
  | (question, bytes_owed) :: rest when t.batches = [] ->
      t.waiting <- rest;
      ignore (send_batch t question bytes_owed : batch)
  | _ -> ()

(* Takes the terminal again after another program had it: its settings are
   the shell's now, input it left is discarded, the size is read, and in
   [`Primary] mode frames wait for a cursor report that says where the
   program's output ended. *)
let take_again t =
  take_fatal_reset t;
  take_raw_mode t;
  t.backend.flush_input ();
  let cols, rows = t.backend.terminal_size () in
  t.width <- max 1 cols;
  t.height <- max 1 rows;
  t.size_report <- Applied;
  forget_screen t;
  take_screen t;
  take_modes t;
  if t.config.mode = `Primary && can_query t then
    t.anchor <- Some (ask_cursor t At_cursor);
  send_waiting t;
  t.dirty <- true

let release t f =
  check t;
  if t.released then invalid_arg "Matrix.release: the terminal is released";
  if t.drawing then t.void <- true;
  t.now <- t.backend.now ();
  give_back t ~exiting:false;
  t.released <- true;
  (* POSIX system(): the child gets the terminal's suspend and termination
     signals; the interrupt and quit characters reach the parent too, which
     ignores them. *)
  let stopping = List.filter (fun s -> List.mem s t.taken) stopping_signals in
  List.iter signal_restore stopping;
  t.taken <- List.filter (fun s -> not (List.mem s stopping)) t.taken;
  let take_back () =
    List.iter
      (fun s -> take_signal t s ~always:false ~restart:(s = Sys.sigtstp))
      stopping;
    t.kept <-
      t.kept
      @ List.filter
          (function
            | Backend.Terminate s -> not (List.mem s interrupting_signals)
            | _ -> true)
          (t.backend.take_signals ());
    t.released <- false;
    t.now <- t.backend.now ();
    take_again t
  in
  match f () with
  | result ->
      take_back ();
      result
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      (try take_back () with _ -> ());
      Printexc.raise_with_backtrace exn bt

let stop_job t =
  check t;
  if t.backend.controlling && not t.tstp_ignored then
    release t signal_stop_group

(* The loop *)

let has_due_timer t = t.heap.length > 0 && heap_min t.heap <= t.now

let take_timer t =
  let e = t.heap.entries.(0) in
  heap_remove t.heap e;
  Timer e.id

(* Frames wait for the cursor report that says where the live region is,
   at most as long as for any answer; then the layout stays as planned. *)
let render_blocked t =
  match t.anchor with
  | Some b when (not b.answered) && t.now < b.sent + answer_wait t -> true
  | Some _ ->
      t.anchor <- None;
      false
  | None -> false

(* A frame is due once its interval has passed by the clock as the turn's
   work ends: work that took the turn's budget may have taken the interval
   with it. *)
let frame_due t =
  t.dirty && (not t.released)
  && (t.now >= t.last_frame + t.interval
     || t.backend.now () >= t.last_frame + t.interval)
  && not (render_blocked t)

(* The time the turn waits until: now when work is left, otherwise the
   earliest deadline, if any. *)
let wait_deadline t =
  if
    (not (Queue.is_empty t.events))
    || (not (Queue.is_empty t.posted))
    || has_due_timer t
    || Atomic.get t.mailbox != []
  then Some t.now
  else
    let deadline = min (heap_min t.heap) (input_deadline t) in
    let deadline =
      if t.dirty then
        let frame = t.last_frame + t.interval in
        match t.anchor with
        | Some b when not b.answered ->
            min deadline (max frame (b.sent + answer_wait t))
        | _ -> min deadline frame
      else deadline
    in
    if deadline = max_int then None else Some deadline

(* Whether a run is ending the process on a termination signal. *)
let ending = Atomic.make false

(* The signals received since the last call, kept ones first. *)
let received t =
  let s = t.kept @ t.backend.take_signals () in
  t.kept <- [];
  s

let rec end_run t s =
  if Atomic.compare_and_set ending false true then (
    t.now <- t.backend.now ();
    (try give_back t ~exiting:true with _ -> ());
    List.iter signal_restore t.taken;
    t.taken <- [];
    (* The other runs' terminals, as an abort restores them. *)
    fatal_restore ();
    signal_raise s;
    (* The default action ends the process; nothing returns here. *)
    exit 2)
  else
    (* Another run ends the process. *)
    let m = Mutex.create () and never = Condition.create () in
    Mutex.lock m;
    while true do
      Condition.wait never m
    done

(* A SIGCONT the run did not cause: whoever continued the process has put
   back the shell's settings. *)
and continued t =
  if not t.released then (
    if t.backend.input_is_tty && t.config.raw_mode then (
      t.backend.set_raw_mode false;
      t.backend.set_raw_mode true);
    Terminal.restore_modes t.terminal;
    forget_screen t;
    t.size_report <- Read;
    t.dirty <- true)

and on_signals t signals =
  List.iter
    (function
      | Backend.Winch ->
          t.size_report <- Read;
          t.dirty <- true
      | Cont -> continued t
      | Tstp ->
          if not t.released then (
            release t (fun () -> signal_raise Sys.sigtstp);
            (* The SIGCONT that ended the stop is this one's. *)
            on_signals t
              (List.filter
                 (function Backend.Cont -> false | _ -> true)
                 (received t)))
      | Terminate s -> end_run t s)
    signals

let take_posts t =
  match Atomic.exchange t.mailbox [] with
  | [] -> ()
  | posts -> List.iter (fun f -> Queue.push f t.posted) (List.rev posts)

let begin_turn t =
  let ready = wait t ~until:(wait_deadline t) in
  (match received t with [] -> () | s -> on_signals t s);
  if Queue.is_empty t.events then read_input t ~ready else expire t;
  take_posts t;
  t.owe_event <- not (Queue.is_empty t.events);
  t.owe_timer <- has_due_timer t;
  t.owe_post <- not (Queue.is_empty t.posted)

(* Frames are an interval apart from when they are drawn, which a turn's
   work may delay past the time the turn read. *)
let start_frame t =
  t.dirty <- false;
  t.drawing <- true;
  t.void <- false;
  t.last_frame <- t.backend.now ();
  Frame

(* Work goes on until one frame interval has passed since the clock was read
   for the turn. *)
let within_budget t = t.backend.now () - t.now < t.interval

let rec step t =
  if t.owe_event && not (Queue.is_empty t.events) then (
    t.owe_event <- false;
    deliver t (Queue.pop t.events))
  else if t.owe_timer && has_due_timer t then (
    t.owe_timer <- false;
    take_timer t)
  else if t.owe_post && not (Queue.is_empty t.posted) then (
    t.owe_post <- false;
    Posted (Queue.pop t.posted))
  else if (not (Queue.is_empty t.events)) && within_budget t then
    deliver t (Queue.pop t.events)
  else if has_due_timer t && within_budget t then take_timer t
  else if (not (Queue.is_empty t.posted)) && within_budget t then
    Posted (Queue.pop t.posted)
  else if t.input_ended && Queue.is_empty t.events then Stopped
  else if frame_due t then begin_frame t
  else (
    begin_turn t;
    step t)

(* The latest size is applied before a frame; a change of the usable size is
   returned first. The frame follows unless it must wait for the cursor
   report that says where the live region went. *)
and begin_frame t =
  if t.size_report <> Applied then apply_size t;
  let usable = size t in
  if usable <> t.usable then (
    t.usable <- usable;
    t.frame_follows <- not (render_blocked t);
    Event (Resize (fst usable, snd usable)))
  else if render_blocked t then (
    begin_turn t;
    step t)
  else start_frame t

let next t =
  check t;
  t.drawing <- false;
  if t.frame_follows then (
    t.frame_follows <- false;
    start_frame t)
  else (
    (* The first turn has the input read while the terminal was taken, and
       what was posted and set since. *)
    if not t.begun then (
      t.begun <- true;
      begin_turn t);
    step t)

(* Running *)

let make backend config =
  let evidence =
    match config.caps with
    | Some caps -> Terminal.Caps.assume caps
    | None -> Terminal.Caps.evidence backend.Backend.env
  in
  let terminal =
    Terminal.make ~tty:backend.output_is_tty
      ~output:(fun s ->
        backend.write_output (Bytes.unsafe_of_string s) 0 (String.length s))
      (Terminal.Caps.derive evidence)
  in
  let cols, rows = backend.terminal_size () in
  let width = max 1 cols and height = max 1 rows in
  let cursor_visible =
    Option.value config.cursor_visible ~default:(config.mode = `Alt)
  in
  let screen =
    Screen.create ~cursor_visible ~explicit_width:config.explicit_width ()
  in
  let now = backend.now () in
  {
    serial = Atomic.fetch_and_add serials 1;
    terminal;
    decoder = Input.Decoder.create ();
    input_buffer = Bytes.create 4096;
    input_ended = false;
    last_input = now;
    rtt = 0;
    evidence;
    batches = [];
    lost_reports = 0;
    waiting = [];
    events = Queue.create ();
    config;
    backend;
    owner = thread_id ();
    screen;
    submit_buf = Ansi.Writer.create 4096;
    width;
    height;
    primary =
      Primary.create ~terminal_height:height
        ~min_live_height:config.min_tui_height;
    layout_ops = [];
    cursor_live_row = None;
    anchor = None;
    scroll_hint = None;
    last_cursor_position = None;
    last_cursor_visible = None;
    last_cursor_shape = None;
    last_cursor_color = None;
    now;
    heap = heap_create ();
    mailbox = Atomic.make [];
    accepting = Atomic.make true;
    posted = Queue.create ();
    begun = false;
    owe_event = false;
    owe_timer = false;
    owe_post = false;
    interval = Float.to_int (Float.round (1e9 /. config.fps));
    dirty = true;
    drawing = false;
    void = false;
    last_frame = min_int / 2;
    force_full_next_frame = true;
    size_report = Applied;
    usable = (width, height);
    frame_follows = false;
    frame_modes_armed = false;
    restore_modes_on_next_focus = false;
    waiting_backend = false;
    released = false;
    taken = [];
    kept = [];
    tstp_ignored = false;
    undo = [];
    debug_overlay_enabled = config.debug_overlay;
    debug_overlay_cb =
      Debug_overlay.on_frame ~corner:config.debug_overlay_corner
        ~capacity:config.debug_overlay_capacity ();
    render_start = 0;
    render_ns = 0;
    frame_dump_every = config.frame_dump_every;
    frame_dump_dir = config.frame_dump_dir;
    frame_dump_pattern = config.frame_dump_pattern;
    frame_dump_hits = config.frame_dump_hits;
    frame_dump_counter = 0;
  }

(* A terminal answers queries only on a raw input: the probe waits for its
   answers, and input that comes meanwhile is kept for the run. *)
let probe t =
  if can_query t then (
    let b =
      send_batch t (Capabilities Probe) (Terminal.Caps.ask t.evidence Probe)
    in
    await t ~from:b.sent [ b ];
    let cols, rows = t.backend.terminal_size () in
    t.width <- max 1 cols;
    t.height <- max 1 rows;
    t.primary <-
      Primary.create ~terminal_height:t.height
        ~min_live_height:t.config.min_tui_height;
    if t.config.mode = `Primary && not t.input_ended then (
      let b = ask_cursor t At_cursor in
      t.anchor <- Some b;
      await t ~from:b.sent [ b ];
      t.anchor <- None))

let setup t =
  take_signals t;
  take_fatal_reset t;
  take_raw_mode t;
  probe t;
  take_screen t;
  take_modes t;
  ask_own t Pixel_size;
  t.usable <- size t

(* The innermost run of the calling thread. *)
let holder () =
  let self = thread_id () in
  Mutex.protect runs_lock (fun () ->
      List.find_opt (fun (Held app) -> app.owner = self) !runs)

(* Registers [t], unless its terminal is the controlling one and another
   run holds it. *)
let hold t =
  Mutex.protect runs_lock (fun () ->
      if
        t.backend.controlling
        && List.exists
             (fun (Held app) -> app.backend.controlling && not app.released)
             !runs
      then false
      else (
        runs := Held t :: !runs;
        true))

let unhold t =
  Mutex.protect runs_lock (fun () ->
      runs := List.filter (fun (Held app) -> app.serial <> t.serial) !runs)

let run ?backend ?(config = config ()) f =
  let backend = match backend with Some b -> b | None -> Backend.unix () in
  let t =
    try make backend config
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      (try backend.protect backend.cleanup with _ -> ());
      Printexc.raise_with_backtrace exn bt
  in
  if not (hold t) then (
    (try backend.protect backend.cleanup with _ -> ());
    invalid_arg "Matrix.run: another run holds the terminal");
  let finish () =
    Atomic.set t.accepting false;
    unhold t;
    t.backend.protect t.backend.cleanup
  in
  let teardown () =
    Fun.protect ~finally:finish (fun () ->
        t.now <- t.backend.now ();
        give_back t ~exiting:true)
  in
  match
    setup t;
    f t
  with
  | result ->
      teardown ();
      result
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      (try teardown () with _ -> ());
      Printexc.raise_with_backtrace exn bt

(* Diagnostics *)

let set_debug_overlay ?corner t ~enabled =
  check t;
  let previous = t.debug_overlay_enabled in
  t.debug_overlay_enabled <- enabled;
  Option.iter
    (fun c ->
      t.debug_overlay_cb <-
        Debug_overlay.on_frame ~corner:c
          ~capacity:t.config.debug_overlay_capacity ())
    corner;
  if previous <> enabled || Option.is_some corner then t.dirty <- true

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
