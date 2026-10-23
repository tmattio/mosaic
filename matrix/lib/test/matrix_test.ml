type entry = Chunk of string | Event of Matrix.event

type t = {
  mutable now : int;
  mutable width : int;
  mutable height : int;
  mode : Matrix.mode;
  fps : float;
  queue : entry Queue.t;
  mutable input_offset : int;
  mutable ended : bool;
  mutable resized : bool;
  woken : bool Atomic.t;
  output : Buffer.t;
  on_idle : t -> until:int option -> unit;
  on_wake : unit -> unit;
  (* The last run seen over [t], read after it ends. *)
  mutable last : Matrix.held option;
}

(* A plain modern terminal: truecolor and Unicode width tables, no Kitty
   extensions. Pinned so rendering never depends on the ambient $TERM. *)
let capabilities : Matrix.Terminal.capabilities =
  {
    rgb = true;
    kitty_keyboard = None;
    kitty_graphics = false;
    bracketed_paste = true;
    focus_tracking = true;
    unicode_width = `Unicode;
    grapheme_mode = None;
    sgr_pixels = false;
    color_scheme_updates = false;
    explicit_width = false;
    explicit_cursor_positioning = false;
    scaled_text = false;
    sixel = false;
    sync = false;
    hyperlinks = false;
    meta_sends_escape = None;
    multiplexer = None;
    identity = None;
  }

let app t =
  match Matrix.holder () with
  | Some held ->
      t.last <- Some held;
      held
  | None -> (
      match t.last with
      | Some held -> held
      | None -> invalid_arg "Matrix_test: no run held the terminal")

let grid t =
  let (Matrix.Held app) = app t in
  Matrix.current_grid app

let size t =
  let (Matrix.Held app) = app t in
  Matrix.size app

let frame_owed _ =
  match Matrix.holder () with
  | Some (Held app) -> Matrix.frame_owed app
  | None -> false

let settle t ~until =
  match until with
  | Some until when frame_owed t && until > t.now ->
      t.now <- until;
      true
  | _ -> false

let feed t bytes = if bytes <> "" then Queue.push (Chunk bytes) t.queue
let feed_event t event = Queue.push (Event event) t.queue

let resize t ~width ~height =
  t.width <- width;
  t.height <- height;
  t.resized <- true

let now t = t.now

let advance t ns =
  if ns < 0 then invalid_arg "Matrix_test.advance: negative time";
  t.now <- t.now + ns

let screen t = Matrix.Grid.to_text (grid t)
let screen_ansi t = Matrix.Grid.to_ansi (grid t)
let output t = Buffer.contents t.output
let stop t = t.ended <- true
let config t = Matrix.config ~mode:t.mode ~fps:t.fps ~caps:capabilities ()

let create ?(mode = `Alt) ?(fps = 60.) ?(now = 0) ?(on_wake = fun () -> ())
    ~on_idle ~width ~height () =
  {
    now;
    width;
    height;
    mode;
    fps;
    queue = Queue.create ();
    input_offset = 0;
    ended = false;
    resized = false;
    woken = Atomic.make false;
    output = Buffer.create 4096;
    on_idle;
    on_wake;
    last = None;
  }

(* Events queued ahead of the next bytes are handed to the run as decoded;
   the input is readable when bytes or its end come next. *)
let rec deliver t =
  match Queue.peek_opt t.queue with
  | Some (Event event) ->
      ignore (Queue.pop t.queue : entry);
      let (Matrix.Held app) = app t in
      Matrix.inject app event;
      deliver t
  | Some (Chunk _) -> true
  | None -> t.ended

let has_input t = (not (Queue.is_empty t.queue)) || t.ended || t.resized
let due t = function Some until -> t.now >= until | None -> false

let wait t ~until ~reading =
  if t.last = None then ignore (app t : Matrix.held);
  if not (has_input t || Atomic.exchange t.woken false || due t until) then
    t.on_idle t ~until;
  reading && deliver t

let rec copy_input t buf off len written =
  if written = len then written
  else
    match Queue.peek_opt t.queue with
    | None | Some (Event _) -> written
    | Some (Chunk chunk) ->
        let remaining = String.length chunk - t.input_offset in
        let count = min (len - written) remaining in
        Bytes.blit_string chunk t.input_offset buf (off + written) count;
        if count = remaining then (
          ignore (Queue.pop t.queue : entry);
          t.input_offset <- 0)
        else t.input_offset <- t.input_offset + count;
        copy_input t buf off len (written + count)

let read_input t buf off len =
  match copy_input t buf off len 0 with
  | 0 when t.ended && Queue.is_empty t.queue -> 0
  | 0 -> -1
  | n -> n

let take_signals t =
  if t.resized then (
    t.resized <- false;
    [ Matrix.Backend.Winch ])
  else []

let backend t : Matrix.Backend.t =
  {
    now = (fun () -> t.now);
    wait = wait t;
    wake =
      (fun () ->
        Atomic.set t.woken true;
        t.on_wake ());
    take_signals = (fun () -> take_signals t);
    read_input = read_input t;
    write_output = (fun buf off len -> Buffer.add_subbytes t.output buf off len);
    terminal_size = (fun () -> (t.width, t.height));
    set_raw_mode = ignore;
    flush_input =
      (fun () ->
        Queue.clear t.queue;
        t.input_offset <- 0);
    input_is_tty = false;
    output_is_tty = false;
    controlling = false;
    protect = (fun f -> f ());
    cleanup = ignore;
    env = Matrix.Terminal.Caps.env (fun _ -> None) ~win32:false;
  }
