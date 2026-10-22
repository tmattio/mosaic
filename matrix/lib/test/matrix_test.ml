type entry = Chunk of string | Event of Matrix.event

type t = {
  mutable now : float;
  mutable width : int;
  mutable height : int;
  queue : entry Queue.t;
  mutable input_offset : int;
  output : Buffer.t;
  on_idle : t -> timeout:float option -> unit;
  mutable app : Matrix.app option;
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
  match t.app with
  | Some app -> app
  | None -> invalid_arg "Matrix_test.app: backend not initialized"

let feed t bytes = if bytes <> "" then Queue.push (Chunk bytes) t.queue
let feed_event t event = Queue.push (Event event) t.queue

let resize t ~width ~height =
  t.width <- width;
  t.height <- height;
  feed_event t (Matrix.Resize (width, height))

let now t = t.now
let set_now t time = t.now <- time
let screen t = Matrix.Grid.to_text (Matrix.current_grid (app t))
let screen_ansi t = Matrix.Grid.to_ansi (Matrix.current_grid (app t))
let output t = Buffer.contents t.output
let stop t = Matrix.stop (app t)

let create ?(mode = `Alt) ?(target_fps = 60.) ?(exit_on_ctrl_c = false)
    ?(now = 0.) ?(on_wake = fun () -> ()) ~on_idle ~width ~height () =
  let output = Buffer.create 4096 in
  let t =
    {
      now;
      width;
      height;
      queue = Queue.create ();
      input_offset = 0;
      output;
      on_idle;
      app = None;
    }
  in
  let rec deliver ~on_event =
    if Queue.is_empty t.queue then false
    else
      match Queue.peek t.queue with
      | Chunk _ -> true
      | Event event ->
          ignore (Queue.pop t.queue : entry);
          on_event event;
          deliver ~on_event
  in
  let wait ~timeout ~on_event =
    (if Queue.is_empty t.queue then
       match timeout with
       | Some s when s <= 0. -> ()
       | Some s when Matrix.redraw_requested (app t) ->
           (* Release a live-animation frame at its pacing deadline. One-shot
              redraws render immediately with [pace_redraws:false], so this
              never advances the virtual clock for a one-shot redraw. *)
           t.now <- t.now +. s
       | timeout -> t.on_idle t ~timeout);
    deliver ~on_event
  in
  let rec copy_input buf off len written =
    if written = len || Queue.is_empty t.queue then written
    else
      match Queue.peek t.queue with
      | Event _ -> written
      | Chunk chunk ->
          let remaining = String.length chunk - t.input_offset in
          let count = min (len - written) remaining in
          Bytes.blit_string chunk t.input_offset buf (off + written) count;
          if count = remaining then (
            ignore (Queue.pop t.queue : entry);
            t.input_offset <- 0)
          else t.input_offset <- t.input_offset + count;
          copy_input buf off len (written + count)
  in
  let backend : Matrix.Backend.t =
    {
      now = (fun () -> t.now);
      protect = (fun f -> f ());
      wake = on_wake;
      write_output = (fun buf off len -> Buffer.add_subbytes output buf off len);
      read_input = (fun buf off len -> copy_input buf off len 0);
      wait;
      input_is_tty = false;
      output_is_tty = false;
      terminal_size = (fun () -> (t.width, t.height));
      set_raw_mode = (fun _ -> ());
      flush_input =
        (fun () ->
          Queue.clear t.queue;
          t.input_offset <- 0);
      cleanup = (fun () -> ());
      env = Matrix.Terminal.Caps.env (fun _ -> None) ~win32:false;
    }
  in
  let app =
    Matrix.attach ~mode ~target_fps:(Some target_fps) ~exit_on_ctrl_c
      ~input_timeout:None ~resize_debounce:(Some 0.) ~start_idle:true
      ~pace_redraws:false ~caps:capabilities ~backend ()
  in
  t.app <- Some app;
  t
