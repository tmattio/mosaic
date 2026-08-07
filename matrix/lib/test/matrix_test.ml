type entry = Chunk of string | Event of Matrix.Input.t

type t = {
  mutable now : float;
  mutable width : int;
  mutable height : int;
  queue : entry Queue.t;
  parser : Matrix.Input.Parser.t;
  output : Buffer.t;
  on_idle : t -> timeout:float option -> unit;
  mutable app : Matrix.app option;
}

(* A plain modern terminal: truecolor and Unicode width tables, no Kitty
   extensions. Pinned so rendering never depends on the ambient $TERM. *)
let capabilities : Matrix.Terminal.capabilities =
  {
    term = "matrix-test";
    rgb = true;
    kitty_keyboard = false;
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
  }

let app t =
  match t.app with
  | Some app -> app
  | None -> invalid_arg "Matrix_test.app: backend not initialized"

let feed t bytes = Queue.push (Chunk bytes) t.queue
let feed_event t event = Queue.push (Event event) t.queue

let resize t ~width ~height =
  t.width <- width;
  t.height <- height;
  feed_event t (Matrix.Input.Resize (width, height))

let now t = t.now
let set_now t time = t.now <- time
let screen t = Matrix.Grid.to_text (Matrix.current_grid (app t))
let screen_ansi t = Matrix.Grid.to_ansi (Matrix.current_grid (app t))
let output t = Buffer.contents t.output
let stop t = Matrix.stop (app t)

let create ?(mode = `Alt) ?(target_fps = 60.) ?(exit_on_ctrl_c = false)
    ?(now = 0.) ?(on_wake = fun () -> ()) ~on_idle ~width ~height () =
  let output = Buffer.create 4096 in
  let terminal =
    Matrix.Terminal.make
      ~output:(fun _ -> ())
      ~tty:false ~initial_caps:capabilities ()
  in
  let t =
    {
      now;
      width;
      height;
      queue = Queue.create ();
      parser = Matrix.Input.Parser.create ();
      output;
      on_idle;
      app = None;
    }
  in
  let deliver ~on_event ~on_response =
    let delivered = ref false in
    while not (Queue.is_empty t.queue) do
      delivered := true;
      match Queue.pop t.queue with
      | Event event -> on_event event
      | Chunk chunk ->
          let input = Bytes.unsafe_of_string chunk in
          Matrix.Input.Parser.feed t.parser input 0 (Bytes.length input)
            ~now:t.now ~on_event ~on_response;
          Matrix.Input.Parser.drain t.parser ~now:t.now ~on_event ~on_response
    done;
    !delivered
  in
  let read_events ~timeout ~on_event ~on_response =
    (if not (deliver ~on_event ~on_response) then
       match timeout with
       | Some s when s <= 0. -> ()
       | Some s when Matrix.redraw_requested (app t) ->
           (* A live-animation frame gated on the render cadence: release it by
              moving virtual time to its deadline rather than parking in
              [on_idle]. One-shot redraws never reach here — [pace_redraws:false]
              renders them at the current instant (their timeout is [0.]), so a
              redraw cannot silently advance the clock that timers read. The
              cadence is runtime pacing, not something tests script, so [on_idle]
              only ever observes a genuinely quiescent frame. *)
           t.now <- t.now +. s
       | timeout ->
           t.on_idle t ~timeout;
           ignore (deliver ~on_event ~on_response : bool));
    (* Drain at the current instant, as the Unix backend does after every wait.
       A lone ESC (and any incomplete sequence) sits on an ambiguity deadline;
       once the driver has stepped the clock past it, the drain emits the
       decoded key even though no new bytes arrived. Forcing this by feeding an
       empty chunk cannot work — [Parser.feed] re-scans the buffered ESC and
       re-arms its deadline into the future — so the flush must be a clock step,
       not a feed. *)
    Matrix.Input.Parser.drain t.parser ~now:t.now ~on_event ~on_response;
    `Continue
  in
  let backend : Matrix.Backend.t =
    {
      now = (fun () -> t.now);
      wake = on_wake;
      write_output = (fun buf off len -> Buffer.add_subbytes output buf off len);
      read_input = (fun _ _ _ -> 0);
      wait_readable = (fun ~timeout:_ -> false);
      read_events;
      terminal_size = (fun () -> (t.width, t.height));
      set_raw_mode = (fun _ -> ());
      flush_input = (fun () -> Queue.clear t.queue);
      cleanup = (fun () -> ());
    }
  in
  let app =
    Matrix.attach ~mode ~target_fps:(Some target_fps) ~exit_on_ctrl_c
      ~signal_handlers:false ~input_timeout:None ~resize_debounce:(Some 0.)
      ~start_idle:true ~pace_redraws:false ~backend ~parser:t.parser ~terminal
      ~width ~height ()
  in
  t.app <- Some app;
  t
