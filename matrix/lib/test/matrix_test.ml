type t = {
  input_r : Unix.file_descr;
  input_w : Unix.file_descr;
  output_fd : Unix.file_descr;
  mode : Matrix.mode;
  fps : float;
  mutable ended : bool;
  (* The run's time when the clock was first read, and how far the test
     has moved it since. *)
  mutable origin : int option;
  mutable elapsed : int;
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

let rec write_all fd s off =
  if off < String.length s then
    match Unix.write_substring fd s off (String.length s - off) with
    | n -> write_all fd s (off + n)
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> write_all fd s off

(* The terminal's own report of its size, as mode 2048 has it. *)
let report_size t ~width ~height =
  write_all t.input_w (Printf.sprintf "\027[48;%d;%d;0;0t" height width) 0

let create ?(mode = `Alt) ?(fps = 60.) ~width ~height () =
  let input_r, input_w = Unix.pipe ~cloexec:true () in
  let path = Filename.temp_file "matrix_test" ".out" in
  let output_fd =
    Unix.openfile path [ O_RDWR; O_APPEND; O_TRUNC; O_CLOEXEC ] 0o600
  in
  Unix.unlink path;
  let t =
    {
      input_r;
      input_w;
      output_fd;
      mode;
      fps;
      ended = false;
      origin = None;
      elapsed = 0;
      last = None;
    }
  in
  report_size t ~width ~height;
  t

let input t = t.input_r
let output t = t.output_fd
let config t = Matrix.config ~mode:t.mode ~fps:t.fps ~caps:capabilities ()

let app t =
  match Matrix.holder () with
  | Some held ->
      t.last <- Some held;
      held
  | None -> (
      match t.last with
      | Some held -> held
      | None -> invalid_arg "Matrix_test: no run held the terminal")

let now t =
  match t.origin with
  | Some origin -> origin + t.elapsed
  | None -> (
      match Matrix.holder () with
      | Some (Held a as held) ->
          t.last <- Some held;
          let origin = Matrix.now a in
          t.origin <- Some origin;
          origin + t.elapsed
      | None -> invalid_arg "Matrix_test.now: no run holds the terminal")

let elapsed t = t.elapsed

let advance t ns =
  if ns < 0 then invalid_arg "Matrix_test.advance: negative time";
  t.elapsed <- t.elapsed + ns

let feed t bytes =
  if t.ended then invalid_arg "Matrix_test.feed: the input has ended";
  write_all t.input_w bytes 0

let feed_event t event =
  let (Matrix.Held a) = app t in
  Matrix.inject a event

let resize t ~width ~height = report_size t ~width ~height

let stop t =
  if not t.ended then (
    t.ended <- true;
    Unix.close t.input_w)

let close t =
  stop t;
  Unix.close t.input_r;
  Unix.close t.output_fd

let frame_owed _ =
  match Matrix.holder () with
  | Some (Held a) -> Matrix.frame_owed a
  | None -> false

let settle t ~until =
  match until with
  | Some until when frame_owed t && until > now t ->
      advance t (until - now t);
      true
  | _ -> false

let grid t =
  let (Matrix.Held a) = app t in
  Matrix.current_grid a

let size t =
  let (Matrix.Held a) = app t in
  Matrix.size a

let screen t = Matrix.Grid.to_text (grid t)
let screen_ansi t = Matrix.Grid.to_ansi (grid t)

let written t =
  let length = (Unix.fstat t.output_fd).st_size in
  let bytes = Bytes.create length in
  let rec go off =
    if off < length then
      match Unix.read t.output_fd bytes off (length - off) with
      | 0 -> ()
      | n -> go (off + n)
  in
  ignore (Unix.lseek t.output_fd 0 SEEK_SET : int);
  go 0;
  Bytes.to_string bytes
