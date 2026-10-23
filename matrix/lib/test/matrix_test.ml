module Grid = Matrix.Grid
module Keystroke = Matrix.Input.Keystroke
module Mouse_report = Matrix.Input.Mouse_report
module Decoder = Matrix.Input.Decoder
module Reply = Matrix.Input.Reply

module Profile = struct
  type t = {
    tty : bool;
    answers : bool;
    env : (string * string) list;
    identity : Vte.identity;
    extensions : Vte.extension list;
    width_method : Matrix.Text.width_method;
    palette : Vte.palette;
    color_scheme : [ `Dark | `Light ];
  }

  let vte =
    {
      tty = true;
      answers = true;
      env = [ ("TERM", "xterm-256color"); ("COLORTERM", "truecolor") ];
      identity = Vte.vt100;
      extensions = Vte.extensions;
      width_method = `Unicode;
      palette = Vte.default_palette;
      color_scheme = `Dark;
    }

  (* A program whose output is piped guesses its terminal from its
     environment, and measures text by character, as such a guess does. *)
  let pipe = { vte with tty = false; answers = false; width_method = `Wcwidth }
end

(* Screens *)

module Screen = struct
  type cursor = {
    row : int;
    col : int;
    shape : Matrix.Ansi.cursor_style;
    blinking : bool;
  }

  (* A copy of the cells, which the terminal goes on changing. *)
  type t = { grid : Grid.t; cursor : cursor option }

  let of_vte vte =
    let row, col = Vte.cursor_pos vte in
    let shape, blinking = Vte.cursor_style vte in
    {
      grid = Grid.copy (Vte.grid vte);
      cursor =
        (if (Vte.modes vte).cursor_visible then
           Some { row; col; shape; blinking }
         else None);
    }

  let rows s = Grid.height s.grid
  let cols s = Grid.width s.grid
  let cursor s = s.cursor

  let check s fn ~row ~col =
    if row < 0 || row >= rows s || col < 0 || col >= cols s then
      invalid_arg
        (Printf.sprintf "Matrix_test.Screen.%s: %d,%d is not a cell of %dx%d" fn
           row col (cols s) (rows s))

  let cell s ~row ~col =
    check s "cell" ~row ~col;
    let idx = Grid.idx s.grid ~x:col ~y:row in
    (Grid.get_text s.grid idx, Grid.get_style s.grid idx)

  (* A row's text, a space for an empty cell, and the column of each byte. *)
  let row_columns s y =
    let b = Buffer.create (cols s) and columns = ref [] in
    for x = 0 to cols s - 1 do
      let idx = Grid.idx s.grid ~x ~y in
      if not (Grid.is_continuation s.grid idx) then (
        let text = Grid.get_text s.grid idx in
        let text = if text = "" then " " else text in
        for _ = 1 to String.length text do
          columns := x :: !columns
        done;
        Buffer.add_string b text)
    done;
    (Buffer.contents b, Array.of_list (List.rev !columns))

  let trim s =
    let n = ref (String.length s) in
    while !n > 0 && s.[!n - 1] = ' ' do
      decr n
    done;
    String.sub s 0 !n

  let row s y =
    check s "row" ~row:y ~col:0;
    trim (fst (row_columns s y))

  let find s text =
    if text = "" then invalid_arg "Matrix_test.Screen.find: empty text";
    let found = ref [] in
    for y = 0 to rows s - 1 do
      let line, columns = row_columns s y in
      let n = String.length text in
      for i = 0 to String.length line - n do
        if String.sub line i n = text then found := (y, columns.(i)) :: !found
      done
    done;
    List.rev !found

  (* Golden text: rows numbered from 1, and the cursor counted from 1. *)

  let number_width s = max 2 (String.length (string_of_int (rows s)))
  let numbered s y = Printf.sprintf "%0*d |" (number_width s) (y + 1)
  let blank_prefix s = String.make (number_width s + 1) ' ' ^ "|"

  let shape_name = function
    | `Block -> "block"
    | `Underline -> "underline"
    | `Line -> "bar"

  let cursor_line = function
    | None -> "cursor hidden"
    | Some c ->
        Printf.sprintf "cursor %d,%d %s%s" (c.row + 1) (c.col + 1)
          (shape_name c.shape)
          (if c.blinking then " blinking" else "")

  let text s =
    let b = Buffer.create (rows s * (cols s + 5)) in
    for y = 0 to rows s - 1 do
      Buffer.add_string b (numbered s y);
      Buffer.add_string b (row s y);
      Buffer.add_char b '\n'
    done;
    Buffer.add_string b (cursor_line s.cursor);
    Buffer.add_char b '\n';
    Buffer.contents b

  (* The default style, which the terminal's blanks have: its default
     colors, no attribute, no hyperlink. *)
  let is_default (st : Matrix.Ansi.Style.t) =
    let default = function
      | None -> true
      | Some c -> Matrix.Ansi.Color.intent c = Default
    in
    default st.fg && default st.bg
    && Matrix.Ansi.Attr.is_empty st.attrs
    && st.link = None

  let attr_name : Matrix.Ansi.Attr.flag -> string = function
    | Bold -> "bold"
    | Dim -> "dim"
    | Italic -> "italic"
    | Underline -> "underline"
    | Double_underline -> "double_underline"
    | Blink -> "blink"
    | Inverse -> "inverse"
    | Hidden -> "hidden"
    | Strikethrough -> "strikethrough"
    | Overline -> "overline"
    | Framed -> "framed"
    | Encircled -> "encircled"

  let describe (st : Matrix.Ansi.Style.t) =
    let color name = function
      | Some c when Matrix.Ansi.Color.intent c <> Default ->
          [ Format.asprintf "%s=%a" name Matrix.Ansi.Color.pp c ]
      | _ -> []
    in
    String.concat " "
      (color "fg" st.fg @ color "bg" st.bg
      @ List.map attr_name (Matrix.Ansi.Attr.to_list st.attrs)
      @ match st.link with Some l -> [ "link=" ^ l ] | None -> [])

  (* Keys name styles in the order they first appear: letters, digits, the
     other printable ASCII characters but [|], then the letters from U+00C0,
     each a cell wide. *)
  let ascii_keys =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`{}~"

  let key_of n =
    if n < String.length ascii_keys then String.make 1 ascii_keys.[n]
    else
      let b = Buffer.create 2 in
      Buffer.add_utf_8_uchar b
        (Uchar.of_int (0xC0 + n - String.length ascii_keys));
      Buffer.contents b

  let styled s =
    let keys = Hashtbl.create 8 and order = ref [] in
    let key st =
      match Hashtbl.find_opt keys st with
      | Some k -> k
      | None ->
          let k = key_of (Hashtbl.length keys) in
          Hashtbl.add keys st k;
          order := (k, st) :: !order;
          k
    in
    let b = Buffer.create (2 * rows s * (cols s + 5)) in
    for y = 0 to rows s - 1 do
      Buffer.add_string b (numbered s y);
      Buffer.add_string b (row s y);
      Buffer.add_char b '\n';
      let line = Buffer.create (cols s) and styled = ref false in
      for x = 0 to cols s - 1 do
        let idx = Grid.idx s.grid ~x ~y in
        let start, _ = Grid.span s.grid idx in
        let st = Grid.get_style s.grid start in
        if is_default st then Buffer.add_char line ' '
        else (
          styled := true;
          Buffer.add_string line (key st))
      done;
      if !styled then (
        Buffer.add_string b (blank_prefix s);
        Buffer.add_string b (trim (Buffer.contents line));
        Buffer.add_char b '\n')
    done;
    Buffer.add_string b (cursor_line s.cursor);
    Buffer.add_char b '\n';
    List.iter
      (fun (k, st) ->
        Buffer.add_string b (Printf.sprintf "%s %s\n" k (describe st)))
      (List.rev !order);
    Buffer.contents b

  (* Vim's classes of a cell's difference, the first that holds: another
     character, another width, another foreground, another background,
     other attributes. *)
  let difference a b idx =
    let ga = a.grid and gb = b.grid in
    if Grid.cells_equal ga idx gb idx then ' '
    else if Grid.get_text ga idx <> Grid.get_text gb idx then 'X'
    else if Grid.cell_width ga idx <> Grid.cell_width gb idx then 'w'
    else if
      not (Matrix.Ansi.Color.equal (Grid.get_fg ga idx) (Grid.get_fg gb idx))
    then 'f'
    else if
      not (Matrix.Ansi.Color.equal (Grid.get_bg ga idx) (Grid.get_bg gb idx))
    then 'b'
    else 'a'

  let diff a b =
    if rows a <> rows b || cols a <> cols b then
      Some
        (Printf.sprintf "size %dx%d, then %dx%d\n" (cols a) (rows a) (cols b)
           (rows b))
    else
      let out = Buffer.create 256 in
      for y = 0 to rows b - 1 do
        let marks = Bytes.make (cols b) ' ' in
        for x = 0 to cols b - 1 do
          Bytes.set marks x (difference a b (Grid.idx b.grid ~x ~y))
        done;
        let marks = trim (Bytes.to_string marks) in
        if marks <> "" then (
          Buffer.add_string out (numbered b y);
          Buffer.add_string out (row b y);
          Buffer.add_char out '\n';
          Buffer.add_string out (blank_prefix b);
          Buffer.add_string out marks;
          Buffer.add_char out '\n')
      done;
      if a.cursor <> b.cursor then
        Buffer.add_string out
          (Printf.sprintf "%s, then %s\n" (cursor_line a.cursor)
             (cursor_line b.cursor));
      if Buffer.length out = 0 then None else Some (Buffer.contents out)
end

(* Terminals *)

module Terminal = struct
  type t = {
    profile : Profile.t;
    vte : Vte.t;
    output : Buffer.t;
    mutable raw : bool;
  }

  let create ?(profile = Profile.vte) ~cols ~rows () =
    let vte =
      Vte.create ~identity:profile.identity ~extensions:profile.extensions
        ~width_method:profile.width_method ~palette:profile.palette ~rows ~cols
        ()
    in
    ignore (Vte.set_color_scheme vte profile.color_scheme : Reply.t list);
    { profile; vte; output = Buffer.create 4096; raw = false }

  let profile t = t.profile
  let vte t = t.vte

  (* Applies what the program wrote, and is the terminal's answer. *)
  let apply t b off len =
    Buffer.add_subbytes t.output b off len;
    let replies = Vte.feed t.vte b off len in
    if t.profile.answers then Reply.encode replies else ""

  (* The user's side *)

  let keystroke t k = Keystroke.encode (Vte.keyboard t.vte) k
  let release t r = Keystroke.encode_release (Vte.keyboard t.vte) r

  let types_nothing (m : Keystroke.mods) =
    m.alt || m.ctrl || m.super || m.hyper || m.meta

  let utf_8 u =
    let b = Buffer.create 4 in
    Buffer.add_utf_8_uchar b u;
    Buffer.contents b

  let key t ?(mods = Keystroke.no_mods) key =
    let text =
      match key with
      | Keystroke.Char u when not (types_nothing mods) -> utf_8 u
      | _ -> ""
    in
    keystroke t { key; mods; text; base = None; keypad = false; repeat = false }

  let text t s =
    let b = Buffer.create (String.length s) in
    let rec go i =
      if i < String.length s then (
        let d = String.get_utf_8_uchar s i in
        if not (Uchar.utf_decode_is_valid d) then
          invalid_arg "Matrix_test.Terminal.text: not UTF-8";
        Buffer.add_string b (key t (Char (Uchar.utf_decode_uchar d)));
        go (i + Uchar.utf_decode_length d))
    in
    go 0;
    Buffer.contents b

  let paste t s =
    Decoder.encode_paste ~bracketed:(Vte.modes t.vte).bracketed_paste s

  let mouse t m =
    match Vte.mouse t.vte with None -> "" | Some e -> Mouse_report.encode e m

  let focus t focused =
    if (Vte.modes t.vte).focus_reports then Decoder.encode_focus focused else ""

  let color_scheme t scheme = Reply.encode (Vte.set_color_scheme t.vte scheme)

  (* A resize, and the report the terminal sends of it. *)
  let resize t ~cols ~rows = Reply.encode (Vte.resize t.vte ~rows ~cols)

  (* Reading *)

  let screen t = Screen.of_vte t.vte

  let scrollback t =
    let n = Vte.scrollback_size t.vte in
    List.init n (fun i -> Vte.text t.vte (i - n))

  let title t = Vte.title t.vte
  let modes t = Vte.modes t.vte
  let raw t = t.raw
  let requests t = Vte.requests t.vte
  let output t = Buffer.contents t.output
end

(* Sessions *)

type program = {
  step : now:int -> [ `Stepped | `Idle | `Stopped ];
  deadline : unit -> int option;
  quiesce : unit -> unit;
}

type t = {
  term : Terminal.t;
  mutable clock : int;
  (* What the terminal sent and the run has not read, one write each, and
     how much of the first the run has read. *)
  writes : string Queue.t;
  mutable read_off : int;
  mutable resized : bool;
  jobs : (unit -> unit) Queue.t;
  mutable program : program option;
  mutable ended : bool;
  mutable frames : int;
  mutable steps : int;
  mutable device : Matrix.device option;
}

let send s bytes = if bytes <> "" then Queue.push bytes s.writes

(* An output that is not a terminal has no window: the size is reported in
   band, as the transport of a served session writes it. *)
let report_size s =
  let vte = Terminal.vte s.term in
  send s (Printf.sprintf "\027[48;%d;%d;0;0t" (Vte.rows vte) (Vte.cols vte))

let read s buf off len =
  match Queue.peek_opt s.writes with
  | None -> -1
  | Some w ->
      let n = min len (String.length w - s.read_off) in
      Bytes.blit_string w s.read_off buf off n;
      s.read_off <- s.read_off + n;
      if s.read_off = String.length w then (
        ignore (Queue.pop s.writes : string);
        s.read_off <- 0);
      n

let make_device s : Matrix.device =
  let profile = Terminal.profile s.term in
  let vte = Terminal.vte s.term in
  {
    read_clock = (fun () -> s.clock);
    wait =
      (fun ~until ->
        if not (Queue.is_empty s.writes) then true
        else
          match until with
          | Some u ->
              if u > s.clock then s.clock <- u;
              false
          | None ->
              invalid_arg
                "Matrix_test: the run waits for input; take its steps with \
                 Matrix_test.attach");
    read = read s;
    write = (fun b off len -> send s (Terminal.apply s.term b off len));
    window =
      (fun () ->
        if profile.tty then Some (Vte.cols vte, Vte.rows vte) else None);
    resized =
      (fun () ->
        let r = s.resized in
        s.resized <- false;
        r);
    set_raw = (fun raw -> s.term.raw <- raw);
    flush =
      (fun () ->
        Queue.clear s.writes;
        s.read_off <- 0);
    tty = profile.tty;
    env =
      Matrix.Terminal.Caps.env
        (fun name -> List.assoc_opt name profile.env)
        ~win32:false;
    frame = (fun () -> s.frames <- s.frames + 1);
  }

let create ?(now = 0) term =
  let s =
    {
      term;
      clock = now;
      writes = Queue.create ();
      read_off = 0;
      resized = false;
      jobs = Queue.create ();
      program = None;
      ended = false;
      frames = 0;
      steps = 0;
      device = None;
    }
  in
  s.device <- Some (make_device s);
  if not (Terminal.profile term).tty then report_size s;
  s

let terminal s = s.term
let config s c = Matrix.on_device (Option.get s.device) c
let perform s job = Queue.push job s.jobs

let run_jobs s =
  while not (Queue.is_empty s.jobs) do
    (Queue.pop s.jobs) ()
  done

let now s = s.clock
let frames s = s.frames
let steps s = s.steps
let ended s = s.ended

let program s fn =
  match s.program with
  | None -> invalid_arg ("Matrix_test." ^ fn ^ ": no program is attached")
  | Some p -> p

let running s fn =
  let p = program s fn in
  if s.ended then invalid_arg ("Matrix_test." ^ fn ^ ": the run has ended");
  p

(* Steps at the session's time until the program has nothing to do, then
   lets its executor finish, and goes on while that gave it something. *)
let settle_program s p =
  let rec go ~quiesced =
    if not s.ended then
      match p.step ~now:s.clock with
      | `Stepped ->
          s.steps <- s.steps + 1;
          go ~quiesced:false
      | `Stopped -> s.ended <- true
      | `Idle ->
          if not quiesced then (
            p.quiesce ();
            go ~quiesced:true)
  in
  go ~quiesced:false

let settle s = settle_program s (running s "settle")

let attach ?quiesce s ~step ~deadline =
  if s.program <> None then
    invalid_arg "Matrix_test.attach: a program is attached";
  let quiesce =
    match quiesce with Some q -> q | None -> fun () -> run_jobs s
  in
  let p = { step; deadline; quiesce } in
  s.program <- Some p;
  settle_program s p

let deadline s = (program s "deadline").deadline ()

let input s bytes =
  let p = running s "input" in
  send s bytes;
  settle_program s p

let resize s ~cols ~rows =
  let p = running s "resize" in
  let report = Terminal.resize s.term ~cols ~rows in
  if (Terminal.profile s.term).tty then (
    s.resized <- true;
    send s report)
  else report_size s;
  settle_program s p

let advance s ns =
  if ns < 0 then invalid_arg "Matrix_test.advance: negative time";
  let p = running s "advance" in
  let target = s.clock + ns in
  settle_program s p;
  let rec go () =
    if not s.ended then
      match p.deadline () with
      | Some d when d <= target ->
          s.clock <- max s.clock d;
          settle_program s p;
          go ()
      | _ ->
          s.clock <- max s.clock target;
          settle_program s p
  in
  go ()

let set_time s time =
  if time < s.clock then
    invalid_arg "Matrix_test.set_time: the time is earlier than the clock's";
  s.clock <- time

(* Programs in another process *)

module Child = struct
  type t = {
    term : Terminal.t;
    pty : Pty.t;
    buf : Bytes.t;
    mutable over : bool;
  }

  let write_all t s =
    let rec go off =
      if off < String.length s then
        match Pty.write_string t.pty s off (String.length s - off) with
        | n -> go (off + n)
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> go off
    in
    try go 0 with Unix.Unix_error ((Unix.EIO | Unix.EPIPE), _, _) -> ()

  let with_spawn term ?(env = []) ?cwd ~prog ~args f =
    let vte = Terminal.vte term in
    let env =
      Array.of_list
        (List.map
           (fun (k, v) -> k ^ "=" ^ v)
           ((Terminal.profile term).env @ env))
    in
    let winsize =
      { Pty.rows = Vte.rows vte; cols = Vte.cols vte; xpixel = 0; ypixel = 0 }
    in
    Pty.with_spawn ~env ?cwd ~winsize ~prog ~args (fun pty ->
        f { term; pty; buf = Bytes.create 65536; over = false })

  let send t bytes = write_all t bytes

  let resize t ~cols ~rows =
    let report = Terminal.resize t.term ~cols ~rows in
    Pty.resize t.pty ~rows ~cols;
    write_all t report

  let esu = "\027[?2026l"

  let rec find s sub from =
    let n = String.length sub in
    if from + n > String.length s then None
    else if String.sub s from n = sub then Some from
    else find s sub (from + 1)

  (* Applies [len] bytes of output from [off], answering as the terminal
     does, and checks [ready] at each point where no synchronized update is
     open: after each end of one, and at the end. *)
  let apply t ~ready off len =
    let vte = Terminal.vte t.term in
    let data = Bytes.sub_string t.buf off len in
    let found = ref false in
    let feed from upto =
      write_all t (Terminal.apply t.term t.buf (off + from) (upto - from));
      if (not !found) && not (Vte.modes vte).synchronized_output then
        found := ready t.term
    in
    let rec go from =
      match find data esu from with
      | Some i ->
          feed from (i + String.length esu);
          go (i + String.length esu)
      | None -> if from < len then feed from len
    in
    go 0;
    !found

  (* Reads output for at most [timeout] seconds; [false] at the end of the
     output. *)
  let read_some t ~ready ~timeout =
    let fd = Pty.file_descr t.pty in
    match Unix.select [ fd ] [] [] timeout with
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> `Nothing
    | [], _, _ -> `Nothing
    | _ -> (
        match Pty.read t.pty t.buf 0 (Bytes.length t.buf) with
        | 0 -> `End
        | n -> if apply t ~ready 0 n then `Ready else `Read
        | exception Unix.Unix_error (Unix.EIO, _, _) -> `End
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> `Nothing)

  let fail_with t what =
    failwith
      (Printf.sprintf "Matrix_test.Child: %s; the screen:\n%s" what
         (Screen.text (Terminal.screen t.term)))

  let wait t ~within ready =
    let vte = Terminal.vte t.term in
    let deadline = Unix.gettimeofday () +. within in
    let rec go () =
      let left = deadline -. Unix.gettimeofday () in
      if left <= 0. then
        fail_with t (Printf.sprintf "not ready within %gs" within)
      else if t.over then fail_with t "the child's output ended"
      else
        match read_some t ~ready ~timeout:left with
        | `Ready -> ()
        | `Read | `Nothing -> go ()
        | `End ->
            t.over <- true;
            go ()
    in
    if not ((not (Vte.modes vte).synchronized_output) && ready t.term) then
      go ()

  let exit t ~within =
    let deadline = Unix.gettimeofday () +. within in
    let rec drain () =
      let left = deadline -. Unix.gettimeofday () in
      if left <= 0. then
        fail_with t (Printf.sprintf "no exit within %gs" within)
      else if not t.over then
        match read_some t ~ready:(fun _ -> false) ~timeout:left with
        | `End -> t.over <- true
        | _ -> drain ()
    in
    drain ();
    Pty.wait t.pty
end
