type t = {
  input : Unix.file_descr;
  output : Unix.file_descr;
  original_termios : Unix.terminal_io option;
  mutable saved_termios : Unix.terminal_io option;
  read_buffer : bytes;
  is_tty : bool;
}

type mode = [ `Raw | `Cooked | `Custom of Unix.terminal_io -> Unix.terminal_io ]

external get_size : int -> int * int = "terminal_get_size"

let is_tty fd = try Unix.isatty fd with Unix.Unix_error _ -> false

let make_raw termios =
  let open Unix in
  {
    termios with
    (* Disable echo and canonical mode *)
    c_echo = false;
    c_icanon = false;
    c_vmin = 1;
    c_vtime = 0;
    (* Disable signals *)
    c_isig = false;
    (* Disable special character processing *)
    c_ixon = false;
    c_icrnl = false;
    c_opost = false;
  }

let create ?(tty = true) input output =
  let is_tty = tty && is_tty input && is_tty output in
  let original_termios =
    if is_tty then
      try Some (Unix.tcgetattr input) with Unix.Unix_error _ -> None
    else None
  in
  let t =
    {
      input;
      output;
      original_termios;
      saved_termios = None;
      read_buffer = Bytes.create 4096;
      is_tty;
    }
  in
  (* Set raw mode by default for TTY *)
  (if is_tty then
     let termios = Unix.tcgetattr t.input in
     Unix.tcsetattr t.input Unix.TCSANOW (make_raw termios));
  t

let set_mode t mode =
  if not t.is_tty then ()
  else
    match mode with
    | `Raw ->
        let termios = Unix.tcgetattr t.input in
        Unix.tcsetattr t.input Unix.TCSANOW (make_raw termios)
    | `Cooked -> (
        match t.original_termios with
        | Some termios -> Unix.tcsetattr t.input Unix.TCSANOW termios
        | None -> ())
    | `Custom f ->
        let termios = Unix.tcgetattr t.input in
        Unix.tcsetattr t.input Unix.TCSANOW (f termios)

let size t = get_size (Obj.magic t.output : int)
let input_fd t = t.input
let output_fd t = t.output

let write t bytes offset length =
  let rec write_all offset remaining =
    if remaining > 0 then
      let n = Unix.write t.output bytes offset remaining in
      write_all (offset + n) (remaining - n)
  in
  write_all offset length

let write_escape t seq = write t (Bytes.of_string seq) 0 (String.length seq)

let enable_alternate_screen t =
  if t.is_tty then write_escape t Ansi.alternate_screen_on

let disable_alternate_screen t =
  if t.is_tty then write_escape t Ansi.alternate_screen_off

let enable_mouse t = if t.is_tty then write_escape t Ansi.mouse_on
let disable_mouse t = if t.is_tty then write_escape t Ansi.mouse_off

let enable_bracketed_paste t =
  if t.is_tty then write_escape t Ansi.bracketed_paste_on

let disable_bracketed_paste t =
  if t.is_tty then write_escape t Ansi.bracketed_paste_off

let show_cursor t = if t.is_tty then write_escape t Ansi.cursor_show
let hide_cursor t = if t.is_tty then write_escape t Ansi.cursor_hide

let release t =
  if t.is_tty then (
    (* Restore visual state before restoring termios *)
    disable_alternate_screen t;
    show_cursor t;
    disable_mouse t;
    disable_bracketed_paste t;
    (* Restore original termios *)
    match t.original_termios with
    | Some termios -> Unix.tcsetattr t.input Unix.TCSANOW termios
    | None -> ())

let flush _ = ()

let read_input t ~timeout =
  let ready_fds =
    match timeout with
    | None -> Unix.select [ t.input ] [] [] (-1.0)
    | Some timeout_val -> Unix.select [ t.input ] [] [] timeout_val
  in
  match ready_fds with
  | [], _, _ -> `Timeout
  | _ -> (
      match Unix.read t.input t.read_buffer 0 4096 with
      | 0 -> `Eof
      | n -> `Input (t.read_buffer, n))

type sigwinch_handler = int * int -> unit

let sigwinch_handler = ref None

let sigwinch_callback _ =
  match !sigwinch_handler with
  | None -> ()
  | Some f ->
      let fd = Unix.stdout in
      let size = get_size (Obj.magic fd : int) in
      f size

let set_sigwinch_handler handler =
  sigwinch_handler := handler;
  match handler with
  | None -> ( try Sys.set_signal 28 Sys.Signal_default with _ -> ())
  | Some _ -> (
      try Sys.set_signal 28 (Sys.Signal_handle sigwinch_callback) with _ -> ())

let save_state t =
  if t.is_tty then
    t.saved_termios <-
      (try Some (Unix.tcgetattr t.input) with Unix.Unix_error _ -> None)

let restore_state t =
  if t.is_tty then
    match t.saved_termios with
    | Some termios -> Unix.tcsetattr t.input Unix.TCSANOW termios
    | None -> ()

(* Testing support *)
let create_from_strings input =
  (* Create pipes for testing *)
  let in_read, in_write = Unix.pipe () in
  let out_read, out_write = Unix.pipe () in

  (* Write input string to the input pipe *)
  let input_bytes = Bytes.of_string input in
  ignore (Unix.write in_write input_bytes 0 (Bytes.length input_bytes));
  Unix.close in_write;

  (* Create terminal with TTY disabled *)
  let term = create ~tty:false in_read out_write in

  (* Function to read output *)
  let get_output () =
    let buffer = Buffer.create 1024 in
    let bytes = Bytes.create 1024 in
    let rec read_all () =
      match Unix.select [ out_read ] [] [] 0.0 with
      | [], _, _ -> Buffer.contents buffer
      | _ -> (
          match Unix.read out_read bytes 0 1024 with
          | 0 -> Buffer.contents buffer
          | n ->
              Buffer.add_subbytes buffer bytes 0 n;
              read_all ())
    in
    let result = read_all () in
    Unix.close out_read;
    Unix.close in_read;
    result
  in

  (term, get_output)
