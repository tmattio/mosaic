type t = {
  input : Unix.file_descr;
  output : Unix.file_descr;
  original_termios : Unix.terminal_io option;
  mutable saved_termios : Unix.terminal_io option;
  is_tty : bool;
  mutable dark_background : bool option; (* Cached background detection result *)
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
    c_opost = true;
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
      is_tty;
      dark_background = None;
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

let enable_kitty_keyboard t =
  if t.is_tty then write_escape t Ansi.kitty_keyboard_on

let disable_kitty_keyboard t =
  if t.is_tty then write_escape t Ansi.kitty_keyboard_off

let show_cursor t = if t.is_tty then write_escape t Ansi.cursor_show
let hide_cursor t = if t.is_tty then write_escape t Ansi.cursor_hide

let flush t =
  (* Force any pending output to be written *)
  try
    ignore (Unix.write t.output (Bytes.create 0) 0 0);
    Unix.fsync t.output
  with _ -> ()

let release t =
  if t.is_tty then (
    (* Flush any pending output first *)
    flush t;
    (* Restore visual state before restoring termios *)
    disable_alternate_screen t;
    show_cursor t;
    disable_mouse t;
    disable_bracketed_paste t;
    (* Restore original termios *)
    match t.original_termios with
    | Some termios -> Unix.tcsetattr t.input Unix.TCSANOW termios
    | None -> ())

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

(* Terminal background detection *)

(* OSC sequence to query background color *)
let query_bg_sequence = "\x1b]11;?\x07"

(* Parse OSC response like "\x1b]11;rgb:de/de/de/de\x07" *)
let parse_osc_response response =
  try
    (* Look for the rgb: pattern *)
    let rgb_start = String.index response ':' + 1 in
    let rgb_end = String.index response '\x07' in
    let rgb_part = String.sub response rgb_start (rgb_end - rgb_start) in
    (* Parse hex values - terminal returns 16-bit values *)
    match String.split_on_char '/' rgb_part with
    | [ r_str; g_str; b_str ] | [ r_str; g_str; b_str; _ ] ->
        let r = int_of_string ("0x" ^ String.sub r_str 0 2) in
        let g = int_of_string ("0x" ^ String.sub g_str 0 2) in
        let b = int_of_string ("0x" ^ String.sub b_str 0 2) in
        Some (r, g, b)
    | _ -> None
  with _ -> None

(* Calculate luminance from RGB values (0-255) *)
let calculate_luminance r g b =
  let r' = float_of_int r /. 255.0 in
  let g' = float_of_int g /. 255.0 in
  let b' = float_of_int b /. 255.0 in
  (* ITU-R BT.709 luminance coefficients *)
  (0.2126 *. r') +. (0.7152 *. g') +. (0.0722 *. b')

(* Query terminal for background color via OSC *)
let query_terminal_for_bg t =
  if not t.is_tty then None
  else
    try
      (* Save current terminal state *)
      let original_termios = Unix.tcgetattr t.input in

      (* Set up raw mode for reading response *)
      let raw_termios =
        {
          original_termios with
          c_icanon = false;
          c_echo = false;
          c_vmin = 0;
          c_vtime = 1;
          (* 100ms timeout *)
        }
      in

      Fun.protect
        ~finally:(fun () ->
          Unix.tcsetattr t.input Unix.TCSANOW original_termios)
        (fun () ->
          Unix.tcsetattr t.input Unix.TCSADRAIN raw_termios;

          (* Send query *)
          let query_bytes = Bytes.of_string query_bg_sequence in
          ignore (Unix.write t.output query_bytes 0 (Bytes.length query_bytes));

          (* Try to read response with timeout *)
          let buf = Bytes.create 256 in
          let rec read_response acc total =
            match Unix.select [ t.input ] [] [] 0.1 with
            (* 100ms timeout *)
            | [], _, _ ->
                if total = 0 then None else Some (Bytes.sub_string acc 0 total)
            | _ -> (
                match Unix.read t.input buf 0 256 with
                | 0 -> Some (Bytes.sub_string acc 0 total)
                | n ->
                    Bytes.blit buf 0 acc total n;
                    if Bytes.contains acc '\x07' then
                      Some (Bytes.sub_string acc 0 (total + n))
                    else read_response acc (total + n))
          in

          match read_response (Bytes.create 1024) 0 with
          | None -> None
          | Some response -> (
              match parse_osc_response response with
              | Some (r, g, b) ->
                  let luminance = calculate_luminance r g b in
                  Some (luminance < 0.5)
              | None -> None))
    with _ -> None

let has_dark_background t =
  match t.dark_background with
  | Some dark -> dark (* Return cached result *)
  | None ->
      let is_dark =
        (* Method 1: Check COLORFGBG environment variable *)
        match Sys.getenv_opt "COLORFGBG" with
        | Some colorfgbg -> (
            (* Format is usually "foreground;background" like "15;0" or "7;0" *)
            match String.split_on_char ';' colorfgbg with
            | [ _; bg ] | [ _; bg; _ ] -> (
                try
                  let bg_num = int_of_string (String.trim bg) in
                  (* Background colors 0-7 are typically dark, 8+ are light *)
                  bg_num < 8
                with _ ->
                  (* Try OSC query as fallback *)
                  Option.value (query_terminal_for_bg t) ~default:true)
            | _ -> Option.value (query_terminal_for_bg t) ~default:true)
        | None -> (
            (* Method 2: Try OSC query *)
            match query_terminal_for_bg t with
            | Some is_dark -> is_dark
            | None -> (
                (* Method 3: Check TERM environment variable *)
                match Sys.getenv_opt "TERM" with
                | Some term ->
                    let len = String.length term in
                    if len >= 6 && String.sub term (len - 6) 6 = "-light" then
                      false
                    else if len >= 6 && String.sub term (len - 6) 6 = "_light"
                    then false
                    else true
                | None -> true (* Default to dark background *)))
      in
      t.dark_background <- Some is_dark;
      is_dark
