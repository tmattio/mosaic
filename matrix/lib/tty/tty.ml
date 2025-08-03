open Unix

(* SIGWINCH signal number - window size change *)
let sigwinch = 28

type t = {
  input : file_descr;
  output : file_descr;
  original_termios : terminal_io option;
  mutable saved_termios : terminal_io list;
  input_is_tty : bool;
  output_is_tty : bool;
  mutable dark_background : bool option;
  mutable truecolor : bool option;
}

type mode = [ `Raw | `Cooked | `Custom of terminal_io -> terminal_io ]

type mouse_mode =
  [ `None | `Normal | `Button | `Any | `SgrNormal | `SgrButton | `SgrAny ]

type feature =
  [ `AlternateScreen
  | `Mouse
  | `Truecolor
  | `Kitty
  | `BracketedPaste
  | `FocusReporting ]

exception Terminal_error of string

external get_size : Unix.file_descr -> int * int = "terminal_get_size"
external enable_vt : Unix.file_descr -> unit = "terminal_enable_vt"

let is_tty fd = try isatty fd with Unix_error _ -> false
let tcgetattr_opt fd = try Some (tcgetattr fd) with Unix_error _ -> None

let make_raw termios =
  {
    termios with
    c_echo = false;
    c_icanon = false;
    c_vmin = 1;
    c_vtime = 0;
    c_ixon = false;
    c_icrnl = false;
  }

let create ?(tty = true) input output =
  let input_is_tty = tty && is_tty input in
  let output_is_tty = tty && is_tty output in
  let original_termios =
    if input_is_tty then try Some (tcgetattr input) with Unix_error _ -> None
    else None
  in
  let t =
    {
      input;
      output;
      original_termios;
      saved_termios = [];
      input_is_tty;
      output_is_tty;
      dark_background = None;
      truecolor = None;
    }
  in
  (if output_is_tty then
     try enable_vt output with _ -> () (* Enable VT on Windows *));
  t

let set_non_blocking t enabled =
  try
    if enabled then
      Unix.set_nonblock
        t.input (* keep output blocking to avoid EAGAIN on write *)
    else Unix.clear_nonblock t.input (* leave output as-is *)
  with Unix_error (e, _, _) -> raise (Terminal_error (error_message e))

let set_mode t mode =
  if not t.input_is_tty then ()
  else
    try
      match mode with
      | `Raw ->
          let termios = tcgetattr t.input in
          tcsetattr t.input TCSANOW (make_raw termios);
          (* Make I/O non-blocking by default in raw mode *)
          set_non_blocking t true
      | `Cooked -> (
          match t.original_termios with
          | Some termios ->
              tcsetattr t.input TCSANOW termios;
              set_non_blocking t false
          | None -> ())
      | `Custom f ->
          let termios = tcgetattr t.input in
          tcsetattr t.input TCSANOW (f termios)
    with Unix_error (e, _, _) -> raise (Terminal_error (error_message e))

let size t = try get_size t.output with _ -> (80, 24)
let input_fd t = t.input
let output_fd t = t.output

let write t bytes offset length =
  let rec write_all ofs rem =
    if rem > 0 then
      match Unix.write t.output bytes ofs rem with
      | n -> write_all (ofs + n) (rem - n)
      | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EINTR), _, _) ->
          ignore (Unix.select [] [ t.output ] [] (-1.0));
          write_all ofs rem
  in
  try write_all offset length
  with Unix_error (e, _, _) -> raise (Terminal_error (error_message e))

let write_string t str =
  let bytes = Bytes.of_string str in
  (* Assumes UTF-8 *)
  write t bytes 0 (Bytes.length bytes)

let write_escape t seq = write_string t seq

let read t bytes offset length =
  try Unix.read t.input bytes offset length
  with Unix_error (e, _, _) -> raise (Terminal_error (error_message e))

let wait_for_input t timeout =
  try
    let r, _, _ = select [ t.input ] [] [] timeout in
    r <> []
  with Unix_error _ -> false

let enable_alternate_screen t =
  if t.output_is_tty then write_escape t Ansi.alternate_screen_on

let disable_alternate_screen t =
  if t.output_is_tty then write_escape t Ansi.alternate_screen_off

let enable_mouse_sgr t = if t.output_is_tty then write_escape t "\x1b[?1006h"
let disable_mouse_sgr t = if t.output_is_tty then write_escape t "\x1b[?1006l"

let set_mouse_mode t mode =
  if t.output_is_tty then
    match mode with
    | `None ->
        write_escape t Ansi.mouse_off;
        disable_mouse_sgr t
    | `Normal -> write_escape t "\x1b[?1000h"
    | `Button -> write_escape t "\x1b[?1002h"
    | `Any -> write_escape t "\x1b[?1003h"
    | `SgrNormal ->
        enable_mouse_sgr t;
        write_escape t "\x1b[?1000h"
    | `SgrButton ->
        enable_mouse_sgr t;
        write_escape t "\x1b[?1002h"
    | `SgrAny ->
        enable_mouse_sgr t;
        write_escape t "\x1b[?1003h"

let enable_focus_reporting t =
  if t.output_is_tty then write_escape t "\x1b[?1004h"

let disable_focus_reporting t =
  if t.output_is_tty then write_escape t "\x1b[?1004l"

let enable_bracketed_paste t =
  if t.output_is_tty then write_escape t Ansi.bracketed_paste_on

let disable_bracketed_paste t =
  if t.output_is_tty then write_escape t Ansi.bracketed_paste_off

let enable_kitty_keyboard t =
  if t.output_is_tty then write_escape t Ansi.kitty_keyboard_on

let disable_kitty_keyboard t =
  if t.output_is_tty then write_escape t Ansi.kitty_keyboard_off

let show_cursor t = if t.output_is_tty then write_escape t Ansi.cursor_show
let hide_cursor t = if t.output_is_tty then write_escape t Ansi.cursor_hide
let clear_screen t = if t.output_is_tty then write_escape t "\x1b[2J"

let move_cursor t row col =
  if t.output_is_tty then write_escape t (Printf.sprintf "\x1b[%d;%dH" row col)

let set_title t title =
  if t.output_is_tty then write_escape t (Printf.sprintf "\x1b]0;%s\x07" title)

let bell t = if t.output_is_tty then write_escape t "\x07"

let flush t =
  try
    ignore (Unix.write t.output (Bytes.create 0) 0 0);
    fsync t.output
  with _ -> ()

let sigwinch_handlers : (unit -> unit) list ref = ref []
let sigwinch_callback _ = List.iter (fun f -> f ()) !sigwinch_handlers

let add_sigwinch_handler f =
  sigwinch_handlers := f :: !sigwinch_handlers;
  Sys.set_signal sigwinch (Sys.Signal_handle sigwinch_callback)

let remove_sigwinch_handler f =
  sigwinch_handlers := List.filter (( != ) f) !sigwinch_handlers;
  if !sigwinch_handlers = [] then Sys.set_signal sigwinch Sys.Signal_default

(* Keep track of sigwinch callbacks associated with terminals *)
let terminal_sigwinch_callbacks : (t * (unit -> unit)) list ref = ref []

let set_resize_handler t handler =
  let callback () =
    let s = size t in
    handler s
  in
  terminal_sigwinch_callbacks := (t, callback) :: !terminal_sigwinch_callbacks;
  add_sigwinch_handler callback

let remove_resize_handlers t =
  (* Find and remove all callbacks associated with this terminal *)
  let callbacks_to_remove =
    List.filter_map
      (fun (term, cb) -> if term == t then Some cb else None)
      !terminal_sigwinch_callbacks
  in
  List.iter remove_sigwinch_handler callbacks_to_remove;
  terminal_sigwinch_callbacks :=
    List.filter (fun (term, _) -> term != t) !terminal_sigwinch_callbacks

let release t =
  if t.output_is_tty then (
    disable_alternate_screen t;
    show_cursor t;
    set_mouse_mode t `None;
    disable_bracketed_paste t;
    disable_kitty_keyboard t;
    disable_focus_reporting t;
    flush t;
    (* Flush after all writes *)
    remove_resize_handlers t);
  (* Clean up resize handlers *)
  if t.input_is_tty then
    match t.original_termios with
    | Some termios -> (
        try tcsetattr t.input TCSANOW termios with Unix_error _ -> ())
    | None -> ()

let with_terminal ?tty input output f =
  let t = create ?tty input output in
  Fun.protect ~finally:(fun () -> release t) (fun () -> f t)

let save_state t =
  if t.input_is_tty then
    match tcgetattr_opt t.input with
    | Some tio -> t.saved_termios <- tio :: t.saved_termios
    | None -> ()

let restore_state t =
  if t.input_is_tty then
    match t.saved_termios with
    | termios :: rest -> (
        t.saved_termios <- rest;
        try tcsetattr t.input TCSANOW termios with Unix_error _ -> ())
    | [] -> ()

(* Background detection *)

let query_bg_sequence = "\x1b]11;?\x07"

let parse_osc_response response =
  try
    let rgb_start = String.index response ':' + 1 in
    let rgb_end = String.index_from response rgb_start '\x07' in
    let rgb_part = String.sub response rgb_start (rgb_end - rgb_start) in
    match String.split_on_char '/' rgb_part with
    | parts when List.length parts >= 3 ->
        let to_8bit hex =
          try
            let full = int_of_string ("0x" ^ hex) in
            let max_val = (1 lsl (String.length hex * 4)) - 1 in
            full * 255 / max_val
          with _ -> 0
        in
        Some
          ( to_8bit (List.nth parts 0),
            to_8bit (List.nth parts 1),
            to_8bit (List.nth parts 2) )
    | _ -> None
  with _ -> None

let calculate_luminance r g b =
  let r' = float r /. 255.0 in
  let g' = float g /. 255.0 in
  let b' = float b /. 255.0 in
  (0.2126 *. r') +. (0.7152 *. g') +. (0.0722 *. b')

let query_terminal_for_bg t =
  if not (t.input_is_tty && t.output_is_tty) then None
  else
    try
      let original_termios = tcgetattr t.input in
      let raw_termios =
        {
          original_termios with
          c_icanon = false;
          c_echo = false;
          c_vmin = 1;
          c_vtime = 0;
        }
      in
      Fun.protect
        ~finally:(fun () ->
          try tcsetattr t.input TCSANOW original_termios
          with Unix_error _ -> ())
        (fun () ->
          (try tcsetattr t.input TCSADRAIN raw_termios with Unix_error _ -> ());
          write_string t query_bg_sequence;
          let buf = Bytes.create 1024 in
          let rec read_response acc =
            if Unix.select [ t.input ] [] [] 0.1 <> ([], [], []) then
              let n = Unix.read t.input buf 0 1024 in
              if n = 0 then acc
              else
                let new_acc = acc ^ Bytes.sub_string buf 0 n in
                if String.contains new_acc '\x07' then new_acc
                else read_response new_acc
            else acc
          in
          let response = read_response "" in
          if response = "" then None
          else
            match parse_osc_response response with
            | Some (r, g, b) -> Some (calculate_luminance r g b < 0.5)
            | None -> None)
    with _ -> None

let set_dark_background t ~dark = t.dark_background <- Some dark

let has_dark_background t =
  match t.dark_background with
  | Some dark -> dark
  | None ->
      let is_dark =
        match Sys.getenv_opt "COLORFGBG" with
        | Some colorfgbg -> (
            match String.split_on_char ';' colorfgbg with
            | _ :: bg :: _ -> (
                try int_of_string (String.trim bg) < 8 with _ -> true)
            | _ -> true)
        | None -> (
            match query_terminal_for_bg t with
            | Some d -> d
            | None -> (
                match Sys.getenv_opt "TERM" with
                | Some term ->
                    let len = String.length term in
                    not
                      (len >= 6
                      && (String.sub term (len - 6) 6 = "-light"
                         || String.sub term (len - 6) 6 = "_light"))
                | None -> true))
      in
      t.dark_background <- Some is_dark;
      is_dark

let has_truecolor_support t =
  match t.truecolor with
  | Some sup -> sup
  | None ->
      let sup =
        match Sys.getenv_opt "COLORTERM" with
        | Some "truecolor" -> true
        | _ -> (
            match query_terminal_for_bg t with
            | Some _ -> true
            | None -> (
                match Sys.getenv_opt "TERM" with
                | Some term ->
                    let term_lower = String.lowercase_ascii term in
                    let contains_substring str sub =
                      let sub_len = String.length sub in
                      let str_len = String.length str in
                      let rec check pos =
                        if pos + sub_len > str_len then false
                        else if String.sub str pos sub_len = sub then true
                        else check (pos + 1)
                      in
                      check 0
                    in
                    contains_substring term_lower "256"
                    || contains_substring term_lower "truecolor"
                    || contains_substring term_lower "24bit"
                | _ -> false))
      in
      t.truecolor <- Some sup;
      sup

let supports_feature t feature =
  if not t.output_is_tty then false
  else
    match feature with
    | `AlternateScreen -> true (* Most modern terminals support this *)
    | `Mouse -> true (* Basic mouse support is widespread *)
    | `Truecolor -> has_truecolor_support t
    | `Kitty -> (
        match Sys.getenv_opt "TERM_PROGRAM" with
        | Some "kitty" -> true
        | _ -> (
            match Sys.getenv_opt "TERM" with
            | Some term ->
                let contains_substring str sub =
                  let sub_len = String.length sub in
                  let str_len = String.length str in
                  let rec check pos =
                    if pos + sub_len > str_len then false
                    else if String.sub str pos sub_len = sub then true
                    else check (pos + 1)
                  in
                  check 0
                in
                contains_substring (String.lowercase_ascii term) "kitty"
            | None -> false))
    | `BracketedPaste -> true (* Most modern terminals support this *)
    | `FocusReporting -> true (* Widely supported in modern terminals *)

(* Testing support *)

let create_from_strings input_str =
  let in_read, in_write = pipe () in
  let out_read, out_write = pipe () in
  let input_bytes = Bytes.of_string input_str in
  ignore (Unix.write in_write input_bytes 0 (Bytes.length input_bytes));
  let term = create ~tty:false in_read out_write in
  let get_output () =
    let buffer = Buffer.create 1024 in
    let bytes = Bytes.create 1024 in
    let rec read_all () =
      if Unix.select [ out_read ] [] [] 0.0 <> ([], [], []) then
        let n = Unix.read out_read bytes 0 1024 in
        if n > 0 then (
          Buffer.add_subbytes buffer bytes 0 n;
          read_all ())
        else ()
    in
    read_all ();
    Buffer.contents buffer
  in
  let close_mock () =
    Unix.close in_write;
    Unix.close out_read;
    Unix.close in_read;
    Unix.close out_write
  in
  (term, get_output, close_mock)
