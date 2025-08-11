(** Eio-based wrapper for Matrix TTY module *)

open Eio

(* Use existential wrappers for the flows *)
type source = Source : _ Flow.source -> source
type sink = Sink : _ Flow.sink -> sink

type t = {
  matrix_tty : Tty.t;
  input : source;
  output : sink;
  output_buf : Buffer.t;  (* Buffer for coalescing writes *)
}

let create ~sw ~env ?tty () =
  (* Get Unix file descriptors - for now we use stdin/stdout directly
     since Eio doesn't expose the underlying FDs in a portable way.
     The key is that our reads/writes go through Eio flows. *)
  let input_fd = Unix.stdin in
  let output_fd = Unix.stdout in
  
  (* Create the underlying Matrix TTY *)
  let matrix_tty = Tty.create ?tty input_fd output_fd in
  
  (* Register cleanup with switch *)
  Switch.on_release sw (fun () -> Tty.release matrix_tty);
  
  {
    matrix_tty;
    input = Source env#stdin;
    output = Sink env#stdout;
    output_buf = Buffer.create 4096;
  }

let with_terminal ~sw ~env ?tty f =
  let t = create ~sw ~env ?tty () in
  Fun.protect (fun () -> f t) ~finally:(fun () -> Tty.release t.matrix_tty)

(** {1 Delegated Functions} *)

let release t = Tty.release t.matrix_tty
let save_state t = Tty.save_state t.matrix_tty
let restore_state t = Tty.restore_state t.matrix_tty
let set_mode t mode = Tty.set_mode t.matrix_tty mode
let size t = Tty.size t.matrix_tty
let input_fd t = Tty.input_fd t.matrix_tty
let output_fd t = Tty.output_fd t.matrix_tty

(** {1 Non-blocking IO Operations} *)

let flush_buffer t =
  let contents = Buffer.contents t.output_buf in
  if String.length contents > 0 then (
    Buffer.clear t.output_buf;
    (* Use Eio flow for non-blocking write *)
    let (Sink output) = t.output in
    Flow.write output [ Cstruct.of_string contents ]
  )

let write t buf ofs len =
  (* Write directly to the output flow for immediate effect *)
  let (Sink output) = t.output in
  Flow.write output [ Cstruct.of_bytes buf ~off:ofs ~len ]

let write_string t s =
  (* Write directly to the output flow *)
  let (Sink output) = t.output in
  Flow.write output [ Cstruct.of_string s ]

let flush t =
  (* Flush our buffer if there's anything in it, then flush the flow *)
  flush_buffer t;
  (* Eio flows auto-flush, but we keep this for compatibility *)
  ()

let read t buf ofs len =
  (* Use Eio flow for non-blocking read *)
  let (Source input) = t.input in
  let cs = Cstruct.create len in
  match Flow.single_read input cs with
  | n ->
      Cstruct.blit_to_bytes cs 0 buf ofs n;
      n
  | exception End_of_file -> 0

(** {1 Screen Management} *)

let enable_alternate_screen t = 
  write_string t "\0277\027[?1049h"

let disable_alternate_screen t = 
  write_string t "\027[?1049l\0278"

let clear_screen t =
  write_string t "\027[2J\027[H"

let set_title t title =
  write_string t (Printf.sprintf "\027]0;%s\007" title)

let bell t =
  write_string t "\007"

(** {1 Cursor Control} *)

let show_cursor t =
  write_string t "\027[?25h"

let hide_cursor t =
  write_string t "\027[?25l"

let move_cursor t row col =
  write_string t (Printf.sprintf "\027[%d;%dH" row col)

(** {1 Mouse Support} *)

let set_mouse_mode t mode =
  (* Write the appropriate escape sequences *)
  let seq = match mode with
    | `None -> "\027[?1000l\027[?1002l\027[?1003l\027[?1006l"
    | `Normal -> "\027[?1000h\027[?1002l\027[?1003l"
    | `Button -> "\027[?1000h\027[?1002h\027[?1003l"
    | `Any -> "\027[?1000h\027[?1002h\027[?1003h"
    | `SgrNormal -> "\027[?1000h\027[?1002l\027[?1003l\027[?1006h"
    | `SgrButton -> "\027[?1000h\027[?1002h\027[?1003l\027[?1006h"
    | `SgrAny -> "\027[?1000h\027[?1002h\027[?1003h\027[?1006h"
  in
  write_string t seq

let enable_mouse_sgr t =
  write_string t "\027[?1006h"

let disable_mouse_sgr t =
  write_string t "\027[?1006l"

(** {1 Advanced Features} *)

let enable_focus_reporting t =
  write_string t "\027[?1004h"

let disable_focus_reporting t =
  write_string t "\027[?1004l"

let enable_bracketed_paste t =
  write_string t "\027[?2004h"

let disable_bracketed_paste t =
  write_string t "\027[?2004l"

let enable_kitty_keyboard t =
  write_string t "\027[>1u"

let disable_kitty_keyboard t =
  write_string t "\027[<u"

(** {1 Window Management} *)

let set_resize_handler t handler =
  Tty.set_resize_handler t.matrix_tty handler

let remove_resize_handlers t =
  Tty.remove_resize_handlers t.matrix_tty

(** {1 Color and Feature Detection} *)

let set_dark_background t ~dark =
  Tty.set_dark_background t.matrix_tty ~dark

let has_dark_background t =
  Tty.has_dark_background t.matrix_tty

let has_truecolor_support t =
  Tty.has_truecolor_support t.matrix_tty

let supports_feature t feature =
  Tty.supports_feature t.matrix_tty feature

(** {1 Testing Support} *)

let create_from_strings ~sw input =
  (* Use Matrix TTY's test support *)
  let matrix_tty, get_output, close = Tty.create_from_strings input in
  
  (* Create a mock flow from the string for input *)
  let input_flow = Eio.Flow.string_source input in
  
  (* Create a buffer for output *)
  let output_buffer = Buffer.create 256 in
  let output_flow = Eio.Flow.buffer_sink output_buffer in
  
  (* Register cleanup with switch *)
  Switch.on_release sw (fun () -> close ());
  
  let t = {
    matrix_tty;
    input = Source input_flow;
    output = Sink output_flow;
    output_buf = Buffer.create 256;
  } in
  
  (t, get_output, close)

(** {1 Internal Access} *)

let get_matrix_tty t = t.matrix_tty