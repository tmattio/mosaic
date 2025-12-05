(* bench_ansi.ml *)

module A = Ansi
module S = Ansi.Style
module C = Ansi.Color
module Esc = Ansi.Escape

(* Helpers *)

let repeat chunk count =
  let len = String.length chunk in
  let buf = Buffer.create (len * count) in
  for _ = 1 to count do
    Buffer.add_string buf chunk
  done;
  Buffer.contents buf

(* Sample payloads *)

(* A realistic log line used across benchmarks *)
let plain_log_line =
  "[2024-01-01T00:00:00Z] INFO  http-server: Listening on \
   http://127.0.0.1:8080 (pid=1234, env=prod)"

let plain_log_block =
  (* Simulate a CI log chunk *)
  repeat (plain_log_line ^ "\n") 128

(* Predefined styles reused in all styled / segment benchmarks *)
let style_timestamp = S.make ~fg:C.bright_black ()
let style_info = S.make ~fg:C.bright_green ~bold:true ()
let style_debug = S.make ~fg:C.bright_blue ()
let style_context = S.make ~fg:C.cyan ()
let style_plain = S.default

(* Log line decomposed into styled segments (typical logging use‑case) *)
let log_segments : (S.t * string) list =
  [
    (style_timestamp, "[2024-01-01T00:00:00Z] ");
    (style_info, "INFO ");
    (style_plain, "http-server: Listening on ");
    (style_debug, "http://127.0.0.1:8080 ");
    (style_context, "(pid=1234, env=prod)");
  ]

let ansi_log_line = A.render log_segments
let ansi_log_block = repeat (ansi_log_line ^ "\n") 128

(* A synthetic "frame" of TUI output with cursor movement, RGB background, etc.
   This approximates what a full-screen TUI would emit for one frame. *)
let tui_frame =
  let rows = 24 in
  let buf = Buffer.create 16_384 in
  Buffer.add_string buf A.enter_alternate_screen;
  Buffer.add_string buf A.hide_cursor;
  for row = 1 to rows do
    (* Move to row start *)
    Buffer.add_string buf (A.cursor_position ~row ~col:1);

    (* Vary the background color a bit by row to keep SGRs non-trivial *)
    let r = row * 10 land 0xFF in
    let g = (255 - (row * 7)) land 0xFF in
    let b = (50 + (row * 3)) land 0xFF in
    Buffer.add_string buf (A.set_background ~r ~g ~b);

    (* Emit a status line / row payload *)
    Buffer.add_string buf "row ";
    Buffer.add_string buf (string_of_int row);
    Buffer.add_string buf " │ ";
    Buffer.add_string buf plain_log_line;
    Buffer.add_string buf A.reset;
    Buffer.add_char buf '\n'
  done;
  Buffer.add_string buf A.show_cursor;
  Buffer.add_string buf A.exit_alternate_screen;
  Buffer.contents buf

(* A dense "worst case" TUI line: alternating styles per cell. *)
let dense_tui_segments : (S.t * string) list =
  let green = S.make ~fg:C.green () in
  let yellow = S.make ~fg:C.yellow ~bold:true () in
  let rec loop i acc =
    if i = 80 then List.rev acc
    else
      let style = if i land 1 = 0 then green else yellow in
      let ch =
        (* cycle a‑z for a bit of variety *)
        Char.chr (Char.code 'a' + (i mod 26))
      in
      loop (i + 1) ((style, String.make 1 ch) :: acc)
  in
  loop 0 []

(* Shared writer buffer for Ansi.emit TUI benchmarks *)
let dense_writer_bytes = Bytes.create 8192

(* Bench group: styled output (producers) *)

let styled_inline =
  Ubench.create "inline_styled" (fun () ->
      (* Logging-style single-call styling, e.g. direct use from application code *)
      let s =
        A.styled ~reset:true ~fg:C.red ~bold:true
          "ERROR: failed to connect to upstream (status=502, retries=3)"
      in
      ignore (Sys.opaque_identity s))

let styled_segments =
  Ubench.create "render_log_segments" (fun () ->
      (* Renderer-style segmented output, e.g. logging framework or TUI header *)
      let out = A.render log_segments in
      ignore (Sys.opaque_identity out))

(* Bench group: stripping & parsing (consumers / CI log tools) *)

let strip_plain_block_bench =
  Ubench.create "strip_plain_block" (fun () ->
      (* Fast path: no escape sequences *)
      let s = A.strip plain_log_block in
      ignore (Sys.opaque_identity s))

let strip_ansi_block_bench =
  Ubench.create "strip_ansi_block" (fun () ->
      (* Heavy path: lots of SGR sequences, typical CI log chunk *)
      let s = A.strip ansi_log_block in
      ignore (Sys.opaque_identity s))

let parse_ansi_block_bench =
  Ubench.create "parse_ansi_block" (fun () ->
      (* Parsing a chunk of colored logs for later reformatting / filtering *)
      let tokens = A.parse ansi_log_block in
      ignore (Sys.opaque_identity tokens))

let parse_tui_frame_bench =
  Ubench.create "parse_tui_frame" (fun () ->
      (* Parsing a TUI frame: lots of CSI, SGR and control sequences mixed
         with text. Representative of terminal emulators or log recorders. *)
      let tokens = A.parse tui_frame in
      ignore (Sys.opaque_identity tokens))

(* Bench group: TUI-style streaming render (Ansi.emit) *)

let tui_segment_emit_dense_line =
  Ubench.create "segment_emit_dense_line" (fun () ->
      let w = Esc.make dense_writer_bytes in
      let _final_style, _active_link = A.emit w dense_tui_segments in
      (* Ensure work is not optimized away *)
      let written = Esc.len w in
      ignore (Sys.opaque_identity written))

(* Bench group: control / cursor sequences *)

let cursor_script_80x24 =
  Ubench.create "cursor_script_80x24" (fun () ->
      (* Typical TUI "paint the grid" pattern using cursor_position *)
      let buf = Buffer.create 32_000 in
      for row = 1 to 24 do
        for col = 1 to 80 do
          Buffer.add_string buf (A.cursor_position ~row ~col);
          Buffer.add_char buf 'X'
        done
      done;
      ignore (Sys.opaque_identity (Buffer.length buf)))

(* Registration *)

let benchmarks =
  Ubench.
    [
      group "styled" [ styled_inline; styled_segments ];
      group "strip_parse"
        [
          strip_plain_block_bench;
          strip_ansi_block_bench;
          parse_ansi_block_bench;
          parse_tui_frame_bench;
        ];
      group "tui_output" [ tui_segment_emit_dense_line ];
      group "control" [ cursor_script_80x24 ];
    ]

let () = Ubench.run_cli benchmarks
