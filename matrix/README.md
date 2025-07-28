# Matrix

Matrix is a terminal programming toolkit for OCaml that provides low-level terminal control, input parsing, and terminal emulation capabilities.

## Overview

Matrix consists of five modules:

- **ANSI**: Generates ANSI escape sequences following ECMA-48/ANSI X3.64 standards
- **Input**: Parses terminal byte sequences into structured events (keyboard, mouse, paste)
- **TTY**: Raw terminal mode control and terminal attribute management
- **PTY**: Pseudo-terminal creation and process spawning
- **VTE**: Virtual terminal emulator with ANSI escape sequence processing

## Installation

```bash
opam install matrix
```

## Module Reference

### ANSI

Generates ANSI escape sequences for terminal control:

```ocaml
(* Generate styled text *)
let red_bold = Ansi.style [`Fg `Red; `Bold] "Error"
(* Output: "\027[31;1mError\027[0m" *)

(* Cursor control *)
let move_cursor = Ansi.cursor_position 10 20  (* Move to row 10, column 20 *)
(* Output: "\027[10;20H" *)

(* 256-color and RGB support *)
let orange = Ansi.sgr [`Fg (`Index 208)]       (* 256-color palette *)
let custom = Ansi.sgr [`Fg (`RGB (255, 128, 0))] (* 24-bit true color *)
```

Supported features:
- 16 basic colors + bright variants
- 256-color palette (0-255)
- 24-bit RGB colors
- Text attributes: bold, italic, underline, strikethrough, etc.
- Cursor movement and visibility
- Screen clearing and scrolling
- Hyperlinks (OSC 8)

### Input Module

Parses terminal input sequences with support for:

- **Keyboard protocols**: Legacy ANSI, Kitty keyboard protocol
- **Mouse protocols**: X10, SGR, URXVT with motion tracking
- **Bracketed paste mode**: Distinguishes typed input from pasted text
- **Key modifiers**: Ctrl, Alt, Shift, Super (Cmd/Win)
- **Special keys**: F1-F20, media keys, navigation keys

```ocaml
let parser = Input.create () in
let events = Input.feed parser bytes 0 len in
List.iter (function
  | Input.Key { key = Char c; modifier; _ } when modifier.ctrl ->
      Printf.printf "Ctrl+%s pressed\n" (Uchar.to_string c)
  | Input.Mouse (Button_press (x, y, Left, _)) ->
      Printf.printf "Left click at (%d, %d)\n" x y  (* 0-based coordinates *)
  | Input.Paste_start -> print_endline "Paste started"
  | _ -> ()
) events
```

The parser handles partial escape sequences split across reads:
```ocaml
(* First read gets partial sequence *)
let events1 = Input.feed parser "\027[" 0 2 in    (* [] - no complete event *)
(* Second read completes it *)
let events2 = Input.feed parser "A" 0 1 in        (* [Key Up] *)
```

### TTY Module

Low-level terminal control:

```ocaml
(* Open terminal and enable raw mode *)
let tty = Tty.of_fd Unix.stdin in
let original_attrs = Tty.get_attrs tty in
Tty.set_raw_mode tty;

(* Terminal operations *)
let (width, height) = Tty.get_size tty in  (* Get terminal dimensions *)

(* Restore original mode on exit *)
Tty.set_attrs tty original_attrs
```

### PTY Module

Pseudo-terminal operations:

```ocaml
(* Create PTY and spawn process *)
let pty = Pty.create () in
let pid = Pty.spawn pty ~argv:[|"/bin/bash"; "-l"|] in

(* Get file descriptors *)
let master_fd = Pty.master_fd pty in  (* Read/write to subprocess *)
let slave_fd = Pty.slave_fd pty in    (* Subprocess's terminal *)

(* Set PTY size *)
Pty.set_size pty ~rows:24 ~cols:80;

(* Cleanup *)
Unix.waitpid [] pid;
Pty.close pty
```

### VTE Module

Virtual terminal emulator that maintains a grid of cells with styling:

```ocaml
(* Create 80x24 terminal with 1000 lines of scrollback *)
let vte = Vte.create ~rows:24 ~cols:80 ~scrollback:1000 () in

(* Feed terminal data *)
Vte.feed vte bytes offset length;

(* Query cell content *)
let cell = Vte.get_cell vte ~row:0 ~col:0 in
match cell with
| Some { Vte.Cell.char; style } ->
    Printf.printf "Character: U+%04X\n" (Uchar.to_int char);
    if style.bold then print_endline "Bold";
    if style.fg <> `Default then print_endline "Colored"
| None -> print_endline "Empty cell"

(* Check if screen needs redrawing *)
if Vte.is_dirty vte then (
  render_screen vte;
  Vte.clear_dirty vte
)
```

VTE supports:
- Full ANSI escape sequence processing (CSI, OSC, etc.)
- Unicode text with combining characters
- 256-color and RGB color support
- Text attributes (bold, italic, underline, etc.)
- Alternate screen buffer
- Scrollback buffer
- Cursor tracking and visibility
- Grid diffing for efficient rendering

## Practical Examples

### Simple Terminal UI

```ocaml
(* Clear screen and draw border *)
print_string Ansi.clear_screen;
print_string (Ansi.cursor_position 1 1);
print_string (Ansi.style [`Fg `Blue] "┌──────────────┐");

(* Enable mouse tracking *)
print_string Ansi.mouse_on;

(* Process input *)
let parser = Input.create () in
let rec loop () =
  let buffer = Bytes.create 1024 in
  let n = Unix.read Unix.stdin buffer 0 1024 in
  let events = Input.feed parser buffer 0 n in
  List.iter (function
    | Input.Key { key = Escape; _ } -> raise Exit
    | Input.Mouse (Button_press (x, y, _, _)) ->
        print_string (Ansi.cursor_position (y + 1) (x + 1));
        print_char 'X'
    | _ -> ()
  ) events;
  if n > 0 then loop ()
in
try loop () with Exit ->
  print_string Ansi.mouse_off;
  print_string Ansi.clear_screen
```

### Terminal Recorder

```ocaml
(* Record terminal session to VTE *)
let record_session () =
  let pty = Pty.create () in
  let vte = Vte.create ~rows:24 ~cols:80 () in
  let pid = Pty.spawn pty ~argv:[|"/bin/bash"|] in
  
  let rec copy_loop () =
    let buffer = Bytes.create 4096 in
    match Unix.select [Pty.master_fd pty] [] [] 0.1 with
    | [fd], _, _ ->
        let n = Unix.read fd buffer 0 4096 in
        Unix.write Unix.stdout buffer 0 n |> ignore;
        Vte.feed vte buffer 0 n;
        copy_loop ()
    | _ -> copy_loop ()
  in
  
  copy_loop ()
```

## License

ISC License. See [LICENSE](../LICENSE) for details.
