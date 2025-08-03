open Tsdl
open Tsdl_ttf

(* Helper for handling TSDL errors *)
let or_else err_msg r =
  match r with
  | Ok v -> v
  | Error (`Msg e) ->
      Sdl.log "%s: %s" err_msg e;
      Sdl.quit ();
      exit 1

(** Convert ANSI colors to TSDL's RGBA format. *)
module Colors = struct
  (* Precompute the 256-color palette to avoid recalculating on each render. *)
  let palette_256 =
    Array.init 256 (fun i ->
        let r, g, b = Ansi.rgb_of_color (Ansi.Index i) in
        Sdl.Color.create ~r ~g ~b ~a:255)

  let default_fg = Sdl.Color.create ~r:230 ~g:230 ~b:230 ~a:255
  let default_bg = Sdl.Color.create ~r:20 ~g:20 ~b:20 ~a:255

  let of_ansi (c : Ansi.color) =
    match c with
    | Ansi.Black -> default_bg
    | Ansi.Red -> Sdl.Color.create ~r:205 ~g:49 ~b:49 ~a:255
    | Ansi.Green -> Sdl.Color.create ~r:13 ~g:188 ~b:121 ~a:255
    | Ansi.Yellow -> Sdl.Color.create ~r:229 ~g:229 ~b:16 ~a:255
    | Ansi.Blue -> Sdl.Color.create ~r:36 ~g:114 ~b:200 ~a:255
    | Ansi.Magenta -> Sdl.Color.create ~r:188 ~g:63 ~b:188 ~a:255
    | Ansi.Cyan -> Sdl.Color.create ~r:17 ~g:168 ~b:205 ~a:255
    | Ansi.White -> Sdl.Color.create ~r:229 ~g:229 ~b:229 ~a:255
    | Ansi.Default -> default_fg (* Default handled by context *)
    | Ansi.Bright_black -> Sdl.Color.create ~r:102 ~g:102 ~b:102 ~a:255
    | Ansi.Bright_red -> Sdl.Color.create ~r:240 ~g:71 ~b:71 ~a:255
    | Ansi.Bright_green -> Sdl.Color.create ~r:22 ~g:219 ~b:147 ~a:255
    | Ansi.Bright_yellow -> Sdl.Color.create ~r:245 ~g:245 ~b:67 ~a:255
    | Ansi.Bright_blue -> Sdl.Color.create ~r:59 ~g:142 ~b:234 ~a:255
    | Ansi.Bright_magenta -> Sdl.Color.create ~r:214 ~g:93 ~b:214 ~a:255
    | Ansi.Bright_cyan -> Sdl.Color.create ~r:41 ~g:184 ~b:219 ~a:255
    | Ansi.Bright_white -> Sdl.Color.create ~r:255 ~g:255 ~b:255 ~a:255
    | Ansi.Index n -> palette_256.(n)
    | Ansi.RGB (r, g, b) -> Sdl.Color.create ~r ~g ~b ~a:255
    | Ansi.RGBA (r, g, b, a) -> Sdl.Color.create ~r ~g ~b ~a
end

type state = {
  window : Sdl.window;
  renderer : Sdl.renderer;
  font : Ttf.font;
  char_width : int;
  char_height : int;
  pty : Pty.t;
  vte : Vte.t;
  mutable should_quit : bool;
  (* Selection state *)
  mutable selection_start : (int * int) option;
  mutable selection_end : (int * int) option;
  (* Force render on next frame (for selection changes, etc) *)
  mutable force_render : bool;
  (* Performance: Cache rendered glyphs to avoid re-rendering on every frame *)
  (* Key: (glyph, fg_color, bg_color, reversed) *)
  texture_cache :
    (string * Ansi.color * Ansi.color * bool, Sdl.texture) Hashtbl.t;
}
(** The state of our terminal emulator application. *)

(** Initializes all resources. *)
let init () =
  Sdl.init Sdl.Init.(video + events) |> or_else "SDL Init";
  Ttf.init () |> or_else "TTF Init";

  let font_size = 16 in
  let font_paths =
    [
      (* macOS system fonts *)
      "/System/Library/Fonts/Monaco.ttf";
      "/System/Library/Fonts/Menlo.ttc";
      "/System/Library/Fonts/Courier.ttc";
      "/System/Library/Fonts/Helvetica.ttc";
      "/Library/Fonts/Courier New.ttf";
      (* Common Linux paths *)
      "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf";
      "/usr/share/fonts/truetype/liberation/LiberationMono-Regular.ttf";
      "/usr/share/fonts/truetype/ubuntu/UbuntuMono-R.ttf";
      (* Windows paths *)
      "C:\\Windows\\Fonts\\consola.ttf";
      "C:\\Windows\\Fonts\\cour.ttf";
      (* Fallback to a basic font *)
      "font.ttf";
    ]
  in

  let rec try_fonts = function
    | [] ->
        Sdl.log "Failed to find any suitable font";
        Sdl.quit ();
        exit 1
    | path :: rest -> (
        match Ttf.open_font path font_size with
        | Ok f ->
            Sdl.log "Using font: %s" path;
            f
        | Error _ -> try_fonts rest)
  in

  let font = try_fonts font_paths in

  (* Terminal dimensions *)
  let term_cols = 80 in
  let term_rows = 24 in

  (* Get character dimensions *)
  let char_w, char_h =
    Ttf.size_text font "M" |> or_else "Failed to size text"
  in

  (* Create window sized to terminal dimensions *)
  let win_width = (term_cols * char_w) + 20 in
  (* +20 for margins *)
  let win_height = (term_rows * char_h) + 20 in

  let window, renderer =
    Sdl.create_window_and_renderer ~w:win_width ~h:win_height
      Sdl.Window.(shown + resizable)
    |> or_else "Failed to create window and renderer"
  in
  Sdl.set_window_title window "Matrix Terminal";

  (* Create VTE *)
  let vte = Vte.create ~rows:term_rows ~cols:term_cols () in

  (* Start PTY with shell *)
  let shell = try Sys.getenv "SHELL" with Not_found -> "/bin/sh" in
  let pty =
    Pty.spawn ~prog:shell ~args:[]
      ~winsize:{ Pty.rows = term_rows; cols = term_cols; x = 0; y = 0 }
      ()
  in
  Pty.set_nonblock pty;

  (* Enable text input *)
  Sdl.start_text_input ();

  {
    window;
    renderer;
    font;
    char_width = char_w;
    char_height = char_h;
    pty;
    vte;
    should_quit = false;
    selection_start = None;
    selection_end = None;
    force_render = true;
    (* Force initial render *)
    texture_cache = Hashtbl.create 1024;
  }

(** Cleans up all resources. *)
let cleanup state =
  Sdl.log "Cleaning up...";
  (try Pty.close state.pty with _ -> ());
  Hashtbl.iter (fun _ tex -> Sdl.destroy_texture tex) state.texture_cache;
  Ttf.close_font state.font;
  Sdl.destroy_renderer state.renderer;
  Sdl.destroy_window state.window;
  Ttf.quit ();
  Sdl.quit ()

(** Translates TSDL keyboard events into byte sequences for the PTY. *)
let handle_key_down state (event : Sdl.event) =
  let scancode = Sdl.Event.(get event keyboard_scancode) in
  let keymod = Sdl.get_mod_state () in
  let ctrl = keymod land Sdl.Kmod.ctrl <> 0 in
  let cmd = keymod land Sdl.Kmod.gui <> 0 in
  (* Cmd key on macOS *)

  (* let shift = (keymod land Sdl.Kmod.shift) <> 0 in *)
  (* let alt = (keymod land Sdl.Kmod.alt) <> 0 in *)

  (* Use a buffer to build the output sequence *)
  let buf = Buffer.create 8 in

  (match Sdl.Scancode.enum scancode with
  (* Handle paste - Cmd+V on macOS, Ctrl+V elsewhere *)
  | `V when cmd || ctrl -> (
      match Sdl.get_clipboard_text () with
      | Ok text -> Buffer.add_string buf text
      | Error _ -> ())
  | `Backspace -> Buffer.add_char buf '\x7f' (* DEL *)
  | `Return -> Buffer.add_char buf '\r'
  | `Up -> Buffer.add_string buf "\x1b[A"
  | `Down -> Buffer.add_string buf "\x1b[B"
  | `Left -> Buffer.add_string buf "\x1b[D"
  | `Right -> Buffer.add_string buf "\x1b[C"
  | `Home -> Buffer.add_string buf "\x1b[H"
  | `End -> Buffer.add_string buf "\x1b[F"
  | `Pageup -> Buffer.add_string buf "\x1b[5~"
  | `Pagedown -> Buffer.add_string buf "\x1b[6~"
  | `Delete -> Buffer.add_string buf "\x1b[3~"
  | `Insert -> Buffer.add_string buf "\x1b[2~"
  | `Tab -> Buffer.add_char buf '\t'
  | `Escape -> Buffer.add_char buf '\x1b'
  (* Handle Ctrl+[A-Z] *)
  | c when ctrl && c >= `A && c <= `Z ->
      let code =
        match c with
        | `A -> 1
        | `B -> 2
        | `C -> 3
        | `D -> 4
        | `E -> 5
        | `F -> 6
        | `G -> 7
        | `H -> 8
        | `I -> 9
        | `J -> 10
        | `K -> 11
        | `L -> 12
        | `M -> 13
        | `N -> 14
        | `O -> 15
        | `P -> 16
        | `Q -> 17
        | `R -> 18
        | `S -> 19
        | `T -> 20
        | `U -> 21
        | `V -> 22
        | `W -> 23
        | `X -> 24
        | `Y -> 25
        | `Z -> 26
        | _ -> 0
      in
      if code > 0 then Buffer.add_char buf (Char.chr code)
  | _ -> () (* Other keys are handled by text_input event *));

  let str_to_write = Buffer.contents buf in
  if String.length str_to_write > 0 then
    ignore
      (Pty.write_string state.pty str_to_write 0 (String.length str_to_write))

(** Handles all TSDL events. *)
let handle_event state event =
  (* Convert mouse coordinates to cell position *)
  let mouse_to_cell x y =
    let col = (x - 10) / state.char_width in
    let row = (y - 10) / state.char_height in
    if
      col >= 0
      && col < Vte.cols state.vte
      && row >= 0
      && row < Vte.rows state.vte
    then Some (row, col)
    else None
  in

  (* Get text from selection *)
  let get_selection_text () =
    match (state.selection_start, state.selection_end) with
    | Some (r1, c1), Some (r2, c2) ->
        let start_row = min r1 r2 in
        let end_row = max r1 r2 in
        let start_col = if r1 < r2 || (r1 = r2 && c1 <= c2) then c1 else c2 in
        let end_col = if r1 < r2 || (r1 = r2 && c1 <= c2) then c2 else c1 in

        let buffer = Buffer.create 256 in
        for row = start_row to end_row do
          let col_start = if row = start_row then start_col else 0 in
          let col_end =
            if row = end_row then end_col else Vte.cols state.vte - 1
          in
          for col = col_start to col_end do
            match Vte.get_cell state.vte ~row ~col with
            | Some cell -> Buffer.add_string buffer (Grid.Cell.get_text cell)
            | None -> Buffer.add_char buffer ' '
          done;
          if row < end_row then Buffer.add_char buffer '\n'
        done;
        Buffer.contents buffer
    | _ -> ""
  in

  let event_type = Sdl.Event.(get event typ) in
  match Sdl.Event.enum event_type with
  | `Quit -> state.should_quit <- true
  | `Key_down -> handle_key_down state event
  | `Text_input ->
      let text = Sdl.Event.(get event text_input_text) in
      ignore (Pty.write_string state.pty text 0 (String.length text))
  | `Mouse_button_down -> (
      let x = Sdl.Event.(get event mouse_button_x) in
      let y = Sdl.Event.(get event mouse_button_y) in
      match mouse_to_cell x y with
      | Some (row, col) ->
          state.selection_start <- Some (row, col);
          state.selection_end <- Some (row, col);
          state.force_render <- true
      | None -> ())
  | `Mouse_motion -> (
      let buttons = Sdl.Event.(get event mouse_motion_state) in
      if buttons <> 0l then
        let x = Sdl.Event.(get event mouse_motion_x) in
        let y = Sdl.Event.(get event mouse_motion_y) in
        match mouse_to_cell x y with
        | Some (row, col) ->
            state.selection_end <- Some (row, col);
            state.force_render <- true
        | None -> ())
  | `Mouse_button_up ->
      (* Copy selection to clipboard *)
      let text = get_selection_text () in
      if String.length text > 0 then ignore (Sdl.set_clipboard_text text)
  | `Window_event -> (
      match Sdl.Event.(window_event_enum (get event window_event_id)) with
      | `Resized ->
          let w, h =
            Sdl.Event.(get event window_data1, get event window_data2)
          in
          let new_cols = (Int32.to_int w - 20) / state.char_width in
          let new_rows = (Int32.to_int h - 20) / state.char_height in
          (* Resize both PTY and VTE *)
          Pty.resize state.pty ~cols:new_cols ~rows:new_rows;
          Vte.resize state.vte ~rows:new_rows ~cols:new_cols;
          state.force_render <- true
      | _ -> ())
  | _ -> ()

(** Renders the entire VTE grid to the screen. *)
let render state =
  let { renderer; vte; char_width; char_height; _ } = state in

  (* Clear with default background color *)
  Sdl.set_render_draw_color renderer 20 20 20 255 |> or_else "Set draw color";
  Sdl.render_clear renderer |> or_else "Render clear";

  (* Get texture for a cell with proper attribute handling *)
  let get_or_create_texture (cell : Grid.Cell.t) =
    let glyph = Grid.Cell.get_text cell in
    (* Skip rendering empty glyphs *)
    if String.length glyph = 0 then None
    else
      let style = Grid.Cell.get_style cell in
      let fg = Ansi.Style.fg style in
      let bg = Ansi.Style.bg style in
      let reversed = Ansi.Style.reversed style in

      (* Handle default colors - map to the terminal's actual colors *)
      let fg = if fg = Ansi.Default then Ansi.White else fg in
      let bg = if bg = Ansi.Default then Ansi.RGB (20, 20, 20) else bg in

      (* Apply reversed attribute *)
      let final_fg, final_bg = if reversed then (bg, fg) else (fg, bg) in

      let cache_key = (glyph, final_fg, final_bg, reversed) in
      match Hashtbl.find_opt state.texture_cache cache_key with
      | Some tex -> Some tex
      | None ->
          let fg_color = Colors.of_ansi final_fg in
          let bg_color = Colors.of_ansi final_bg in
          let surface =
            Ttf.render_utf8_shaded state.font glyph fg_color bg_color
            |> or_else "Failed to render char"
          in
          let texture =
            Sdl.create_texture_from_surface renderer surface
            |> or_else "Failed to create texture"
          in
          Sdl.free_surface surface;
          Hashtbl.add state.texture_cache cache_key texture;
          Some texture
  in

  (* Render grid cells *)
  for row = 0 to Vte.rows vte - 1 do
    for col = 0 to Vte.cols vte - 1 do
      (* Check if this cell is selected *)
      let is_selected =
        match (state.selection_start, state.selection_end) with
        | Some (r1, c1), Some (r2, c2) ->
            let start_row = min r1 r2 in
            let end_row = max r1 r2 in
            let start_col =
              if r1 < r2 || (r1 = r2 && c1 <= c2) then c1 else c2
            in
            let end_col = if r1 < r2 || (r1 = r2 && c1 <= c2) then c2 else c1 in

            if row >= start_row && row <= end_row then
              if row = start_row && row = end_row then
                col >= start_col && col <= end_col
              else if row = start_row then col >= start_col
              else if row = end_row then col <= end_col
              else true
            else false
        | _ -> false
      in

      match Vte.get_cell vte ~row ~col with
      | None ->
          if is_selected then (
            (* Draw selection background for empty cells *)
            Sdl.set_render_draw_color renderer 100 100 200 255
            |> or_else "Set selection bg";
            let rect =
              Sdl.Rect.create
                ~x:(10 + (col * char_width))
                ~y:(10 + (row * char_height))
                ~w:char_width ~h:char_height
            in
            Sdl.render_fill_rect renderer (Some rect)
            |> or_else "Fill selection")
      | Some cell -> (
          (* Handle selection by temporarily modifying cell attributes *)
          let cell =
            if is_selected then
              let style = Grid.Cell.get_style cell in
              let fg = Ansi.Style.bg style in
              let new_style =
                style |> Ansi.Style.with_fg fg |> Ansi.Style.with_bg Ansi.Blue
              in
              Grid.Cell.make_glyph (Grid.Cell.get_text cell) ~style:new_style
                ~east_asian_context:false
            else cell
          in

          match get_or_create_texture cell with
          | Some texture ->
              let dst =
                Sdl.Rect.create
                  ~x:(10 + (col * char_width))
                  ~y:(10 + (row * char_height))
                  ~w:(char_width * Grid.Cell.width cell)
                  ~h:char_height
              in
              Sdl.render_copy renderer texture ~dst
              |> or_else "Failed to copy texture"
          | None ->
              (* For empty glyphs, just render the background color *)
              let style = Grid.Cell.get_style cell in
              let bg = Ansi.Style.bg style in
              (* Handle default background color - same as in texture rendering *)
              let bg =
                if bg = Ansi.Default then Ansi.RGB (20, 20, 20) else bg
              in
              let bg_color = Colors.of_ansi bg in
              Sdl.set_render_draw_color renderer (Sdl.Color.r bg_color)
                (Sdl.Color.g bg_color) (Sdl.Color.b bg_color)
                (Sdl.Color.a bg_color)
              |> or_else "Set bg color";
              let rect =
                Sdl.Rect.create
                  ~x:(10 + (col * char_width))
                  ~y:(10 + (row * char_height))
                  ~w:char_width ~h:char_height
              in
              Sdl.render_fill_rect renderer (Some rect)
              |> or_else "Fill background")
    done
  done;

  (* Render cursor *)
  if Vte.is_cursor_visible vte then (
    let r, c = Vte.cursor_pos vte in
    Sdl.set_render_draw_color renderer 255 255 0 255
    |> or_else "Set cursor color";
    let cursor_rect =
      Sdl.Rect.create
        ~x:(10 + (c * char_width))
        ~y:(10 + (r * char_height))
        ~w:char_width ~h:char_height
    in
    Sdl.render_fill_rect renderer (Some cursor_rect) |> or_else "Render cursor");

  Sdl.render_present renderer

(** The main loop of the application. *)
let main_loop state =
  let pty_fd = Pty.in_fd state.pty in
  let read_buffer = Bytes.create 4096 in

  let rec loop () =
    (* Read from PTY *)
    let readable, _, _ = Unix.select [ pty_fd ] [] [] 0.01 in
    if readable <> [] then (
      try
        let bytes_read =
          Pty.read state.pty read_buffer 0 (Bytes.length read_buffer)
        in
        if bytes_read > 0 then Vte.feed state.vte read_buffer 0 bytes_read
        else
          (* 0 bytes read means EOF, the child process has exited *)
          state.should_quit <- true
      with
      | Unix.Unix_error (Unix.EAGAIN, _, _) -> ()
      | ex ->
          Sdl.log "Read error: %s" (Printexc.to_string ex);
          state.should_quit <- true);

    (* Only render if VTE state has changed or force_render is set *)
    if Vte.is_dirty state.vte || state.force_render then (
      render state;
      Vte.clear_dirty state.vte;
      state.force_render <- false);

    (* Handle events *)
    let event = Sdl.Event.create () in
    while Sdl.poll_event (Some event) && not state.should_quit do
      handle_event state event
    done;

    if not state.should_quit then (
      Sdl.delay 16l;
      loop ())
  in

  loop ()

(** Main entry point. *)
let run () =
  let state = init () in
  Fun.protect ~finally:(fun () -> cleanup state) (fun () -> main_loop state)

(* Start the emulator *)
let () = run ()
