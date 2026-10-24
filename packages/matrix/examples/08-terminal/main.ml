(** Your shell in a pseudo-terminal, drawn through [Vte]. A thread waits for its
    output and posts to the loop ([Matrix.post]), which reads it.

    Keys go to the shell (Ctrl+D ends it); Esc quits. *)

open Matrix

let shell_prog = try Sys.getenv "SHELL" with Not_found -> "/bin/sh"

type state = { mutable pty : Pty.t option; vte : Vte.t; cols : int; rows : int }

let create_state ~cols ~rows =
  let vte = Vte.create ~rows ~cols () in
  let winsize = { Pty.rows; cols; xpixel = 0; ypixel = 0 } in
  let pty =
    try
      let p = Pty.spawn ~prog:shell_prog ~args:[] ~winsize () in
      Pty.set_nonblock p;
      Some p
    with _ -> None
  in
  { pty; vte; cols; rows }

let cleanup state =
  match state.pty with Some pty -> Pty.close pty | None -> ()

(* Write all of [s] to [pty]. The PTY is non-blocking: after a short write,
   or when it can take nothing, wait until it can take more. *)
let rec write_all pty s off len =
  if len > 0 then
    match Pty.write_string pty s off len with
    | n when n > 0 -> write_all pty s (off + n) (len - n)
    | _ | (exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _)) ->
        ignore (Unix.select [] [ Pty.out_fd pty ] [] (-1.));
        write_all pty s off len

(* What the watcher tells the loop: the shell wrote. *)
type message = Output

(* A thread waits for the shell's output and tells the loop, which reads it:
   the loop owns the state, and waits for nothing. The thread waits again
   once the loop has read, which the loop says by releasing [read]. *)
let watch app pty =
  let read = Semaphore.Binary.make false in
  let rec go () =
    match Unix.select [ Pty.in_fd pty ] [] [] (-1.) with
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> go ()
    | exception Unix.Unix_error _ -> ()
    | _ ->
        Matrix.post app Output;
        Semaphore.Binary.acquire read;
        go ()
  in
  ignore (Thread.create go () : Thread.t);
  read

let read_pty_output state =
  match state.pty with
  | None -> ()
  | Some pty ->
      let buf = Bytes.create 4096 in
      let rec read_loop () =
        match Pty.read pty buf 0 (Bytes.length buf) with
        | n when n > 0 ->
            (* The program waits for the answers to its queries. *)
            let replies = Input.Reply.encode (Vte.feed state.vte buf 0 n) in
            write_all pty replies 0 (String.length replies);
            read_loop ()
        | _ -> ()
        | exception Unix.Unix_error (Unix.EAGAIN, _, _) -> ()
        | exception Unix.Unix_error (Unix.EWOULDBLOCK, _, _) -> ()
        | exception Unix.Unix_error (Unix.EIO, _, _) ->
            Pty.close pty;
            state.pty <- None
        | exception _ -> ()
      in
      read_loop ()

let send state s =
  match state.pty with
  | None -> ()
  | Some pty -> (
      try write_all pty s 0 (String.length s)
      with Unix.Unix_error (Unix.EIO, _, _) ->
        Pty.close pty;
        state.pty <- None)

(* The bytes the terminal sends for a keystroke, in the encoding the shell's
   program has set; a key that no terminal in it sends is dropped. *)
let send_key state ke =
  match Input.Keystroke.encode (Vte.keyboard state.vte) ke with
  | bytes -> send state bytes
  | exception Invalid_argument _ -> ()

(* A paste goes to the program as it came, between the bracketed paste
   markers when the program asked for them. *)
let send_paste state event =
  let bracketed = (Vte.modes state.vte).bracketed_paste in
  match event with
  | Paste_start -> if bracketed then send state "\x1b[200~"
  | Paste_data data -> send state data
  | Paste_end _ -> if bracketed then send state "\x1b[201~"
  | _ -> ()

let resize_state state ~cols ~rows =
  if cols <> state.cols || rows <> state.rows then (
    let report = Vte.resize state.vte ~rows ~cols in
    (match state.pty with
    | Some pty ->
        let winsize = { Pty.rows; cols; xpixel = 0; ypixel = 0 } in
        Pty.set_winsize pty winsize
    | None -> ());
    send state (Input.Reply.encode report);
    { state with cols; rows })
  else state

let draw_border grid ~cols ~rows =
  let border_style =
    Ansi.Style.make ~fg:Ansi.Color.bright_black
      ~bg:(Ansi.Color.grayscale ~level:2)
      ()
  in
  (* Top border *)
  let title = " Embedded Terminal (Ctrl+D to exit shell, Esc to quit) " in
  let padding = (cols - String.length title) / 2 in
  let top_line =
    String.make padding '-' ^ title
    ^ String.make (cols - padding - String.length title) '-'
  in
  Grid.draw_text ~style:border_style grid ~x:0 ~y:0 ~text:top_line;
  (* Bottom border *)
  let bottom = String.make cols '-' in
  Grid.draw_text ~style:border_style grid ~x:0 ~y:(rows - 1) ~text:bottom

let draw_vte grid state ~offset_y =
  let vte_grid = Vte.grid state.vte in
  let vte_rows = min (Grid.height vte_grid) (Grid.height grid - offset_y - 1) in
  let vte_cols = min (Grid.width vte_grid) (Grid.width grid) in
  Grid.blit_region ~src:vte_grid ~dst:grid ~src_x:0 ~src_y:0 ~width:vte_cols
    ~height:vte_rows ~dst_x:0 ~dst_y:offset_y

let draw_no_pty grid ~cols ~rows =
  let msg = "Failed to spawn shell. Press Esc to quit." in
  let style = Ansi.Style.make ~fg:Ansi.Color.bright_red () in
  Grid.draw_text ~style grid
    ~x:((cols - String.length msg) / 2)
    ~y:(rows / 2) ~text:msg

let () =
  let config = Matrix.config ~mouse:`Off () in
  let state =
    Matrix.run ~config @@ fun app ->
    let cols, rows = Matrix.size app in
    let state = ref (create_state ~cols ~rows:(max 1 (rows - 2))) in
    let read = Option.map (watch app) !state.pty in
    let rec loop () =
      match Matrix.next app with
      | Stopped | Event (Key { key = Escape; _ }) -> ()
      | Event (Key ke) ->
          send_key !state ke;
          loop ()
      | Event (Text text) ->
          send !state text;
          loop ()
      | Event ((Paste_start | Paste_data _ | Paste_end _) as event) ->
          send_paste !state event;
          loop ()
      | Event (Resize (cols, rows)) ->
          state := resize_state !state ~cols ~rows:(max 1 (rows - 2));
          loop ()
      | Posted Output ->
          (* The shell wrote: its output is read, then the watcher waits
             again. The run ends with the shell. *)
          read_pty_output !state;
          Option.iter Semaphore.Binary.release read;
          Matrix.request_redraw app;
          if !state.pty <> None then loop ()
      | Frame ->
          Matrix.draw app (fun grid ->
              let cols, rows = Matrix.size app in
              Grid.clear ~color:Ansi.Color.black grid;
              draw_border grid ~cols ~rows;
              match !state.pty with
              | Some _ -> draw_vte grid !state ~offset_y:1
              | None -> draw_no_pty grid ~cols ~rows);
          loop ()
      | Event _ | Timer _ -> loop ()
    in
    loop ();
    !state
  in
  cleanup state
