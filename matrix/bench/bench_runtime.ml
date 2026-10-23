(* A frame of the loop: a turn that finds a frame asked for returns it, and
   the frame is prepared and presented. The backend's clock moves a second
   at every read, so every frame is due at once and no turn waits. *)

let backend () : Matrix.Backend.t =
  let clock = ref 0 in
  {
    now =
      (fun () ->
        clock := !clock + 1_000_000_000;
        !clock);
    wait = (fun ~until:_ ~reading:_ -> false);
    wake = (fun () -> ());
    take_signals = (fun () -> []);
    read_input = (fun _ _ _ -> -1);
    write_output = (fun _bytes _off _len -> ());
    terminal_size = (fun () -> (160, 48));
    set_raw_mode = (fun _ -> ());
    flush_input = (fun () -> ());
    input_is_tty = false;
    output_is_tty = false;
    controlling = false;
    on_fatal = ignore;
    protect = (fun f -> f ());
    cleanup = (fun () -> ());
    env = Matrix.Terminal.Caps.env (fun _ -> None) ~win32:false;
  }

let config =
  Matrix.config ~mode:`Alt ~raw_mode:false ~mouse:`Off ~bracketed_paste:false
    ~kitty_keyboard:`Off ~cursor_visible:false ()

let frame app draw =
  Matrix.request_redraw app;
  match Matrix.next app with
  | Frame -> Matrix.draw app draw
  | _ -> failwith "runtime benchmark: no frame"

let () =
  Matrix.run ~backend:(backend ()) ~config @@ fun app ->
  frame app ignore;
  frame app ignore;
  let no_changes =
    Thumper.bench "frame/no-changes" (fun () -> frame app ignore)
  in
  let single_cell =
    let next = ref false in
    Thumper.bench "frame/single-cell" (fun () ->
        next := not !next;
        let char = if !next then 'X' else 'Y' in
        frame app (fun grid ->
            Matrix.Grid.set_uchar grid ~x:0 ~y:0 ~uchar:(Uchar.of_char char)
              ~fg:Matrix.Ansi.Color.white ~bg:Matrix.Ansi.Color.black
              ~attrs:Matrix.Ansi.Attr.empty ()))
  in
  Thumper.run "runtime"
    ~budgets:
      [
        Thumper.Budget.no_slower_than 0.05; Thumper.Budget.no_more_alloc_than 0.;
      ]
    [ Thumper.group "render" [ no_changes; single_cell ] ]
