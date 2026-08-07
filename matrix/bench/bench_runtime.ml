let make_app () =
  let terminal = Matrix.Terminal.make ~output:(fun _ -> ()) ~tty:false () in
  let parser = Matrix.Input.Parser.create () in
  let app =
    Matrix.attach ~mode:`Alt ~raw_mode:false ~target_fps:None
      ~mouse_enabled:false ~bracketed_paste:false ~focus_reporting:false
      ~kitty_keyboard:`Disabled ~cursor_visible:false ~signal_handlers:false
      ~backend:
        {
          now = (fun () -> 0.);
          wake = (fun () -> ());
          write_output = (fun _bytes _off _len -> ());
          read_input = (fun _ _ _ -> 0);
          wait_readable = (fun ~timeout:_ -> false);
          read_events = (fun ~timeout:_ ~on_event:_ ~on_response:_ -> `Continue);
          terminal_size = (fun () -> (160, 48));
          set_raw_mode = (fun _ -> ());
          flush_input = (fun () -> ());
          cleanup = (fun () -> ());
        }
      ~parser ~terminal ~width:160 ~height:48 ()
  in
  Matrix.prepare app;
  Matrix.submit app;
  Matrix.prepare app;
  Matrix.submit app;
  app

let no_changes =
  Thumper.bench_with_setup ~setup:make_app ~teardown:Matrix.close
    "submit/no-changes" (fun app ->
      Matrix.prepare app;
      Matrix.submit app)

let single_cell =
  let next = ref false in
  Thumper.bench_with_setup ~setup:make_app ~teardown:Matrix.close
    "submit/single-cell" (fun app ->
      next := not !next;
      Matrix.prepare app;
      let char = if !next then 'X' else 'Y' in
      Matrix.Grid.set_cell (Matrix.grid app) ~x:0 ~y:0
        ~cell:(Matrix.Grid.Cell.of_uchar (Uchar.of_char char))
        ~fg:Matrix.Ansi.Color.white ~bg:Matrix.Ansi.Color.black
        ~attrs:Matrix.Ansi.Attr.empty ();
      Matrix.submit app)

let () =
  Thumper.run "runtime"
    ~budgets:
      [
        Thumper.Budget.no_slower_than 0.05; Thumper.Budget.no_more_alloc_than 0.;
      ]
    [ Thumper.group "render" [ no_changes; single_cell ] ]
