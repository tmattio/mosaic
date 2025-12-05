open Engine
open Ubench

let benchmarks =
  [
    group "Commands"
      [
        create "simple_msg" (fun () ->
            Cmd.msg () |> Cmd.run ~dispatch:(fun _ -> ()));
        create "batch_100_msgs" (fun () ->
            Cmd.batch (List.init 100 (fun _ -> Cmd.msg ()))
            |> Cmd.run ~dispatch:(fun _ -> ()));
        create "perform_simple" (fun () ->
            Cmd.perform (fun () -> None) |> Cmd.run ~dispatch:(fun _ -> ()));
        create "tick" (fun () ->
            Cmd.tick 1.0 (fun _ -> ()) |> Cmd.run ~dispatch:(fun _ -> ()));
        create "seq_100_msgs" (fun () ->
            Cmd.seq (List.init 100 (fun _ -> Cmd.msg ()))
            |> Cmd.run ~dispatch:(fun _ -> ()));
        create "map_chain_10" (fun () ->
            let c = Cmd.msg () in
            let rec chain n cmd =
              if n <= 0 then cmd else chain (n - 1) (Cmd.map (fun x -> x) cmd)
            in
            chain 10 c |> Cmd.run ~dispatch:(fun _ -> ()));
      ];
    group "Subscriptions"
      [
        create "keyboard_simple" (fun () ->
            Sub.keyboard (fun _ -> ())
            |> Sub.run
                 ~dispatch:(fun _ -> ())
                 (Event.Input
                    (Input.Key
                       {
                         key = Input.Char (Uchar.of_char 'a');
                         modifier =
                           {
                             ctrl = false;
                             alt = false;
                             shift = false;
                             super = false;
                             hyper = false;
                             meta = false;
                             caps_lock = false;
                             num_lock = false;
                           };
                         event_type = Press;
                         associated_text = "";
                         shifted_key = None;
                         base_key = None;
                       })));
        create "batch_100_keyboard" (fun () ->
            Sub.batch (List.init 100 (fun _ -> Sub.keyboard (fun _ -> ())))
            |> Sub.run
                 ~dispatch:(fun _ -> ())
                 (Event.Input
                    (Input.Key
                       {
                         key = Input.Char (Uchar.of_char 'a');
                         modifier =
                           {
                             ctrl = false;
                             alt = false;
                             shift = false;
                             super = false;
                             hyper = false;
                             meta = false;
                             caps_lock = false;
                             num_lock = false;
                           };
                         event_type = Press;
                         associated_text = "";
                         shifted_key = None;
                         base_key = None;
                       })));
        create "timer_accumulate_100_ticks" (fun () ->
            let s = Sub.timer ~every:1.0 (fun () -> ()) in
            let d = fun _ -> () in
            for _ = 1 to 100 do
              Sub.run ~dispatch:d (Event.Tick 0.01) s
            done);
      ];
    group "Focus Manager"
      [
        create "register_1000" (fun () ->
            let m = Focus_manager.create () in
            for i = 1 to 200 do
              Focus_manager.register m
                {
                  key = Ui.Key.of_int i;
                  tab_index = None;
                  auto_focus = false;
                  order = 0;
                }
            done);
        create "focus_next_loop_1000" (fun () ->
            let m = Focus_manager.create () in
            for i = 1 to 100 do
              Focus_manager.register m
                {
                  key = Ui.Key.of_int i;
                  tab_index = Some i;
                  auto_focus = false;
                  order = 0;
                }
            done;
            for _ = 1 to 100 do
              Focus_manager.focus_next m
            done);
        create "unregister_1000" (fun () ->
            let m = Focus_manager.create () in
            for i = 1 to 200 do
              Focus_manager.register m
                {
                  key = Ui.Key.of_int i;
                  tab_index = None;
                  auto_focus = false;
                  order = 0;
                }
            done;
            for i = 1 to 200 do
              Focus_manager.unregister m (Ui.Key.of_int i)
            done);
      ];
    group "Input Router"
      [
        create "subscribe_1000_mixed" (fun () ->
            let r = Input_router.create () in
            for i = 1 to 1000 do
              ignore
                (Input_router.subscribe r
                   (Click (Ui.Key.of_int i, fun () -> true)));
              ignore
                (Input_router.subscribe r
                   (Hover (Ui.Key.of_int i, fun _ -> ())));
              ignore
                (Input_router.subscribe r
                   (Key_press (Ui.Key.of_int i, fun _ -> true)))
            done);
        create "on_mouse_motion_1000_hits" (fun () ->
            let r = Input_router.create () in
            let s = Ui.Layout_snapshot.create () in
            for i = 1 to 1000 do
              Ui.Layout_snapshot.record s (Ui.Key.of_int i)
                {
                  rect = { x = 0; y = 0; w = 1; h = 1 };
                  z_index = i;
                  clipping = None;
                };
              ignore
                (Input_router.subscribe r
                   (Hover (Ui.Key.of_int i, fun _ -> ())))
            done;
            Input_router.set_snapshot r s;
            for _ = 1 to 100 do
              ignore (Input_router.on_mouse r ~x:0 ~y:0 Motion)
            done);
        create "on_mouse_drag_1000" (fun () ->
            let r = Input_router.create () in
            let s = Ui.Layout_snapshot.create () in
            for i = 1 to 1000 do
              Ui.Layout_snapshot.record s (Ui.Key.of_int i)
                {
                  rect = { x = 0; y = 0; w = 1; h = 1 };
                  z_index = i;
                  clipping = None;
                };
              ignore
                (Input_router.subscribe r (Drag (Ui.Key.of_int i, fun _ -> ())))
            done;
            Input_router.set_snapshot r s;
            ignore (Input_router.on_mouse r ~x:0 ~y:0 (Button_down Input.Left));
            for _ = 1 to 10 do
              ignore (Input_router.on_mouse r ~x:1 ~y:1 Motion)
            done;
            Input_router.on_mouse r ~x:2 ~y:2 (Button_up Input.Left));
        create "on_keyboard_1000_focused" (fun () ->
            let r = Input_router.create () in
            for i = 1 to 1000 do
              ignore
                (Input_router.subscribe r
                   (Key_press (Ui.Key.of_int i, fun _ -> true)))
            done;
            Input_router.set_focused r (Some (Ui.Key.of_int 500));
            let evt =
              {
                Input.key = Input.Char (Uchar.of_char 'a');
                modifier =
                  {
                    ctrl = false;
                    alt = false;
                    shift = false;
                    super = false;
                    hyper = false;
                    meta = false;
                    caps_lock = false;
                    num_lock = false;
                  };
                event_type = Press;
                associated_text = "";
                shifted_key = None;
                base_key = None;
              }
            in
            for _ = 1 to 100 do
              ignore (Input_router.on_keyboard r evt)
            done);
      ];
  ]

let () = Ubench.run_cli benchmarks
