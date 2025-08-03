open Alcotest

(* Create Alcotest testable for events *)
let event_testable =
  let pp = Vcr.Event.pp in
  let equal e1 e2 =
    match (e1, e2) with
    | Vcr.Event.Sleep s1, Vcr.Event.Sleep s2 ->
        Float.abs (s1.at -. s2.at) < 0.1
        && Float.abs (s1.duration -. s2.duration) < 0.01
    | Vcr.Event.Cursor_move c1, Vcr.Event.Cursor_move c2 ->
        Float.abs (c1.at -. c2.at) < 0.1 && c1.row = c2.row && c1.col = c2.col
    | Vcr.Event.Screen_change sc1, Vcr.Event.Screen_change sc2 ->
        Float.abs (sc1.at -. sc2.at) < 0.1
        && List.length sc1.changed_rows = List.length sc2.changed_rows
        && List.for_all2
             (fun (row1, cells1) (row2, cells2) ->
               row1 = row2
               && Array.length cells1 = Array.length cells2
               && Array.for_all2
                    (fun c1 c2 ->
                      match (c1, c2) with
                      | None, None -> true
                      | Some c1, Some c2 -> Grid.Cell.equal c1 c2
                      | _ -> false)
                    cells1 cells2)
             sc1.changed_rows sc2.changed_rows
    | _ -> false
  in
  testable pp equal

let test_demo_tape () =
  let tape_content =
    {|
Output vcr/examples/demo.gif

Require echo

Set Shell "bash"
Set FontSize 32
Set Width 1200
Set Height 600

Type "echo 'Welcome to VCR!'" Sleep 500ms  Enter

Sleep 3s
|}
  in

  let tape =
    match Tape_lang.from_string tape_content with
    | Ok tape -> tape
    | Error err -> failwith (Printf.sprintf "Failed to parse tape: %s" err)
  in

  (* Build expected event list for the demo tape *)
  (* Type command "echo 'Welcome to VCR!'" - one event per character *)

  (* Helper to create a glyph cell *)
  let glyph text =
    Some
      (Grid.Cell.make_glyph ~style:Ansi.Style.default ~east_asian_context:false
         text)
  in

  (* The string is "echo 'Welcome to VCR!'" *)
  let expected_events =
    [
      (* "e" *)
      Vcr.Event.Cursor_move { at = 0.05; row = 0; col = 1 };
      Vcr.Event.Screen_change
        { at = 0.05; changed_rows = [ (0, [| glyph "e" |]) ] };
      (* "c" *)
      Vcr.Event.Cursor_move { at = 0.10; row = 0; col = 2 };
      Vcr.Event.Screen_change
        { at = 0.10; changed_rows = [ (0, [| None; glyph "c" |]) ] };
      (* "h" *)
      Vcr.Event.Cursor_move { at = 0.15; row = 0; col = 3 };
      Vcr.Event.Screen_change
        { at = 0.15; changed_rows = [ (0, [| None; None; glyph "h" |]) ] };
      (* "o" *)
      Vcr.Event.Cursor_move { at = 0.20; row = 0; col = 4 };
      Vcr.Event.Screen_change
        { at = 0.20; changed_rows = [ (0, [| None; None; None; glyph "o" |]) ] };
      (* " " *)
      Vcr.Event.Cursor_move { at = 0.25; row = 0; col = 5 };
      Vcr.Event.Screen_change
        {
          at = 0.25;
          changed_rows = [ (0, [| None; None; None; None; glyph " " |]) ];
        };
      (* "'" *)
      Vcr.Event.Cursor_move { at = 0.30; row = 0; col = 6 };
      Vcr.Event.Screen_change
        {
          at = 0.30;
          changed_rows = [ (0, [| None; None; None; None; None; glyph "'" |]) ];
        };
      (* "W" *)
      Vcr.Event.Cursor_move { at = 0.35; row = 0; col = 7 };
      Vcr.Event.Screen_change
        {
          at = 0.35;
          changed_rows =
            [ (0, [| None; None; None; None; None; None; glyph "W" |]) ];
        };
      (* "e" *)
      Vcr.Event.Cursor_move { at = 0.40; row = 0; col = 8 };
      Vcr.Event.Screen_change
        {
          at = 0.40;
          changed_rows =
            [ (0, [| None; None; None; None; None; None; None; glyph "e" |]) ];
        };
      (* "l" *)
      Vcr.Event.Cursor_move { at = 0.45; row = 0; col = 9 };
      Vcr.Event.Screen_change
        {
          at = 0.45;
          changed_rows =
            [
              ( 0,
                [| None; None; None; None; None; None; None; None; glyph "l" |]
              );
            ];
        };
      (* "c" *)
      Vcr.Event.Cursor_move { at = 0.50; row = 0; col = 10 };
      Vcr.Event.Screen_change
        {
          at = 0.50;
          changed_rows =
            [
              ( 0,
                [|
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  glyph "c";
                |] );
            ];
        };
      (* "o" *)
      Vcr.Event.Cursor_move { at = 0.55; row = 0; col = 11 };
      Vcr.Event.Screen_change
        {
          at = 0.55;
          changed_rows =
            [
              ( 0,
                [|
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  glyph "o";
                |] );
            ];
        };
      (* "m" *)
      Vcr.Event.Cursor_move { at = 0.60; row = 0; col = 12 };
      Vcr.Event.Screen_change
        {
          at = 0.60;
          changed_rows =
            [
              ( 0,
                [|
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  glyph "m";
                |] );
            ];
        };
      (* "e" *)
      Vcr.Event.Cursor_move { at = 0.65; row = 0; col = 13 };
      Vcr.Event.Screen_change
        {
          at = 0.65;
          changed_rows =
            [
              ( 0,
                [|
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  glyph "e";
                |] );
            ];
        };
      (* " " *)
      Vcr.Event.Cursor_move { at = 0.70; row = 0; col = 14 };
      Vcr.Event.Screen_change
        {
          at = 0.70;
          changed_rows =
            [
              ( 0,
                [|
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  glyph " ";
                |] );
            ];
        };
      (* "t" *)
      Vcr.Event.Cursor_move { at = 0.75; row = 0; col = 15 };
      Vcr.Event.Screen_change
        {
          at = 0.75;
          changed_rows =
            [
              ( 0,
                [|
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  glyph "t";
                |] );
            ];
        };
      (* "o" *)
      Vcr.Event.Cursor_move { at = 0.80; row = 0; col = 16 };
      Vcr.Event.Screen_change
        {
          at = 0.80;
          changed_rows =
            [
              ( 0,
                [|
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  glyph "o";
                |] );
            ];
        };
      (* " " *)
      Vcr.Event.Cursor_move { at = 0.85; row = 0; col = 17 };
      Vcr.Event.Screen_change
        {
          at = 0.85;
          changed_rows =
            [
              ( 0,
                [|
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  glyph " ";
                |] );
            ];
        };
      (* "V" *)
      Vcr.Event.Cursor_move { at = 0.90; row = 0; col = 18 };
      Vcr.Event.Screen_change
        {
          at = 0.90;
          changed_rows =
            [
              ( 0,
                [|
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  glyph "V";
                |] );
            ];
        };
      (* "C" *)
      Vcr.Event.Cursor_move { at = 0.95; row = 0; col = 19 };
      Vcr.Event.Screen_change
        {
          at = 0.95;
          changed_rows =
            [
              ( 0,
                [|
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  glyph "C";
                |] );
            ];
        };
      (* "R" *)
      Vcr.Event.Cursor_move { at = 1.00; row = 0; col = 20 };
      Vcr.Event.Screen_change
        {
          at = 1.00;
          changed_rows =
            [
              ( 0,
                [|
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  glyph "R";
                |] );
            ];
        };
      (* "!" *)
      Vcr.Event.Cursor_move { at = 1.05; row = 0; col = 21 };
      Vcr.Event.Screen_change
        {
          at = 1.05;
          changed_rows =
            [
              ( 0,
                [|
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  glyph "!";
                |] );
            ];
        };
      (* Sleep 500ms *)
      Vcr.Event.Sleep { at = 1.10; duration = 0.5 };
      (* "'" - appears after sleep *)
      Vcr.Event.Cursor_move { at = 1.10; row = 0; col = 22 };
      Vcr.Event.Screen_change
        {
          at = 1.10;
          changed_rows =
            [
              ( 0,
                [|
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  None;
                  glyph "'";
                |] );
            ];
        };
      (* Sleep 3s *)
      Vcr.Event.Sleep { at = 1.70; duration = 3.0 };
      (* Enter key press moves cursor to next line *)
      Vcr.Event.Cursor_move { at = 1.70; row = 1; col = 0 };
    ]
  in

  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  match Vcr.Executor.run ~sw ~env tape with
  | Error err ->
      failwith (Printf.sprintf "Executor failed: %s" (Vcr.Error.to_string err))
  | Ok (events, _vte, _config) ->
      (* Compare the actual events with expected events *)
      check (list event_testable) "event sequence" expected_events events

let () =
  run "VCR Executor"
    [
      ( "demo tape",
        [ test_case "produces expected event sequence" `Quick test_demo_tape ]
      );
    ]
