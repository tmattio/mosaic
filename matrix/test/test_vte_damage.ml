(* No need to open Matrix - access modules directly *)

let test_damage_tracking () =
  let grid = Grid.create ~rows:5 ~cols:10 in

  (* Initially no damage *)
  let damage, () = Grid.flush_damage grid in
  Alcotest.(check (list pass)) "initially no damage" [] damage;

  (* Write a character - should create damage *)
  Grid.set grid ~row:0 ~col:0
    (Some { Grid.Cell.glyph = "A"; width = 1; attrs = Grid.Cell.default_style });
  let damage, () = Grid.flush_damage grid in
  Alcotest.(check bool) "has damage after write" true (List.length damage > 0);

  (* Check damage region *)
  match damage with
  | [ rect ] ->
      Alcotest.(check int) "damage row" 0 rect.Grid.row;
      Alcotest.(check int) "damage col" 0 rect.Grid.col;
      Alcotest.(check int) "damage width" 1 rect.Grid.width;
      Alcotest.(check int) "damage height" 1 rect.Grid.height
  | _ -> Alcotest.fail "Expected single damage rect"

let test_damage_clear () =
  let grid = Grid.create ~rows:5 ~cols:10 in

  (* Write some text *)
  for i = 0 to 4 do
    Grid.set grid ~row:0 ~col:i
      (Some
         {
           Grid.Cell.glyph = String.make 1 (Char.chr (65 + i));
           width = 1;
           attrs = Grid.Cell.default_style;
         })
  done;

  (* Flush damage *)
  let damage1, () = Grid.flush_damage grid in
  Alcotest.(check bool) "has damage" true (List.length damage1 > 0);

  (* Second flush should be empty *)
  let damage2, () = Grid.flush_damage grid in
  Alcotest.(check (list pass)) "damage cleared after flush" [] damage2

let test_damage_multiple_regions () =
  let grid = Grid.create ~rows:5 ~cols:10 in

  (* Write in different locations *)
  Grid.set grid ~row:0 ~col:0
    (Some { Grid.Cell.glyph = "A"; width = 1; attrs = Grid.Cell.default_style });
  Grid.set grid ~row:2 ~col:4
    (Some { Grid.Cell.glyph = "B"; width = 1; attrs = Grid.Cell.default_style });

  let damage, () = Grid.flush_damage grid in
  Alcotest.(check bool)
    "has multiple damage regions" true
    (List.length damage >= 1)

let () =
  Alcotest.run "VTE damage tracking"
    [
      ( "damage",
        [
          Alcotest.test_case "basic tracking" `Quick test_damage_tracking;
          Alcotest.test_case "damage clear" `Quick test_damage_clear;
          Alcotest.test_case "multiple regions" `Quick
            test_damage_multiple_regions;
        ] );
    ]
