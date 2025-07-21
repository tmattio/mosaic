open Mosaic

let () =
  (* Example 1: Basic progress bar *)
  let bar1 = Ui.progress_bar ~completed:50.0 () in
  print_endline "Basic progress bar at 50%:";
  Ui.print bar1;
  print_newline ();

  (* Example 2: Custom width and styling *)
  let bar2 =
    Ui.progress_bar ~completed:75.0 ~width:40
      ~complete_style:Ui.Style.(fg Blue)
      ~bar_style:Ui.Style.(fg White)
      ()
  in
  print_endline "Wider bar at 75% with custom colors:";
  Ui.print bar2;
  print_newline ();

  (* Example 3: Finished bar *)
  let bar3 = Ui.progress_bar ~completed:100.0 () in
  print_endline "Completed bar:";
  Ui.print bar3;
  print_newline ();

  (* Example 4: Pulse animation (indeterminate) *)
  let bar4 = Ui.progress_bar ~total:None ~pulse:true ~animation_time:0.5 () in
  print_endline "Pulse animation (snapshot):";
  Ui.print bar4;
  print_newline ();

  (* Example 5: Different completion levels *)
  let bar5 = Ui.progress_bar ~completed:25.0 () in
  print_endline "Progress at 25%:";
  Ui.print bar5;

  let bar5_updated = Ui.progress_bar ~completed:85.0 () in
  print_endline "Progress at 85%:";
  Ui.print bar5_updated;
  print_newline ();

  (* Example 6: Custom total *)
  let bar6 = Ui.progress_bar ~total:(Some 200.0) ~completed:85.0 () in
  print_endline "Progress bar with custom total (85/200):";
  Ui.print bar6
