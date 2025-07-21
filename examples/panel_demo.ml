open Mosaic

let () =
  let open Ui in
  (* Simple panel with default settings *)
  let simple_panel = panel (text "Hello, World!") in

  (* Panel with title *)
  let titled_panel =
    panel (text "This is the content of the panel") ~title:"Panel Title"
  in

  (* Panel with title and subtitle *)
  let full_panel =
    panel
      (vbox ~gap:1
         [
           text "Line 1 of content";
           text "Line 2 of content";
           text "Line 3 of content";
         ])
      ~title:"Header Title" ~subtitle:"Footer Subtitle" ~title_align:`Center
      ~subtitle_align:`Right
  in

  (* Panel with custom border style *)
  let double_border_panel =
    panel
      (text "Content with double border")
      ~box_style:Double ~title:"Double Border"
      ~border_style:Style.(fg Blue)
  in

  (* Fitted panel (doesn't expand) *)
  let fitted_panel =
    panel (text "Compact content") ~title:"Fitted Panel" ~expand:false
  in

  (* Panel with padding *)
  let padded_panel =
    panel (text "Padded content") ~title:"With Padding" ~padding:(padding_all 2)
  in

  (* Nested panels *)
  let nested_panels =
    panel
      (vbox ~gap:1
         [
           panel (text "Inner panel 1") ~title:"First";
           panel (text "Inner panel 2") ~title:"Second" ~box_style:ASCII;
         ])
      ~title:"Outer Panel" ~box_style:Thick
  in

  (* Display all examples *)
  let demo =
    vbox ~gap:2
      [
        text ~style:Style.(bold ++ fg Yellow) "Panel Examples:";
        simple_panel;
        titled_panel;
        full_panel;
        double_border_panel;
        fitted_panel;
        padded_panel;
        nested_panels;
      ]
  in

  Ui.print demo
