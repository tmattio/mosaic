open Ui

let () =
  (* Helper to create tree nodes with guide_style *)
  let node ?guide_style ~label ~expanded children =
    { Ui.label; expanded; children; guide_style }
  in

  let demo =
    vbox ~gap:2
      [
        text ~style:Style.(bold ++ fg Blue) "Mosaic UI Primitives Demo";
        separator ();
        (* Progress bars *)
        text ~style:Style.bold "Progress Bars:";
        progress_bar ~completed:0.0 ();
        progress_bar ~completed:0.15 ();
        progress_bar ~completed:0.33 ();
        progress_bar ~completed:0.5 ();
        progress_bar ~completed:0.67 ();
        progress_bar ~completed:0.85 ();
        progress_bar ~completed:1.0 ();
        progress_bar ~completed:0.42 ~width:40 ~bar_style:Style.(fg Green) ();
        separator ();
        (* Checkboxes and Radio buttons *)
        text ~style:Style.bold "Checkboxes and Radio Buttons:";
        checkbox ~checked:true ~label:"Enable notifications" ();
        checkbox ~checked:false ~label:"Show hidden files" ();
        radio ~checked:true ~label:"Option A" ();
        radio ~checked:false ~label:"Option B" ();
        radio ~checked:false ~label:"Option C" ();
        separator ();
        (* Lists *)
        text ~style:Style.bold "Lists:";
        list
          ~items:
            [
              text "First item";
              text "Second item";
              text "Third item with a longer description";
            ]
          ();
        list ~numbering:true
          ~items:
            [
              text "Step one: Initialize the project";
              text "Step two: Install dependencies";
              text "Step three: Run the application";
            ]
          ();
        separator ();
        (* Table *)
        text ~style:Style.bold "Table:";
        table ~headers:[ "Name"; "Age"; "City" ]
          ~rows:
            [
              [ "Alice"; "25"; "New York" ];
              [ "Bob"; "30"; "San Francisco" ];
              [ "Charlie"; "28"; "Chicago" ];
              [ "Diana"; "35"; "Los Angeles" ];
            ]
          ~border:rounded_border ();
        separator ();
        (* ASCII Art / Image *)
        text ~style:Style.bold "ASCII Art:";
        image
          ~lines:
            [
              "    ╭─────╮    ";
              "   │ ●   ● │   ";
              "   │   ▼   │   ";
              "   │  ───  │   ";
              "    ╰─────╯    ";
            ]
          ~align:`Center ();
        separator ();
        (* Vertical separator demo *)
        text ~style:Style.bold "Layout with Vertical Separator:";
        hbox ~gap:2
          [
            vbox
              [
                text "Left Panel";
                text "- Item 1";
                text "- Item 2";
                text "- Item 3";
              ];
            separator ~orientation:`Vertical ();
            vbox
              [
                text "Right Panel";
                text "- Item A";
                text "- Item B";
                text "- Item C";
              ];
          ];
        separator ();
        (* Tree view demo *)
        text ~style:Style.bold "Tree View:";
        (let file_tree =
           node
             ~label:(text ~style:Style.(fg Yellow) "project/")
             ~expanded:true
             [
               node
                 ~label:(text ~style:Style.(fg Yellow) "src/")
                 ~expanded:true
                 [
                   node ~label:(text "main.ml") ~expanded:false [];
                   node ~label:(text "utils.ml") ~expanded:false [];
                   node
                     ~label:(text ~style:Style.(fg Yellow) "lib/")
                     ~expanded:true
                     [
                       node ~label:(text "core.ml") ~expanded:false [];
                       node ~label:(text "helpers.ml") ~expanded:false [];
                     ];
                 ];
               node
                 ~label:(text ~style:Style.(fg Yellow) "tests/")
                 ~expanded:false
                 [
                   node ~label:(text "test_main.ml") ~expanded:false [];
                   node ~label:(text "test_utils.ml") ~expanded:false [];
                 ];
               node ~label:(text "README.md") ~expanded:false [];
               node ~label:(text "dune-project") ~expanded:false [];
             ]
         in
         tree file_tree);
        separator ();
        (* Another tree example with different styling *)
        text ~style:Style.bold "Component Tree (Collapsed):";
        (let component_tree =
           node
             ~label:(text ~style:Style.(bold ++ fg Blue) "App")
             ~expanded:true
             [
               node ~label:(text "Header") ~expanded:false
                 [
                   node ~label:(text "Logo") ~expanded:false [];
                   node ~label:(text "Navigation") ~expanded:false [];
                 ];
               node ~label:(text "MainContent") ~expanded:true
                 [
                   node ~label:(text "Sidebar") ~expanded:false [];
                   node ~label:(text "ContentArea") ~expanded:false [];
                 ];
               node ~label:(text "Footer") ~expanded:false [];
             ]
         in
         tree ~style:Style.(fg (gray 12)) component_tree);
        separator ();
        (* Tree with hide_root option *)
        text ~style:Style.bold "Tree with hidden root:";
        (let hidden_root_tree =
           node ~label:(text "Hidden Root") ~expanded:true
             [
               node ~label:(text "Visible Child 1") ~expanded:false [];
               node ~label:(text "Visible Child 2") ~expanded:true
                 [ node ~label:(text "Grandchild") ~expanded:false [] ];
             ]
         in
         tree ~hide_root:true hidden_root_tree);
        separator ();
        (* Tree with ASCII guide style *)
        text ~style:Style.bold "Tree with ASCII guides:";
        (let ascii_tree =
           node ~label:(text "ASCII Tree") ~expanded:true
             [
               node ~label:(text "Branch 1") ~expanded:false [];
               node ~label:(text "Branch 2") ~expanded:true
                 [ node ~label:(text "Leaf") ~expanded:false [] ];
             ]
         in
         tree ~guides:Ui.ASCII ascii_tree);
        separator ();
        (* Tree with bold guide style *)
        text ~style:Style.bold "Tree with bold guides:";
        (let bold_tree =
           node ~label:(text "Bold Tree") ~expanded:true
             [
               node ~label:(text "Strong Branch") ~expanded:false [];
               node ~label:(text "Another Branch") ~expanded:false [];
             ]
         in
         tree ~guides:Ui.Bold ~guide_style:Style.(fg Red) bold_tree);
        separator ();
        (* Tree with double guide style *)
        text ~style:Style.bold "Tree with double guides:";
        (let double_tree =
           node ~label:(text "Double Tree") ~expanded:true
             [
               node ~label:(text "Double Branch 1") ~expanded:false [];
               node ~label:(text "Double Branch 2") ~expanded:false [];
             ]
         in
         tree ~guides:Ui.Double ~guide_style:Style.(fg Blue) double_tree);
        separator ();
        (* Tree with expanded override *)
        text ~style:Style.bold "Tree with all nodes expanded (override):";
        (let override_tree =
           node ~label:(text "Root")
             ~expanded:false (* This will be overridden *)
             [
               node ~label:(text "Child 1")
                 ~expanded:false (* This will be overridden *)
                 [ node ~label:(text "Grandchild") ~expanded:false [] ];
               node ~label:(text "Child 2") ~expanded:false [];
             ]
         in
         tree ~expanded:true override_tree);
        separator ();
        (* Tree without root connector *)
        text ~style:Style.bold "File Browser:";
        (let simple_tree =
           node
             ~label:(text ~style:Style.(bold) "Files")
             ~expanded:true
             [
               node ~label:(text "document.txt") ~expanded:false [];
               node ~label:(text "image.png") ~expanded:false [];
               node
                 ~label:(text ~style:Style.(fg Yellow) "folder/")
                 ~expanded:true
                 [ node ~label:(text "nested.txt") ~expanded:false [] ];
             ]
         in
         tree simple_tree);
      ]
  in

  (* Render to screen *)
  print ~width:80 demo
