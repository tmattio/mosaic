open Test_utils

let%expect_test "tree - simple tree" =
  let tree_node = 
    Ui.{ Tree.label = text "Root"; expanded = true; children = []; guide_style = None }
  in
  print_ui ~width:20 ~height:1 (Ui.tree tree_node);
  [%expect_exact {|
┌--------------------┐
|Root                |
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "tree - tree with children" =
  let tree_node = 
    Ui.{
      Tree.label = text "Parent";
      expanded = true;
      children = [
        { Tree.label = text "Child 1"; expanded = false; children = []; guide_style = None };
        { Tree.label = text "Child 2"; expanded = false; children = []; guide_style = None };
      ];
      guide_style = None
    }
  in
  print_ui ~width:20 ~height:3 (Ui.tree tree_node);
  [%expect_exact {|
┌--------------------┐
|Parent              |
|├── Child 1         |
|└── Child 2         |
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "tree - nested tree" =
  let tree_node = 
    Ui.{
      Tree.label = text "Root";
      expanded = true;
      children = [
        { 
          Tree.label = text "Branch 1";
          expanded = true;
          children = [
            { Tree.label = text "Leaf 1.1"; expanded = false; children = []; guide_style = None };
            { Tree.label = text "Leaf 1.2"; expanded = false; children = []; guide_style = None };
          ];
          guide_style = None
        };
        { Tree.label = text "Branch 2"; expanded = false; children = []; guide_style = None };
      ];
      guide_style = None
    }
  in
  print_ui ~width:20 ~height:6 (Ui.tree tree_node);
  [%expect_exact {|
┌--------------------┐
|Root                |
|├── Branch 1        |
|│   ├── Leaf 1.1    |
|│   └── Leaf 1.2    |
|└── Branch 2        |
|                    |
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "tree - collapsed branch" =
  let tree_node = 
    Ui.{
      Tree.label = text "Root";
      expanded = true;
      children = [
        { 
          Tree.label = text "Collapsed";
          expanded = false;
          children = [
            { Tree.label = text "Hidden 1"; expanded = false; children = []; guide_style = None };
            { Tree.label = text "Hidden 2"; expanded = false; children = []; guide_style = None };
          ];
          guide_style = None
        };
        { Tree.label = text "Visible"; expanded = false; children = []; guide_style = None };
      ];
      guide_style = None
    }
  in
  print_ui ~width:20 ~height:3 (Ui.tree tree_node);
  [%expect_exact {|
┌--------------------┐
|Root                |
|├── Collapsed       |
|└── Visible         |
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "tree - hide root" =
  let tree_node = 
    Ui.{
      Tree.label = text "Hidden Root";
      expanded = true;
      children = [
        { Tree.label = text "Item 1"; expanded = false; children = []; guide_style = None };
        { Tree.label = text "Item 2"; expanded = false; children = []; guide_style = None };
      ];
      guide_style = None
    }
  in
  print_ui ~width:20 ~height:2 (Ui.tree ~hide_root:true tree_node);
  [%expect_exact {|
┌--------------------┐
|├── Item 1          |
|└── Item 2          |
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "tree - ASCII guides" =
  let tree_node = 
    Ui.{
      Tree.label = text "Root";
      expanded = true;
      children = [
        { Tree.label = text "Child 1"; expanded = false; children = []; guide_style = None };
        { Tree.label = text "Child 2"; expanded = false; children = []; guide_style = None };
      ];
      guide_style = None
    }
  in
  print_ui ~width:20 ~height:3 (Ui.tree ~guides:Ui.Tree.ASCII tree_node);
  [%expect_exact {|
┌--------------------┐
|Root                |
|+-- Child 1         |
|`-- Child 2         |
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "tree - Bold guides" =
  let tree_node = 
    Ui.{
      Tree.label = text "Root";
      expanded = true;
      children = [
        { Tree.label = text "Child 1"; expanded = false; children = []; guide_style = None };
        { Tree.label = text "Child 2"; expanded = false; children = []; guide_style = None };
      ];
      guide_style = None
    }
  in
  print_ui ~width:20 ~height:3 (Ui.tree ~guides:Ui.Tree.Bold tree_node);
  [%expect_exact {|
┌--------------------┐
|Root                |
|┣━━ Child 1         |
|┗━━ Child 2         |
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "tree - Double guides" =
  let tree_node = 
    Ui.{
      Tree.label = text "Root";
      expanded = true;
      children = [
        { Tree.label = text "Child 1"; expanded = false; children = []; guide_style = None };
        { Tree.label = text "Child 2"; expanded = false; children = []; guide_style = None };
      ];
      guide_style = None
    }
  in
  print_ui ~width:20 ~height:3 (Ui.tree ~guides:Ui.Tree.Double tree_node);
  [%expect_exact {|
┌--------------------┐
|Root                |
|╠══ Child 1         |
|╚══ Child 2         |
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "tree - styled Tree.labels" =
  let tree_node = 
    Ui.{
      Tree.label = text ~style:Style.(fg Yellow ++ bold) "Styled Root";
      expanded = true;
      children = [
        { 
          Tree.label = text ~style:Style.(fg Green) "Green Child";
          expanded = false;
          children = [];
          guide_style = None
        };
        { 
          Tree.label = text ~style:Style.(fg Blue) "Blue Child";
          expanded = false;
          children = [];
          guide_style = None
        };
      ];
      guide_style = None
    }
  in
  print_ui ~width:20 ~height:3 (Ui.tree tree_node);
  [%expect_exact {|
┌--------------------┐
|Styled Root         |
|├── Green Child     |
|└── Blue Child      |
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "tree - guide style" =
  let tree_node = 
    Ui.{
      Tree.label = text "Root";
      expanded = true;
      children = [
        { Tree.label = text "Child 1"; expanded = false; children = []; guide_style = None };
        { Tree.label = text "Child 2"; expanded = false; children = []; guide_style = None };
      ];
      guide_style = None
    }
  in
  print_ui ~width:20 ~height:3 (Ui.tree ~guide_style:Ui.Style.(fg Blue) tree_node);
  [%expect_exact {|
┌--------------------┐
|Root                |
|├── Child 1         |
|└── Child 2         |
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "tree - per-node guide style" =
  let tree_node = 
    Ui.{
      Tree.label = text "Root";
      expanded = true;
      children = [
        { 
          Tree.label = text "Special";
          expanded = false;
          children = [];
          guide_style = Some Style.(fg Red)
        };
        { 
          Tree.label = text "Normal";
          expanded = false;
          children = [];
          guide_style = None
        };
      ];
      guide_style = None
    }
  in
  print_ui ~width:20 ~height:3 (Ui.tree tree_node);
  [%expect_exact {|
┌--------------------┐
|Root                |
|├── Special         |
|└── Normal          |
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "tree - deep nesting" =
  let tree_node = 
    Ui.{
      Tree.label = text "Level 1";
      expanded = true;
      children = [
        { 
          Tree.label = text "Level 2";
          expanded = true;
          children = [
            { 
              Tree.label = text "Level 3";
              expanded = true;
              children = [
                { Tree.label = text "Level 4"; expanded = false; children = []; guide_style = None };
              ];
              guide_style = None
            };
          ];
          guide_style = None
        };
      ];
      guide_style = None
    }
  in
  print_ui ~width:20 ~height:4 (Ui.tree tree_node);
  [%expect_exact {|
┌--------------------┐
|Level 1             |
|└── Level 2         |
|    └── Level 3     |
|        └── Level 4 |
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "tree - multiple branches" =
  let tree_node = 
    Ui.{
      Tree.label = text "Project";
      expanded = true;
      children = [
        { 
          Tree.label = text "src/";
          expanded = true;
          children = [
            { Tree.label = text "main.ml"; expanded = false; children = []; guide_style = None };
            { Tree.label = text "utils.ml"; expanded = false; children = []; guide_style = None };
          ];
          guide_style = None
        };
        { 
          Tree.label = text "test/";
          expanded = true;
          children = [
            { Tree.label = text "test_main.ml"; expanded = false; children = []; guide_style = None };
          ];
          guide_style = None
        };
        { Tree.label = text "README.md"; expanded = false; children = []; guide_style = None };
      ];
      guide_style = None
    }
  in
  print_ui ~width:25 ~height:8 (Ui.tree tree_node);
  [%expect_exact {|
┌-------------------------┐
|Project                  |
|├── src/                 |
|│   ├── main.ml          |
|│   └── utils.ml         |
|├── test/                |
|│   └── test_main.ml     |
|└── README.md            |
|                         |
└-------------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "tree - empty tree" =
  let tree_node = 
    Ui.{ Tree.label = text "Empty"; expanded = true; children = []; guide_style = None }
  in
  print_ui ~width:20 ~height:1 (Ui.tree tree_node);
  [%expect_exact {|
┌--------------------┐
|Empty               |
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "tree - override expanded state" =
  let tree_node = 
    Ui.{
      Tree.label = text "Root";
      expanded = false; (* This is false but will be overridden *)
      children = [
        { 
          Tree.label = text "Branch";
          expanded = false; (* This is also false *)
          children = [
            { Tree.label = text "Leaf"; expanded = false; children = []; guide_style = None };
          ];
          guide_style = None
        };
      ];
      guide_style = None
    }
  in
  print_ui ~width:20 ~height:4 (Ui.tree ~expanded:true tree_node);
  [%expect_exact {|
┌--------------------┐
|Root                |
|└── Branch          |
|    └── Leaf        |
|                    |
└--------------------┘
|}] [@@ocamlformat "disable"]

let%expect_test "tree - non-text elements" =
  let tree_node = 
    Ui.{
      Tree.label = hbox ~gap:(`Cells 1) [text "["; text ~style:Style.(fg Green) "✓"; text "]"; text "Done"];
      expanded = true;
      children = [
        { 
          Tree.label = hbox ~gap:(`Cells 1) [text "["; text " "; text "]"; text "Todo"];
          expanded = false;
          children = [];
          guide_style = None
        };
      ];
      guide_style = None
    }
  in
  print_ui ~width:20 ~height:2 (Ui.tree tree_node);
  [%expect_exact {|
┌--------------------┐
|[ ✓ ] Done          |
|└── [   ] Todo      |
└--------------------┘
|}] [@@ocamlformat "disable"]
