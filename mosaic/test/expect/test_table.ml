open Mosaic_ui
open Expect_harness

let cols =
  [
    Table.column "Name";
    Table.column ~width:(`Fixed 5) ~alignment:`Right "Age";
    Table.column "City";
  ]

let rows =
  [
    [| Table.cell "Alice"; Table.cell "30"; Table.cell "NYC" |];
    [| Table.cell "Bob"; Table.cell "25"; Table.cell "London" |];
    [| Table.cell "Charlie"; Table.cell "35"; Table.cell "Paris" |];
  ]

let paging_rows count =
  List.init count (fun index -> [| Table.cell ("row " ^ string_of_int index) |])

let dispatch_mouse app = function
  | Input.Mouse event -> Renderer.dispatch_mouse app.renderer event
  | Input.Key _ | Input.Paste _ | Input.Resize _ | Input.Focus | Input.Blur
  | Input.Error _ ->
      invalid_arg "expected mouse input"

(* ── Basic Rendering ── *)

let%expect_test "basic table with border" =
  render ~width:25 ~height:7 (Vnode.table ~columns:cols ~rows ~border:true ());
  [%expect_exact
    {|
┌───────────────────────┐
│Name      Age City     │
├───────────────────────┤
│Alice      30 NYC      │
│Bob        25 London   │
│Charlie    35 Paris    │
└───────────────────────┘|}]

let%expect_test "table without border" =
  render ~width:25 ~height:4 (Vnode.table ~columns:cols ~rows ~border:false ());
  [%expect_exact
    {|
Name      Age City
Alice      30 NYC
Bob        25 London
Charlie    35 Paris|}]

let%expect_test "table without header" =
  render ~width:25 ~height:3
    (Vnode.table ~columns:cols ~rows ~border:false ~show_header:false ());
  [%expect_exact
    {|
Alice      30 NYC
Bob        25 London
Charlie    35 Paris|}]

(* ── Column Alignment ── *)

let%expect_test "center aligned column" =
  let center_cols =
    [
      Table.column ~alignment:`Center "Name";
      Table.column ~width:(`Fixed 5) ~alignment:`Center "Age";
      Table.column ~alignment:`Center "City";
    ]
  in
  render ~width:25 ~height:4
    (Vnode.table ~columns:center_cols ~rows ~border:false ());
  [%expect_exact
    {|
 Name    Age   City
 Alice   30    NYC
  Bob    25   London
Charlie  35   Paris|}]

let%expect_test "right aligned column" =
  let right_cols =
    [
      Table.column ~alignment:`Right "Name";
      Table.column ~width:(`Fixed 5) ~alignment:`Right "Age";
      Table.column ~alignment:`Right "City";
    ]
  in
  render ~width:25 ~height:4
    (Vnode.table ~columns:right_cols ~rows ~border:false ());
  [%expect_exact
    {|
   Name   Age   City
  Alice    30    NYC
    Bob    25 London
Charlie    35  Paris|}]

(* ── Column Separators ── *)

let%expect_test "column separators" =
  render ~width:25 ~height:4
    (Vnode.table ~columns:cols ~rows ~border:false ~show_column_separator:true
       ());
  [%expect_exact
    {|
Name   │  Age│City
Alice  │   30│NYC
Bob    │   25│London
Charlie│   35│Paris|}]

(* ── Row Separators ── *)

let%expect_test "row separators no border" =
  render ~width:25 ~height:7
    (Vnode.table ~columns:cols ~rows ~border:false ~show_row_separator:true ());
  [%expect_exact
    {|
Name      Age City
Alice      30 NYC
─────────────────────────
Bob        25 London
─────────────────────────
Charlie    35 Paris
|}]

(* ── Empty Table ── *)

let%expect_test "empty table with border" =
  render ~width:20 ~height:5 (Vnode.table ~columns:cols ~border:true ());
  [%expect_exact
    {|
┌──────────────────┐
│Name   Age City   │
├──────────────────┤

└──────────────────┘|}]

let%expect_test "empty columns empty rows" =
  render ~width:20 ~height:3 (Vnode.table ~border:false ());
  [%expect_exact {|


|}]

(* ── Navigation ── *)

let%expect_test "keyboard navigation changes selection" =
  let app = make_app () in
  let node = ref None in
  reconcile app
    (Vnode.table
       ~ref:(fun n -> node := Some n)
       ~columns:cols ~rows ~border:false ());
  focus app (Option.get !node);
  frame app ~width:25 ~height:4;
  send_key app Input.Key.Down;
  frame app ~width:25 ~height:4;
  [%expect_exact
    {|
Name      Age City
Alice      30 NYC
Bob        25 London
Charlie    35 Paris
Name      Age City
Alice      30 NYC
Bob        25 London
Charlie    35 Paris|}]

let%expect_test "page down uses the measured bordered table body" =
  let app = make_app () in
  let node = ref None in
  let changes = ref [] in
  reconcile app
    (Vnode.table
       ~ref:(fun renderable -> node := Some renderable)
       ~columns:[ Table.column ~width:(`Flex 1.) "Name" ]
       ~rows:(paging_rows 12) ~selected_row:1 ~border:true ~show_header:true
       ~show_scroll_indicator:true
       ~on_change:(fun index -> changes := index :: !changes)
       ());
  focus app (Option.get !node);
  frame app ~width:14 ~height:8;
  send_key app Input.Key.Page_down;
  frame app ~width:14 ~height:8;
  Format.printf "\nchanges=%s\n"
    (String.concat "," (List.rev_map string_of_int !changes));
  [%expect_exact
    {|
┌────────────┐
│Name        │
├────────────┤
│row 0      ││
│row 1      ││
│row 2      ││
│row 3      ↓│
└────────────┘
┌────────────┐
│Name        │
├────────────┤
│row 3      ↑│
│row 4      ││
│row 5      ││
│row 6      ↓│
└────────────┘
changes=5
|}]

let%expect_test "page up accounts for measured row separators" =
  let app = make_app () in
  let node = ref None in
  let changes = ref [] in
  reconcile app
    (Vnode.table
       ~ref:(fun renderable -> node := Some renderable)
       ~columns:[ Table.column ~width:(`Flex 1.) "" ]
       ~rows:(paging_rows 12) ~selected_row:8 ~border:false ~show_header:false
       ~show_row_separator:true ~show_scroll_indicator:true
       ~on_change:(fun index -> changes := index :: !changes)
       ());
  focus app (Option.get !node);
  frame app ~width:12 ~height:5;
  send_key app Input.Key.Page_up;
  frame app ~width:12 ~height:5;
  Format.printf "\nchanges=%s\n"
    (String.concat "," (List.rev_map string_of_int !changes));
  [%expect_exact
    {|
row 7      ↑
────────────
row 8      │
────────────
row 9      ↓
row 4      ↑
────────────
row 5      │
────────────
row 6      ↓
changes=5
|}]

let%expect_test "declarative hover and click callbacks report exact rows" =
  let app = make_app () in
  let hovered = ref [] in
  let activated = ref [] in
  reconcile app
    (Vnode.table ~columns:cols ~rows ~border:false ~show_header:false
       ~activate_on_click:true
       ~on_hover:(fun row -> hovered := row :: !hovered)
       ~on_activate:(fun row -> activated := row :: !activated)
       ());
  frame app ~width:25 ~height:3;
  ignore (Renderer.render app.renderer : string);
  dispatch_mouse app (Input.mouse_move 2 1);
  dispatch_mouse app (Input.mouse_press 2 2 Input.Mouse.Left);
  Format.printf "hover=%s activate=%s\n"
    (String.concat ","
       (List.rev_map (Option.fold ~none:"none" ~some:string_of_int) !hovered))
    (String.concat "," (List.rev_map string_of_int !activated));
  [%expect_exact
    {|
Alice      30 NYC
Bob        25 London
Charlie    35 Parishover=1,2 activate=2
|}]

(* ── Reconciliation ── *)

let%expect_test "props change updates data" =
  let app = make_app () in
  reconcile app (Vnode.table ~columns:cols ~rows ~border:false ());
  frame app ~width:25 ~height:4;
  let new_rows =
    [
      [| Table.cell "Xena"; Table.cell "99"; Table.cell "Mars" |];
      [| Table.cell "Yuki"; Table.cell "88"; Table.cell "Moon" |];
    ]
  in
  reconcile app (Vnode.table ~columns:cols ~rows:new_rows ~border:false ());
  frame app ~width:25 ~height:4;
  [%expect_exact
    {|
Name      Age City
Alice      30 NYC
Bob        25 London
Charlie    35 Paris
Name   Age City
Xena    99 Mars
Yuki    88 Moonondon
Charlie    35 Paris|}]

(* ── Text Overflow ── *)

let%expect_test "ellipsis overflow" =
  let overflow_cols =
    [
      Table.column ~overflow:`Ellipsis ~max_width:8 "Name";
      Table.column ~width:(`Fixed 5) ~alignment:`Right "Age";
      Table.column "City";
    ]
  in
  let overflow_rows =
    [
      [| Table.cell "Alexander"; Table.cell "30"; Table.cell "NYC" |];
      [| Table.cell "Bob"; Table.cell "25"; Table.cell "London" |];
    ]
  in
  render ~width:25 ~height:3
    (Vnode.table ~columns:overflow_cols ~rows:overflow_rows ~border:false
       ~show_header:false ());
  [%expect_exact {|
Alexa...    30 NYC
Bob         25 London
|}]

let%expect_test "crop overflow" =
  let crop_cols =
    [
      Table.column ~overflow:`Crop ~max_width:6 "Name";
      Table.column ~width:(`Fixed 5) ~alignment:`Right "Age";
      Table.column "City";
    ]
  in
  let crop_rows =
    [
      [| Table.cell "Alexander"; Table.cell "30"; Table.cell "NYC" |];
      [| Table.cell "Bob"; Table.cell "25"; Table.cell "London" |];
    ]
  in
  render ~width:25 ~height:3
    (Vnode.table ~columns:crop_cols ~rows:crop_rows ~border:false
       ~show_header:false ());
  [%expect_exact {|
Alexan    30 NYC
Bob       25 London
|}]

(* ── Cell Padding ── *)

let%expect_test "cell padding" =
  let pad_cols = [ Table.column "A"; Table.column "B" ] in
  let pad_rows =
    [
      [| Table.cell "1"; Table.cell "2" |]; [| Table.cell "3"; Table.cell "4" |];
    ]
  in
  render ~width:15 ~height:3
    (Vnode.table ~columns:pad_cols ~rows:pad_rows ~border:false
       ~show_header:false ~cell_padding:1 ());
  [%expect_exact {|
 1   2
 3   4
|}]

(* ── Column Min/Max Width ── *)

let%expect_test "column min width" =
  let min_cols = [ Table.column ~min_width:10 "A"; Table.column "B" ] in
  let min_rows = [ [| Table.cell "1"; Table.cell "2" |] ] in
  render ~width:20 ~height:2
    (Vnode.table ~columns:min_cols ~rows:min_rows ~border:false
       ~show_header:false ());
  [%expect_exact {|
1          2
|}]

let%expect_test "column max width" =
  let max_cols = [ Table.column ~max_width:5 "Name"; Table.column "City" ] in
  let max_rows =
    [
      [| Table.cell "Alexander"; Table.cell "NYC" |];
      [| Table.cell "Bob"; Table.cell "London" |];
    ]
  in
  render ~width:15 ~height:3
    (Vnode.table ~columns:max_cols ~rows:max_rows ~border:false
       ~show_header:false ());
  [%expect_exact {|
Al... NYC
Bob   London
|}]

(* ── Alternating Row Styles ── *)

let%expect_test "alternating row styles (layout)" =
  let gray = Ansi.Style.make ~bg:(Ansi.Color.of_rgba 80 80 80 255) () in
  let alt_rows =
    [
      [| Table.cell "Alice"; Table.cell "001" |];
      [| Table.cell "Bob"; Table.cell "002" |];
      [| Table.cell "Carol"; Table.cell "003" |];
      [| Table.cell "Dave"; Table.cell "004" |];
    ]
  in
  render ~width:15 ~height:5
    (Vnode.table
       ~columns:[ Table.column "Name"; Table.column "ID" ]
       ~rows:alt_rows ~border:false
       ~row_styles:[ Ansi.Style.default; gray ]
       ());
  [%expect_exact {|
Name  ID
Alice 001
Bob   002
Carol 003
Dave  004|}]

(* ── Flex Columns ── *)

let%expect_test "flex columns" =
  let flex_cols =
    [
      Table.column ~width:(`Fixed 4) "ID";
      Table.column ~width:(`Flex 1.) "Name";
      Table.column ~width:(`Flex 2.) "Desc";
    ]
  in
  let flex_rows =
    [
      [| Table.cell "1"; Table.cell "Alpha"; Table.cell "First item" |];
      [| Table.cell "2"; Table.cell "Beta"; Table.cell "Second item" |];
    ]
  in
  render ~width:36 ~height:3
    (Vnode.table ~columns:flex_cols ~rows:flex_rows ~border:false
       ~show_header:false ());
  [%expect_exact {|
1    Alpha      First item
2    Beta       Second item
|}]

(* ── Headers Only ── *)

let%expect_test "headers only no data" =
  render ~width:20 ~height:4
    (Vnode.table
       ~columns:[ Table.column "A"; Table.column "B" ]
       ~border:true ());
  [%expect_exact
    {|
┌──────────────────┐
│A B               │
├──────────────────┤
└──────────────────┘|}]

(* ── Viewport Discoverability ── *)

let%expect_test "scroll indicator follows the actual viewport" =
  let rows =
    List.init 6 (fun index -> [| Table.cell ("row " ^ string_of_int index) |])
  in
  render ~width:12 ~height:3
    (Vnode.table
       ~columns:[ Table.column ~width:(`Flex 1.) "" ]
       ~rows ~selected_row:0 ~border:false ~show_header:false
       ~show_scroll_indicator:true ());
  [%expect_exact {|
row 0      │
row 1      │
row 2      ↓|}]

let%expect_test "scroll indicator reports rows above the viewport" =
  let rows =
    List.init 6 (fun index -> [| Table.cell ("row " ^ string_of_int index) |])
  in
  render ~width:12 ~height:3
    (Vnode.table
       ~columns:[ Table.column ~width:(`Flex 1.) "" ]
       ~rows ~selected_row:5 ~border:false ~show_header:false
       ~show_scroll_indicator:true ());
  [%expect_exact {|
row 3      ↑
row 4      │
row 5      │|}]

let%expect_test "one-row viewport reports overflow in both directions" =
  let rows =
    List.init 6 (fun index -> [| Table.cell ("row " ^ string_of_int index) |])
  in
  render ~width:12 ~height:1
    (Vnode.table
       ~columns:[ Table.column ~width:(`Flex 1.) "" ]
       ~rows ~selected_row:3 ~border:false ~show_header:false
       ~show_scroll_indicator:true ());
  [%expect_exact {|
row 3      ↕|}]

(* ── Row Separators with Border ── *)

let%expect_test "row separators with border" =
  let small_rows =
    [
      [| Table.cell "1"; Table.cell "2" |]; [| Table.cell "3"; Table.cell "4" |];
    ]
  in
  render ~width:15 ~height:7
    (Vnode.table
       ~columns:[ Table.column "A"; Table.column "B" ]
       ~rows:small_rows ~border:true ~show_row_separator:true ());
  [%expect_exact
    {|
┌─────────────┐
│A B          │
├─────────────┤
│1 2          │
├─────────────┤
│3 4          │
└─────────────┘|}]
