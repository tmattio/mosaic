open Mosaic_ui

let render_boxed ?(width = 20) ?(height = 1) element =
  let content =
    box ~id:"outer" ~border:true
      ~size:(size ~width:(width + 2) ~height:(height + 2))
      [ element ]
  in
  print_newline ();
  print ~colors:false ~width:(width + 2) ~height:(height + 2) content

let%expect_test "empty text input shows placeholder" =
  render_boxed ~width:20 ~height:1
    (text_input ~id:"ti" ~placeholder:"Type here..."
       ~size:(size ~width:20 ~height:1) ());
  [%expect_exact
    {|
┌────────────────────┐
│Type here...        │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "text input with value" =
  render_boxed ~width:20 ~height:1
    (text_input ~id:"ti" ~value:"Hello world"
       ~size:(size ~width:20 ~height:1) ());
  [%expect_exact
    {|
┌────────────────────┐
│Hello world         │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "text input empty without placeholder" =
  render_boxed ~width:15 ~height:1
    (text_input ~id:"ti" ~size:(size ~width:15 ~height:1) ());
  [%expect_exact
    {|
┌───────────────┐
│               │
└───────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "text input truncated placeholder" =
  render_boxed ~width:10 ~height:1
    (text_input ~id:"ti" ~placeholder:"This is a very long placeholder"
       ~size:(size ~width:10 ~height:1) ());
  [%expect_exact
    {|
┌──────────┐
│This is a │
└──────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "long value with cursor at end scrolls to show end" =
  render_boxed ~width:12 ~height:1
    (text_input ~id:"ti" ~value:"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
       ~size:(size ~width:12 ~height:1) ());
  [%expect_exact
    {|
┌────────────┐
│QRSTUVWXYZ  │
└────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "long value with cursor at start scrolls to show start" =
  render_boxed ~width:12 ~height:1
    (text_input ~id:"ti" ~value:"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
       ~on_mount:(fun ti -> Text_input.set_cursor ti 0)
       ~size:(size ~width:12 ~height:1) ());
  [%expect_exact
    {|
┌────────────┐
│ABCDEFGHIJK │
└────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "long value with cursor in middle scrolls to show cursor" =
  render_boxed ~width:12 ~height:1
    (text_input ~id:"ti" ~value:"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
       ~on_mount:(fun ti -> Text_input.set_cursor ti 13)
       ~size:(size ~width:12 ~height:1) ());
  [%expect_exact
    {|
┌────────────┐
│DEFGHIJKLMN │
└────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "set_value updates content" =
  render_boxed ~width:15 ~height:1
    (text_input ~id:"ti" ~value:"Initial"
       ~on_mount:(fun ti -> Text_input.set_value ti "Changed")
       ~size:(size ~width:15 ~height:1) ());
  [%expect_exact
    {|
┌───────────────┐
│Changed        │
└───────────────┘
|}] [@@ocamlformat "disable"]

(* Interaction tests *)

let%expect_test "set_value with cursor preserves cursor position when valid" =
  render_boxed ~width:15 ~height:1
    (text_input ~id:"ti" ~value:"Hello"
       ~on_mount:(fun ti ->
         Text_input.set_cursor ti 2;
         Text_input.set_value ti "World")
       ~size:(size ~width:15 ~height:1) ());
  [%expect_exact
    {|
┌───────────────┐
│World          │
└───────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "set_value clamps cursor to new length" =
  render_boxed ~width:15 ~height:1
    (text_input ~id:"ti" ~value:"LongText"
       ~on_mount:(fun ti ->
         (* cursor at end = 8, then set shorter value *)
         Text_input.set_value ti "Hi")
       ~size:(size ~width:15 ~height:1) ());
  [%expect_exact
    {|
┌───────────────┐
│Hi             │
└───────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "cursor at position 0 shows start of long text" =
  render_boxed ~width:12 ~height:1
    (text_input ~id:"ti" ~value:"0123456789ABCDEF"
       ~on_mount:(fun ti -> Text_input.set_cursor ti 0)
       ~size:(size ~width:12 ~height:1) ());
  [%expect_exact
    {|
┌────────────┐
│0123456789A │
└────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "cursor at end shows end of long text" =
  render_boxed ~width:12 ~height:1
    (text_input ~id:"ti" ~value:"0123456789ABCDEF"
       ~size:(size ~width:12 ~height:1) ());
  (* cursor defaults to end *)
  [%expect_exact
    {|
┌────────────┐
│6789ABCDEF  │
└────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "cursor movement scrolls viewport" =
  render_boxed ~width:10 ~height:1
    (text_input ~id:"ti" ~value:"ABCDEFGHIJKLMNOP"
       ~on_mount:(fun ti ->
         (* Start at end, move cursor to position 5 *)
         Text_input.set_cursor ti 5)
       ~size:(size ~width:10 ~height:1) ());
  [%expect_exact
    {|
┌──────────┐
│ABCDEFGHI │
└──────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "set_placeholder updates placeholder text" =
  render_boxed ~width:20 ~height:1
    (text_input ~id:"ti" ~placeholder:"Original"
       ~on_mount:(fun ti -> Text_input.set_placeholder ti "Updated placeholder")
       ~size:(size ~width:20 ~height:1) ());
  [%expect_exact
    {|
┌────────────────────┐
│Updated placeholder │
└────────────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "set_max_length truncates existing content" =
  render_boxed ~width:15 ~height:1
    (text_input ~id:"ti" ~value:"Hello World"
       ~on_mount:(fun ti -> Text_input.set_max_length ti 5)
       ~size:(size ~width:15 ~height:1) ());
  [%expect_exact
    {|
┌───────────────┐
│Hello          │
└───────────────┘
|}] [@@ocamlformat "disable"]

let%expect_test "max_length prevents longer initial value" =
  render_boxed ~width:15 ~height:1
    (text_input ~id:"ti" ~value:"TooLong" ~max_length:3
       ~size:(size ~width:15 ~height:1) ());
  (* max_length is applied after set_value, so initial value is not truncated *)
  [%expect_exact
    {|
┌───────────────┐
│TooLong        │
└───────────────┘
|}] [@@ocamlformat "disable"]
