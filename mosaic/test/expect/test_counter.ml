open Mosaic_tea

type model = { count : int }
type msg = Inc | Dec

let init () = ({ count = 0 }, Cmd.none)

let update msg model =
  match msg with
  | Inc -> ({ count = model.count + 1 }, Cmd.none)
  | Dec -> ({ count = model.count - 1 }, Cmd.none)

let subscriptions _model =
  Sub.on_key (fun event ->
      let kev = Mosaic_ui.Event.Key.data event in
      match kev.key with
      | Input.Key.Char c when Uchar.to_char c = '+' -> Some Inc
      | Input.Key.Char c when Uchar.to_char c = '-' -> Some Dec
      | _ -> None)

let view (m : model) : msg t =
  box
    ~size:(size ~width:100 ~height:100)
    [ text (Printf.sprintf "Count: %d" m.count) ]

let%expect_test "tea counter: initial render" =
  let t = Harness.create ~width:12 ~height:1 ~init ~update ~view ~subscriptions () in
  print_endline (Harness.step t ~delta:0.0);
  [%expect_exact {|Count: 0    
|}]
[@@ocamlformat "disable"]

let%expect_test "tea counter: key events update model and rerender" =
  let t = Harness.create ~width:12 ~height:1 ~init ~update ~view ~subscriptions () in
  Harness.send_key t (Harness.char '+');
  Harness.send_key t (Harness.char '+');
  Harness.send_key t (Harness.char '-');
  print_endline (Harness.step t ~delta:0.0);
  [%expect_exact {|Count: 1    
|}]
[@@ocamlformat "disable"]

let%expect_test "tea counter: resize triggers layout and snapshot changes" =
  let t = Harness.create ~width:10 ~height:2 ~init ~update ~view ~subscriptions () in
  print_endline (Harness.step t ~delta:0.0);
  Harness.resize t ~width:14 ~height:3;
  Harness.tick t ~delta:0.0;
  print_endline "--- after resize ---";
  print_endline (Harness.step t ~delta:0.0);
  [%expect_exact {|Count: 0  
          
--- after resize ---
Count: 0      
              
              
|}]
[@@ocamlformat "disable"]
