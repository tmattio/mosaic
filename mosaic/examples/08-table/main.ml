(** Data tables with columns, headers, and styling. *)

open Mosaic_tea

type box_style = Rounded | Heavy | Double | Ascii | Minimal
type model = { style : box_style }
type msg = Cycle_style | Quit

let init () = ({ style = Rounded }, Cmd.none)

let update msg model =
  match msg with
  | Cycle_style ->
      let style =
        match model.style with
        | Rounded -> Heavy
        | Heavy -> Double
        | Double -> Ascii
        | Ascii -> Minimal
        | Minimal -> Rounded
      in
      ({ style }, Cmd.none)
  | Quit -> (model, Cmd.quit)

let style_to_prop = function
  | Rounded -> Table.Rounded
  | Heavy -> Heavy
  | Double -> Double
  | Ascii -> Ascii
  | Minimal -> Minimal

let style_name = function
  | Rounded -> "Rounded"
  | Heavy -> "Heavy"
  | Double -> "Double"
  | Ascii -> "ASCII"
  | Minimal -> "Minimal"

let columns =
  [
    Table.column ~header:(Table.cell "Host") ~width:(`Fixed 16) ~justify:`Left
      "host";
    Table.column ~header:(Table.cell "Region") ~width:(`Fixed 12) ~justify:`Left
      "region";
    Table.column ~header:(Table.cell "CPU") ~width:(`Fixed 6) ~justify:`Right
      "cpu";
    Table.column ~header:(Table.cell "Memory") ~width:(`Fixed 8) ~justify:`Right
      "memory";
    Table.column ~header:(Table.cell "Status") ~width:(`Fixed 10)
      ~justify:`Center "status";
  ]

let rows =
  [
    Table.row
      [
        Table.cell "web-prod-01";
        Table.cell "us-east-1";
        Table.cell "12%";
        Table.cell "4.2 GB";
        Table.cell ~style:(Ansi.Style.make ~fg:Ansi.Color.green ()) "Healthy";
      ];
    Table.row
      [
        Table.cell "web-prod-02";
        Table.cell "us-east-1";
        Table.cell "45%";
        Table.cell "6.8 GB";
        Table.cell ~style:(Ansi.Style.make ~fg:Ansi.Color.green ()) "Healthy";
      ];
    Table.row
      [
        Table.cell "api-prod-01";
        Table.cell "eu-west-1";
        Table.cell "78%";
        Table.cell "12.1 GB";
        Table.cell ~style:(Ansi.Style.make ~fg:Ansi.Color.yellow ()) "Warning";
      ];
    Table.row
      [
        Table.cell "db-primary";
        Table.cell "us-east-1";
        Table.cell "34%";
        Table.cell "28.4 GB";
        Table.cell ~style:(Ansi.Style.make ~fg:Ansi.Color.green ()) "Healthy";
      ];
    Table.row
      [
        Table.cell "cache-01";
        Table.cell "ap-south-1";
        Table.cell "8%";
        Table.cell "1.9 GB";
        Table.cell ~style:(Ansi.Style.make ~fg:Ansi.Color.red ()) "Critical";
      ];
  ]

(* Palette *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let footer_bg = Ansi.Color.grayscale ~level:3
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()

let view model =
  box ~flex_direction:Column
    ~size:{ width = pct 100; height = pct 100 }
    [
      (* Header *)
      box ~padding:(padding 1) ~background:header_bg
        [
          box ~flex_direction:Row ~justify_content:Space_between
            ~align_items:Center
            ~size:{ width = pct 100; height = auto }
            [
              text ~style:(Ansi.Style.make ~bold:true ()) "▸ Table";
              text ~style:muted "▄▀ mosaic";
            ];
        ];
      (* Content *)
      box ~flex_grow:1. ~align_items:Center ~justify_content:Center
        [
          box ~flex_direction:Column ~gap:(gap 2)
            [
              (* Table with current style *)
              table ~columns ~rows
                ~box_style:(style_to_prop model.style)
                ~show_header:true ~show_edge:true ~show_lines:true
                ~table_padding:(1, 1, 1, 1)
                ~header_style:(Ansi.Style.make ~bold:true ())
                ~row_styles:
                  [
                    Ansi.Style.default;
                    Ansi.Style.make ~bg:(Ansi.Color.grayscale ~level:3) ();
                  ]
                ();
              (* Style indicator *)
              text ~style:hint
                (Printf.sprintf "Style: %s" (style_name model.style));
            ];
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [ text ~style:hint "s cycle style  •  q quit" ];
    ]

let subscriptions _model =
  Sub.on_key (fun ev ->
      match (Mosaic_ui.Event.Key.data ev).key with
      | Char c when Uchar.equal c (Uchar.of_char 's') -> Some Cycle_style
      | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
      | Escape -> Some Quit
      | _ -> None)

let () = run { init; update; view; subscriptions }
