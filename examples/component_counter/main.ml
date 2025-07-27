open Mosaic
module Counter = Counter

type model = { top_counter : Counter.model; bottom_counter : Counter.model }
type msg = Top of Counter.msg | Bottom of Counter.msg | Reset | Quit

let top_counter_inst =
  Component.connect
    (module Counter)
    ~get:(fun m -> m.top_counter)
    ~set:(fun c m -> { m with top_counter = c })
    ~wrap:(fun child_msg -> Top child_msg)

let bottom_counter_inst =
  Component.connect
    (module Counter)
    ~get:(fun m -> m.bottom_counter)
    ~set:(fun c m -> { m with bottom_counter = c })
    ~wrap:(fun child_msg -> Bottom child_msg)

let init () =
  let m1, _ = Counter.init () in
  let m2, _ = Counter.init () in
  ({ top_counter = m1; bottom_counter = m2 }, Cmd.none)

let update msg model =
  match msg with
  | Top child_msg ->
      let model, cmd, _outgoing = top_counter_inst.update child_msg model in
      (model, cmd)
  | Bottom child_msg ->
      let model, cmd, _outgoing = bottom_counter_inst.update child_msg model in
      (model, cmd)
  | Reset ->
      let m, cmd = init () in
      (m, Cmd.batch [ cmd; Cmd.log "Counters were reset!" ])
  | Quit -> (model, Cmd.quit)

let view model =
  let open Ui in
  vbox ~gap:1
    [
      text ~style:Style.(fg (Index 14)) "Component Counter Demo";
      spacer 1;
      text "Top Counter:";
      top_counter_inst.view model;
      spacer 1;
      text "Bottom Counter:";
      bottom_counter_inst.view model;
      spacer 2;
      hbox ~gap:2
        [
          text ~style:Style.(fg (Index 8)) "[r] Reset";
          text ~style:Style.(fg (Index 8)) "[q/Ctrl+C] Quit";
        ];
    ]

let subscriptions model =
  Sub.batch
    [
      top_counter_inst.subscriptions model;
      bottom_counter_inst.subscriptions model;
      Sub.on_char 'r' Reset;
      Sub.on_char 'q' Quit;
      Sub.on_key ~ctrl:true (Char (Uchar.of_char 'c')) Quit;
    ]

let app = Mosaic.app ~init ~update ~view ~subscriptions ()
let () = Mosaic.run app
