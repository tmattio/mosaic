open Mosaic

type model = int
type msg = [ `Inc | `Dec ]
type outgoing = unit

let init () = (0, Cmd.none)

let update msg model =
  match msg with
  | `Inc -> (model + 1, Cmd.none, None)
  | `Dec -> (model - 1, Cmd.none, None)

let view model =
  let open Ui in
  hbox ~gap:2
    [ text (Printf.sprintf "Count: %d" model); text "[Press +/- keys]" ]

let subscriptions _model =
  Sub.batch [ Sub.on_char '+' `Inc; Sub.on_char '-' `Dec ]
