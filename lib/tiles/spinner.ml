(** Implementation of the spinner component *)

open Mosaic

(* Spinner style type *)
type spinner_style = { frames : string array; interval : float }

(* Predefined spinner styles *)
let dots =
  {
    frames = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |];
    interval = 0.08;
  }

let line = { frames = [| "-"; "\\"; "|"; "/" |]; interval = 0.13 }

let arrow =
  { frames = [| "←"; "↖"; "↑"; "↗"; "→"; "↘"; "↓"; "↙" |]; interval = 0.1 }

let box_bounce = { frames = [| "▖"; "▘"; "▝"; "▗" |]; interval = 0.12 }
let circle = { frames = [| "◐"; "◓"; "◑"; "◒" |]; interval = 0.12 }
let square = { frames = [| "◰"; "◳"; "◲"; "◱" |]; interval = 0.12 }
let triangle = { frames = [| "◢"; "◣"; "◤"; "◥" |]; interval = 0.15 }

let bar =
  { frames = [| "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉"; "█" |]; interval = 0.08 }

let pulse =
  {
    frames =
      [|
        "▁";
        "▂";
        "▃";
        "▄";
        "▅";
        "▆";
        "▇";
        "█";
        "▇";
        "▆";
        "▅";
        "▄";
        "▃";
        "▂";
        "▁";
      |];
    interval = 0.06;
  }

let bounce = { frames = [| "⠁"; "⠂"; "⠄"; "⠂" |]; interval = 0.12 }

(* Model *)
type model = {
  spinner_style : spinner_style;
  color : Style.t;
  frame_index : int;
  is_spinning : bool;
}

(* Messages *)
type msg = Tick

(* Custom spinner creation *)
let custom frames interval = { frames; interval }

let from_string s =
  let len = String.length s in
  let frames = Array.init len (fun i -> String.sub s i 1) in
  { frames; interval = 0.1 }

(* Initialization *)
let init ?(style = dots) ?(color = Style.empty) () =
  let model =
    { spinner_style = style; color; frame_index = 0; is_spinning = true }
  in
  (* Start ticking immediately *)
  let cmd = Cmd.tick style.interval (fun _ -> Tick) in
  (model, cmd)

(* Update *)
let update msg model =
  match msg with
  | Tick ->
      if model.is_spinning then
        let frame_count = Array.length model.spinner_style.frames in
        let next_index = (model.frame_index + 1) mod frame_count in
        let model' = { model with frame_index = next_index } in
        (* Schedule next tick *)
        let cmd = Cmd.tick model.spinner_style.interval (fun _ -> Tick) in
        (model', cmd)
      else (model, Cmd.none)

(* View *)
let view model =
  let open Ui in
  let frame = model.spinner_style.frames.(model.frame_index) in
  text ~style:model.color frame

(* Subscriptions *)
let subscriptions _ = Sub.none

(* Component export *)
let component = Mosaic.app ~init ~update ~view ~subscriptions ()

(* Control functions *)
let start model =
  if not model.is_spinning then
    let model' = { model with is_spinning = true } in
    let cmd = Cmd.tick model.spinner_style.interval (fun _ -> Tick) in
    (model', cmd)
  else (model, Cmd.none)

let stop model = { model with is_spinning = false }
let is_spinning model = model.is_spinning

(* Customization *)
let set_style style model =
  {
    model with
    spinner_style = style;
    frame_index = 0 (* Reset to first frame *);
  }

let set_color color model = { model with color }
