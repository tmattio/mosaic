(* A chat application in mentat's shape, for the flow tests: a prompt, a
   turn whose reply a provider streams, a spinner and a clock while the turn
   runs, the window title and a desktop notification for the turn's end, and
   a quit that Ctrl+C arms and a second Ctrl+C confirms. The provider is a
   function given the prompt and a dispatch: the tests hold the dispatch and
   stream the reply themselves; the program streams one. *)

type delta = Text of string | Done

type msg =
  | Typed of string
  | Erase
  | Submit
  | Interrupt
  | Disarm
  | Delta of delta
  | Second
  | Spin
  | Focus of bool

type turn = { prompt : string; reply : string; elapsed : int; done_ : bool }

type model = {
  draft : string;
  turns : turn list; (* newest first *)
  armed : bool;
  focused : bool;
  spin : int;
}

let spinner = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]

let running m =
  match m.turns with t :: _ when not t.done_ -> Some t | _ -> None

let update provider msg m =
  let none = Mosaic.Cmd.none in
  match msg with
  | Typed s -> ({ m with draft = m.draft ^ s; armed = false }, none)
  | Erase ->
      let n = String.length m.draft in
      ( { m with draft = (if n = 0 then "" else String.sub m.draft 0 (n - 1)) },
        none )
  | Submit when m.draft = "" || running m <> None -> (m, none)
  | Submit ->
      let prompt = m.draft in
      ( {
          m with
          draft = "";
          turns = { prompt; reply = ""; elapsed = 0; done_ = false } :: m.turns;
        },
        Mosaic.Cmd.batch
          [
            Mosaic.Cmd.set_title "◐ chat";
            Mosaic.Cmd.perform (fun dispatch ->
                provider prompt (fun d -> dispatch (Delta d)));
          ] )
  | Interrupt ->
      if m.draft <> "" then ({ m with draft = "" }, none)
      else if m.armed then (m, Mosaic.Cmd.quit)
      else ({ m with armed = true }, none)
  | Disarm -> ({ m with armed = false }, none)
  | Delta (Text s) -> (
      match m.turns with
      | t :: rest when not t.done_ ->
          ({ m with turns = { t with reply = t.reply ^ s } :: rest }, none)
      | _ -> (m, none))
  | Delta Done -> (
      match m.turns with
      | t :: rest when not t.done_ ->
          ( { m with turns = { t with done_ = true } :: rest },
            Mosaic.Cmd.batch
              (Mosaic.Cmd.set_title "✳ chat"
              ::
              (if m.focused then []
               else [ Mosaic.Cmd.notify ~title:"chat" ~body:"the turn ended" ])
              ) )
      | _ -> (m, none))
  | Second -> (
      match m.turns with
      | t :: rest when not t.done_ ->
          ({ m with turns = { t with elapsed = t.elapsed + 1 } :: rest }, none)
      | _ -> (m, none))
  | Spin -> ({ m with spin = m.spin + 1 }, none)
  | Focus focused -> ({ m with focused }, none)

let view m =
  let turn t =
    [ "❯ " ^ t.prompt ]
    @ (if t.reply = "" then [] else [ "⏺ " ^ t.reply ])
    @ if t.done_ then [ Printf.sprintf "  Worked for %ds" t.elapsed ] else []
  in
  let working =
    match running m with
    | Some t ->
        [
          Printf.sprintf "%s Working… (%ds · ctrl+c to quit)"
            spinner.(m.spin mod Array.length spinner)
            t.elapsed;
        ]
    | None -> []
  in
  let footer =
    if m.armed then "press ctrl+c again to quit"
    else "enter to send · ctrl+c to quit"
  in
  Mosaic.box ~flex_direction:Column
    (List.map Mosaic.text
       (("chat — message the model" :: List.concat_map turn (List.rev m.turns))
       @ working
       @ [ "> " ^ m.draft; footer ]))

let keys key =
  let k = Mosaic.Event.Key.data key in
  if Matrix.Input.Keystroke.is_interrupt k then (
    Mosaic.Event.Key.prevent_default key;
    Some Interrupt)
  else
    match k.key with
    | Enter -> Some Submit
    | Backspace -> Some Erase
    | Char _ when k.text <> "" -> Some (Typed k.text)
    | _ -> None

let subscriptions m =
  Mosaic.Sub.batch
    ([
       Mosaic.Sub.on_key keys;
       Mosaic.Sub.on_focus (Focus true);
       Mosaic.Sub.on_blur (Focus false);
     ]
    @ (if m.armed then [ Mosaic.Sub.after ~id:"disarm" 2.0 Disarm ] else [])
    @
    match running m with
    | Some _ ->
        [
          Mosaic.Sub.every ~id:"second" 1.0 (fun _ -> Second);
          Mosaic.Sub.every ~id:"spin" 0.1 (fun _ -> Spin);
        ]
    | None -> [])

(* [provider prompt send] streams the reply to [prompt] with [send]. *)
let app provider =
  {
    Mosaic.init =
      (fun () ->
        ( { draft = ""; turns = []; armed = false; focused = true; spin = 0 },
          Mosaic.Cmd.set_title "✳ chat" ));
    update = update provider;
    view;
    subscriptions;
  }
