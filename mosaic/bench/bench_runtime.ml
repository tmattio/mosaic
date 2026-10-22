(* Suspend only at Matrix_test's presented idle boundary. Mounting and the
   preflight oracle run in setup; a sample resumes the real parser/update/view/
   layout/paint/submit loop and returns at the next presented idle. *)
type _ Effect.t += Presented : unit Effect.t
type model = { editor : Mosaic.Editor.t; scroll : Mosaic.Scroll.t }
type msg = Edit of Mosaic.Editor.t | Scroll of Mosaic.Scroll.t
type workload = Keystroke | Wheel_burst | Resize

type driver = {
  terminal : Matrix_test.t;
  continuation : (unit, unit) Effect.Deep.continuation option ref;
  mutable alternate : bool;
}

let initial_width = 250
let initial_height = 70
let none = Mosaic.Cmd.none

let block i =
  Mosaic.box ~key:(string_of_int i) ~flex_direction:Mosaic.Flex_direction.Column
    ~flex_shrink:0.
    [
      Mosaic.text (Printf.sprintf "message %04d" i);
      Mosaic.text
        "The worker completed its checkpoint; the next request can reuse the \
         retained result.";
      Mosaic.text "";
    ]

let blocks_view blocks =
  Mosaic.fragment (List.init blocks (fun i -> Mosaic.memo ~key:i block))

let transcript blocks = Mosaic.memo ~key:blocks blocks_view

let view blocks model =
  Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column
    ~size:(Mosaic.size_wh (Mosaic.pct 100) (Mosaic.pct 100))
    [
      Mosaic.scroll_box ~flex_grow:1. ~flex_basis:(Mosaic.px 0)
        ~min_size:(Mosaic.size ~width:0 ~height:0)
        ~scroll:model.scroll
        ~on_scroll:(fun next -> Scroll next)
        ~show_scrollbars:false
        [ transcript blocks ];
      Mosaic.input
        ~size:(Mosaic.size_wh (Mosaic.pct 100) (Mosaic.px 1))
        ~flex_shrink:0. ~focused:true ~state:model.editor
        ~on_change:(fun next -> Edit next)
        ();
    ]

let resume driver =
  match !(driver.continuation) with
  | None -> failwith "runtime benchmark: missing presented continuation"
  | Some continuation ->
      driver.continuation := None;
      Effect.Deep.continue continuation ()

let wheel_down = String.concat "" (List.init 8 (fun _ -> "\027[<65;2;2M"))
let wheel_up = String.concat "" (List.init 8 (fun _ -> "\027[<64;2;2M"))

let step workload driver =
  driver.alternate <- not driver.alternate;
  (match workload with
  | Keystroke ->
      Matrix_test.feed driver.terminal
        (if driver.alternate then "x" else "\127")
  | Wheel_burst ->
      Matrix_test.feed driver.terminal
        (if driver.alternate then wheel_down else wheel_up)
  | Resize ->
      let width, height = if driver.alternate then (249, 69) else (250, 70) in
      Matrix_test.resize driver.terminal ~width ~height);
  resume driver;
  if !(driver.continuation) = None then
    failwith "runtime benchmark: loop ended before presentation"

(* Each wheel sample returns to its starting viewport. Alternating directions
   in separate samples makes allocation evidence depend on sample parity. *)
let measure workload driver =
  step workload driver;
  match workload with
  | Wheel_burst -> step workload driver
  | Keystroke | Resize -> ()

let close driver =
  Matrix_test.stop driver.terminal;
  resume driver

let require condition message =
  if not condition then failwith ("runtime benchmark: " ^ message)

let setup ~blocks workload () =
  let continuation : (unit, unit) Effect.Deep.continuation option ref =
    ref None
  in
  let accepted =
    ref { editor = Mosaic.Editor.of_string "a"; scroll = Mosaic.Scroll.at 7500 }
  in
  let terminal =
    Matrix_test.create ~width:initial_width ~height:initial_height
      ~on_idle:(fun terminal ~timeout ->
        match timeout with
        | Some delay ->
            Matrix_test.set_now terminal
              (Matrix_test.now terminal +. max 0.000001 delay)
        | None -> Effect.perform Presented)
      ()
  in
  let application =
    {
      Mosaic.init = (fun () -> (!accepted, none));
      update =
        (fun message model ->
          let model =
            match message with
            | Edit editor -> { model with editor }
            | Scroll scroll -> { model with scroll }
          in
          accepted := model;
          (model, none));
      view = view blocks;
      subscriptions = (fun _ -> Mosaic.Sub.none);
    }
  in
  Effect.Deep.match_with
    (fun () -> Mosaic.run ~matrix:(Matrix_test.app terminal) application)
    ()
    {
      retc = (fun () -> ());
      exnc = raise;
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Presented ->
              Some
                (fun (k : (a, unit) Effect.Deep.continuation) ->
                  continuation := Some k)
          | _ -> None);
    };
  let driver = { terminal; continuation; alternate = false } in
  (* Validate both directions before measuring. Captured ANSI and frame text
     are deliberately inspected here, never copied in the measured loop. *)
  for _ = 1 to 2 do
    let before = Matrix_test.screen terminal in
    let written = String.length (Matrix_test.output terminal) in
    step workload driver;
    require
      (not (String.equal before (Matrix_test.screen terminal)))
      "input did not change the presented frame";
    require
      (String.length (Matrix_test.output terminal) > written)
      "changed frame emitted no ANSI output";
    match workload with
    | Keystroke ->
        require
          (Mosaic.Editor.text !accepted.editor
          = if driver.alternate then "ax" else "a")
          "editor proposal was not accepted"
    | Wheel_burst ->
        let _, y =
          Mosaic.Scroll.resolve !accepted.scroll ~width:250 ~height:69
            ~content_width:250 ~content_height:(blocks * 3)
        in
        require
          (y = if driver.alternate then 7508 else 7500)
          (Printf.sprintf "wheel burst resolved to %d" y)
    | Resize ->
        let grid = Matrix.current_grid (Matrix_test.app terminal) in
        require
          (Matrix.Grid.width grid = if driver.alternate then 249 else 250)
          "resize width was not presented";
        require
          (Matrix.Grid.height grid = if driver.alternate then 69 else 70)
          "resize height was not presented"
  done;
  driver

let () =
  let metrics =
    if Sys.getenv_opt "RUNTIME_TIMING" = Some "1" then
      Thumper.Metric.[ alloc_words; wall_time; promoted_words ]
    else [ Thumper.Metric.alloc_words ]
  in
  Thumper.run "runtime"
    ~config:
      (Thumper.Config.default
      |> Thumper.Config.deadline 60.
      |> Thumper.Config.metrics metrics)
    ~budgets:[ Thumper.Budget.no_more_alloc_than 0.05 ]
    (* Per-input cost must not grow with mounted content: the 20,000-block
       transcript repeats the input workloads. *)
    (List.map
       (fun (blocks, workloads) ->
         Thumper.group
           (Printf.sprintf "transcript-%d" blocks)
           (List.map
              (fun (name, workload) ->
                Thumper.bench_with_setup ~setup:(setup ~blocks workload)
                  ~teardown:close name (measure workload))
              workloads))
       [
         ( 5000,
           [
             ("keystroke-to-present", Keystroke);
             ("wheel-roundtrip-2x8", Wheel_burst);
             ("resize-250x70", Resize);
           ] );
         ( 20000,
           [
             ("keystroke-to-present", Keystroke);
             ("wheel-roundtrip-2x8", Wheel_burst);
           ] );
       ])
