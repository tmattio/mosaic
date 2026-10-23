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
  wheel_down : string;
  wheel_up : string;
  mutable alternate : bool;
}

let initial_width = 250
let initial_height = 70

(* In [`Primary] mode the application is an inline region under 20 rows of
   earlier output, so its live region is shorter than the terminal: its scroll
   box takes 40 of the terminal's 70 rows. *)
let primary_viewport_height = 40

let earlier_output =
  Mosaic.fragment
    (List.init 20 (fun i ->
         Mosaic.text (Printf.sprintf "earlier output %02d" i)))

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

let transcript_view blocks = Mosaic.memo ~key:blocks blocks_view

let view ~mode blocks model =
  let size, flex_grow, flex_basis, flex_shrink, scroll_size =
    match mode with
    | `Alt ->
        ( Mosaic.size_wh (Mosaic.pct 100) (Mosaic.pct 100),
          Some 1.,
          Some (Mosaic.px 0),
          None,
          None )
    | `Primary ->
        ( Mosaic.size_wh (Mosaic.pct 100) Mosaic.auto,
          None,
          None,
          Some 0.,
          Some
            (Mosaic.size_wh (Mosaic.pct 100)
               (Mosaic.px primary_viewport_height)) )
  in
  Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column ~size
    [
      Mosaic.scroll_box ?flex_grow ?flex_basis ?flex_shrink ?size:scroll_size
        ~min_size:(Mosaic.size ~width:0 ~height:0)
        ~scroll:model.scroll
        ~on_scroll:(fun next -> Scroll next)
        ~show_scrollbars:false
        [ transcript_view blocks ];
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

(* Eight wheel events over the transcript's second row, [row] counting the
   terminal's rows from 1. *)
let wheel ~button ~row =
  String.concat ""
    (List.init 8 (fun _ -> Printf.sprintf "\027[<%d;2;%dM" button row))

let step workload driver =
  driver.alternate <- not driver.alternate;
  (match workload with
  | Keystroke ->
      Matrix_test.feed driver.terminal
        (if driver.alternate then "x" else "\127")
  | Wheel_burst ->
      Matrix_test.feed driver.terminal
        (if driver.alternate then driver.wheel_down else driver.wheel_up)
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

(* Runs [application] over [t], the benchmark driving its steps at [t]'s
   clock: [on_idle] runs whenever the program has nothing to do and nothing
   has come for it, given the run's deadline. *)
let drive ?process_perform ~on_idle t application =
  Mosaic.start ~input:(Matrix_test.input t) ~output:(Matrix_test.output t)
    ~config:(Matrix_test.config t) ?process_perform application (fun r ->
      let rec loop () =
        match Mosaic.step r ~now:(Matrix_test.now t) with
        | `Stepped -> loop ()
        | `Stopped -> ()
        | `Idle ->
            (match Mosaic.deadline r with
            | Some deadline when deadline <= Matrix_test.now t -> ()
            | until ->
                if not (List.exists Matrix.Tty.readable (Mosaic.fds r)) then
                  on_idle t ~until);
            loop ()
      in
      loop ())

let setup ~mode ~blocks workload () =
  let continuation : (unit, unit) Effect.Deep.continuation option ref =
    ref None
  in
  let accepted =
    ref { editor = Mosaic.Editor.of_string "a"; scroll = Mosaic.Scroll.at 7500 }
  in
  let terminal =
    Matrix_test.create ~mode ~width:initial_width ~height:initial_height ()
  in
  let on_idle terminal ~until =
    match until with
    | Some until ->
        Matrix_test.advance terminal (max 1 (until - Matrix_test.now terminal))
    | None -> Effect.perform Presented
  in
  let application =
    {
      Mosaic.init =
        (fun () ->
          ( !accepted,
            match mode with
            | `Alt -> none
            | `Primary -> Mosaic.Cmd.static_commit earlier_output ));
      update =
        (fun message model ->
          let model =
            match message with
            | Edit editor -> { model with editor }
            | Scroll scroll -> { model with scroll }
          in
          accepted := model;
          (model, none));
      view = view ~mode blocks;
      subscriptions = (fun _ -> Mosaic.Sub.none);
    }
  in
  Effect.Deep.match_with
    (fun () -> drive ~on_idle terminal application)
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
  (* The live region ends at the terminal's last row. *)
  let top = initial_height - snd (Matrix_test.size terminal) in
  require
    (top > 0 = (mode = `Primary))
    "only a primary-mode live region is shorter than the terminal";
  let viewport_height =
    match mode with
    | `Alt -> initial_height - 1
    | `Primary -> primary_viewport_height
  in
  let driver =
    {
      terminal;
      continuation;
      wheel_down = wheel ~button:65 ~row:(top + 2);
      wheel_up = wheel ~button:64 ~row:(top + 2);
      alternate = false;
    }
  in
  (* Validate both directions before measuring. Captured ANSI and frame text
     are deliberately inspected here, never copied in the measured loop. *)
  for _ = 1 to 2 do
    let before = Matrix_test.screen terminal in
    let written = String.length (Matrix_test.written terminal) in
    step workload driver;
    require
      (not (String.equal before (Matrix_test.screen terminal)))
      "input did not change the presented frame";
    require
      (String.length (Matrix_test.written terminal) > written)
      "changed frame emitted no ANSI output";
    match workload with
    | Keystroke ->
        require
          (Mosaic.Editor.text !accepted.editor
          = if driver.alternate then "ax" else "a")
          "editor proposal was not accepted"
    | Wheel_burst ->
        let _, y =
          Mosaic.Scroll.resolve !accepted.scroll ~width:250
            ~height:viewport_height ~content_width:250
            ~content_height:(blocks * 3)
        in
        require
          (y = if driver.alternate then 7508 else 7500)
          (Printf.sprintf "wheel burst resolved to %d" y)
    | Resize ->
        let grid = Matrix_test.grid terminal in
        require
          (Matrix.Grid.width grid = if driver.alternate then 249 else 250)
          "resize width was not presented";
        require
          (Matrix.Grid.height grid = if driver.alternate then 69 else 70)
          "resize height was not presented"
  done;
  driver

(* The loop's own costs, on a small program: the run suspends whenever
   [pause] says so when the loop goes idle, and a sample resumes it. *)

type loop = {
  t : Matrix_test.t;
  suspended : (unit, unit) Effect.Deep.continuation option ref;
  dispatch : (unit -> unit) ref;
  views : int ref;
}

let loop_resume loop =
  match !(loop.suspended) with
  | None -> failwith "runtime benchmark: the loop is not suspended"
  | Some k ->
      loop.suspended := None;
      Effect.Deep.continue k ();
      if !(loop.suspended) = None then
        failwith "runtime benchmark: loop ended before it went idle"

let loop_close loop =
  Matrix_test.stop loop.t;
  match !(loop.suspended) with
  | Some k ->
      loop.suspended := None;
      Effect.Deep.continue k ()
  | None -> ()

(* A program that counts keys, over a 40x2 terminal; its messages from
   elsewhere change nothing. [pause] runs at each idle turn with no deadline
   left. *)
let loop_setup ~pause () =
  let suspended : (unit, unit) Effect.Deep.continuation option ref = ref None in
  let dispatch = ref ignore in
  let views = ref 0 in
  let t = Matrix_test.create ~width:40 ~height:2 () in
  let on_idle t ~until =
    match until with
    | Some until -> Matrix_test.advance t (max 1 (until - Matrix_test.now t))
    | None -> if pause () then Effect.perform Presented
  in
  let application =
    {
      Mosaic.init =
        (fun () ->
          ( 0,
            Mosaic.Cmd.perform (fun m ->
                dispatch := fun () -> m.dispatch `Nothing) ));
      update =
        (fun msg n ->
          match msg with `Key -> (n + 1, none) | `Nothing -> (n, none));
      view =
        (fun n ->
          incr views;
          Mosaic.text (string_of_int n));
      subscriptions = (fun _ -> Mosaic.Sub.on_key (fun _ -> Some `Key));
    }
  in
  Effect.Deep.match_with
    (fun () ->
      drive ~on_idle ~process_perform:(fun work -> work ()) t application)
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
                  suspended := Some k)
          | _ -> None);
    };
  { t; suspended; dispatch; views }

(* The frames written in [output] from [from]: each begins by hiding the
   cursor. *)
let paints ~from output =
  let hide = "\027[?25l" in
  let n = String.length hide in
  let count = ref 0 in
  for i = from to String.length output - n do
    if String.sub output i n = hide then incr count
  done;
  !count

(* 10,000 idle turns a sample: the loop waits, finds nothing, waits again. *)
let idle_turns () =
  let turns = ref 0 in
  let loop =
    loop_setup
      ~pause:(fun () ->
        incr turns;
        !turns mod 10_000 = 0)
      ()
  in
  require (!(loop.suspended) <> None) "the loop did not go idle";
  loop

(* 1,000 keys in one read, applied, viewed and painted once. *)
let burst () =
  let loop = loop_setup ~pause:(fun () -> true) () in
  let views = !(loop.views) in
  let written = String.length (Matrix_test.written loop.t) in
  Matrix_test.feed loop.t (String.make 1000 'a');
  loop_resume loop;
  require (!(loop.views) - views = 1000) "a burst is not 1,000 views";
  require
    (paints ~from:written (Matrix_test.written loop.t) = 1)
    "a burst is not one paint";
  loop

let burst_sample loop =
  Matrix_test.feed loop.t (String.make 1000 'a');
  loop_resume loop

(* 1,000 messages posted from elsewhere, taken and applied: they change
   nothing, so nothing is viewed or painted. *)
let posts loop =
  for _ = 1 to 1000 do
    !(loop.dispatch) ()
  done;
  loop_resume loop

let loop_benches =
  Thumper.group "loop"
    [
      Thumper.bench_with_setup ~setup:idle_turns ~teardown:loop_close
        "idle-turns-10000" loop_resume;
      Thumper.bench_with_setup ~setup:burst ~teardown:loop_close
        "burst-1000-keys" burst_sample;
      Thumper.bench_with_setup
        ~setup:(loop_setup ~pause:(fun () -> true))
        ~teardown:loop_close "posts-1000" posts;
    ]

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
       transcript repeats the input workloads. The primary-mode group runs them
       in an inline region shorter than the terminal. *)
    (loop_benches
    :: List.map
         (fun (mode, blocks, workloads) ->
           Thumper.group
             (match mode with
             | `Alt -> Printf.sprintf "transcript-%d" blocks
             | `Primary -> Printf.sprintf "primary-transcript-%d" blocks)
             (List.map
                (fun (name, workload) ->
                  Thumper.bench_with_setup
                    ~setup:(setup ~mode ~blocks workload)
                    ~teardown:close name (measure workload))
                workloads))
         [
           ( `Alt,
             5000,
             [
               ("keystroke-to-present", Keystroke);
               ("wheel-roundtrip-2x8", Wheel_burst);
               ("resize-250x70", Resize);
             ] );
           ( `Alt,
             20000,
             [
               ("keystroke-to-present", Keystroke);
               ("wheel-roundtrip-2x8", Wheel_burst);
             ] );
           ( `Primary,
             5000,
             [
               ("keystroke-to-present", Keystroke);
               ("wheel-roundtrip-2x8", Wheel_burst);
             ] );
         ])
