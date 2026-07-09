open Windtrap

(* Proves the concurrent driver pattern a deterministic Eio test harness
   builds on matrix.test: the Matrix loop runs in one fiber, blocked inside
   [on_idle] on an [Eio.Condition]; the test script drives it from another
   fiber; an asynchronous fiber (a completed perform, in TEA terms) wakes the
   quiescent loop through [Matrix.request_redraw]'s wake channel. Runs under
   [Eio_mock.Backend]: single domain, deterministic scheduling.

   The condition-variable discipline matters: [signaled] is set before the
   broadcast and re-checked in a loop, so a wake that lands while the loop is
   between draining and awaiting is never lost. On a single domain the loop
   fiber only yields inside [Eio.Condition.await_no_mutex], so observing a
   fresh [idles] increment from the script implies the loop is parked and the
   presented frame is stable. *)

type driver = {
  mutable backend : Matrix_test.t option;
  cond : Eio.Condition.t;
  mutable signaled : bool;
  mutable idles : int;
  mutable stopping : bool;
}

let make_driver ~width ~height =
  let driver =
    {
      backend = None;
      cond = Eio.Condition.create ();
      signaled = false;
      idles = 0;
      stopping = false;
    }
  in
  let on_idle _t ~timeout:_ =
    driver.idles <- driver.idles + 1;
    while (not driver.signaled) && not driver.stopping do
      Eio.Condition.await_no_mutex driver.cond
    done;
    driver.signaled <- false
  in
  let wake () =
    driver.signaled <- true;
    Eio.Condition.broadcast driver.cond
  in
  let backend = Matrix_test.create ~on_idle ~on_wake:wake ~width ~height () in
  driver.backend <- Some backend;
  driver

let backend driver = Option.get driver.backend

(* Block the script fiber until the loop parks at a quiescent point newer
   than [beyond]. *)
let await_idle driver ~beyond =
  while driver.idles <= beyond do
    Eio.Fiber.yield ()
  done

let feed driver bytes =
  Matrix_test.feed (backend driver) bytes;
  driver.signaled <- true;
  Eio.Condition.broadcast driver.cond

let stop driver =
  driver.stopping <- true;
  (* [Matrix.stop] raises the wake channel, which unparks [on_idle]. *)
  Matrix_test.stop (backend driver)

let run_typed_app driver ~script =
  let t = backend driver in
  let typed = Buffer.create 8 in
  let frames = ref 0 in
  let loop () =
    Matrix.run (Matrix_test.app t)
      ~on_input:(fun _app event ->
        match event with
        | Matrix.Input.Key { key = Matrix.Input.Key.Char u; _ }
          when Uchar.is_char u ->
            Buffer.add_char typed (Uchar.to_char u)
        | _ -> ())
      ~on_render:(fun app ->
        incr frames;
        Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0
          ~text:("typed:" ^ Buffer.contents typed))
  in
  Eio.Fiber.both loop (fun () -> script ~frames)

let test_input_crosses_fibers () =
  Eio_mock.Backend.run @@ fun () ->
  let driver = make_driver ~width:24 ~height:2 in
  run_typed_app driver ~script:(fun ~frames:_ ->
      await_idle driver ~beyond:0;
      equal ~msg:"initial frame presented" string "typed:"
        (String.trim (Matrix_test.screen (backend driver)));
      let before = driver.idles in
      feed driver "hi";
      await_idle driver ~beyond:before;
      equal ~msg:"input crossed fibers and re-rendered" string "typed:hi"
        (String.trim (Matrix_test.screen (backend driver)));
      stop driver)

let test_async_wake_reaches_quiescent_loop () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let driver = make_driver ~width:24 ~height:2 in
  run_typed_app driver ~script:(fun ~frames ->
      await_idle driver ~beyond:0;
      let settled_frames = !frames in
      let before = driver.idles in
      (* A completed perform calls [Matrix.request_redraw] from its own
         fiber; the wake channel must unpark the loop. *)
      Eio.Fiber.fork ~sw (fun () ->
          Matrix.request_redraw (Matrix_test.app (backend driver)));
      await_idle driver ~beyond:before;
      equal ~msg:"async wake produced exactly one frame" int
        (settled_frames + 1) !frames;
      stop driver)

let () =
  run "matrix-eio.driver"
    [
      group "Concurrent driver"
        [
          test "input crosses fibers" test_input_crosses_fibers;
          test "async wake reaches a quiescent loop"
            test_async_wake_reaches_quiescent_loop;
        ];
    ]
