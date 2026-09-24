open Windtrap
open Mosaic_ui
open Test_harness

let red = Ansi.Style.fg Ansi.Color.red Ansi.Style.default
let blue = Ansi.Style.fg Ansi.Color.blue Ansi.Style.default

let syntax_style color =
  Syntax_style.make ~base:Ansi.Style.default [ ("keyword", color) ]

let highlights = Syntax_highlight.of_triples [ (0, 3, "keyword") ]

let make_code ?content ?syntax ?text_style () =
  let t = make_ctx () in
  let root = make_root t in
  let code = Code.create ~parent:root ?content ?syntax ?text_style () in
  (t, code)

let render_code code =
  let grid = make_grid ~width:40 ~height:5 () in
  Renderable.Private.render_full (Code.node code) ~grid ~delta:0.

let line_spans code = Text_buffer.line_spans (Code.buffer code) 0
let span_text span = (span : Text_buffer.span).text
let span_style span = (span : Text_buffer.span).style

type async_job = {
  request : Code.Highlighter.request;
  notify : unit -> unit;
  mutable outcome : Code.Highlighter.result option;
  mutable cancelled : bool;
}

let async_highlighter () =
  let jobs = ref [] in
  let highlighter =
    Code.Highlighter.async (fun request ~notify ->
        let job = { request; notify; outcome = None; cancelled = false } in
        jobs := !jobs @ [ job ];
        Code.Highlighter.job
          ~poll:(fun () ->
            match job.outcome with
            | None -> None
            | Some outcome ->
                job.outcome <- None;
                Some outcome)
          ~cancel:(fun () -> job.cancelled <- true))
  in
  (jobs, highlighter)

let complete job outcome =
  job.outcome <- Some outcome;
  job.notify ()

let plain_without_syntax () =
  let _ctx, code = make_code ~content:"let x = 1" () in
  equal ~msg:"plain text" string "let x = 1"
    (Text_buffer.plain_text (Code.buffer code));
  equal ~msg:"span count" int 1 (List.length (line_spans code))

let precomputed_highlights_apply_style () =
  let syntax = Code.syntax ~style:(syntax_style red) highlights in
  let _ctx, code = make_code ~content:"let x = 1" ~syntax () in
  match line_spans code with
  | first :: _ ->
      equal ~msg:"highlighted text" string "let" (span_text first);
      is_true ~msg:"highlighted style" (Ansi.Style.equal red (span_style first))
  | [] -> fail "expected highlighted span"

let sync_highlighter_applies_style () =
  let highlighter =
    Code.Highlighter.sync (fun ~language ~content ->
        equal ~msg:"request content" string "let x" content;
        equal ~msg:"request language" string "ocaml" language;
        highlights)
  in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  is_false ~msg:"not highlighting" (Code.is_highlighting code);
  match line_spans code with
  | first :: _ ->
      equal ~msg:"highlighted text" string "let" (span_text first);
      is_true ~msg:"highlighted style" (Ansi.Style.equal red (span_style first))
  | [] -> fail "expected highlighted span"

let async_highlighter_applies_completed_result () =
  let jobs, highlighter = async_highlighter () in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  is_true ~msg:"highlighting" (Code.is_highlighting code);
  equal ~msg:"plain while pending" string "let x"
    (Text_buffer.plain_text (Code.buffer code));
  let job = List.hd !jobs in
  equal ~msg:"request content" string "let x" job.request.content;
  equal ~msg:"request language" string "ocaml" job.request.language;
  complete job (Ok highlights);
  render_code code;
  is_false ~msg:"done highlighting" (Code.is_highlighting code);
  match line_spans code with
  | first :: _ ->
      is_true ~msg:"highlighted style" (Ansi.Style.equal red (span_style first))
  | [] -> fail "expected highlighted span"

let async_draw_unstyled_false_hides_pending_render () =
  let jobs, highlighter = async_highlighter () in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      ~draw_unstyled:false highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  is_true ~msg:"highlighting" (Code.is_highlighting code);
  equal ~msg:"content remains measurable" string "let x"
    (Text_buffer.plain_text (Code.buffer code));
  is_false ~msg:"render disabled while pending"
    (Text_surface.render_enabled (Code.surface code));
  complete (List.hd !jobs) (Ok highlights);
  render_code code;
  is_true ~msg:"render enabled after highlight"
    (Text_surface.render_enabled (Code.surface code));
  equal ~msg:"content after highlight" string "let x"
    (Text_buffer.plain_text (Code.buffer code))

let async_streaming_retains_previous_buffer () =
  let jobs, highlighter = async_highlighter () in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      ~streaming:true highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  let first_job = List.hd !jobs in
  complete first_job (Ok highlights);
  render_code code;
  equal ~msg:"initial highlighted content" string "let x"
    (Text_buffer.plain_text (Code.buffer code));
  Code.set_content code "var x";
  equal ~msg:"previous content retained" string "let x"
    (Text_buffer.plain_text (Code.buffer code));
  let second_job = List.nth !jobs 1 in
  complete second_job (Ok highlights);
  render_code code;
  equal ~msg:"fresh content after highlight" string "var x"
    (Text_buffer.plain_text (Code.buffer code))

let async_content_change_cancels_previous_job () =
  let jobs, highlighter = async_highlighter () in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  let first_job = List.hd !jobs in
  Code.set_content code "var x";
  is_true ~msg:"first job cancelled" first_job.cancelled;
  complete first_job (Ok highlights);
  render_code code;
  equal ~msg:"current plain text retained" string "var x"
    (Text_buffer.plain_text (Code.buffer code));
  let second_job = List.nth !jobs 1 in
  complete second_job (Ok highlights);
  render_code code;
  match line_spans code with
  | first :: _ ->
      equal ~msg:"highlighted current content" string "var" (span_text first)
  | [] -> fail "expected highlighted span"

let async_line_info_stabilizes_on_completion () =
  let jobs, highlighter = async_highlighter () in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  let changes = ref 0 in
  Code.set_on_line_info_change code (Some (fun () -> incr changes));
  is_false ~msg:"pending line info" (Code.line_info_stable code);
  complete (List.hd !jobs) (Ok highlights);
  equal ~msg:"callback waits for polling" int 0 !changes;
  render_code code;
  is_true ~msg:"stable line info" (Code.line_info_stable code);
  equal ~msg:"line info callback" int 1 !changes

let async_starter_exception_falls_back_to_plain_text () =
  let highlighter =
    Code.Highlighter.async (fun _request ~notify:_ ->
        raise (Failure "starter failed"))
  in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  is_false ~msg:"not highlighting" (Code.is_highlighting code);
  equal ~msg:"plain fallback" string "let x"
    (Text_buffer.plain_text (Code.buffer code));
  is_true ~msg:"render enabled after fallback"
    (Text_surface.render_enabled (Code.surface code))

let async_poll_exception_falls_back_to_plain_text () =
  let highlighter =
    Code.Highlighter.async (fun _request ~notify:_ ->
        Code.Highlighter.job
          ~poll:(fun () -> raise (Failure "poll failed"))
          ~cancel:(fun () -> ()))
  in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  is_false ~msg:"not highlighting" (Code.is_highlighting code);
  equal ~msg:"plain fallback" string "let x"
    (Text_buffer.plain_text (Code.buffer code))

let invalid_highlights_fall_back_to_plain_text () =
  let highlights = Syntax_highlight.of_triples [ (0, 20, "keyword") ] in
  let syntax = Code.syntax ~style:(syntax_style red) highlights in
  let _ctx, code = make_code ~content:"let" ~syntax () in
  equal ~msg:"plain text" string "let"
    (Text_buffer.plain_text (Code.buffer code));
  match line_spans code with
  | [ span ] ->
      is_true ~msg:"default style"
        (Ansi.Style.equal Ansi.Style.default (span_style span))
  | _ -> fail "expected one plain span"

let conceal_metadata_applies () =
  let meta = { Syntax_highlight.default_meta with conceal = Some "*" } in
  let highlights =
    [
      Syntax_highlight.range ~meta ~start_byte:0 ~end_byte:3 ~scope:"keyword" ();
    ]
  in
  let syntax = Code.syntax ~style:(syntax_style red) highlights in
  let _ctx, code = make_code ~content:"let x = 1" ~syntax () in
  equal ~msg:"concealed text" string "* x = 1"
    (Text_buffer.plain_text (Code.buffer code))

let set_content_reapplies_syntax () =
  let syntax = Code.syntax ~style:(syntax_style red) highlights in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  Code.set_content code "var x";
  match line_spans code with
  | first :: _ ->
      equal ~msg:"highlighted text" string "var" (span_text first);
      is_true ~msg:"highlighted style" (Ansi.Style.equal red (span_style first))
  | [] -> fail "expected highlighted span"

let set_syntax_none_renders_plain () =
  let syntax = Code.syntax ~style:(syntax_style red) highlights in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  Code.set_syntax code None;
  match line_spans code with
  | [ span ] ->
      equal ~msg:"plain text" string "let x" (span_text span);
      is_true ~msg:"default style"
        (Ansi.Style.equal Ansi.Style.default (span_style span))
  | _ -> fail "expected one plain span"

let apply_props_updates_syntax_style () =
  let syntax = Code.syntax ~style:(syntax_style red) highlights in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  let syntax = Code.syntax ~style:(syntax_style blue) highlights in
  Code.apply_props code (Code.Props.make ~content:"let x" ~syntax ());
  match line_spans code with
  | first :: _ ->
      is_true ~msg:"updated style" (Ansi.Style.equal blue (span_style first))
  | [] -> fail "expected highlighted span"

let text_style_restamps_plain_content () =
  let _ctx, code = make_code ~content:"plain" () in
  Code.apply_props code (Code.Props.make ~content:"plain" ~text_style:red ());
  match line_spans code with
  | [ span ] ->
      is_true ~msg:"restamped style" (Ansi.Style.equal red (span_style span))
  | _ -> fail "expected one plain span"

let is_highlighting_false_for_sync_highlights () =
  let syntax = Code.syntax ~style:(syntax_style red) highlights in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  is_false ~msg:"not highlighting" (Code.is_highlighting code)

let await label ready =
  let deadline = Unix.gettimeofday () +. 5. in
  while not (ready ()) do
    if Unix.gettimeofday () >= deadline then fail ("timed out: " ^ label);
    Thread.delay 0.001
  done

let join_worker label worker =
  let joined = Atomic.make false in
  ignore
    (Thread.create
       (fun () ->
         Thread.join worker;
         Atomic.set joined true)
       ()
      : Thread.t);
  await label (fun () -> Atomic.get joined)

let gate () =
  let mutex = Mutex.create () in
  let condition = Condition.create () in
  let released = ref false in
  let wait () =
    Mutex.protect mutex (fun () ->
        while not !released do
          Condition.wait condition mutex
        done)
  in
  let release () =
    Mutex.protect mutex (fun () ->
        released := true;
        Condition.broadcast condition)
  in
  (wait, release)

let threaded_requests_are_bounded_and_cancelled () =
  let wait, release = gate () in
  let calls = Atomic.make [] in
  let highlighter =
    Code.Highlighter.threaded (fun ~language ~content ->
        if language <> "ocaml" then failwith "wrong request language";
        Atomic.set calls (content :: Atomic.get calls);
        if content = "let old" then wait ();
        highlights)
  in
  let renderer = Renderer.create () in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let code =
    Code.create ~parent:(Renderer.root renderer) ~content:"let old" ~syntax ()
  in
  Fun.protect ~finally:release (fun () ->
      await "first request started" (fun () -> Atomic.get calls <> []);
      let obsolete = Weak.create 1 in
      let queue_obsolete () =
        let content = String.make 10000 'x' in
        Weak.set obsolete 0 (Some content);
        Code.set_content code content
      in
      queue_obsolete ();
      for i = 1 to 100 do
        Code.set_content code (Printf.sprintf "let edit%d" i)
      done;
      Code.set_content code "let final";
      let discarded =
        Code.create ~parent:(Renderer.root renderer) ~content:"let discarded"
          ~syntax ()
      in
      Renderable.destroy (Code.node discarded);
      let other =
        Code.create ~parent:(Renderer.root renderer) ~content:"let other"
          ~syntax ()
      in
      Thread.delay 0.01;
      equal ~msg:"one executing request, no worker per edit" (list string)
        [ "let old" ] (Atomic.get calls);
      Gc.full_major ();
      is_false ~msg:"cancelled queued source is released"
        (Weak.check obsolete 0);
      release ();
      await "live requests completed" (fun () ->
          render_code code;
          render_code other;
          not (Code.is_highlighting code || Code.is_highlighting other));
      equal ~msg:"only live FIFO requests execute" (list string)
        [ "let old"; "let final"; "let other" ]
        (List.rev (Atomic.get calls));
      equal ~msg:"stale result cannot replace current source" string "let final"
        (Text_buffer.plain_text (Code.buffer code));
      is_false ~msg:"destroyed widget cancelled"
        (Code.is_highlighting discarded))

let threaded_failure_and_idle_restart () =
  let worker = Atomic.make None in
  let highlighter =
    Code.Highlighter.threaded (fun ~language:_ ~content ->
        Atomic.set worker (Some (Thread.self ()));
        if content = "bad" then failwith "highlight failed" else highlights)
  in
  let renderer = Renderer.create () in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let code =
    Code.create ~parent:(Renderer.root renderer) ~content:"bad" ~syntax ()
  in
  await "failure delivered" (fun () ->
      render_code code;
      not (Code.is_highlighting code));
  equal ~msg:"exception falls back to plain text" string "bad"
    (Text_buffer.plain_text (Code.buffer code));
  let first = Option.get (Atomic.get worker) in
  join_worker "idle worker exits" first;
  Code.set_content code "let x";
  await "restarted worker delivered" (fun () ->
      render_code code;
      not (Code.is_highlighting code));
  let second = Option.get (Atomic.get worker) in
  is_false ~msg:"idle worker is replaced on demand"
    (Thread.id first = Thread.id second);
  join_worker "restarted worker exits" second;
  match line_spans code with
  | first :: _ ->
      is_true ~msg:"worker survives an earlier failure"
        (Ansi.Style.equal red (span_style first))
  | [] -> fail "expected highlighted span"

let destroy_cancels_job_once () =
  let cancellations = ref 0 in
  let highlighter =
    Code.Highlighter.async (fun _ ~notify:_ ->
        Code.Highlighter.job
          ~poll:(fun () -> None)
          ~cancel:(fun () -> incr cancellations))
  in
  let syntax = Code.with_highlighter ~language:"ocaml" highlighter in
  let ctx, code = make_code ~content:"let x" ~syntax () in
  Renderable.destroy (Code.node code);
  Renderable.destroy (Code.node code);
  equal ~msg:"cleanup is idempotent" int 1 !cancellations;
  is_false ~msg:"destroyed code has no pending job" (Code.is_highlighting code);
  equal ~msg:"node unregistered" (list string)
    [ Renderable.id (Code.node code) ]
    !(ctx.unregister_log)

let () =
  run "mosaic.code"
    [
      group "Syntax"
        [
          test "plain without syntax" plain_without_syntax;
          test "precomputed highlights apply style"
            precomputed_highlights_apply_style;
          test "sync highlighter applies style" sync_highlighter_applies_style;
          test "async highlighter applies completed result"
            async_highlighter_applies_completed_result;
          test "async draw_unstyled false hides pending render"
            async_draw_unstyled_false_hides_pending_render;
          test "async streaming retains previous buffer"
            async_streaming_retains_previous_buffer;
          test "async content change cancels previous job"
            async_content_change_cancels_previous_job;
          test "async line info stabilizes on completion"
            async_line_info_stabilizes_on_completion;
          test "async starter exception falls back to plain text"
            async_starter_exception_falls_back_to_plain_text;
          test "async poll exception falls back to plain text"
            async_poll_exception_falls_back_to_plain_text;
          test "invalid highlights fall back to plain text"
            invalid_highlights_fall_back_to_plain_text;
          test "conceal metadata applies" conceal_metadata_applies;
        ];
      group "Workers"
        [
          test "bounded queue cancels obsolete work"
            threaded_requests_are_bounded_and_cancelled;
          test "failure and idle restart" threaded_failure_and_idle_restart;
          test "destroy cancels once" destroy_cancels_job_once;
        ];
      group "Updates"
        [
          test "set_content reapplies syntax" set_content_reapplies_syntax;
          test "set_syntax None renders plain" set_syntax_none_renders_plain;
          test "apply_props updates syntax style"
            apply_props_updates_syntax_style;
          test "text_style restamps plain content"
            text_style_restamps_plain_content;
          test "is_highlighting false for sync highlights"
            is_highlighting_false_for_sync_highlights;
        ];
    ]
