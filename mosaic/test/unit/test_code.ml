open Windtrap
open Mosaic_ui
open Test_harness

let red = Ansi.Style.fg Ansi.Color.red Ansi.Style.default
let blue = Ansi.Style.fg Ansi.Color.blue Ansi.Style.default

let syntax_style color =
  Syntax_style.make ~base:Ansi.Style.default [ ("keyword", color) ]

let highlights =
  Syntax_highlight.of_triples ~content:"let x" [ (0, 3, "keyword") ]

let make_code ?content ?syntax ?text_style () =
  let t = make_ctx () in
  let root = make_root t in
  let code = Code.create ~parent:root ?content ?syntax ?text_style () in
  (t, code)

let render_code code =
  let grid = make_grid ~width:40 ~height:5 () in
  Renderable.Private.render_full (Code.node code) ~grid ~delta:0.

let styled_text code = Text_buffer.styled_text (Code.buffer code)
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
  equal ~msg:"span count" int 1 (List.length (styled_text code))

let precomputed_highlights_apply_style () =
  let syntax = Code.syntax ~style:(syntax_style red) highlights in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  match styled_text code with
  | first :: _ ->
      equal ~msg:"highlighted text" string "let" (span_text first);
      is_true ~msg:"highlighted style" (Ansi.Style.equal red (span_style first))
  | [] -> fail "expected highlighted span"

let sync_highlighter_applies_style () =
  let highlighter =
    Code.Highlighter.sync (fun ~language ~content ->
        equal ~msg:"request content" string "let x" content;
        equal ~msg:"request language" string "ocaml" language;
        Syntax_highlight.of_triples ~content [ (0, 3, "keyword") ])
  in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  is_false ~msg:"not highlighting" (Code.is_highlighting code);
  match styled_text code with
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
  match styled_text code with
  | first :: _ ->
      is_true ~msg:"highlighted style" (Ansi.Style.equal red (span_style first))
  | [] -> fail "expected highlighted span"

let style_of code byte = fst (Text_buffer.style_at (Code.buffer code) byte)
let is_red code byte = Ansi.Style.equal red (style_of code byte)

let keyword_at content start_byte end_byte =
  Syntax_highlight.of_triples ~content [ (start_byte, end_byte, "keyword") ]

let pending_append_keeps_highlighted_prefix () =
  let jobs, highlighter = async_highlighter () in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  complete (List.hd !jobs) (Ok highlights);
  render_code code;
  Code.set_content code "let x = 1";
  is_true ~msg:"highlighting" (Code.is_highlighting code);
  equal ~msg:"current content shown" string "let x = 1"
    (Text_buffer.plain_text (Code.buffer code));
  is_true ~msg:"shared prefix keeps its styling" (is_red code 0);
  is_false ~msg:"appended tail is plain" (is_red code 8);
  let second = List.nth !jobs 1 in
  complete second (Ok (keyword_at second.request.content 6 7));
  render_code code;
  is_false ~msg:"done" (Code.is_highlighting code);
  is_false ~msg:"fresh result replaces carried styling" (is_red code 0);
  is_true ~msg:"fresh result applied" (is_red code 6)

let pending_edit_keeps_shared_prefix_and_suffix () =
  let jobs, highlighter = async_highlighter () in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"let x in y" ~syntax () in
  complete (List.hd !jobs)
    (Ok
       (Syntax_highlight.of_triples ~content:"let x in y"
          [ (0, 3, "keyword"); (4, 5, "keyword"); (6, 8, "keyword") ]));
  render_code code;
  Code.set_content code "let yy in y";
  equal ~msg:"current content shown" string "let yy in y"
    (Text_buffer.plain_text (Code.buffer code));
  is_true ~msg:"prefix range kept" (is_red code 0);
  is_false ~msg:"range over changed bytes dropped" (is_red code 4);
  is_true ~msg:"suffix range shifted" (is_red code 7 && is_red code 8);
  is_false ~msg:"shift is exact" (is_red code 6)

let stale_result_styles_shared_bytes () =
  let jobs, highlighter = async_highlighter () in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  Code.set_content code "let x = 1";
  is_false ~msg:"nothing to carry yet" (is_red code 0);
  complete (List.hd !jobs) (Ok highlights);
  render_code code;
  is_true ~msg:"still highlighting the current content"
    (Code.is_highlighting code);
  equal ~msg:"current content shown" string "let x = 1"
    (Text_buffer.plain_text (Code.buffer code));
  is_true ~msg:"stale result styles the shared bytes" (is_red code 0);
  equal ~msg:"current content requested" string "let x = 1"
    (List.nth !jobs 1).request.content

let new_producer_starts_plain () =
  let jobs, highlighter = async_highlighter () in
  let next_jobs, next_highlighter = async_highlighter () in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  complete (List.hd !jobs) (Ok highlights);
  render_code code;
  Code.set_syntax code
    (Some
       (Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
          next_highlighter));
  is_true ~msg:"highlighting" (Code.is_highlighting code);
  is_false ~msg:"another producer's captures are not carried" (is_red code 0);
  equal int 1 (List.length !next_jobs)

let async_content_changes_coalesce () =
  let jobs, highlighter = async_highlighter () in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  let first_job = List.hd !jobs in
  Code.set_content code "var x";
  for i = 1 to 100 do
    Code.set_content code (Printf.sprintf "var %d" i)
  done;
  Code.set_content code "var x";
  equal ~msg:"one running job during rapid updates" int 1 (List.length !jobs);
  is_false ~msg:"obsolete job remains observable until completion"
    first_job.cancelled;
  complete first_job (Ok highlights);
  render_code code;
  equal ~msg:"current plain text retained" string "var x"
    (Text_buffer.plain_text (Code.buffer code));
  let second_job = List.nth !jobs 1 in
  complete second_job
    (Ok
       (Syntax_highlight.of_triples ~content:second_job.request.content
          [ (0, 3, "keyword") ]));
  render_code code;
  match styled_text code with
  | first :: _ ->
      equal ~msg:"highlighted current content" string "var" (span_text first)
  | [] -> fail "expected highlighted span"

let async_configuration_changes_coalesce () =
  let jobs, highlighter = async_highlighter () in
  let next_jobs, next_highlighter = async_highlighter () in
  let syntax = Code.with_highlighter ~language:"ocaml" highlighter in
  let _ctx, code = make_code ~content:"old" ~syntax () in
  Code.set_content code "";
  is_false (Code.is_highlighting code);
  Code.set_syntax code
    (Some (Code.with_highlighter ~language:"json" next_highlighter));
  Code.set_content code "new";
  equal ~msg:"switching producer cannot overlap jobs" int 0
    (List.length !next_jobs);
  complete (List.hd !jobs) (Ok highlights);
  render_code code;
  let latest = List.hd !next_jobs in
  equal string "new" latest.request.content;
  equal string "json" latest.request.language;
  complete latest
    (Ok (Syntax_highlight.make ~content:latest.request.content []));
  render_code code;
  equal string "new" (Text_buffer.plain_text (Code.buffer code));
  is_false (Code.is_highlighting code)

let async_clear_discards_pending_request () =
  let jobs, highlighter = async_highlighter () in
  let syntax = Code.with_highlighter ~language:"ocaml" highlighter in
  let _ctx, code = make_code ~content:"old" ~syntax () in
  Code.set_content code "pending";
  Code.set_syntax code None;
  complete (List.hd !jobs) (Ok highlights);
  render_code code;
  equal ~msg:"removed syntax cannot restart pending work" int 1
    (List.length !jobs);
  equal string "pending" (Text_buffer.plain_text (Code.buffer code));
  is_false (Code.is_highlighting code)

let async_line_info_stabilizes_on_completion () =
  let jobs, highlighter = async_highlighter () in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  let changes = ref 0 in
  Code.set_on_line_info_change code (Some (fun () -> incr changes));
  is_true ~msg:"pending line info" (Code.is_highlighting code);
  complete (List.hd !jobs) (Ok highlights);
  equal ~msg:"callback waits for polling" int 0 !changes;
  render_code code;
  is_false ~msg:"stable line info" (Code.is_highlighting code);
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
    (Text_buffer.plain_text (Code.buffer code))

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
  let highlights =
    Syntax_highlight.of_triples ~content:"old" [ (0, 3, "keyword") ]
  in
  let syntax = Code.syntax ~style:(syntax_style red) highlights in
  let _ctx, code = make_code ~content:"let" ~syntax () in
  equal ~msg:"plain text" string "let"
    (Text_buffer.plain_text (Code.buffer code));
  match styled_text code with
  | [ span ] ->
      is_true ~msg:"default style"
        (Ansi.Style.equal Ansi.Style.default (span_style span))
  | _ -> fail "expected one plain span"

let conceal_metadata_applies () =
  let meta = { Syntax_highlight.default_meta with conceal = Some "*" } in
  let highlights =
    Syntax_highlight.make ~content:"let x = 1"
      [
        Syntax_highlight.range ~meta ~start_byte:0 ~end_byte:3 ~scope:"keyword"
          ();
      ]
  in
  let syntax = Code.syntax ~style:(syntax_style red) highlights in
  let _ctx, code = make_code ~content:"let x = 1" ~syntax () in
  equal ~msg:"concealed text" string "* x = 1"
    (Text_buffer.plain_text (Code.buffer code))

let set_content_reapplies_syntax () =
  let syntax = Code.syntax ~style:(syntax_style red) highlights in
  let ctx, code = make_code ~content:"let x" ~syntax () in
  ignore ctx;
  Code.set_content code "var x";
  equal ~msg:"stale same-length captures leave current text plain" string
    "var x"
    (Text_buffer.plain_text (Code.buffer code));
  is_true
    (Ansi.Style.equal Ansi.Style.default
       (fst (Text_buffer.style_at (Code.buffer code) 0)))

let set_syntax_none_renders_plain () =
  let syntax = Code.syntax ~style:(syntax_style red) highlights in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  Code.set_syntax code None;
  match styled_text code with
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
  match styled_text code with
  | first :: _ ->
      is_true ~msg:"updated style" (Ansi.Style.equal blue (span_style first))
  | [] -> fail "expected highlighted span"

let syntax_compares_styles_by_value () =
  let calls = ref 0 in
  let highlighter =
    Code.Highlighter.sync (fun ~language:_ ~content ->
        incr calls;
        Syntax_highlight.of_triples ~content [ (0, 3, "keyword") ])
  in
  (* A view rebuilds its syntax settings, style included, on every render. *)
  let props style =
    Code.Props.make ~content:"let x"
      ~syntax:(Code.with_highlighter ~language:"ocaml" ~style highlighter)
      ()
  in
  is_true ~msg:"rebuilt equal styles are equal props"
    (Code.Props.equal (props (syntax_style red)) (props (syntax_style red)));
  is_false ~msg:"a changed style is a change"
    (Code.Props.equal (props (syntax_style red)) (props (syntax_style blue)));
  is_false ~msg:"a fresh highlighter is a new producer"
    (Code.Props.equal
       (Code.Props.make ~content:"let x"
          ~syntax:(Code.with_highlighter ~language:"ocaml" highlighter)
          ())
       (Code.Props.make ~content:"let x"
          ~syntax:
            (Code.with_highlighter ~language:"ocaml"
               (Code.Highlighter.sync (fun ~language:_ ~content ->
                    Syntax_highlight.make ~content [])))
          ()));
  let _ctx, code = make_code () in
  Code.apply_props code (props (syntax_style red));
  Code.apply_props code (props (syntax_style blue));
  equal ~msg:"a style change restyles without highlighting again" int 1 !calls;
  match styled_text code with
  | first :: _ ->
      is_true ~msg:"restyled" (Ansi.Style.equal blue (span_style first))
  | [] -> fail "expected highlighted span"

let text_style_restamps_plain_content () =
  let _ctx, code = make_code ~content:"plain" () in
  Code.apply_props code (Code.Props.make ~content:"plain" ~text_style:red ());
  match styled_text code with
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
        Syntax_highlight.of_triples ~content [ (0, 3, "keyword") ])
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
        [ "let old"; "let other"; "let final" ]
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
        if content = "bad" then failwith "highlight failed"
        else Syntax_highlight.of_triples ~content [ (0, 3, "keyword") ])
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
  match styled_text code with
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
  Code.set_content code "pending";
  Renderable.destroy (Code.node code);
  Renderable.destroy (Code.node code);
  equal ~msg:"cleanup is idempotent" int 1 !cancellations;
  is_false ~msg:"destroyed code has no pending job" (Code.is_highlighting code);
  equal ~msg:"node unregistered" (list string)
    [ Renderable.id (Code.node code) ]
    !(ctx.unregister_log)

let theme_changes_reuse_highlighter_captures () =
  let calls = ref 0 in
  let highlighter =
    Code.Highlighter.sync (fun ~language:_ ~content ->
        incr calls;
        Syntax_highlight.of_triples ~content [ (0, 3, "keyword") ])
  in
  let syntax color =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style color)
      highlighter
  in
  let ctx, code = make_code ~content:"let x" ~syntax:(syntax red) () in
  ignore ctx;
  let source = Text_buffer.source (Code.buffer code) in
  let layout = Text_surface.layout (Code.surface code) in
  Code.apply_props code
    (Code.Props.make ~content:"let x" ~syntax:(syntax blue) ());
  equal ~msg:"theme does not invoke parser" int 1 !calls;
  is_true
    (Ansi.Style.equal blue (fst (Text_buffer.style_at (Code.buffer code) 0)));
  is_true ~msg:"theme keeps indexed source"
    (source == Text_buffer.source (Code.buffer code));
  is_true ~msg:"theme keeps layout"
    (layout == Text_surface.layout (Code.surface code));
  Code.set_content code "let y";
  equal ~msg:"source change invokes parser" int 2 !calls

let pending_result_uses_latest_theme () =
  let jobs, highlighter = async_highlighter () in
  let syntax color =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style color)
      highlighter
  in
  let ctx, code = make_code ~content:"let x" ~syntax:(syntax red) () in
  ignore ctx;
  Code.set_syntax code (Some (syntax blue));
  equal ~msg:"theme creates no second job" int 1 (List.length !jobs);
  complete (List.hd !jobs) (Ok highlights);
  render_code code;
  equal ~msg:"completion creates no deferred second job" int 1
    (List.length !jobs);
  is_false (Code.is_highlighting code);
  is_true ~msg:"completion uses latest accepted theme"
    (Ansi.Style.equal blue (fst (Text_buffer.style_at (Code.buffer code) 0)))

let stale_async_capture_falls_back () =
  let jobs, highlighter = async_highlighter () in
  let ctx, code =
    make_code ~content:"let x"
      ~syntax:(Code.with_highlighter ~language:"ocaml" highlighter)
      ()
  in
  ignore ctx;
  complete (List.hd !jobs)
    (Ok (Syntax_highlight.of_triples ~content:"var x" [ (0, 3, "keyword") ]));
  render_code code;
  equal string "let x" (Text_buffer.plain_text (Code.buffer code));
  is_true
    (Ansi.Style.equal Ansi.Style.default
       (fst (Text_buffer.style_at (Code.buffer code) 0)))

let equivalent_capture_declaration_does_not_repaint () =
  let style = syntax_style red in
  let ctx, code =
    make_code ~content:"let x" ~syntax:(Code.syntax ~style highlights) ()
  in
  ignore ctx;
  let notifications = ref 0 in
  Code.set_on_line_info_change code (Some (fun () -> incr notifications));
  let fresh =
    Syntax_highlight.of_triples
      ~content:(String.init 5 (String.get "let x"))
      [ (0, 3, "keyword") ]
  in
  Code.apply_props code
    (Code.Props.make ~content:"let x" ~syntax:(Code.syntax ~style fresh) ());
  equal ~msg:"equivalent capture declaration does not redo paint" int 0
    !notifications

let () =
  run "mosaic.code"
    [
      group "Syntax"
        [
          test "equivalent captures do not repaint"
            equivalent_capture_declaration_does_not_repaint;
          test "theme changes reuse captures"
            theme_changes_reuse_highlighter_captures;
          test "pending result uses latest theme"
            pending_result_uses_latest_theme;
          test "stale async capture falls back" stale_async_capture_falls_back;
          test "plain without syntax" plain_without_syntax;
          test "precomputed highlights apply style"
            precomputed_highlights_apply_style;
          test "sync highlighter applies style" sync_highlighter_applies_style;
          test "async highlighter applies completed result"
            async_highlighter_applies_completed_result;
          test "pending append keeps highlighted prefix"
            pending_append_keeps_highlighted_prefix;
          test "pending edit keeps shared prefix and suffix"
            pending_edit_keeps_shared_prefix_and_suffix;
          test "stale result styles shared bytes"
            stale_result_styles_shared_bytes;
          test "new producer starts plain" new_producer_starts_plain;
          test "async content changes coalesce" async_content_changes_coalesce;
          test "async configuration changes coalesce"
            async_configuration_changes_coalesce;
          test "async clear discards pending request"
            async_clear_discards_pending_request;
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
          test "syntax compares styles by value" syntax_compares_styles_by_value;
          test "text_style restamps plain content"
            text_style_restamps_plain_content;
          test "is_highlighting false for sync highlights"
            is_highlighting_false_for_sync_highlights;
        ];
    ]
