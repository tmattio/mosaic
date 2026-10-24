open Mosaic_ui

let small_text = String.make 128 'x'
let large_text = String.make 1_000_000 'x'
let persistent_cursor editor = editor |> Editor.move_left |> Editor.move_right
let persistent_insert editor = Editor.insert "y" editor

let history_check () =
  let count = 240 in
  let editors = Weak.create count in
  let sources = Weak.create count in
  let texts = Weak.create count in
  let final =
    let current = ref (Editor.of_string (String.make 4096 'x')) in
    for i = 0 to count - 1 do
      Weak.set editors i (Some !current);
      Weak.set sources i (Some (Editor.source !current));
      Weak.set texts i (Some (Editor.text !current));
      current := Editor.insert "y" !current
    done;
    !current
  in
  Gc.full_major ();
  for i = 0 to count - 1 do
    if Weak.check editors i then failwith "editor retained an obsolete state";
    if Weak.check sources i then failwith "editor retained an obsolete source";
    if Weak.check texts i then failwith "editor history retained an old text"
  done;
  ignore (Sys.opaque_identity final);
  print_endline "editor history retains no old states, texts or indexes"

let () =
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "history" then history_check ()
  else
    Thumper.run "editor"
      ~config:
        (Thumper.Config.default
        |> Thumper.Config.metrics
             Thumper.Metric.[ wall_time; alloc_words; promoted_words ])
      ~budgets:
        [
          Thumper.Budget.no_slower_than 0.05;
          Thumper.Budget.no_more_alloc_than 0.05;
          Thumper.Budget.relative ~metric:Thumper.Metric.promoted_words
            ~max_regression:0.5 ();
        ]
      [
        Thumper.bench_with_setup
          ~setup:(fun () -> Editor.of_string small_text)
          "cursor/persistent-128b" persistent_cursor;
        Thumper.bench_with_setup
          ~setup:(fun () -> Editor.of_string large_text)
          "cursor/persistent-1mb" persistent_cursor;
        Thumper.bench_with_setup
          ~setup:(fun () ->
            Editor.of_string large_text |> Editor.set_cursor 500_000)
          "insert/persistent-1mb" persistent_insert;
      ]
