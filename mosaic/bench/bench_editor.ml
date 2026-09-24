open Mosaic_ui

let small_text = String.make 128 'x'
let large_text = String.make 1_000_000 'x'

let mutable_cursor buffer =
  ignore (Edit_buffer.move_left buffer : bool);
  ignore (Edit_buffer.move_right buffer : bool);
  Edit_buffer.cursor buffer

let persistent_cursor editor = editor |> Editor.move_left |> Editor.move_right

let mutable_insert original =
  (* Both variants preserve the input and insert into an independent branch.
     This isolates the persistent wrapper's cost from flat-string indexing. *)
  let buffer = Edit_buffer.copy original in
  ignore (Edit_buffer.insert buffer "y" : bool);
  buffer

let persistent_insert editor = Editor.insert "y" editor
let engine text = Edit_buffer.create ~max_length:max_int text

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
    if Weak.check texts i <> (i >= count - 200) then
      failwith
        "editor history did not retain exactly its bounded text snapshots"
  done;
  ignore (Sys.opaque_identity final);
  print_endline
    "editor history retains 200 text snapshots and no old states or indexes"

let () =
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "history" then history_check ()
  else
    Thumper.run "editor"
      ~config:
        (Thumper.Config.default
        |> Thumper.Config.metrics
             (if Sys.getenv_opt "EDITOR_TIMING" = Some "1" then
                Thumper.Metric.[ alloc_words; wall_time; promoted_words ]
              else [ Thumper.Metric.alloc_words ]))
      ~budgets:[ Thumper.Budget.no_more_alloc_than 0.05 ]
      [
        Thumper.bench_with_setup
          ~setup:(fun () -> engine small_text)
          "cursor/engine-128b" mutable_cursor;
        Thumper.bench_with_setup
          ~setup:(fun () -> Editor.of_string small_text)
          "cursor/persistent-128b" persistent_cursor;
        Thumper.bench_with_setup
          ~setup:(fun () -> engine large_text)
          "cursor/engine-1mb" mutable_cursor;
        Thumper.bench_with_setup
          ~setup:(fun () -> Editor.of_string large_text)
          "cursor/persistent-1mb" persistent_cursor;
        Thumper.bench_with_setup
          ~setup:(fun () ->
            let buffer = engine large_text in
            Edit_buffer.set_cursor buffer 500_000;
            buffer)
          "insert/engine-copy-1mb" mutable_insert;
        Thumper.bench_with_setup
          ~setup:(fun () ->
            Editor.of_string large_text |> Editor.set_cursor 500_000)
          "insert/persistent-1mb" persistent_insert;
      ]
