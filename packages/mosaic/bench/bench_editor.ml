open Mosaic_ui

let small_text = String.make 128 'x'
let large_text = String.make 1_000_000 'x'
let persistent_cursor editor = editor |> Editor.move_left |> Editor.move_right
let persistent_insert editor = Editor.insert "y" editor

(* THUMPER_MACHINE=exact selects the section CI checks: allocation alone,
   whose counts are the same on any x86_64 Linux under the locked compiler in
   release, while time compares only on one machine. *)
let exact = Sys.getenv_opt "THUMPER_MACHINE" = Some "exact"

let () =
  Thumper.run "editor"
    ~config:
      Thumper.Config.(
        default
        |> metrics
             (if exact then [ Thumper.Metric.alloc_words ]
              else Thumper.Metric.[ wall_time; alloc_words; promoted_words ]))
    ~budgets:
      (Thumper.Budget.no_more_alloc_than 0.
      ::
      (if exact then []
       else
         [
           Thumper.Budget.no_slower_than 0.05;
           Thumper.Budget.relative ~metric:Thumper.Metric.promoted_words
             ~max_regression:0.5 ();
         ]))
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
