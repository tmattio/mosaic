open Mosaic_ui

(* The reconciler runs once per redraw over the whole view, so its per-frame
   cost on a large keyed list is a contract: a chat/log transcript re-rendered
   per keystroke must not pay quadratic placement work. Both cases build the
   vnode list inside the measured region — a TEA view rebuilds its vnodes
   every frame, so that allocation is part of the reconcile cost. *)

let rows = 500
let viewport_width = 80
let keys = Array.init rows (fun i -> Printf.sprintf "k%d" i)

let labels =
  Array.init rows (fun i ->
      Thumper.black_box
        (Printf.sprintf "row %d: a representative transcript line" i))

(* [offset] rotates the keyed list: offset 0 is the identity order, any other
   offset moves the head block to the tail, forcing keyed matching and child
   moves without creating or destroying fibers. *)
let view ~offset =
  Vnode.box
    (List.init rows (fun i ->
         let j = (i + offset) mod rows in
         Vnode.text ~key:keys.(j) labels.(j)))

type state = { reconciler : Mosaic.Reconciler.t; mutable offset : int }

let make_state () =
  let renderer = Renderer.create () in
  let reconciler =
    Mosaic.Reconciler.create ~container:(Renderer.root renderer)
  in
  Mosaic.Reconciler.render reconciler ~viewport_width (view ~offset:0);
  { reconciler; offset = 0 }

let steady_keyed_list =
  Thumper.bench_with_setup ~setup:make_state "steady/keyed-list-500"
    (fun state ->
      Mosaic.Reconciler.render state.reconciler ~viewport_width (view ~offset:0))

let reorder_keyed_list =
  Thumper.bench_with_setup ~setup:make_state "reorder/keyed-list-500"
    (fun state ->
      state.offset <- (state.offset + 1) mod rows;
      Mosaic.Reconciler.render state.reconciler ~viewport_width
        (view ~offset:state.offset))

let () =
  Thumper.run "reconciler"
    ~config:
      Thumper.Config.(
        default
        |> metrics
             Thumper.Metric.
               [ wall_time; alloc_words; promoted_words; minor_collections ])
    ~budgets:
      [
        Thumper.Budget.no_slower_than 0.05; Thumper.Budget.no_more_alloc_than 0.;
      ]
    [ Thumper.group "reconcile" [ steady_keyed_list; reorder_keyed_list ] ]
