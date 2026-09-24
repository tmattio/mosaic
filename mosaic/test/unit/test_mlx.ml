open Windtrap
open Mosaic_ui

let make () =
  let renderer = Renderer.create () in
  let container = Renderer.root renderer in
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_size (Vnode.size ~width:20 ~height:8)
  in
  Renderable.set_style container style;
  (renderer, Mosaic.Reconciler.create ~container)

let render renderer reconciler view dispatch =
  Mosaic.Reconciler.render reconciler ~viewport_width:20
    (Vnode.map dispatch view);
  Renderer.render_frame renderer ~width:20 ~height:8 ~delta:0.

let children_and_editor_state () =
  let renderer, reconciler = make () in
  let state = ref (Mosaic.Editor.of_string "a") in
  let view () =
    Mosaic_mlx.box ~flex_direction:Mosaic.Flex_direction.Column
      ~children:
        [
          Mosaic_mlx.text ~children:[ "head"; "er" ] ();
          Mosaic_mlx.input ~focused:true ~state:!state
            ~on_change:(fun next -> next)
            ~children:[] ();
        ]
      ()
  in
  let accept next = state := next in
  render renderer reconciler (view ()) accept;
  let grid = Matrix_screen.next_grid (Renderer.screen renderer) in
  equal ~msg:"text children concatenate" string "h"
    (Matrix_grid.get_text grid 0);
  equal ~msg:"last text child follows first" string "r"
    (Matrix_grid.get_text grid 5);
  let node = Option.get (Mosaic.Reconciler.focused reconciler) in
  ignore (Renderer.focus renderer node : bool);
  ignore
    (Renderer.dispatch_key renderer (Matrix_input.Key.of_char 'b') : Event.key);
  equal ~msg:"editor callback proposes model state" string "ab"
    (Mosaic.Editor.text !state);
  render renderer reconciler (view ()) accept;
  ignore
    (Renderer.dispatch_key renderer (Matrix_input.Key.of_char 'c') : Event.key);
  equal ~msg:"next edit uses accepted declaration" string "abc"
    (Mosaic.Editor.text !state)

let scroll_children_and_state () =
  let renderer, reconciler = make () in
  let scroll = ref (Mosaic.Scroll.at 0) in
  let view () =
    Mosaic_mlx.scroll_box
      ~size:(Mosaic.size ~width:10 ~height:4)
      ~scroll:!scroll
      ~on_scroll:(fun next -> next)
      ~show_scrollbars:false
      ~children:
        [
          Mosaic_mlx.box ~flex_direction:Mosaic.Flex_direction.Column
            ~flex_shrink:0.
            ~children:
              (List.init 20 (fun i ->
                   Mosaic_mlx.text ~children:[ string_of_int i ] ()))
            ();
        ]
      ()
  in
  let frame () =
    render renderer reconciler (view ()) (fun next -> scroll := next);
    ignore (Renderer.render ~full:true renderer : string)
  in
  let wheel () =
    match Matrix_input.mouse_scroll 1 1 Matrix_input.Mouse.Scroll_down with
    | Matrix_input.Mouse mouse ->
        ignore (Renderer.dispatch_mouse renderer mouse : Event.mouse)
    | _ -> assert false
  in
  let position () =
    snd
      (Mosaic.Scroll.resolve !scroll ~width:10 ~height:4 ~content_width:10
         ~content_height:20)
  in
  frame ();
  wheel ();
  let first = position () in
  is_true ~msg:"scroll callback proposes a position" (first > 0);
  frame ();
  wheel ();
  is_true ~msg:"next wheel starts from accepted model position"
    (position () > first)

let line_number_requires_one_child () =
  raises_match ~msg:"line numbers require exactly one child"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () -> ignore (Mosaic_mlx.line_number ~children:[] ()))

let () =
  run "mosaic.mlx"
    [
      test "children and model-owned editor" children_and_editor_state;
      test "children and model-owned scroll" scroll_children_and_state;
      test "line-number children contract" line_number_requires_one_child;
    ]
