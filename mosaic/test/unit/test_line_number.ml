open Windtrap
open Mosaic_ui

(* ── Helpers ── *)

let red = Ansi.Color.red
let blue = Ansi.Color.blue

let make_line_number ?line_colors ?line_signs ?line_numbers ?hidden_line_numbers
    ?show_line_numbers () =
  let renderer = Renderer.create () in
  let ln =
    Line_number.create ~parent:(Renderer.root renderer) ?line_colors ?line_signs
      ?line_numbers ?hidden_line_numbers ?show_line_numbers ()
  in
  let (_ : Text.t) =
    Text.create ~parent:(Line_number.node ln) ~content:"alpha\nbeta\ngamma" ()
  in
  (renderer, ln)

let render renderer =
  Renderer.render_frame renderer ~width:20 ~height:3 ~delta:0.;
  ignore (Renderer.render ~full:true renderer : string);
  Matrix_screen.current_grid (Renderer.screen renderer)

(* ── Props ── *)

let props_equal_detects_line_color_diff () =
  let a =
    Line_number.Props.make
      ~line_colors:[ (0, { Line_number.gutter = red; content = None }) ]
      ()
  in
  let b = Line_number.Props.make () in
  is_false ~msg:"different" (Line_number.Props.equal a b);
  is_true ~msg:"equal" (Line_number.Props.equal a a)

let props_equal_detects_length_mismatch () =
  let a = Line_number.Props.make ~hidden_line_numbers:[ 1 ] () in
  let b = Line_number.Props.make ~hidden_line_numbers:[ 1; 2 ] () in
  is_false ~msg:"different lengths" (Line_number.Props.equal a b);
  is_false ~msg:"symmetric" (Line_number.Props.equal b a)

(* ── Rendering ── *)

let line_colors_render_in_gutter () =
  let renderer, _ln =
    make_line_number
      ~line_colors:[ (1, { Line_number.gutter = red; content = None }) ]
      ()
  in
  let grid = render renderer in
  let bg_of y =
    Matrix_grid.get_background grid (Matrix_grid.idx grid ~x:0 ~y)
  in
  is_false ~msg:"row 0 uncolored" (Ansi.Color.equal red (bg_of 0));
  is_true ~msg:"row 1 colored" (Ansi.Color.equal red (bg_of 1))

let apply_props_refreshes_line_colors () =
  (* Row lookups are cached; a props update must invalidate the cache. *)
  let renderer, ln =
    make_line_number
      ~line_colors:[ (1, { Line_number.gutter = red; content = None }) ]
      ()
  in
  let (_ : Matrix_grid.t) = render renderer in
  Line_number.apply_props ln
    (Line_number.Props.make
       ~line_colors:[ (2, { Line_number.gutter = blue; content = None }) ]
       ());
  let grid = render renderer in
  let bg_of y =
    Matrix_grid.get_background grid (Matrix_grid.idx grid ~x:0 ~y)
  in
  is_false ~msg:"old row no longer colored" (Ansi.Color.equal red (bg_of 1));
  is_true ~msg:"new row colored" (Ansi.Color.equal blue (bg_of 2))

let signs_reserve_measured_width () =
  (* Sign columns come from the precomputed maxima; a wide before sign must
     shift the line numbers right. *)
  let renderer, _ln =
    make_line_number
      ~line_signs:
        [
          ( 0,
            {
              Line_number.before = Some "+>";
              after = None;
              before_color = None;
              after_color = None;
            } );
        ]
      ()
  in
  let grid = render renderer in
  equal ~msg:"before sign rendered" string "+"
    (Matrix_grid.get_text grid (Matrix_grid.idx grid ~x:0 ~y:0));
  equal ~msg:"line number shifted past the sign column" string "1"
    (Matrix_grid.get_text grid (Matrix_grid.idx grid ~x:3 ~y:0))

(* ── Runner ── *)

let () =
  run "mosaic.line_number"
    [
      group "Props"
        [
          test "detects line color difference"
            props_equal_detects_line_color_diff;
          test "detects length mismatch" props_equal_detects_length_mismatch;
        ];
      group "Rendering"
        [
          test "line colors render in gutter" line_colors_render_in_gutter;
          test "apply_props refreshes line colors"
            apply_props_refreshes_line_colors;
          test "signs reserve measured width" signs_reserve_measured_width;
        ];
    ]
