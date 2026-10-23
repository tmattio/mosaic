(* Links the terminal core with no unix (see the dune file); each library is
   used, so that none is left out of the link. *)

let () =
  let grid = Matrix_grid.create ~width:8 ~height:1 () in
  let hits = Matrix_screen.Hit_grid.create ~width:8 ~height:1 in
  Matrix_image.draw ~hits (Matrix_image.text "pure") grid;
  let screen = Matrix_screen.create () in
  Matrix_screen.build screen ~width:8 ~height:1 (fun g _ ->
      Matrix_grid.blit ~src:grid ~dst:g);
  ignore (Matrix_screen.render screen : string);
  ignore (Vte.feed_string (Vte.create ~rows:1 ~cols:8 ()) "pure" : string);
  ignore (Matrix_input.Decoder.create () : Matrix_input.Decoder.t);
  ignore (Matrix_text.grapheme_count "pure" : int);
  ignore (Matrix_charts.empty () : Matrix_charts.t);
  ignore (Ansi.strip "pure" : string)
