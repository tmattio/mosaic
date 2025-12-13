(* Code for resolving name grid lines and areas *)

open Style
open Geometry

(* Resolver that takes grid lines names and area names as input and can then be used to
    resolve line names of grid placement properties into line numbers *)
type t = {
  row_lines : (string, int list) Hashtbl.t;
      (* Map of row line names to line numbers. Each line name may correspond to multiple lines *)
  column_lines : (string, int list) Hashtbl.t;
      (* Map of column line names to line numbers. Each line name may correspond to multiple lines *)
  area_column_count : int;
      (* Number of columns implied by grid area definitions *)
  area_row_count : int; (* Number of rows implied by grid area definitions *)
  mutable explicit_column_count : int;
      (* The number of explicit columns in the grid. This is an *input* to the NamedLineResolver and is
      used when computing the fallback line when a non-existent named line is specified *)
  mutable explicit_row_count : int;
      (* The number of explicit rows in the grid. This is an *input* to the NamedLineResolver and is
      used when computing the fallback line when a non-existent named line is specified *)
}

(* Utility function to create or update an entry in a line name map *)
let upsert_line_name_map map key value =
  match Hashtbl.find_opt map key with
  | Some lines -> Hashtbl.replace map key (lines @ [ value ])
  | None -> Hashtbl.add map key [ value ]

(* Create and initialise a new NamedLineResolver *)
let create style column_auto_repetitions row_auto_repetitions =
  let column_lines = Hashtbl.create 16 in
  let row_lines = Hashtbl.create 16 in

  let area_column_count = ref 0 in
  let area_row_count = ref 0 in

  (* Process grid template areas *)
  List.iter
    (fun area ->
      area_column_count :=
        max !area_column_count (max 1 (Grid.Template_area.column_end area) - 1);
      area_row_count :=
        max !area_row_count (max 1 (Grid.Template_area.row_end area) - 1);

      let col_start_name =
        Printf.sprintf "%s-start" (Grid.Template_area.name area)
      in
      upsert_line_name_map column_lines col_start_name
        (Grid.Template_area.column_start area);
      let col_end_name =
        Printf.sprintf "%s-end" (Grid.Template_area.name area)
      in
      upsert_line_name_map column_lines col_end_name
        (Grid.Template_area.column_end area);
      let row_start_name =
        Printf.sprintf "%s-start" (Grid.Template_area.name area)
      in
      upsert_line_name_map row_lines row_start_name
        (Grid.Template_area.row_start area);
      let row_end_name =
        Printf.sprintf "%s-end" (Grid.Template_area.name area)
      in
      upsert_line_name_map row_lines row_end_name
        (Grid.Template_area.row_end area))
    (grid_template_areas style);

  (* Process grid template columns *)
  let current_line = ref 0 in
  let column_tracks = grid_template_columns style in
  let column_line_names = grid_template_column_names style in
  let column_tracks_ref = ref column_tracks in

  List.iter
    (fun line_names ->
      incr current_line;
      List.iter
        (fun line_name ->
          upsert_line_name_map column_lines line_name !current_line)
        line_names;

      match !column_tracks_ref with
      | Grid.Template_component.Repeat rep :: rest ->
          column_tracks_ref := rest;
          let repeat_count =
            match Grid.Repetition.count rep with
            | Grid.Repetition_count.Count count -> count
            | Grid.Repetition_count.Auto_fill | Grid.Repetition_count.Auto_fit
              ->
                column_auto_repetitions
          in

          for _ = 0 to repeat_count - 1 do
            List.iter
              (fun line_name_set ->
                List.iter
                  (fun line_name ->
                    upsert_line_name_map column_lines line_name !current_line)
                  line_name_set;
                incr current_line)
              (Grid.Repetition.line_names rep);
            (* Last line name set collapses with following line name set *)
            decr current_line
          done;
          (* Last line name set collapses with following line name set *)
          decr current_line
      | _ :: rest -> column_tracks_ref := rest
      | [] -> ())
    column_line_names;

  (* Sort and dedup lines for each column name *)
  Hashtbl.iter
    (fun name lines ->
      let sorted = List.sort_uniq Int.compare lines in
      Hashtbl.replace column_lines name sorted)
    column_lines;

  (* Process grid template rows - similar logic *)
  let current_line = ref 0 in
  let row_tracks = grid_template_rows style in
  let row_line_names = grid_template_row_names style in
  let row_tracks_ref = ref row_tracks in

  List.iter
    (fun line_names ->
      incr current_line;
      List.iter
        (fun line_name ->
          upsert_line_name_map row_lines line_name !current_line)
        line_names;

      match !row_tracks_ref with
      | Grid.Template_component.Repeat rep :: rest ->
          row_tracks_ref := rest;
          let repeat_count =
            match Grid.Repetition.count rep with
            | Grid.Repetition_count.Count count -> count
            | Grid.Repetition_count.Auto_fill | Grid.Repetition_count.Auto_fit
              ->
                row_auto_repetitions
          in

          for _ = 0 to repeat_count - 1 do
            List.iter
              (fun line_name_set ->
                List.iter
                  (fun line_name ->
                    upsert_line_name_map row_lines line_name !current_line)
                  line_name_set;
                incr current_line)
              (Grid.Repetition.line_names rep);
            (* Last line name set collapses with following line name set *)
            decr current_line
          done;
          (* Last line name set collapses with following line name set *)
          decr current_line
      | _ :: rest -> row_tracks_ref := rest
      | [] -> ())
    row_line_names;

  (* Sort and dedup lines for each row name *)
  Hashtbl.iter
    (fun name lines ->
      let sorted = List.sort_uniq Int.compare lines in
      Hashtbl.replace row_lines name sorted)
    row_lines;

  {
    area_column_count = !area_column_count;
    area_row_count = !area_row_count;
    explicit_column_count = 0;
    (* Overwritten later *)
    explicit_row_count = 0;
    row_lines;
    column_lines;
  }

(* Get the number of columns defined by the grid areas *)
let area_column_count t = t.area_column_count

(* Get the number of rows defined by the grid areas *)
let area_row_count t = t.area_row_count

(* Set the number of columns in the explicit grid *)
let set_explicit_column_count t count = t.explicit_column_count <- count

(* Set the number of rows in the explicit grid *)
let set_explicit_row_count t count = t.explicit_row_count <- count

(* Get the number of columns in the explicit grid *)
let explicit_column_count t = t.explicit_column_count

(* Get the number of rows in the explicit grid *)
let explicit_row_count t = t.explicit_row_count

(* Find a line index by name *)
let find_line_index t name idx axis =
  let lines_map =
    match axis with `Row -> t.row_lines | `Column -> t.column_lines
  in
  match Hashtbl.find_opt lines_map name with
  | None -> 0 (* Fallback to 0 if named line not found *)
  | Some lines ->
      (* idx is 1-based, so we need to convert to 0-based index *)
      let zero_based_idx =
        if idx > 0 then idx - 1 else List.length lines + idx
      in
      if zero_based_idx >= 0 && zero_based_idx < List.length lines then
        List.nth lines zero_based_idx
      else 0 (* Fallback to 0 if index out of bounds *)

(* Resolve named lines for a grid placement *)
let resolve_line_names t line axis =
  let resolve_single placement =
    match placement with
    | Grid.Placement.Named_line (name, idx) ->
        Grid.Placement.Line (find_line_index t name idx axis)
    | _ -> placement
  in
  {
    Geometry.Line.start = resolve_single line.Geometry.Line.start;
    end_ = resolve_single line.Geometry.Line.end_;
  }

(* Resolve named lines for row axis *)
let resolve_row_names t line = resolve_line_names t line `Row

(* Resolve named lines for column axis *)
let resolve_column_names t line = resolve_line_names t line `Column

(* Resolve absolutely positioned grid tracks *)
let resolve_absolutely_positioned_grid_tracks oz_placement =
  let open Grid.Origin_zero_placement in
  match (oz_placement.Line.start, oz_placement.Line.end_) with
  | Line track1, Line track2 ->
      if track1 = track2 then
        Line.{ start = Some track1; end_ = Some (track1 + 1) }
      else
        Line.
          { start = Some (min track1 track2); end_ = Some (max track1 track2) }
  | Line track, Span span ->
      Line.{ start = Some track; end_ = Some (track + span) }
  | Line track, Auto -> Line.{ start = Some track; end_ = None }
  | Span span, Line track ->
      Line.{ start = Some (track - span); end_ = Some track }
  | Auto, Line track -> Line.{ start = None; end_ = Some track }
  | _ -> Line.{ start = None; end_ = None }
