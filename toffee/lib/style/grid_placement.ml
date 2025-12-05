type grid_line = int

type t =
  | Auto (* Place item according to auto-placement algorithm *)
  | Line of grid_line (* Place item at specified line index *)
  | Named_line of string * int (* Place item at specified named line *)
  | Span of int (* Item should span specified number of tracks *)
  | Named_span of
      string * int (* Item should span until nth line named <name> *)

let auto = Auto
let line index = Line index
let span count = Span count
let named_line name index = Named_line (name, index)
let named_span name count = Named_span (name, count)
let default = Auto

let to_string = function
  | Auto -> "auto"
  | Line n -> string_of_int n
  | Named_line (name, n) -> Printf.sprintf "%s %d" name n
  | Span n -> Printf.sprintf "span %d" n
  | Named_span (name, n) -> Printf.sprintf "span %s %d" name n

let equal a b =
  match (a, b) with
  | Auto, Auto -> true
  | Line n1, Line n2 -> n1 = n2
  | Named_line (name1, n1), Named_line (name2, n2) ->
      String.equal name1 name2 && n1 = n2
  | Span n1, Span n2 -> n1 = n2
  | Named_span (name1, n1), Named_span (name2, n2) ->
      String.equal name1 name2 && n1 = n2
  | _ -> false

let compare a b =
  match (a, b) with
  | Auto, Auto -> 0
  | Auto, _ -> -1
  | _, Auto -> 1
  | Line n1, Line n2 -> Int.compare n1 n2
  | Line _, _ -> -1
  | _, Line _ -> 1
  | Named_line (name1, n1), Named_line (name2, n2) ->
      let c = String.compare name1 name2 in
      if c <> 0 then c else Int.compare n1 n2
  | Named_line _, _ -> -1
  | _, Named_line _ -> 1
  | Span n1, Span n2 -> Int.compare n1 n2
  | Span _, _ -> -1
  | _, Span _ -> 1
  | Named_span (name1, n1), Named_span (name2, n2) ->
      let c = String.compare name1 name2 in
      if c <> 0 then c else Int.compare n1 n2

let pp fmt t = Format.pp_print_string fmt (to_string t)

(* Check if placement is definite (has at least one non-auto placement) *)
let is_definite = function
  | Line n when n <> 0 -> true (* 0 is invalid and treated as Auto *)
  | Named_line _ -> true
  | _ -> false

(* Convert grid placement to origin-zero placement, ignoring named lines *)
let into_origin_zero_placement_ignoring_named t explicit_track_count =
  match t with
  | Auto -> Grid.Origin_zero_placement.Auto
  | Span span -> Grid.Origin_zero_placement.Span span
  | Line line ->
      (* Grid line zero is an invalid index, so it gets treated as Auto *)
      if line = 0 then Grid.Origin_zero_placement.Auto
      else
        Grid.Origin_zero_placement.Line
          (Grid.grid_line_to_origin_zero_line line explicit_track_count)
  | Named_line _ -> Grid.Origin_zero_placement.Auto
  | Named_span _ -> Grid.Origin_zero_placement.Auto

(* Convert grid placement to origin-zero placement (named lines should already be resolved) *)
let into_origin_zero_placement t explicit_track_count =
  match t with
  | Auto -> Grid.Origin_zero_placement.Auto
  | Span span -> Grid.Origin_zero_placement.Span span
  | Line line ->
      (* Grid line zero is an invalid index, so it gets treated as Auto *)
      if line = 0 then Grid.Origin_zero_placement.Auto
      else
        Grid.Origin_zero_placement.Line
          (Grid.grid_line_to_origin_zero_line line explicit_track_count)
  | Named_line _ ->
      failwith
        "into_origin_zero_placement: Named lines should be resolved before \
         conversion"
  | Named_span _ ->
      failwith
        "into_origin_zero_placement: Named spans should be resolved before \
         conversion"

(* Module for operations on Line<GridPlacement> *)
module Line = struct
  let into_origin_zero_ignoring_named line explicit_track_count =
    let open Geometry.Line in
    {
      start =
        into_origin_zero_placement_ignoring_named line.start
          explicit_track_count;
      end_ =
        into_origin_zero_placement_ignoring_named line.end_ explicit_track_count;
    }

  let into_origin_zero line explicit_track_count =
    let open Geometry.Line in
    {
      start = into_origin_zero_placement line.start explicit_track_count;
      end_ = into_origin_zero_placement line.end_ explicit_track_count;
    }

  let is_definite line =
    match (line.Geometry.Line.start, line.Geometry.Line.end_) with
    | Line n, _ when n <> 0 -> true
    | _, Line n when n <> 0 -> true
    | Named_line _, _ -> true
    | _, Named_line _ -> true
    | _ -> false
end
