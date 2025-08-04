(* A typed representation of a repeat(..) in grid-template-* value *)

type t = {
  count : Repetition_count.t; (* The number of times the repeat is repeated *)
  tracks : Track_sizing_function.t list; (* The tracks to repeat *)
  line_names : string list list; (* The line names for the repeated tracks *)
}

let make ~count ~tracks ~line_names = { count; tracks; line_names }
let count t = t.count
let tracks t = t.tracks
let line_names t = t.line_names

let equal a b =
  Repetition_count.equal a.count b.count
  && List.equal Track_sizing_function.equal a.tracks b.tracks
  && List.equal (List.equal String.equal) a.line_names b.line_names

let compare a b =
  let c = Repetition_count.compare a.count b.count in
  if c <> 0 then c
  else
    let c = List.compare Track_sizing_function.compare a.tracks b.tracks in
    if c <> 0 then c
    else List.compare (List.compare String.compare) a.line_names b.line_names

let is_auto_repetition t = Repetition_count.is_auto t.count
let track_count t = List.length t.tracks
