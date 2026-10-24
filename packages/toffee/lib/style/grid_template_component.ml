type t =
  | Single of Track_sizing_function.t (* A single non-repeated track *)
  | Repeat of Grid_repetition.t
(* Automatically generate grid tracks to fit the available space *)

let single tsf = Single tsf
let repeat rep = Repeat rep

(* Common constructors matching CSS values *)
let auto = Single Track_sizing_function.auto
let min_content = Single Track_sizing_function.min_content
let max_content = Single Track_sizing_function.max_content
let zero = Single Track_sizing_function.zero
let fr value = Single (Track_sizing_function.fr value)
let length value = Single (Track_sizing_function.length value)
let percent value = Single (Track_sizing_function.percent value)
let fit_content lp = Single (Track_sizing_function.fit_content lp)
let minmax ~min ~max = Single (Track_sizing_function.minmax ~min ~max)

let is_auto_repetition = function
  | Single _ -> false
  | Repeat rep -> Grid_repetition.is_auto_repetition rep

let equal a b =
  match (a, b) with
  | Single tsf1, Single tsf2 -> Track_sizing_function.equal tsf1 tsf2
  | Repeat rep1, Repeat rep2 -> Grid_repetition.equal rep1 rep2
  | _ -> false

let compare a b =
  match (a, b) with
  | Single tsf1, Single tsf2 -> Track_sizing_function.compare tsf1 tsf2
  | Single _, _ -> -1
  | _, Single _ -> 1
  | Repeat rep1, Repeat rep2 -> Grid_repetition.compare rep1 rep2

let to_string = function
  | Single tsf -> Track_sizing_function.to_string tsf
  | Repeat rep ->
      let count_str = Repetition_count.to_string (Grid_repetition.count rep) in
      let format_line_names names =
        if names = [] then "" else "[" ^ String.concat " " names ^ "] "
      in
      let tracks = Grid_repetition.tracks rep in
      let line_names = Grid_repetition.line_names rep in
      (* Interleave line names and tracks *)
      let rec format_tracks_with_names tracks names acc =
        match (tracks, names) with
        | [], [] -> List.rev acc
        | [], names :: _ -> List.rev (format_line_names names :: acc)
        | track :: rest_tracks, [] ->
            format_tracks_with_names rest_tracks []
              (Track_sizing_function.to_string track :: acc)
        | track :: rest_tracks, names :: rest_names ->
            format_tracks_with_names rest_tracks rest_names
              (Track_sizing_function.to_string track
              :: format_line_names names :: acc)
      in
      let content =
        String.concat "" (format_tracks_with_names tracks line_names [])
      in
      Printf.sprintf "repeat(%s, %s)" count_str content

let pp fmt t = Format.pp_print_string fmt (to_string t)
