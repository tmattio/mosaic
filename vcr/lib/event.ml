(** Event log for terminal simulation *)

(** A single event in the simulation *)
type t =
  | Screen_change of {
      at : float;
      changed_rows : (int * Grid.Cell.t option array) list;
          (** List of (row_index, new_row_data) for rows that changed *)
    }  (** Screen content changed - only stores changed rows *)
  | Cursor_move of { at : float; row : int; col : int }
      (** Cursor position change *)
  | Sleep of { at : float; duration : float }
      (** Explicit sleep/delay in simulation *)

type log = t list
(** Event log is a list of events in chronological order *)

(** Compare events by timestamp for sorting *)
let compare e1 e2 =
  let get_time = function
    | Screen_change { at; _ } -> at
    | Cursor_move { at; _ } -> at
    | Sleep { at; _ } -> at
  in
  Float.compare (get_time e1) (get_time e2)

(** Sort events chronologically, preserving order for events at same timestamp
*)
let sort_log log = List.stable_sort compare log

(** Get timestamp of an event *)
let timestamp = function
  | Screen_change { at; _ } -> at
  | Cursor_move { at; _ } -> at
  | Sleep { at; _ } -> at

(** Get the end time of the simulation *)
let end_time log =
  List.fold_left
    (fun max_t event ->
      let t = timestamp event in
      match event with
      | Sleep { at; duration } -> Float.max max_t (at +. duration)
      | _ -> Float.max max_t t)
    0.0 log

(** Convert an event to a string for debugging *)
let to_string = function
  | Screen_change { at; changed_rows } ->
      Printf.sprintf "Screen_change(at=%.3f, changed_rows=%d)" at
        (List.length changed_rows)
  | Cursor_move { at; row; col } ->
      Printf.sprintf "Cursor_move(at=%.3f, row=%d, col=%d)" at row col
  | Sleep { at; duration } ->
      Printf.sprintf "Sleep(at=%.3f, duration=%.3f)" at duration

(** Pretty-print an event with full details *)
let pp fmt = function
  | Screen_change { at; changed_rows } ->
      Fmt.pf fmt
        "@[<v>Screen_change {@;<1 2>at = %.3f;@;<1 2>changed_rows = [@[<v>" at;
      List.iter
        (fun (row_idx, cells) ->
          Fmt.pf fmt "@,row %d: [@[<h>" row_idx;
          Array.iteri
            (fun col_idx cell_opt ->
              match cell_opt with
              | None -> ()
              | Some cell ->
                  if col_idx > 0 then Fmt.pf fmt ";@ ";
                  Fmt.pf fmt "col %d: %a" col_idx Grid.Cell.pp cell)
            cells;
          Fmt.pf fmt "@]]")
        changed_rows;
      Fmt.pf fmt "@,]@;<1 0>}@]"
  | Cursor_move { at; row; col } ->
      Fmt.pf fmt "Cursor_move { at = %.3f; row = %d; col = %d }" at row col
  | Sleep { at; duration } ->
      Fmt.pf fmt "Sleep { at = %.3f; duration = %.3f }" at duration
