(** Defines a grid area *)

type t = {
  name : string;  (** The name of the grid area *)
  row_start : int;
      (** The index of the row at which the grid area starts in grid coordinates
      *)
  row_end : int;
      (** The index of the row at which the grid area ends in grid coordinates
      *)
  column_start : int;
      (** The index of the column at which the grid area starts in grid
          coordinates *)
  column_end : int;
      (** The index of the column at which the grid area ends in grid
          coordinates *)
}

let make ~name ~row_start ~row_end ~column_start ~column_end =
  { name; row_start; row_end; column_start; column_end }

let name t = t.name
let row_start t = t.row_start
let row_end t = t.row_end
let column_start t = t.column_start
let column_end t = t.column_end

let equal a b =
  String.equal a.name b.name && a.row_start = b.row_start
  && a.row_end = b.row_end
  && a.column_start = b.column_start
  && a.column_end = b.column_end

let compare a b =
  let c = String.compare a.name b.name in
  if c <> 0 then c
  else
    let c = Int.compare a.row_start b.row_start in
    if c <> 0 then c
    else
      let c = Int.compare a.row_end b.row_end in
      if c <> 0 then c
      else
        let c = Int.compare a.column_start b.column_start in
        if c <> 0 then c else Int.compare a.column_end b.column_end
