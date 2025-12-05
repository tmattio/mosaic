type 'a binding = {
  key : Event.Key.t;
  ctrl : bool option;
  alt : bool option;
  shift : bool option;
  data : 'a;
}

type 'a t = 'a binding list

let empty = []
let add_binding map binding = binding :: map

let add ?ctrl ?alt ?shift map key data =
  add_binding map { key; ctrl; alt; shift; data }

let add_char ?ctrl ?alt ?shift map c data =
  add_binding map { key = Char (Uchar.of_char c); ctrl; alt; shift; data }

let add_key ?ctrl ?alt ?shift map key data =
  add_binding map { key; ctrl; alt; shift; data }

let matches_modifier (cond : _ binding) (actual : Event.Key.modifier) =
  let check_opt opt field =
    match opt with None -> true | Some v -> v = field
  in
  check_opt cond.ctrl actual.ctrl
  && check_opt cond.alt actual.alt
  && check_opt cond.shift actual.shift

let find map = function
  | Event.Key { key; modifier; _ } ->
      let rec loop = function
        | [] -> None
        | b :: rest ->
            if Event.Key.equal key b.key && matches_modifier b modifier then
              Some b.data
            else loop rest
      in
      loop map
  | _ -> None
