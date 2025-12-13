type 'a binding = {
  key : Event.Key.t;
  ctrl : bool option;
  alt : bool option;
  shift : bool option;
  super : bool option;
  hyper : bool option;
  meta : bool option;
  data : 'a;
}

type 'a t = 'a binding list

let empty = []
let add_binding map binding = binding :: map

let add ?ctrl ?alt ?shift ?super ?hyper ?meta map key data =
  add_binding map { key; ctrl; alt; shift; super; hyper; meta; data }

let add_char ?ctrl ?alt ?shift ?super ?hyper ?meta map c data =
  add_binding map
    { key = Char (Uchar.of_char c); ctrl; alt; shift; super; hyper; meta; data }

let add_key ?ctrl ?alt ?shift ?super ?hyper ?meta map key data =
  add_binding map { key; ctrl; alt; shift; super; hyper; meta; data }

let matches_modifier (cond : _ binding) (actual : Event.Key.modifier) =
  let check_opt opt field =
    match opt with None -> true | Some v -> v = field
  in
  check_opt cond.ctrl actual.ctrl
  && check_opt cond.alt actual.alt
  && check_opt cond.shift actual.shift
  && check_opt cond.super actual.super
  && check_opt cond.hyper actual.hyper
  && check_opt cond.meta actual.meta

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
