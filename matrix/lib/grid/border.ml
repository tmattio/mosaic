type t = {
  top_left : int32;
  top_right : int32;
  bottom_left : int32;
  bottom_right : int32;
  horizontal : int32;
  vertical : int32;
  top_t : int32;
  bottom_t : int32;
  left_t : int32;
  right_t : int32;
  cross : int32;
}

type side = [ `Top | `Right | `Bottom | `Left ]

let all = [ `Top; `Right; `Bottom; `Left ]

(* Constants *)
let single =
  {
    top_left = 0x250Cl;
    top_right = 0x2510l;
    bottom_left = 0x2514l;
    bottom_right = 0x2518l;
    horizontal = 0x2500l;
    vertical = 0x2502l;
    top_t = 0x252Cl;
    bottom_t = 0x2534l;
    left_t = 0x251Cl;
    right_t = 0x2524l;
    cross = 0x253Cl;
  }

let double =
  {
    top_left = 0x2554l;
    top_right = 0x2557l;
    bottom_left = 0x255Al;
    bottom_right = 0x255Dl;
    horizontal = 0x2550l;
    vertical = 0x2551l;
    top_t = 0x2566l;
    bottom_t = 0x2569l;
    left_t = 0x2560l;
    right_t = 0x2563l;
    cross = 0x256Cl;
  }

let rounded =
  {
    single with
    top_left = 0x256Dl;
    top_right = 0x256El;
    bottom_left = 0x2570l;
    bottom_right = 0x256Fl;
  }

let heavy =
  {
    top_left = 0x250Fl;
    top_right = 0x2513l;
    bottom_left = 0x2517l;
    bottom_right = 0x251Bl;
    horizontal = 0x2501l;
    vertical = 0x2503l;
    top_t = 0x2533l;
    bottom_t = 0x253Bl;
    left_t = 0x2523l;
    right_t = 0x252Bl;
    cross = 0x254Bl;
  }

let ascii =
  let c v = Int32.of_int (Char.code v) in
  {
    top_left = c '+';
    top_right = c '+';
    bottom_left = c '+';
    bottom_right = c '+';
    horizontal = c '-';
    vertical = c '|';
    top_t = c '+';
    bottom_t = c '+';
    left_t = c '+';
    right_t = c '+';
    cross = c '+';
  }

let empty =
  let s = 32l in
  {
    top_left = s;
    top_right = s;
    bottom_left = s;
    bottom_right = s;
    horizontal = s;
    vertical = s;
    top_t = s;
    bottom_t = s;
    left_t = s;
    right_t = s;
    cross = s;
  }

let modify ?top_left ?top_right ?bottom_left ?bottom_right ?horizontal ?vertical
    ?top_t ?bottom_t ?left_t ?right_t ?cross t =
  let u = function
    | None -> None
    | Some c -> Some (Int32.of_int (Uchar.to_int c))
  in
  let get default = function Some x -> x | None -> default in
  {
    top_left = get t.top_left (u top_left);
    top_right = get t.top_right (u top_right);
    bottom_left = get t.bottom_left (u bottom_left);
    bottom_right = get t.bottom_right (u bottom_right);
    horizontal = get t.horizontal (u horizontal);
    vertical = get t.vertical (u vertical);
    top_t = get t.top_t (u top_t);
    bottom_t = get t.bottom_t (u bottom_t);
    left_t = get t.left_t (u left_t);
    right_t = get t.right_t (u right_t);
    cross = get t.cross (u cross);
  }
