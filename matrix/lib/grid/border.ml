type t = {
  top_left : int;
  top_right : int;
  bottom_left : int;
  bottom_right : int;
  horizontal : int;
  vertical : int;
  top_t : int;
  bottom_t : int;
  left_t : int;
  right_t : int;
  cross : int;
}

type side = [ `Top | `Right | `Bottom | `Left ]

let all = [ `Top; `Right; `Bottom; `Left ]

(* Constants *)
let single =
  {
    top_left = 0x250C;
    top_right = 0x2510;
    bottom_left = 0x2514;
    bottom_right = 0x2518;
    horizontal = 0x2500;
    vertical = 0x2502;
    top_t = 0x252C;
    bottom_t = 0x2534;
    left_t = 0x251C;
    right_t = 0x2524;
    cross = 0x253C;
  }

let double =
  {
    top_left = 0x2554;
    top_right = 0x2557;
    bottom_left = 0x255A;
    bottom_right = 0x255D;
    horizontal = 0x2550;
    vertical = 0x2551;
    top_t = 0x2566;
    bottom_t = 0x2569;
    left_t = 0x2560;
    right_t = 0x2563;
    cross = 0x256C;
  }

let rounded =
  {
    single with
    top_left = 0x256D;
    top_right = 0x256E;
    bottom_left = 0x2570;
    bottom_right = 0x256F;
  }

let heavy =
  {
    top_left = 0x250F;
    top_right = 0x2513;
    bottom_left = 0x2517;
    bottom_right = 0x251B;
    horizontal = 0x2501;
    vertical = 0x2503;
    top_t = 0x2533;
    bottom_t = 0x253B;
    left_t = 0x2523;
    right_t = 0x252B;
    cross = 0x254B;
  }

let ascii =
  let c v = Char.code v in
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
  let s = 32 in
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
  let u = function None -> None | Some c -> Some (Uchar.to_int c) in
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
