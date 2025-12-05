type t = {
  mutable fg_r : float;
  mutable fg_g : float;
  mutable fg_b : float;
  mutable fg_a : float;
  mutable bg_r : float;
  mutable bg_g : float;
  mutable bg_b : float;
  mutable bg_a : float;
  mutable attrs : int;
}

let create () =
  {
    fg_r = -1.;
    fg_g = -1.;
    fg_b = -1.;
    fg_a = -1.;
    bg_r = -1.;
    bg_g = -1.;
    bg_b = -1.;
    bg_a = -1.;
    attrs = -1;
  }

let reset t =
  t.fg_r <- -1.;
  t.fg_g <- -1.;
  t.fg_b <- -1.;
  t.fg_a <- -1.;
  t.bg_r <- -1.;
  t.bg_g <- -1.;
  t.bg_b <- -1.;
  t.bg_a <- -1.;
  t.attrs <- -1

let[@inline] floats_neq a b = Float.abs (a -. b) > 0.00001

let[@inline] float_to_u8 f =
  let v = Float.round (f *. 255.) |> int_of_float in
  if v < 0 then 0 else if v > 255 then 255 else v

let attr_enable_code = function
  | Attr.Bold -> 1
  | Attr.Dim -> 2
  | Attr.Italic -> 3
  | Attr.Underline -> 4
  | Attr.Double_underline -> 21
  | Attr.Blink -> 5
  | Attr.Inverse -> 7
  | Attr.Hidden -> 8
  | Attr.Strikethrough -> 9
  | Attr.Overline -> 53
  | Attr.Framed -> 51
  | Attr.Encircled -> 52

let update t w ~fg_r ~fg_g ~fg_b ~fg_a ~bg_r ~bg_g ~bg_b ~bg_a ~attrs =
  let had_fg = t.fg_r >= 0. && t.fg_g >= 0. && t.fg_b >= 0. && t.fg_a >= 0. in
  let fg_diff =
    floats_neq fg_r t.fg_r || floats_neq fg_g t.fg_g || floats_neq fg_b t.fg_b
    || floats_neq fg_a t.fg_a
  in
  let bg_visible = bg_a > 0.0001 in
  let prev_bg_visible = t.bg_a > 0.0001 in
  let bg_diff =
    bg_visible <> prev_bg_visible
    || bg_visible
       && (floats_neq bg_r t.bg_r || floats_neq bg_g t.bg_g
         || floats_neq bg_b t.bg_b)
  in
  let attr_diff = attrs <> t.attrs in

  if fg_diff || bg_diff || attr_diff then (
    Escape.sgr_direct
      (fun push ->
        push 0;

        (* reset *)
        if fg_diff || had_fg then (
          push 38;
          push 2;
          push (float_to_u8 fg_r);
          push (float_to_u8 fg_g);
          push (float_to_u8 fg_b));

        if bg_visible then (
          push 48;
          push 2;
          push (float_to_u8 bg_r);
          push (float_to_u8 bg_g);
          push (float_to_u8 bg_b));

        if attrs <> 0 then
          Attr.iter
            (fun flag -> push (attr_enable_code flag))
            (Attr.unpack attrs))
      w;

    t.fg_r <- fg_r;
    t.fg_g <- fg_g;
    t.fg_b <- fg_b;
    t.fg_a <- fg_a;
    t.bg_r <- bg_r;
    t.bg_g <- bg_g;
    t.bg_b <- bg_b;
    t.bg_a <- bg_a;
    t.attrs <- attrs)
