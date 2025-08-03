open Ansi

let () = Random.self_init ()
let rec ( -- ) a b = if a > b then [] else a :: (succ a -- b)

let utf8_of_code_point =
  let buf = Buffer.create 7 in
  fun cp ->
    Buffer.clear buf;
    Uutf.Buffer.add_utf_8 buf (Uchar.of_int cp);
    Buffer.contents buf

let nsym = 4096
let glitch = nsym / 20

let symbols =
  Array.(
    concat
      [
        init 58 (fun x -> utf8_of_code_point (0xff66 + x));
        init 10 (fun x -> utf8_of_code_point (0x30 + x));
      ])

let sym () = symbols.(Random.int (Array.length symbols))
let syms = Array.init nsym (fun _ -> sym ())

let gen_wait h = `Wait Random.(int (h / 2))

and gen_line h =
  `Line Random.(0, int (nsym - h), int (h + (h / 2)) + 1, int 2 + 1)

let gen (w, h) =
  let lines =
    1 -- w
    |> List.map @@ fun _ ->
       if Random.float 1. < 0.1 then gen_line h else gen_wait h
  in
  ((w, h), lines)

let step ((w, h), xs) =
  let xs =
    xs
    |> List.map @@ function
       | `Wait 0 -> gen_line h
       | `Wait n -> `Wait (n - 1)
       | `Line (i, _, win, k) when i - win + k >= h -> gen_wait h
       | `Line (i, s, win, k) -> `Line (i + k, s, win, k)
  in
  Random.(
    for _ = 0 to int glitch do
      syms.(int nsym) <- sym ()
    done);
  ((w, h), xs)

let bgc = Style.make ~bg:Style.Black ()

let color i n =
  let t = float i /. float n in
  let t1 = exp (-.t /. 0.02) in
  (* Fast decay for red/blue *)
  let t2 = exp (-.t /. 0.45) in
  (* Slow decay for green *)

  (* Convert to 8-bit values *)
  let r = int_of_float (t1 *. 255.) in
  let g = int_of_float (t2 *. 255.) in
  let b = r in
  (* Same as red *)

  (* Use RGB but be aware it will be quantized to 7-7-8 bits internally *)
  Style.make ~fg:(Style.RGB (r, g, b)) ~bg:Style.Black ()

let show ((w, h), xs) grid =
  (* Clear the grid first *)
  Grid.clear grid;

  (* Fill background with black *)
  for row = 0 to h - 1 do
    for col = 0 to w - 1 do
      Grid.set_grapheme grid ~row ~col ~glyph:" " ~attrs:bgc
    done
  done;

  (* Render each column *)
  List.iteri
    (fun col_idx -> function
      | `Wait _ -> ()
      | `Line (i, sym, win, _) ->
          let last = i - win in
          let off = max 0 (i - h + 1) in

          (* Build list of characters like Notty *)
          let rec chars w =
            let ix = w + last in
            if 0 <= min ix w then syms.(sym + ix) :: chars (w - 1) else []
          in

          (* Render images with correct indexing - matches Notty exactly *)
          let rec images acc idx = function
            | [] -> acc (* Don't reverse - Notty returns in reverse order *)
            | x :: xs ->
                let style = color idx win in
                images ((x, style) :: acc) (idx + 1) xs
          in

          let char_list = chars (win - off - 1) in
          let styled_chars = images [] off char_list in

          (* Place characters on grid with vertical padding *)
          let vpad = max 0 (i - win) in
          List.iteri
            (fun idx (char, style) ->
              let row = vpad + idx in
              if row >= 0 && row < h then
                Grid.set_grapheme grid ~row ~col:col_idx ~glyph:char
                  ~attrs:style)
            styled_chars)
    xs

let frame = 0.075

let () =
  let term = Tty.create Unix.stdin Unix.stdout in
  try
    Tty.set_mode term `Raw;
    Tty.hide_cursor term;
    Tty.enable_alternate_screen term;

    let cols, rows = Tty.size term in
    let screen = Screen.create ~rows ~cols () in
    let state = ref (gen (cols, rows)) in

    let rec loop deadline =
      let now = Unix.gettimeofday () in
      if deadline <= now then (
        Screen.begin_frame screen;
        Grid.with_updates (Screen.back screen) (fun grid -> show !state grid);
        let patches = Screen.render screen in
        let sgr = Screen.patches_to_sgr patches in
        Tty.write_string term sgr;
        Tty.flush term;
        state := step !state;
        loop (frame +. deadline))
      else
        let delay = deadline -. now in
        if Tty.wait_for_input term delay then
          let buf = Bytes.create 256 in
          let n = Tty.read term buf 0 256 in
          if n > 0 then
            let s = Bytes.sub_string buf 0 n in
            (* Check for escape sequences *)
            if String.contains s '\027' || String.contains s '\003' then
              raise Exit
            else if String.contains s ' ' then (
              (* Space key pressed - regenerate *)
              let cols, rows = Tty.size term in
              state := gen (cols, rows);
              loop deadline)
            else loop deadline
          else loop deadline
        else loop deadline
    in

    loop (Unix.gettimeofday ())
  with
  | Exit | Sys.Break ->
      Tty.disable_alternate_screen term;
      Tty.show_cursor term;
      Tty.release term
  | e ->
      Tty.disable_alternate_screen term;
      Tty.show_cursor term;
      Tty.release term;
      raise e
