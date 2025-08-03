open Types

let validate_color c =
  c.r >= 0 && c.r <= 255 && c.g >= 0 && c.g <= 255 && c.b >= 0 && c.b <= 255

let of_array colors =
  let len = Array.length colors in
  if len = 0 then Error (Invalid_argument "Palette cannot be empty")
  else if len > 256 then Error (Palette_too_large len)
  else
    let rec check_colors i =
      if i >= Array.length colors then Ok (Array.copy colors)
      else
        let c = colors.(i) in
        if not (validate_color c) then
          Error (Color_out_of_range (c.r, c.g, c.b))
        else check_colors (i + 1)
    in
    check_colors 0

let to_array p = Array.copy p
let size p = Array.length p
let get p idx = if idx >= 0 && idx < Array.length p then Some p.(idx) else None
let get_exn p idx = p.(idx)
let get_opt p idx = get p idx

module Quantize = struct
  let find_nearest_color (r, g, b) palette =
    let min_dist = ref max_int in
    let best_idx = ref 0 in
    for i = 0 to Array.length palette - 1 do
      let c = palette.(i) in
      let dist = abs (r - c.r) + abs (g - c.g) + abs (b - c.b) in
      if dist < !min_dist then (
        min_dist := dist;
        best_idx := i)
    done;
    !best_idx

  let count_unique_colors rgb_data =
    let module ColorMap = Map.Make (struct
      type t = int * int * int

      let compare (r1, g1, b1) (r2, g2, b2) =
        match compare r1 r2 with
        | 0 -> ( match compare g1 g2 with 0 -> compare b1 b2 | c -> c)
        | c -> c
    end) in
    let counts =
      Array.fold_left
        (fun acc color ->
          ColorMap.update color
            (function None -> Some 1 | Some n -> Some (n + 1))
            acc)
        ColorMap.empty rgb_data
    in
    ColorMap.bindings counts

  type box = {
    colors : (int * int * int * int) array; (* r, g, b, count *)
    start_idx : int;
    end_idx : int; (* exclusive *)
  }

  let box_volume box =
    if box.start_idx >= box.end_idx then 0
    else
      let min_r = ref 255 and max_r = ref 0 in
      let min_g = ref 255 and max_g = ref 0 in
      let min_b = ref 255 and max_b = ref 0 in
      for i = box.start_idx to box.end_idx - 1 do
        let r, g, b, _ = box.colors.(i) in
        min_r := min !min_r r;
        max_r := max !max_r r;
        min_g := min !min_g g;
        max_g := max !max_g g;
        min_b := min !min_b b;
        max_b := max !max_b b
      done;
      (!max_r - !min_r + 1) * (!max_g - !min_g + 1) * (!max_b - !min_b + 1)

  let split_box box =
    let start_idx = box.start_idx in
    let end_idx = box.end_idx in
    let n = end_idx - start_idx in

    if n <= 1 then None
    else
      (* Find dimension with greatest range *)
      let min_r = ref 255 and max_r = ref 0 in
      let min_g = ref 255 and max_g = ref 0 in
      let min_b = ref 255 and max_b = ref 0 in
      for i = start_idx to end_idx - 1 do
        let r, g, b, _ = box.colors.(i) in
        min_r := min !min_r r;
        max_r := max !max_r r;
        min_g := min !min_g g;
        max_g := max !max_g g;
        min_b := min !min_b b;
        max_b := max !max_b b
      done;

      let range_r = !max_r - !min_r in
      let range_g = !max_g - !min_g in
      let range_b = !max_b - !min_b in

      (* Sort along longest dimension *)
      let compare_fn =
        if range_r >= range_g && range_r >= range_b then
          fun (r1, _, _, _) (r2, _, _, _) -> compare r1 r2
        else if range_g >= range_b then fun (_, g1, _, _) (_, g2, _, _) ->
          compare g1 g2
        else fun (_, _, b1, _) (_, _, b2, _) -> compare b1 b2
      in

      (* Sort the segment in-place *)
      let segment = Array.sub box.colors start_idx n in
      Array.sort compare_fn segment;
      Array.blit segment 0 box.colors start_idx n;

      (* Split at median *)
      let mid = start_idx + (n / 2) in
      Some ({ box with end_idx = mid }, { box with start_idx = mid })

  let box_average box =
    let total_count = ref 0 in
    let sum_r = ref 0 in
    let sum_g = ref 0 in
    let sum_b = ref 0 in
    for i = box.start_idx to box.end_idx - 1 do
      let r, g, b, count = box.colors.(i) in
      total_count := !total_count + count;
      sum_r := !sum_r + (r * count);
      sum_g := !sum_g + (g * count);
      sum_b := !sum_b + (b * count)
    done;
    if !total_count = 0 then { r = 0; g = 0; b = 0 }
    else
      {
        r = !sum_r / !total_count;
        g = !sum_g / !total_count;
        b = !sum_b / !total_count;
      }

  let create_palette rgb_data max_colors =
    if max_colors > 256 then Error (Palette_too_large max_colors)
    else if max_colors <= 0 then
      Error (Invalid_argument "max_colors must be positive")
    else
      (* Count unique colors *)
      let unique_colors = count_unique_colors rgb_data in
      let num_unique = List.length unique_colors in

      (* If we have fewer unique colors than requested, use them all *)
      if num_unique <= max_colors then
        let colors =
          unique_colors
          |> List.map (fun ((r, g, b), _) -> { r; g; b })
          |> Array.of_list
        in
        of_array colors
      else
        (* Convert to array for median cut *)
        let color_array =
          unique_colors
          |> List.map (fun ((r, g, b), count) -> (r, g, b, count))
          |> Array.of_list
        in

        (* Start with one box containing all colors *)
        let initial_box =
          {
            colors = color_array;
            start_idx = 0;
            end_idx = Array.length color_array;
          }
        in

        (* Priority queue of boxes (using volume as priority) *)
        let module BoxSet = Set.Make (struct
          type t = int * box (* volume * box *)

          let compare (v1, b1) (v2, b2) =
            (* Larger volume first *)
            match compare v2 v1 with
            | 0 -> compare b1.start_idx b2.start_idx (* tie breaker *)
            | c -> c
        end) in
        let rec median_cut boxes n =
          if n >= max_colors then boxes
          else
            match BoxSet.min_elt_opt boxes with
            | None -> boxes
            | Some (_, box) -> (
                let boxes' = BoxSet.remove (box_volume box, box) boxes in
                match split_box box with
                | None -> boxes (* Can't split further *)
                | Some (box1, box2) ->
                    let vol1 = box_volume box1 in
                    let vol2 = box_volume box2 in
                    let boxes'' =
                      boxes'
                      |> BoxSet.add (vol1, box1)
                      |> BoxSet.add (vol2, box2)
                    in
                    median_cut boxes'' (n + 1))
        in

        let initial_set =
          BoxSet.singleton (box_volume initial_box, initial_box)
        in
        let final_boxes = median_cut initial_set 1 in

        (* Extract average colors from boxes *)
        let colors =
          BoxSet.elements final_boxes
          |> List.map (fun (_, box) -> box_average box)
          |> Array.of_list
        in
        of_array colors
end

let rgb_to_indexed ?dither ~width rgb_data palette =
  let height = Array.length rgb_data / width in
  if Array.length rgb_data mod width <> 0 then
    Error (Invalid_argument "rgb_data length not divisible by width")
  else if height = 0 then Error (Invalid_dimensions (width, 0))
  else
    let indexed = Bytes.create (width * height) in

    match dither with
    | None | Some No_dither ->
        (* Simple nearest color mapping *)
        for i = 0 to Array.length rgb_data - 1 do
          let idx = Quantize.find_nearest_color rgb_data.(i) palette in
          Bytes.set indexed i (Char.chr idx)
        done;
        Ok indexed
    | Some Floyd_steinberg ->
        (* Floyd-Steinberg dithering *)
        (* Create a working copy of the image *)
        let work =
          Array.map (fun (r, g, b) -> (float r, float g, float b)) rgb_data
        in

        for y = 0 to height - 1 do
          for x = 0 to width - 1 do
            let i = (y * width) + x in
            let r, g, b = work.(i) in

            (* Clamp to valid range *)
            let r' = int_of_float (max 0. (min 255. r)) in
            let g' = int_of_float (max 0. (min 255. g)) in
            let b' = int_of_float (max 0. (min 255. b)) in

            (* Find nearest color *)
            let idx = Quantize.find_nearest_color (r', g', b') palette in
            Bytes.set indexed i (Char.chr idx);

            (* Calculate error *)
            let c = palette.(idx) in
            let err_r = r -. float c.r in
            let err_g = g -. float c.g in
            let err_b = b -. float c.b in

            (* Distribute error to neighbors *)
            let distribute x_off y_off factor =
              let x' = x + x_off in
              let y' = y + y_off in
              if x' >= 0 && x' < width && y' >= 0 && y' < height then
                let i' = (y' * width) + x' in
                let r', g', b' = work.(i') in
                work.(i') <-
                  ( r' +. (err_r *. factor),
                    g' +. (err_g *. factor),
                    b' +. (err_b *. factor) )
            in

            distribute 1 0 (7. /. 16.);
            (* right *)
            distribute (-1) 1 (3. /. 16.);
            (* bottom-left *)
            distribute 0 1 (5. /. 16.);
            (* bottom *)
            distribute 1 1 (1. /. 16.)
            (* bottom-right *)
          done
        done;

        Ok indexed
