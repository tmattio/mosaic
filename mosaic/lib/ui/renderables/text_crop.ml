let measure ~width_method s = Matrix.Text.measure ~width_method ~tab_width:2 s

let crop_to_width ~width_method text target_width =
  if target_width <= 0 then ""
  else begin
    let result = Buffer.create (String.length text) in
    let current_width = ref 0 in
    let stop = ref false in
    Matrix.Text.iter_graphemes
      (fun ~offset ~len ->
        if not !stop then begin
          let gw =
            Matrix.Text.measure_sub ~width_method ~tab_width:2 text ~pos:offset
              ~len
          in
          if !current_width + gw <= target_width then begin
            Buffer.add_substring result text offset len;
            current_width := !current_width + gw
          end
          else stop := true
        end)
      text;
    Buffer.contents result
  end

let truncate_with_ellipsis ~width_method ?(ellipsis = "...") text target_width =
  let tw = measure ~width_method text in
  if tw <= target_width then text
  else
    let ew = measure ~width_method ellipsis in
    if target_width <= ew then crop_to_width ~width_method text target_width
    else crop_to_width ~width_method text (target_width - ew) ^ ellipsis
