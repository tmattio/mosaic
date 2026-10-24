let measure ~width_method s = Matrix.Text.measure ~width_method ~tab_width:2 s

let crop_to_width ~width_method text target_width =
  if target_width <= 0 then ""
  else begin
    let module Source = Matrix.Text.Layout.Source in
    let source = Source.of_string text in
    let lo = ref 0 and hi = ref (Source.length source) in
    while !lo < !hi do
      let mid = !lo + ((!hi - !lo + 1) / 2) in
      if Source.columns ~width_method source ~first:0 ~last:mid <= target_width
      then lo := mid
      else hi := mid - 1
    done;
    Source.slice source ~first:0 ~last:!lo
  end

let truncate_with_ellipsis ~width_method ?(ellipsis = "...") text target_width =
  let tw = measure ~width_method text in
  if tw <= target_width then text
  else
    let ew = measure ~width_method ellipsis in
    if target_width <= ew then crop_to_width ~width_method text target_width
    else crop_to_width ~width_method text (target_width - ew) ^ ellipsis
