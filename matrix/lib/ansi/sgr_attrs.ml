let all = Attr.unpack (-1)
let flags = Attr.to_list all
let mask flag = Attr.pack (Attr.add flag Attr.empty)

let enabled =
  Array.of_list
    (List.map (fun flag -> (mask flag, Attr.flag_to_sgr_code flag)) flags)

let disabled =
  let codes = ref [] in
  Attr.iter_sgr_disable_codes (fun code -> codes := code :: !codes) all;
  Array.of_list
    (List.rev_map
       (fun code ->
         let group =
           List.fold_left
             (fun acc flag ->
               if Attr.flag_to_sgr_disable_code flag = code then
                 acc lor mask flag
               else acc)
             0 flags
         in
         (group, code))
       !codes)

let intensity = Attr.pack (Attr.union Attr.bold Attr.dim)
let underline = Attr.pack (Attr.union Attr.underline Attr.double_underline)
let enclosure = Attr.pack (Attr.union Attr.framed Attr.encircled)

let added ~prev ~attrs =
  let removed = prev land lnot attrs in
  let added = attrs land lnot prev in
  let added =
    if removed land intensity <> 0 then added lor (attrs land intensity)
    else added
  in
  let added =
    if removed land underline <> 0 then added lor (attrs land underline)
    else added
  in
  if removed land enclosure <> 0 then added lor (attrs land enclosure)
  else added

let rec emit_codes w first codes i remaining =
  if remaining = 0 then first
  else
    let mask, code = Array.unsafe_get codes i in
    let first =
      if remaining land mask = 0 then first
      else (
        if not first then Escape.sgr_sep w;
        Escape.sgr_code w code;
        false)
    in
    emit_codes w first codes (i + 1) (remaining land lnot mask)

let emit_enabled w first attrs =
  emit_codes w first enabled 0 (attrs land Attr.pack all)

let emit_disabled w first attrs =
  emit_codes w first disabled 0 (attrs land Attr.pack all)

let rec iter_codes f codes i remaining =
  if remaining <> 0 then (
    let mask, code = Array.unsafe_get codes i in
    if remaining land mask <> 0 then f code;
    iter_codes f codes (i + 1) (remaining land lnot mask))

let iter_enabled f attrs = iter_codes f enabled 0 (attrs land Attr.pack all)
let iter_disabled f attrs = iter_codes f disabled 0 (attrs land Attr.pack all)
