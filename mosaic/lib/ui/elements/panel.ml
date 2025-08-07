(* Use qualified names instead of open to avoid name conflicts *)
module E = Element

type align = [ `Left | `Center | `Right ]

let panel ?(box_style = Border.Rounded) ?title ?(title_align = `Center)
    ?subtitle ?(subtitle_align = `Center) ?(expand = false)
    ?(style = Style.empty) ?(border_style = Style.empty) ?width ?height ?padding
    ?(highlight = false) child =
  (* Apply default if no fg set *)
  let border_style =
    if border_style.fg = None then
      Style.(border_style ++ fg Ansi.Default ++ dim)
    else border_style
  in
  
  let styled_child =
    if Style.equal style Style.empty then child else E.styled style child
  in
  let actual_padding = Option.value padding ~default:(E.xy 1 0) in
  
  (* Prepare text overlays for borders *)
  let title_style =
    if highlight then Style.merge border_style Style.bold else border_style
  in
  
  let top_text = 
    match title with
    | Some "" | None -> None
    | Some t -> Some { Border.text = t; align = title_align; style = Some title_style }
  in
  
  let bottom_text = 
    match subtitle with
    | Some "" | None -> None
    | Some s -> Some { Border.text = s; align = subtitle_align; style = Some title_style }
  in
  
  (* Create border with text overlays *)
  let border = Border.make ~line_style:box_style ?top_text ?bottom_text () in
  let border = Border.with_style border border_style in
  
  (* Calculate minimum width based on title/subtitle with padding *)
  let title_min_width = 
    match title with
    | Some t when t <> "" -> String.length t + 4 (* 2 for padding, 2 for border chars *)
    | _ -> 0
  in
  let subtitle_min_width = 
    match subtitle with
    | Some s when s <> "" -> String.length s + 4
    | _ -> 0
  in
  let text_based_min_width = max title_min_width subtitle_min_width in
  
  (* Create the panel using a regular box with the enhanced border *)
  let panel_content = 
    E.box
      ~border
      ~border_style
      ~padding:actual_padding
      ?width:(Option.map (fun w -> `Cells w) width)
      ?height:(Option.map (fun h -> `Cells h) height)
      ~min_width:(if text_based_min_width > 0 then `Cells text_based_min_width else `Auto)
      ~flex_grow:(if expand && width = None then 1.0 else 0.0)
      ~align_self:(if expand && width = None then `Stretch else `Start)
      [ styled_child ]
  in
  panel_content
