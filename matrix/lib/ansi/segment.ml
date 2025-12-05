module Esc = Escape

let emit ?prev ?(hyperlinks_enabled = true) (w : Esc.writer) spans =
  let prev_style = ref (Option.value prev ~default:Style.default) in
  let prev_link = ref (Style.link !prev_style) in
  let link_active = ref (Option.is_some !prev_link) in
  List.iter
    (fun (style, text) ->
      let next_link = Style.link style in
      if !prev_link <> next_link then (
        if !link_active then (
          Esc.hyperlink_end w;
          link_active := false);
        match next_link with
        | Some url when hyperlinks_enabled ->
            Esc.hyperlink_start ~url w;
            link_active := true
        | Some _ ->
            (* Hyperlinks disabled - skip OSC 8 sequences *)
            ()
        | None -> ());
      Style.emit ~prev:!prev_style style w;
      Esc.literal text w;
      prev_style := style;
      prev_link := next_link)
    spans;
  (!prev_style, if !link_active then !prev_link else None)

let render ?prev ?(hyperlinks_enabled = true) spans =
  let t (w : Esc.writer) =
    let _, active_link = emit ?prev ~hyperlinks_enabled w spans in
    (match active_link with None -> () | Some _ -> Esc.hyperlink_end w);
    Esc.reset w
  in
  Esc.to_string t
