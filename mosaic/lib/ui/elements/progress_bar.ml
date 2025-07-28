(* progress_bar.ml *)

(* --- Preset Definitions --- *)

type preset_def = {
  delimiters : (string * string) option;
  filled_char : string;
  empty_char : string;
  progress_stages : string list;
}

type preset =
  (* Classic Styles *)
  | ASCII
  | UTF8
  (* Line Styles *)
  | Line_double
  | Line_single
  | Line_arrow
  (* Block Styles *)
  | Block_shade_light
  | Block_shade_medium
  | Block_shade_dark
  | Block_dotted
  (* Custom record for full control *)
  | Custom of preset_def

let presets = function
  | ASCII ->
      {
        delimiters = Some ("[", "]");
        filled_char = "#";
        empty_char = "-";
        progress_stages = [];
      }
  | UTF8 ->
      {
        delimiters = Some ("│", "│");
        filled_char = "█";
        empty_char = " ";
        progress_stages = [ "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉" ];
      }
  | Line_double ->
      {
        delimiters = Some ("╢", "╟");
        filled_char = "═";
        empty_char = "─";
        progress_stages = [ "-" ];
      }
  | Line_single ->
      {
        delimiters = Some ("├", "┤");
        filled_char = "━";
        empty_char = "─";
        progress_stages = [ "-" ];
      }
  | Line_arrow ->
      {
        delimiters = Some ("", "");
        filled_char = "=";
        empty_char = "-";
        progress_stages = [ ">" ];
      }
  | Block_shade_light ->
      {
        delimiters = None;
        filled_char = "▓";
        empty_char = "░";
        progress_stages = [ "▒" ];
      }
  | Block_shade_medium ->
      {
        delimiters = None;
        filled_char = "█";
        empty_char = "▒";
        progress_stages = [ "▓" ];
      }
  | Block_shade_dark ->
      {
        delimiters = None;
        filled_char = "█";
        empty_char = "▓";
        progress_stages = [ "█" ];
      }
  | Block_dotted ->
      {
        delimiters = Some ("┋", "┋");
        filled_char = "⣿";
        empty_char = "⣀";
        progress_stages = [ "⣄"; "⣆"; "⣇"; "⣧"; "⣷" ];
      }
  | Custom def -> def

(* --- Rendering Logic --- *)

let render_pulse ~time ~width ~pulse_style ~bar_style ~delimiters ~filled_char =
  let l_delim, r_delim = Option.value delimiters ~default:("", "") in
  let l_width = String.length l_delim in
  let r_width = String.length r_delim in
  let bar_width = max 0 (width - l_width - r_width) in

  if bar_width <= 0 then
    Element.rich_text [ (l_delim, bar_style); (r_delim, bar_style) ]
  else
    let pulse_size = 20 in
    let offset = int_of_float (time *. 15.0) mod (bar_width + pulse_size) in

    let segments = ref [] in
    for i = 0 to bar_width - 1 do
      let pos_in_pulse = i - offset in
      let fade =
        if pos_in_pulse >= 0 && pos_in_pulse < pulse_size then
          0.5
          +. cos
               (float_of_int pos_in_pulse /. float_of_int pulse_size *. 2.0
              *. Float.pi)
             /. 2.0
        else 0.0
      in
      let is_pulse = fade > 0.7 in
      let style = if is_pulse then pulse_style else bar_style in
      segments := (filled_char, style) :: !segments
    done;

    Element.rich_text
      ([ (l_delim, bar_style) ] @ List.rev !segments @ [ (r_delim, bar_style) ])

let progress_bar
    (* Core State *)
    ?(total = Some 100.0) ?(completed = 0.0) ?(width = 20)
    (* Animation *)
    ?(pulse = false) ?(animation_time = 0.0)
    (* Styling *)
    ?(bar_style = Style.(fg Ansi.Default ++ dim))
    ?(complete_style = Style.(fg Green))
    ?(finished_style = Style.(fg Bright_green)) ?(pulse_style = Style.(fg Cyan))
    (* Theme / Visuals *)
    ?preset ?delimiters ?filled_char ?empty_char ?progress_stages () =
  (* 1. Resolve theme arguments based on preset and overrides *)
  let default_def =
    {
      delimiters = None;
      filled_char = "━";
      empty_char = "─";
      progress_stages = [ "╸" ];
    }
  in
  let base_def =
    match preset with Some p -> presets p | None -> default_def
  in

  let final_delimiters = Option.value delimiters ~default:base_def.delimiters in
  let final_filled_char =
    Option.value filled_char ~default:base_def.filled_char
  in
  let final_empty_char = Option.value empty_char ~default:base_def.empty_char in
  let final_progress_stages =
    Option.value progress_stages ~default:base_def.progress_stages
  in

  (* 2. Decide whether to pulse *)
  let should_pulse = pulse || total = None in

  if should_pulse then
    render_pulse ~time:animation_time ~width ~pulse_style ~bar_style
      ~delimiters:final_delimiters ~filled_char:final_filled_char
  else
    (* 3. Setup for determinate bar *)
    let is_finished =
      match total with None -> false | Some total -> completed >= total
    in
    let current_complete_style =
      if is_finished then finished_style else complete_style
    in

    let completed =
      match total with
      | None -> 0.0
      | Some total -> Float.min total (Float.max 0.0 completed)
    in

    let progress_ratio =
      match total with
      | None -> 0.0
      | Some total when total > 0.0 -> completed /. total
      | _ -> 0.0
    in

    (* 4. Calculate character counts with sub-character resolution *)
    let l_delim, r_delim = Option.value final_delimiters ~default:("", "") in
    let delim_width = String.length l_delim + String.length r_delim in
    let bar_width = max 0 (width - delim_width) in

    let num_stages = List.length final_progress_stages + 1 in
    let total_sub_steps = bar_width * num_stages in
    let completed_sub_steps =
      int_of_float (float_of_int total_sub_steps *. progress_ratio)
    in

    let filled_count = completed_sub_steps / num_stages in
    let stage_index = completed_sub_steps mod num_stages in

    let partial_char =
      if stage_index > 0 && not (List.is_empty final_progress_stages) then
        Some (List.nth final_progress_stages (stage_index - 1))
      else None
    in

    let empty_count =
      bar_width - filled_count - if partial_char <> None then 1 else 0
    in

    (* 5. Build the rich_text segments *)
    let segments = ref [] in

    (* Left delimiter *)
    if l_delim <> "" then segments := (l_delim, bar_style) :: !segments;

    (* Filled part *)
    (if filled_count > 0 then
       let s =
         String.concat "" (List.init filled_count (fun _ -> final_filled_char))
       in
       segments := (s, current_complete_style) :: !segments);

    (* Partial stage part *)
    (match partial_char with
    | Some c -> segments := (c, current_complete_style) :: !segments
    | None -> ());

    (* Empty part *)
    (if empty_count > 0 then
       let s =
         String.concat "" (List.init empty_count (fun _ -> final_empty_char))
       in
       segments := (s, bar_style) :: !segments);

    (* Right delimiter *)
    if r_delim <> "" then segments := (r_delim, bar_style) :: !segments;

    Element.rich_text (List.rev !segments)
