let cache_size = 9

type 'a cache_entry = {
  known_dimensions : float option Geometry.size;
  available_space : Available_space.t Geometry.size;
  content : 'a;
}

type t = {
  mutable final_layout_entry : Layout_output.t cache_entry option;
  mutable measure_entries : float Geometry.size cache_entry option array;
  mutable is_empty : bool;
}

let make () =
  {
    final_layout_entry = None;
    measure_entries = Array.make cache_size None;
    is_empty = true;
  }

type clear_state = Cleared | Already_empty

let compute_cache_slot known_dimensions available_space =
  let open Available_space in
  let has_known_width = Option.is_some known_dimensions.Geometry.Size.width in
  let has_known_height = Option.is_some known_dimensions.Geometry.Size.height in

  (* Slot 0: Both known_dimensions were set *)
  if has_known_width && has_known_height then 0
    (* Slots 1-2: width but not height known_dimension was set *)
  else if has_known_width && not has_known_height then
    1 + if available_space.Geometry.Size.height = Min_content then 1 else 0
    (* Slots 3-4: height but not width known_dimension was set *)
  else if has_known_height && not has_known_width then
    3 + if available_space.Geometry.Size.width = Min_content then 1 else 0
  (* Slots 5-8: Neither known_dimensions were set *)
    else
    match
      (available_space.Geometry.Size.width, available_space.Geometry.Size.height)
    with
    | (Max_content | Definite _), (Max_content | Definite _) -> 5
    | (Max_content | Definite _), Min_content -> 6
    | Min_content, (Max_content | Definite _) -> 7
    | Min_content, Min_content -> 8

let is_roughly_equal av1 av2 =
  let open Available_space in
  match (av1, av2) with
  | Definite x, Definite y -> Float.abs (x -. y) < 0.0001
  | Min_content, Min_content -> true
  | Max_content, Max_content -> true
  | _ -> false

let get t ~known_dimensions ~available_space ~run_mode =
  let open Run_mode in
  match run_mode with
  | Perform_layout ->
      Option.bind t.final_layout_entry (fun entry ->
          let cached_size = entry.content.Layout_output.size in
          if
            (known_dimensions.Geometry.Size.width
             = entry.known_dimensions.Geometry.Size.width
            || known_dimensions.Geometry.Size.width
               = Some cached_size.Geometry.Size.width)
            && (known_dimensions.Geometry.Size.height
                = entry.known_dimensions.Geometry.Size.height
               || known_dimensions.Geometry.Size.height
                  = Some cached_size.Geometry.Size.height)
            && (Option.is_some known_dimensions.Geometry.Size.width
               || is_roughly_equal entry.available_space.Geometry.Size.width
                    available_space.Geometry.Size.width)
            && (Option.is_some known_dimensions.Geometry.Size.height
               || is_roughly_equal entry.available_space.Geometry.Size.height
                    available_space.Geometry.Size.height)
          then Some entry.content
          else None)
  | Compute_size ->
      let rec loop i =
        if i >= cache_size then None
        else
          match t.measure_entries.(i) with
          | None -> loop (i + 1)
          | Some entry ->
              let cached_size = entry.content in
              if
                (known_dimensions.Geometry.Size.width
                 = entry.known_dimensions.Geometry.Size.width
                || known_dimensions.Geometry.Size.width
                   = Some cached_size.Geometry.Size.width)
                && (known_dimensions.Geometry.Size.height
                    = entry.known_dimensions.Geometry.Size.height
                   || known_dimensions.Geometry.Size.height
                      = Some cached_size.Geometry.Size.height)
                && (Option.is_some known_dimensions.Geometry.Size.width
                   || is_roughly_equal entry.available_space.Geometry.Size.width
                        available_space.Geometry.Size.width)
                && (Option.is_some known_dimensions.Geometry.Size.height
                   || is_roughly_equal
                        entry.available_space.Geometry.Size.height
                        available_space.Geometry.Size.height)
              then Some (Layout_output.from_outer_size cached_size)
              else loop (i + 1)
      in
      loop 0
  | Perform_hidden_layout -> None

let store t ~known_dimensions ~available_space ~run_mode layout_output =
  let open Run_mode in
  match run_mode with
  | Perform_layout ->
      t.is_empty <- false;
      t.final_layout_entry <-
        Some { known_dimensions; available_space; content = layout_output }
  | Compute_size ->
      t.is_empty <- false;
      let cache_slot = compute_cache_slot known_dimensions available_space in
      t.measure_entries.(cache_slot) <-
        Some
          {
            known_dimensions;
            available_space;
            content = layout_output.Layout_output.size;
          }
  | Perform_hidden_layout -> ()

let clear t =
  if t.is_empty then Already_empty
  else (
    t.is_empty <- true;
    t.final_layout_entry <- None;
    Array.fill t.measure_entries 0 cache_size None;
    Cleared)

let is_empty t =
  t.final_layout_entry = None
  && not (Array.exists Option.is_some t.measure_entries)
