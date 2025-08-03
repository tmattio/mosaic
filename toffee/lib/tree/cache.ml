open Geometry
open Layout
module Available_space = Style.Available_space

module Clear_state = struct
  type t = Already_empty | Cleared
end

type clear_state = Clear_state.t

let cache_size = 9

type 'a cache_entry = {
  known_dimensions : float option size;
  available_space : Available_space.t size;
  content : 'a;
}

type t = {
  mutable final_layout_entry : Layout_output.t cache_entry option;
  mutable measure_entries : float size cache_entry option array;
  mutable is_empty : bool;
}

let empty () =
  {
    final_layout_entry = None;
    measure_entries =
      Array.make cache_size (None : float size cache_entry option);
    is_empty = true;
  }

let roughly_equal a b =
  match (a, b) with
  | Available_space.Min_content, Available_space.Min_content
  | Available_space.Max_content, Available_space.Max_content ->
      true
  | Available_space.Definite x, Available_space.Definite y ->
      Float.abs (x -. y) < 1e-6
  | _ -> false

let compute_cache_slot known_dimensions available_space =
  let has_known_width = Option.is_some known_dimensions.width in
  let has_known_height = Option.is_some known_dimensions.height in

  if has_known_width && has_known_height then 0
  else if has_known_width && not has_known_height then
    if available_space.height = Available_space.Min_content then 2 else 1
  else if has_known_height && not has_known_width then
    if available_space.width = Available_space.Min_content then 4 else 3
  else
    match (available_space.width, available_space.height) with
    | ( (Available_space.Max_content | Available_space.Definite _),
        (Available_space.Max_content | Available_space.Definite _) ) ->
        5
    | ( (Available_space.Max_content | Available_space.Definite _),
        Available_space.Min_content ) ->
        6
    | ( Available_space.Min_content,
        (Available_space.Max_content | Available_space.Definite _) ) ->
        7
    | Available_space.Min_content, Available_space.Min_content -> 8

let is_empty c =
  c.final_layout_entry = None
  && not (Array.exists Option.is_some c.measure_entries)

let clear c =
  if c.is_empty then Clear_state.Already_empty
  else (
    c.is_empty <- true;
    c.final_layout_entry <- None;
    Array.fill c.measure_entries 0 cache_size None;
    Clear_state.Cleared)

let get c ~known_dimensions ~available_space ~run_mode =
  match run_mode with
  | Run_mode.Perform_hidden_layout -> None
  | Run_mode.Perform_layout -> (
      match c.final_layout_entry with
      | None -> None
      | Some entry ->
          let cached_size = entry.content.size in
          let matches =
            (known_dimensions.width = entry.known_dimensions.width
            || known_dimensions.width = Some cached_size.width)
            && (known_dimensions.height = entry.known_dimensions.height
               || known_dimensions.height = Some cached_size.height)
            && (Option.is_some known_dimensions.width
               || roughly_equal entry.available_space.width
                    available_space.width)
            && (Option.is_some known_dimensions.height
               || roughly_equal entry.available_space.height
                    available_space.height)
          in
          if matches then Option.Some entry.content else Option.None)
  | Run_mode.Compute_size ->
      let found = ref None in
      Array.iter
        (function
          | None -> ()
          | Some _entry when !found <> None -> ()
          | Some entry ->
              let cached_size = entry.content in
              let matches =
                (known_dimensions.width = entry.known_dimensions.width
                || known_dimensions.width = Some cached_size.width)
                && (known_dimensions.height = entry.known_dimensions.height
                   || known_dimensions.height = Some cached_size.height)
                && (Option.is_some known_dimensions.width
                   || roughly_equal entry.available_space.width
                        available_space.width)
                && (Option.is_some known_dimensions.height
                   || roughly_equal entry.available_space.height
                        available_space.height)
              in
              if matches then
                found := Some (Layout_output.of_outer_size cached_size))
        c.measure_entries;
      !found

let store c ~known_dimensions ~available_space ~run_mode ~layout_output =
  c.is_empty <- false;
  match run_mode with
  | Run_mode.Perform_layout ->
      c.final_layout_entry <-
        Some { known_dimensions; available_space; content = layout_output }
  | Run_mode.Compute_size ->
      let cache_slot = compute_cache_slot known_dimensions available_space in
      c.measure_entries.(cache_slot) <-
        Some { known_dimensions; available_space; content = layout_output.size }
  | Run_mode.Perform_hidden_layout -> ()
