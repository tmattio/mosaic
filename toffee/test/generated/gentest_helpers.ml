(* Shared test helpers for generated tests *)
open Toffee
open Geometry

module Writing_mode = struct
  type t = Horizontal | Vertical
end

type measure_data =
  | Zero
  | Fixed of float size
  | Aspect_ratio of aspect_ratio_data
  | Ahem_text of ahem_text_data

and aspect_ratio_data = { width : float; height_ratio : float }
and ahem_text_data = { text_content : string; writing_mode : Writing_mode.t }

type test_node_context = { mutable count : int; measure_data : measure_data }

let new_test_tree () = new_tree ()
let zero = { count = 0; measure_data = Zero }
let fixed width height = { count = 0; measure_data = Fixed { width; height } }

let aspect_ratio width height_ratio =
  { count = 0; measure_data = Aspect_ratio { width; height_ratio } }

let ahem_text text_content writing_mode =
  { count = 0; measure_data = Ahem_text { text_content; writing_mode } }

let option_or_zero = function Some value -> value | None -> 0.0

let apply_known_dimension known fallback =
  match known with Some value -> value | None -> fallback

let measure_aspect_ratio (known_dimensions : float option size)
    (data : aspect_ratio_data) =
  let Size.{ width = known_width; height = known_height } = known_dimensions in
  let { width = data_width; height_ratio } = data in
  let width = apply_known_dimension known_width data_width in
  let height = apply_known_dimension known_height (width *. height_ratio) in
  Size.{ width; height }

let split_on_zws str =
  let zws =
    "\xE2\x80\x8B"
    (* U+200B zero width space *)
  in
  let zws_len = String.length zws in
  let rec loop acc start =
    match String.index_from_opt str start zws.[0] with
    | None -> List.rev (String.sub str start (String.length str - start) :: acc)
    | Some idx ->
        if
          idx + zws_len <= String.length str && String.sub str idx zws_len = zws
        then
          let part = String.sub str start (idx - start) in
          loop (part :: acc) (idx + zws_len)
        else loop acc (idx + 1)
  in
  loop [] 0

let measure_ahem_text (known_dimensions : float option size)
    (available_space : Available_space.t size) (data : ahem_text_data) =
  let { text_content; writing_mode } = data in
  let Size.{ width = known_width; height = known_height } = known_dimensions in
  let Size.{ width = available_width; height = available_height } =
    available_space
  in
  let inline_axis =
    match writing_mode with
    | Writing_mode.Horizontal -> Absolute_axis.Horizontal
    | Writing_mode.Vertical -> Absolute_axis.Vertical
  in
  let lines = split_on_zws text_content in
  let min_line_length =
    List.fold_left (fun acc line -> max acc (String.length line)) 0 lines
  in
  let max_line_length =
    List.fold_left (fun acc line -> acc + String.length line) 0 lines
  in
  let inline_available =
    match inline_axis with
    | Absolute_axis.Horizontal -> available_width
    | Absolute_axis.Vertical -> available_height
  in
  let inline_known, block_known =
    match inline_axis with
    | Absolute_axis.Horizontal -> (known_width, known_height)
    | Absolute_axis.Vertical -> (known_height, known_width)
  in
  let h_width = 10.0 in
  let h_height = 10.0 in
  let inline_size =
    match inline_known with
    | Some size -> size
    | None -> (
        match inline_available with
        | Available_space.Min_content -> float_of_int min_line_length *. h_width
        | Available_space.Max_content -> float_of_int max_line_length *. h_width
        | Available_space.Definite size ->
            Float.min size (float_of_int max_line_length *. h_width))
  in
  let inline_size =
    Float.max inline_size (float_of_int min_line_length *. h_width)
  in
  let block_size =
    match block_known with
    | Some size -> size
    | None ->
        let inline_line_length =
          int_of_float (Float.floor (inline_size /. h_width))
        in
        let line_count = ref 1 in
        let current_line_length = ref 0 in
        List.iter
          (fun line ->
            let line_len = String.length line in
            if !current_line_length + line_len > inline_line_length then begin
              if !current_line_length > 0 then incr line_count;
              current_line_length := line_len
            end
            else current_line_length := !current_line_length + line_len)
          lines;
        float_of_int !line_count *. h_height
  in
  match writing_mode with
  | Writing_mode.Horizontal -> Size.{ width = inline_size; height = block_size }
  | Writing_mode.Vertical -> Size.{ width = block_size; height = inline_size }

let test_measure_function (known_dimensions : float option size)
    (available_space : Available_space.t size) (node_id : Node_id.t)
    (context : test_node_context option) (style : Style.t) =
  ignore node_id;
  ignore style;
  let Size.{ width = known_width; height = known_height } = known_dimensions in
  match known_dimensions with
  | Size.{ width = Some width; height = Some height } -> Size.{ width; height }
  | _ -> (
      match context with
      | None ->
          Size.
            {
              width = option_or_zero known_width;
              height = option_or_zero known_height;
            }
      | Some context ->
          context.count <- context.count + 1;
          let computed =
            match context.measure_data with
            | Zero -> Size.{ width = 0.0; height = 0.0 }
            | Fixed size -> size
            | Aspect_ratio data -> measure_aspect_ratio known_dimensions data
            | Ahem_text data ->
                measure_ahem_text known_dimensions available_space data
          in
          Size.
            {
              width = apply_known_dimension known_width computed.width;
              height = apply_known_dimension known_height computed.height;
            })
