(** ASCII/TXT renderer for vcr - renders terminal output as plain text *)

type config = { separator : string }

let default_config = { separator = String.make 80 '-' (* 80 dashes like VHS *) }

type t = {
  rows : int;
  cols : int;
  config : config;
  mutable frames : string list list;
      (* List of frames, each frame is a list of lines *)
}

let create ~rows ~cols config = { rows; cols; config; frames = [] }

(** Render a frame to ASCII text *)
let render_frame_to_ascii ~frame =
  let grid = frame.Renderer_intf.grid in
  let rows = Grid.rows grid in
  let cols = Grid.cols grid in
  let lines = ref [] in

  (* Read each line from the grid *)
  for row = 0 to rows - 1 do
    let line_bytes = Bytes.create cols in
    let last_non_space = ref (-1) in

    (* Get characters for this line *)
    for col = 0 to cols - 1 do
      match Grid.get grid ~row ~col with
      | None -> Bytes.set line_bytes col ' '
      | Some cell ->
          let ch =
            if Grid.Cell.is_empty cell then ' '
            else
              let text = Grid.Cell.get_text cell in
              if String.length text > 0 then text.[0] else ' '
          in
          Bytes.set line_bytes col ch;
          if ch <> ' ' then last_non_space := col
    done;

    (* Convert to string and trim trailing spaces *)
    let trimmed_line =
      if !last_non_space >= 0 then
        Bytes.sub_string line_bytes 0 (!last_non_space + 1)
      else ""
    in
    lines := trimmed_line :: !lines
  done;

  List.rev !lines

let write_frame t frame ~incremental ~writer =
  let _ = incremental in
  (* Not used for ASCII renderer *)
  let _ = writer in
  (* Not used - we buffer frames for finalize *)
  let _ = t.rows in
  (* Mark as used *)
  let _ = t.cols in
  (* Mark as used *)

  (* Render frame to lines *)
  let lines = render_frame_to_ascii ~frame in

  (* Store frame for finalize *)
  t.frames <- lines :: t.frames

let finalize t ~writer =
  (* Write frames in order (they were added in reverse) *)
  let frames_in_order = List.rev t.frames in

  List.iteri
    (fun i frame ->
      (* Write each line of the frame *)
      List.iter
        (fun line ->
          let line_bytes = Bytes.of_string (line ^ "\n") in
          writer line_bytes 0 (Bytes.length line_bytes))
        frame;

      (* Add separator between frames (except after the last one) *)
      if i < List.length frames_in_order - 1 then
        let sep_bytes = Bytes.of_string (t.config.separator ^ "\n") in
        writer sep_bytes 0 (Bytes.length sep_bytes))
    frames_in_order
