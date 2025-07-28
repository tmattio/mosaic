(** ASCII/TXT renderer for vcr - renders terminal output as plain text *)

type config = { separator : string }

let default_config = { separator = String.make 80 '-' (* 80 dashes like VHS *) }

type frame = string list
type t = { vte : Vte.t; config : config; mutable frames : frame list }

let create vte config = { vte; config; frames = [] }

let capture_frame t =
  let rows = Vte.rows t.vte in
  let lines = ref [] in

  (* Read each line from the terminal buffer *)
  for row = 0 to rows - 1 do
    let cols_count = Vte.cols t.vte in
    let line_bytes = Bytes.create cols_count in
    let last_non_space = ref (-1) in

    (* Get characters for this line *)
    for col = 0 to cols_count - 1 do
      match Vte.get_cell t.vte ~row ~col with
      | None -> Bytes.set line_bytes col ' '
      | Some cell ->
          let ch =
            try Uchar.to_char cell.char with Invalid_argument _ -> '?'
            (* Non-ASCII character *)
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

  (* Store frame (lines are in reverse order, so reverse them back) *)
  t.frames <- List.rev !lines :: t.frames

let add_pending_delay _ _ = () (* ASCII doesn't use frame delays *)

let render t =
  let buffer = Buffer.create 4096 in
  (* Write frames in order (they were added in reverse) *)
  let frames_in_order = List.rev t.frames in
  List.iteri
    (fun i frame ->
      (* Write each line of the frame *)
      List.iter
        (fun line ->
          Buffer.add_string buffer line;
          Buffer.add_char buffer '\n')
        frame;

      (* Add separator between frames (except after the last one) *)
      if i < List.length frames_in_order - 1 then (
        Buffer.add_string buffer t.config.separator;
        Buffer.add_char buffer '\n'))
    frames_in_order;
  Buffer.contents buffer
