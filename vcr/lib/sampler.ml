(** Optimized frame sampler - converts event log to frames at target FPS *)

type frame = {
  timestamp : float;
  grid : Grid.t;
  cursor_row : int;
  cursor_col : int;
  cursor_visible : bool;
  dirty_regions : Grid.dirty_region list;
  cursor_moved : bool;
}

type strategy = Drop

type terminal_state = {
  grid : Grid.t;
  cursor_row : int;
  cursor_col : int;
  cursor_visible : bool;
}
(** Terminal state at a point in time *)

type event_cursor = {
  events : Event.t list;
  position : int;
  current_state : terminal_state;
}
(** Event cursor for efficient traversal *)

(** Create initial event cursor *)
let create_cursor events initial_state =
  { events; position = 0; current_state = initial_state }

(** Advance cursor to time t, updating state along the way *)
let advance_cursor_to cursor target_time =
  let rec advance cursor =
    match List.nth_opt cursor.events cursor.position with
    | None -> cursor
    | Some event ->
        let event_time = Event.timestamp event in
        if event_time > target_time then cursor
        else
          let new_state =
            match event with
            | Event.Screen_change { changed_rows; _ } ->
                (* Apply row changes to current grid *)
                let grid = Grid.copy cursor.current_state.grid in
                List.iter
                  (fun (row, row_data) ->
                    let cells =
                      Array.map
                        (fun opt -> Option.value opt ~default:Grid.Cell.empty)
                        row_data
                    in
                    Grid.set_row grid row cells)
                  changed_rows;
                { cursor.current_state with grid }
            | Event.Cursor_move { row; col; _ } ->
                { cursor.current_state with cursor_row = row; cursor_col = col }
            | Event.Sleep _ -> cursor.current_state
          in
          advance
            {
              cursor with
              position = cursor.position + 1;
              current_state = new_state;
            }
  in
  advance cursor

(** Collect changes between two times using cursor *)
let collect_changes_between cursor start_time end_time =
  let rec collect pos dirty_regions cursor_moved =
    match List.nth_opt cursor.events pos with
    | None -> (dirty_regions, cursor_moved)
    | Some event ->
        let t = Event.timestamp event in
        if t > end_time then (dirty_regions, cursor_moved)
        else if t > start_time then
          match event with
          | Event.Screen_change { changed_rows; _ } ->
              (* Convert changed rows to dirty regions *)
              let new_regions =
                List.map
                  (fun (row, _) ->
                    let ncols = Grid.cols cursor.current_state.grid in
                    Grid.
                      {
                        min_row = row;
                        max_row = row;
                        min_col = 0;
                        max_col = ncols - 1;
                      })
                  changed_rows
              in
              collect (pos + 1) (new_regions @ dirty_regions) cursor_moved
          | Event.Cursor_move _ -> collect (pos + 1) dirty_regions true
          | _ -> collect (pos + 1) dirty_regions cursor_moved
        else collect (pos + 1) dirty_regions cursor_moved
  in
  collect cursor.position [] false

(** Sample frames from event log at given FPS *)
let sample ~fps ~strategy ~initial_cols ~initial_rows events =
  let frame_interval = 1.0 /. fps in
  let end_time = Event.end_time events in

  (* Initial state *)
  let initial_state =
    {
      grid = Grid.create ~rows:initial_rows ~cols:initial_cols ();
      cursor_row = 0;
      cursor_col = 0;
      cursor_visible = true;
    }
  in

  (* Create cursor *)
  let initial_cursor = create_cursor events initial_state in

  (* Sample at regular intervals *)
  let rec sample_at_times time frames cursor last_time =
    if time > end_time then List.rev frames
    else
      (* Advance cursor to current time *)
      let cursor = advance_cursor_to cursor time in

      (* Collect changes since last frame *)
      let dirty_regions, cursor_moved =
        collect_changes_between cursor last_time time
      in

      let frame =
        {
          timestamp = time;
          grid = cursor.current_state.grid;
          cursor_row = cursor.current_state.cursor_row;
          cursor_col = cursor.current_state.cursor_col;
          cursor_visible = cursor.current_state.cursor_visible;
          dirty_regions;
          cursor_moved;
        }
      in

      (* Determine next sample time based on strategy *)
      let next_time =
        match strategy with
        | Drop ->
            (* Always advance by frame interval during active periods *)
            time +. frame_interval
      in

      sample_at_times next_time (frame :: frames) cursor time
  in

  sample_at_times 0.0 [] initial_cursor (-1.0)

(** Check if two frames are visually identical *)
let frames_equal f1 f2 =
  (* For now, just check if nothing changed *)
  f1.dirty_regions = [] && (not f1.cursor_moved) && f2.dirty_regions = []
  && (not f2.cursor_moved)
  && f1.cursor_row = f2.cursor_row
  && f1.cursor_col = f2.cursor_col

(** Deduplicate consecutive identical frames *)
let deduplicate_frames frames =
  let rec dedup acc last_frame accumulated_time = function
    | [] -> (
        (* Add the last frame with accumulated time *)
        match last_frame with
        | Some f ->
            List.rev
              ({ f with timestamp = f.timestamp +. accumulated_time } :: acc)
        | None -> List.rev acc)
    | frame :: rest -> (
        match last_frame with
        | None ->
            (* First frame *)
            dedup acc (Some frame) 0.0 rest
        | Some prev ->
            if frames_equal prev frame then
              (* Identical frame - accumulate time *)
              let time_diff = frame.timestamp -. prev.timestamp in
              dedup acc last_frame (accumulated_time +. time_diff) rest
            else
              (* Different frame - emit previous with accumulated time *)
              let prev_with_time =
                { prev with timestamp = prev.timestamp +. accumulated_time }
              in
              dedup (prev_with_time :: acc) (Some frame) 0.0 rest)
  in
  dedup [] None 0.0 frames

(** Calculate frame delays in centiseconds *)
let calculate_delays frames =
  (* First deduplicate frames *)
  let unique_frames = deduplicate_frames frames in
  let () =
    Printf.eprintf "[DEBUG] Frames after deduplication: %d\n"
      (List.length unique_frames)
  in

  match unique_frames with
  | [] -> []
  | [ frame ] -> [ (frame, 10) ] (* Single frame gets 100ms delay *)
  | first :: rest ->
      let rec calc acc prev remaining =
        match remaining with
        | [] ->
            (* Last frame gets remaining time or minimum delay *)
            List.rev ((prev, 10) :: acc)
        | curr :: rest ->
            let delay_s = curr.timestamp -. prev.timestamp in
            let delay_cs = int_of_float (delay_s *. 100.0) in
            let delay_cs = max 1 delay_cs in
            (* Minimum 10ms *)
            calc ((prev, delay_cs) :: acc) curr rest
      in
      calc [] first rest
