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

(** Sample frames from event log - create frames only when there are changes *)
let sample ~fps:_ ~strategy:_ ~initial_cols ~initial_rows events =
  (* Initial state *)
  let initial_state =
    {
      grid = Grid.create ~rows:initial_rows ~cols:initial_cols ();
      cursor_row = 0;
      cursor_col = 0;
      cursor_visible = true;
    }
  in

  (* Process events and create frames only when there are visual changes *)
  (* Group events at the same timestamp into a single frame *)
  let rec process_events cursor frames last_frame_time = function
    | [] ->
        (* Add final frame if needed *)
        if last_frame_time < Event.end_time events then
          let final_frame =
            {
              timestamp = Event.end_time events;
              grid = cursor.current_state.grid;
              cursor_row = cursor.current_state.cursor_row;
              cursor_col = cursor.current_state.cursor_col;
              cursor_visible = cursor.current_state.cursor_visible;
              dirty_regions = [];
              cursor_moved = false;
            }
          in
          List.rev (final_frame :: frames)
        else List.rev frames
    | events_list ->
        (* Group events by timestamp *)
        let rec collect_same_time acc time = function
          | [] -> (List.rev acc, [])
          | (Event.Sleep _ as evt) :: rest when Event.timestamp evt = time ->
              (* Include Sleep events in the group *)
              collect_same_time (evt :: acc) time rest
          | evt :: rest when Event.timestamp evt = time ->
              collect_same_time (evt :: acc) time rest
          | rest -> (List.rev acc, rest)
        in

        let first_event = List.hd events_list in
        let event_time = Event.timestamp first_event in
        let same_time_events, remaining =
          collect_same_time [] event_time events_list
        in

        (* Check if any events have visual changes *)
        let has_visual_change =
          List.exists
            (function
              | Event.Screen_change _ | Event.Cursor_move _ -> true
              | Event.Sleep _ -> false)
            same_time_events
        in

        if has_visual_change then
          (* Apply all events at this timestamp *)
          let rec apply_events state dirty_regions cursor_moved = function
            | [] -> (state, dirty_regions, cursor_moved)
            | event :: rest ->
                let new_state =
                  match event with
                  | Event.Screen_change { changed_rows; _ } ->
                      let grid =
                        if state.grid == cursor.current_state.grid then
                          Grid.copy cursor.current_state.grid
                        else state.grid
                      in
                      List.iter
                        (fun (row, row_data) ->
                          if Array.length row_data = Grid.cols grid then
                            let cells =
                              Array.map
                                (fun opt ->
                                  Option.value opt ~default:Grid.Cell.empty)
                                row_data
                            in
                            Grid.set_row grid row cells
                          else
                            Array.iteri
                              (fun col cell_opt ->
                                match cell_opt with
                                | Some cell ->
                                    Grid.set grid ~row ~col (Some cell)
                                | None -> ())
                              row_data)
                        changed_rows;
                      { state with grid }
                  | Event.Cursor_move { row; col; _ } ->
                      { state with cursor_row = row; cursor_col = col }
                  | Event.Sleep _ -> state
                in

                let new_regions =
                  match event with
                  | Event.Screen_change { changed_rows; _ } ->
                      List.filter_map
                        (fun (row, row_data) ->
                          (* Find the actual range of changed cells *)
                          let min_col = ref max_int in
                          let max_col = ref (-1) in
                          Array.iteri
                            (fun col cell_opt ->
                              if cell_opt <> None then (
                                min_col := min !min_col col;
                                max_col := max !max_col col))
                            row_data;
                          if !max_col >= 0 then
                            Some
                              Grid.
                                {
                                  min_row = row;
                                  max_row = row;
                                  min_col = !min_col;
                                  max_col = !max_col;
                                }
                          else None)
                        changed_rows
                  | _ -> []
                in

                let moved =
                  match event with
                  | Event.Cursor_move _ -> true
                  | _ -> cursor_moved
                in

                apply_events new_state (new_regions @ dirty_regions) moved rest
          in

          let final_state, all_regions, cursor_moved =
            apply_events cursor.current_state [] false same_time_events
          in

          (* Create frame with all changes at this timestamp *)
          let frame =
            {
              timestamp = event_time;
              grid = final_state.grid;
              cursor_row = final_state.cursor_row;
              cursor_col = final_state.cursor_col;
              cursor_visible = final_state.cursor_visible;
              dirty_regions = all_regions;
              cursor_moved;
            }
          in

          (* Update cursor *)
          let new_cursor =
            {
              events = cursor.events;
              position = cursor.position + List.length same_time_events;
              current_state = final_state;
            }
          in

          process_events new_cursor (frame :: frames) event_time remaining
        else
          (* No visual changes in this group - just advance *)
          let new_cursor =
            {
              cursor with
              position = cursor.position + List.length same_time_events;
            }
          in
          process_events new_cursor frames last_frame_time remaining
  in

  let initial_cursor = create_cursor events initial_state in
  process_events initial_cursor [] 0.0 events

(** Check if two frames are visually identical *)
let frames_equal f1 f2 =
  (* For now, just check if nothing changed *)
  f1.dirty_regions = [] && (not f1.cursor_moved) && f2.dirty_regions = []
  && (not f2.cursor_moved)
  && f1.cursor_row = f2.cursor_row
  && f1.cursor_col = f2.cursor_col

(** Deduplicate consecutive identical frames *)
let deduplicate_frames frames =
  let rec dedup acc last_frame = function
    | [] -> (
        (* Add the last frame *)
        match last_frame with
        | Some f -> List.rev (f :: acc)
        | None -> List.rev acc)
    | frame :: rest -> (
        match last_frame with
        | None ->
            (* First frame *)
            dedup acc (Some frame) rest
        | Some prev ->
            if frames_equal prev frame then
              (* Identical frame - skip it *)
              dedup acc last_frame rest
            else
              (* Different frame - emit previous *)
              dedup (prev :: acc) (Some frame) rest)
  in
  dedup [] None frames

(** Calculate frame delays in centiseconds *)
let calculate_delays frames =
  (* First deduplicate frames *)
  let unique_frames = deduplicate_frames frames in

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
