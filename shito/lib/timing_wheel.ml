(** A simple timing wheel implementation for managing time-based alarms *)

module Time = Time

type 'a alarm_entry = {
  mutable at : Time.t;
  value : 'a;
  mutable next : 'a alarm_entry option;  (* For linked list in bucket *)
}

module Alarm = struct
  type 'a t = 
    | Null
    | Valid of 'a alarm_entry
    
  let null () = Null
  let value _wheel = function
    | Null -> failwith "Alarm.value called on null alarm"
    | Valid v -> v
end

type 'a alarm = 'a Alarm.t

type 'a t = {
  mutable now : Time.t;
  alarm_precision : Time.Span.t;
  (* We use a simple array of buckets. Each bucket represents alarm_precision time. *)
  (* For simplicity, we'll use a fixed-size wheel that wraps around. *)
  wheel_size : int;
  buckets : 'a alarm_entry option array;
  mutable min_alarm_time : Time.t option;
  (* We also keep a far-future list for alarms beyond the wheel range *)
  mutable far_future : 'a alarm_entry list;
}

let create ~alarm_precision ~start =
  let wheel_size = 3600 in  (* Cover 1 hour with 1-second precision *)
  {
    now = start;
    alarm_precision;
    wheel_size;
    buckets = Array.make wheel_size None;
    min_alarm_time = None;
    far_future = [];
  }

let now t = t.now
let alarm_precision t = t.alarm_precision

let time_to_bucket t time =
  let offset = Time.diff time t.now in
  let bucket_offset = int_of_float (offset /. t.alarm_precision) in
  if bucket_offset < 0 then
    None  (* In the past *)
  else if bucket_offset >= t.wheel_size then
    None  (* Too far in future for wheel *)
  else
    Some (bucket_offset mod t.wheel_size)

let add t ~at value =
  if Time.(at <= t.now) then
    failwith "Cannot add alarm in the past";
  
  let alarm = { at; value; next = None } in
  
  (* Update min alarm time *)
  (match t.min_alarm_time with
   | None -> t.min_alarm_time <- Some at
   | Some min_time -> 
       if Time.(at < min_time) then t.min_alarm_time <- Some at);
  
  (match time_to_bucket t at with
  | Some bucket_idx ->
      (* Add to bucket as head of linked list *)
      alarm.next <- t.buckets.(bucket_idx);
      t.buckets.(bucket_idx) <- Some alarm
  | None ->
      (* Add to far future list *)
      t.far_future <- alarm :: t.far_future);
  
  Alarm.Valid alarm

let mem _t _alarm = true  (* Simplified: assume all alarms are valid *)

let remove t alarm =
  match alarm with
  | Alarm.Null -> ()
  | Alarm.Valid entry ->
      (* For simplicity, we'll just mark it as removed by setting time to past *)
      entry.at <- Time.(t.now - Span.of_sec 1.)

let rec fire_alarms_in_list alarms ~now:now_time ~handle_fired acc =
  match alarms with
  | [] -> List.rev acc
  | alarm :: rest ->
      if Time.(alarm.at <= now_time) then (
        handle_fired alarm.value;
        fire_alarms_in_list rest ~now:now_time ~handle_fired acc
      ) else
        fire_alarms_in_list rest ~now:now_time ~handle_fired (alarm :: acc)

let fire_bucket t bucket_idx ~handle_fired =
  let rec process_list alarm_opt =
    match alarm_opt with
    | None -> None
    | Some alarm ->
        if Time.(alarm.at <= t.now) then (
          handle_fired alarm.value;
          process_list alarm.next
        ) else (
          alarm.next <- process_list alarm.next;
          Some alarm
        )
  in
  t.buckets.(bucket_idx) <- process_list t.buckets.(bucket_idx)

let advance_clock t ~to_ ~handle_fired =
  if Time.(to_ < t.now) then
    failwith "Cannot move time backwards";
  
  let old_now = t.now in
  t.now <- to_;
  
  (* Fire all alarms between old_now and to_ *)
  let start_bucket = 
    match time_to_bucket { t with now = old_now } old_now with
    | Some b -> b
    | None -> 0
  in
  let end_bucket = 
    match time_to_bucket t to_ with
    | Some b -> b
    | None -> t.wheel_size - 1
  in
  
  (* Fire alarms in affected buckets *)
  if start_bucket <= end_bucket then
    for i = start_bucket to end_bucket do
      fire_bucket t i ~handle_fired
    done
  else (
    (* Wrapped around *)
    for i = start_bucket to t.wheel_size - 1 do
      fire_bucket t i ~handle_fired
    done;
    for i = 0 to end_bucket do
      fire_bucket t i ~handle_fired
    done
  );
  
  (* Move alarms from far future to wheel if they're now in range *)
  let rec move_far_future acc = function
    | [] -> acc
    | alarm :: rest ->
        if time_to_bucket t alarm.at <> None then (
          (* Move to wheel *)
          ignore (add t ~at:alarm.at alarm.value);
          move_far_future acc rest
        ) else
          move_far_future (alarm :: acc) rest
  in
  t.far_future <- move_far_future [] t.far_future;
  
  (* Update min alarm time *)
  t.min_alarm_time <- None;
  Array.iter (function
    | None -> ()
    | Some alarm ->
        match t.min_alarm_time with
        | None -> t.min_alarm_time <- Some alarm.at
        | Some min_time ->
            if Time.(alarm.at < min_time) then
              t.min_alarm_time <- Some alarm.at
  ) t.buckets;
  List.iter (fun alarm ->
    match t.min_alarm_time with
    | None -> t.min_alarm_time <- Some alarm.at
    | Some min_time ->
        if Time.(alarm.at < min_time) then
          t.min_alarm_time <- Some alarm.at
  ) t.far_future

let fire_past_alarms t ~handle_fired =
  (* Fire any alarms that are now in the past *)
  for i = 0 to t.wheel_size - 1 do
    fire_bucket t i ~handle_fired
  done;
  t.far_future <- fire_alarms_in_list t.far_future ~now:t.now ~handle_fired []

let length t =
  let count = ref 0 in
  Array.iter (function
    | None -> ()
    | Some alarm ->
        let rec count_list a =
          incr count;
          match a.next with
          | None -> ()
          | Some next -> count_list next
        in
        count_list alarm
  ) t.buckets;
  count := !count + List.length t.far_future;
  !count