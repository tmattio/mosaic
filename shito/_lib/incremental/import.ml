(* Debug flag for assertions and checks *)
let debug = false

module Uopt = struct
  type 'a t = 'a option

  let none = None
  let some x = Some x
  let is_some = function Some _ -> true | None -> false
  let is_none = function None -> true | Some _ -> false

  let value_exn = function
    | Some x -> x
    | None -> failwith "Uopt.value_exn: None"

  let unsafe_value = value_exn
end

module Uniform_array = struct
  type 'a t = 'a array

  let create ~len x = Array.make len x
  let length = Array.length
  let get = Array.get
  let set = Array.set
  let unsafe_get = Array.unsafe_get
  let unsafe_set = Array.unsafe_set
end

module Stack = struct
  type 'a t = 'a list ref

  let create () = ref []
  let push t x = t := x :: !t

  let pop t =
    match !t with
    | [] -> None
    | h :: tl ->
        t := tl;
        Some h

  let is_empty t = !t = []
end

module Thread_safe_queue = struct
  type 'a t = 'a Queue.t

  let create () = Queue.create ()
  let enqueue = Queue.add
  let dequeue_exn = Queue.take
end

module Weak_hashtbl = struct
  type ('a, 'b) t = ('a, 'b) Hashtbl.t
end

module Type_equal = struct
  type ('a, 'b) t = T : ('a, 'a) t
end

(* These are now defined below, remove duplicates *)

module Sequence = struct
  type 'a t = 'a list
end

module Step_function = struct
  type 'a t = unit
end

module Backtrace = struct
  type t = Printexc.raw_backtrace option
end

module Node_id = Node_id
module Stabilization_num = Stabilization_num
module Cutoff = Cutoff

module On_update_handler = On_update_handler

module Before_or_after = Before_or_after
module Raised_exn = Raised_exn

external phys_equal : 'a. ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%eq"

let phys_same (type a b) (a : a) (b : b) = phys_equal a (Stdlib.Obj.magic b : a)

(* Invariant module - mimics Core's Invariant *)
module Invariant = struct
  type 'a t = 'a -> unit
  
  module type S = sig
    type t
    val invariant : t -> unit
  end
  
  module type S1 = sig
    type 'a t
    val invariant : 'a t -> ('a -> unit) -> 'a t -> unit
  end
  
  module type S2 = sig
    type ('a, 'b) t
    val invariant : ('a -> unit) -> ('b -> unit) -> ('a, 'b) t -> unit
  end
  
  (* The main invariant function that wraps exceptions with context *)
  let invariant ?here:(_ = Stdlib.Lexing.dummy_pos) _t _sexp_of_t f =
    try f () with
    | exn ->
      (* In Core, this would create a nice sexp error message.
         For our version, we re-raise with some context *)
      let msg = 
        Printf.sprintf "invariant failed: %s" 
          (Printexc.to_string exn)
      in
      failwith msg
  
  let check_field _t f field = f field
end

(* Simple timing wheel implementation for time-based incrementals *)
module Timing_wheel = struct
  module Alarm = struct
    type 'a t = {
      mutable is_valid : bool;
      mutable value : 'a option;
      mutable time : float;
      mutable id : int;
    }

    let null () = { is_valid = false; value = None; time = 0.0; id = -1 }
    let is_null t = (not t.is_valid) || t.id = -1
  end

  type 'a t = {
    mutable alarms : 'a Alarm.t list;
    mutable next_id : int;
    mutable current_time : float;
  }

  let create () = { alarms = []; next_id = 0; current_time = 0.0 }

  let add t ~at value =
    let alarm =
      { Alarm.is_valid = true; value = Some value; time = at; id = t.next_id }
    in
    t.next_id <- t.next_id + 1;
    (* Insert sorted by time *)
    let rec insert = function
      | [] -> [ alarm ]
      | h :: t as l ->
          if alarm.Alarm.time <= h.Alarm.time then alarm :: l else h :: insert t
    in
    t.alarms <- insert t.alarms;
    alarm

  let remove t alarm =
    alarm.Alarm.is_valid <- false;
    t.alarms <- List.filter (fun a -> a.Alarm.id <> alarm.Alarm.id) t.alarms

  let advance_clock t ~to_time ~handle_fired =
    let rec process_alarms = function
      | [] -> []
      | alarm :: rest ->
          if alarm.Alarm.time <= to_time && alarm.Alarm.is_valid then (
            handle_fired alarm;
            process_alarms rest)
          else alarm :: rest
    in
    t.current_time <- to_time;
    t.alarms <- process_alarms t.alarms

  let now t = t.current_time
end

module Time_ns = struct
  type t = float

  module Span = struct
    type t = float

    let to_sec t = t
    let of_sec t = t
    let is_positive t = t > 0.0
  end

  let now () = Unix.gettimeofday ()
  let add t span = t +. span
  let diff t1 t2 = t1 -. t2
  let to_float t = t
  let of_float t = t
end

module Alarm_precision = struct
  type t = [ `About_one_second | `About_one_millisecond ]

  let about_one_second = `About_one_second
  let about_one_millisecond = `About_one_millisecond
end
