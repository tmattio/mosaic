open Import
module Alarm = Timing_wheel.Alarm

type t = Types.Alarm_value.t Alarm.t

let invariant (_ : t) = ()
let null = Alarm.null ()
let get_null () = Alarm.null ()
