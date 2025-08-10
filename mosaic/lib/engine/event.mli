(** General event type for subscriptions *)

type t =
  | Input of Input.event  (** Terminal input events *)
  | Tick of float  (** Animation tick with elapsed time in seconds *)
