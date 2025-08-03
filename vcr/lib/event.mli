(** Event log for terminal simulation *)

(** A single event in the simulation *)
type t =
  | Screen_change of {
      at : float;
      changed_rows : (int * Grid.Cell.t option array) list;
          (** List of (row_index, new_row_data) for rows that changed *)
    }  (** Screen content changed - only stores changed rows *)
  | Cursor_move of { at : float; row : int; col : int }
      (** Cursor position change *)
  | Sleep of { at : float; duration : float }
      (** Explicit sleep/delay in simulation *)

type log = t list
(** Event log is a list of events in chronological order *)

val compare : t -> t -> int
(** Compare events by timestamp for sorting *)

val sort_log : log -> log
(** Sort events chronologically *)

val timestamp : t -> float
(** Get timestamp of an event *)

val end_time : log -> float
(** Get the end time of the simulation (accounting for sleeps) *)

val to_string : t -> string
(** Convert an event to a string for debugging *)

val pp : Format.formatter -> t -> unit
(** Pretty-print an event with full details including cell contents *)
