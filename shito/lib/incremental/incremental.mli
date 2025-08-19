(** Shito: A zero-dependency incremental computation library for OCaml.

    This library allows defining computations that depend on variables and
    automatically recompute when variables change. Computations form a DAG, and
    changes propagate efficiently during stabilization.

    Key concepts:
    - [Var.t]: Mutable variables that trigger recomputation when set.
    - ['a Incr.t]: Incremental values (nodes in the DAG).
    - [Observer.t]: Handles to observe values and react to changes.
    - [stabilize]: Propagates changes through the DAG.
    - [Cutoff.t]: Optimization to stop propagation if values don't "change"
      meaningfully.

    Usage: Apply the [Make] functor to get an instance. All incrementals in one
    instance share a global state for stabilization. *)

module type S = sig
  (** Type of incremental values. These are nodes in the computation DAG. *)

  module Incr : sig
    type 'a t
  end

  type 'a t = 'a Incr.t

  (** For time-based incremental nodes *)
  type before_or_after =
    | Before
    | After

  module Var : sig
    type 'a t
    (** Mutable variables (leaves in the DAG). *)

    val create : 'a -> 'a t
    (** Create a new variable with an initial value. *)

    val set : 'a t -> 'a -> unit
    (** Set the variable's value. Changes take effect after the next
        [stabilize]. *)

    val value : 'a t -> 'a
    (** Get the current value (reflects sets outside stabilization). *)

    val watch : 'a t -> 'a Incr.t
    (** Convert to an incremental that tracks the variable's value. *)
  end

  module Observer : sig
    type 'a t
    (** Observers make incrementals "necessary" and allow reacting to changes.
    *)

    val observing : 'a t -> 'a Incr.t
    (** The incremental being observed. *)

    val value_exn : 'a t -> 'a
    (** Get the current value, or raise if unstable/invalid. *)

    (** Update kinds for [on_update_exn]. *)
    module Update : sig
      type 'a t =
        | Initialized of 'a  (** First stabilization after creation. *)
        | Changed of 'a * 'a  (** Old and new values. *)
        | Invalidated  (** Incremental became invalid. *)
    end

    val on_update_exn : 'a t -> f:('a Update.t -> unit) -> unit
    (** Register a handler called after each stabilization where the value
        changes. Handlers run outside stabilization and can modify the graph
        (except stabilize). *)

    val disallow_future_use : 'a t -> unit
    (** Disallow future use of this observer (makes observed incremental
        unnecessary if possible). *)
  end

  val observe : 'a t -> 'a Observer.t
  (** Create an observer for an incremental (makes it necessary). *)

  val const : 'a -> 'a t
  (** Constant incremental (never changes). *)

  val map : 'a t -> f:('a -> 'b) -> 'b t
  (** Map over one or more incrementals. Efficient for static graphs. *)

  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  val map3 : 'a t -> 'b t -> 'c t -> f:('a -> 'b -> 'c -> 'd) -> 'd t
  val map4 : 'a t -> 'b t -> 'c t -> 'd t -> f:('a -> 'b -> 'c -> 'd -> 'e) -> 'e t
  (* ... up to map15 for practicality; can extend if needed *)

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  (** Bind: Dynamic graphs. Less efficient than map (modifies DAG on LHS
      change). *)

  val bind2 : 'a t -> 'b t -> f:('a -> 'b -> 'c t) -> 'c t
  val bind3 : 'a t -> 'b t -> 'c t -> f:('a -> 'b -> 'c -> 'd t) -> 'd t
  val bind4 : 'a t -> 'b t -> 'c t -> 'd t -> f:('a -> 'b -> 'c -> 'd -> 'e t) -> 'e t

  val if_ : bool t -> then_:'a t -> else_:'a t -> 'a t
  (** [if_ test ~then_ ~else_] dynamically switches between branches based on test value. *)

  val join : 'a t t -> 'a t
  (** [join] flattens a nested incremental. *)

  val freeze : ?when_:('a -> bool) -> 'a t -> 'a t
  (** [freeze] stops propagating changes when the predicate returns true. *)

  (** Infix operators for map/bind. *)
  module Infix : sig
    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  (** Let-syntax for readable bindings/maps. *)
  module Let_syntax : sig
    val return : 'a -> 'a t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
    val both : 'a t -> 'b t -> ('a * 'b) t
    val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
    (* ... similar for map3 etc. *)
  end

  (** Cutoff: Stop propagating changes if [should_cutoff old new] is true.
      Default is physical equality. *)
  module Cutoff : sig
    type 'a t

    val create : (old:'a -> new_:'a -> bool) -> 'a t

    val phys_equal : 'a t
    (** Default cutoff. *)

    val never : 'a t
    (** Always propagate. *)

    val always : 'a t
    (** Never propagate (constant after first compute). *)
  end

  val set_cutoff : 'a t -> 'a Cutoff.t -> unit
  val get_cutoff : 'a t -> 'a Cutoff.t

  val stabilize : unit -> unit
  (** Propagate changes through the DAG (recomputes stale necessary nodes). *)

  val is_stabilizing : unit -> bool
  (** True if currently stabilizing. *)

  (** Clock module for managing time-based computations *)
  module Clock : sig
    val create : start:Time.t -> unit -> unit
    (** Create a clock with the specified start time. *)
    
    val now : unit -> Time.t
    (** Get the current time of the clock. *)
    
    val advance_clock : to_:Time.t -> unit -> unit
    (** Advance the clock to the specified time. *)
  end

  (** Clock API for time-based incremental nodes *)
  val at : Time.t -> before_or_after t
  (** [at time] creates an incremental that transitions from [Before] to [After] when the clock reaches the specified time. *)

  val after : Time.Span.t -> before_or_after t
  (** [after span] creates an incremental that transitions after the given time span from now. *)

  val at_intervals : base:Time.t -> interval:Time.Span.t -> unit t
  (** [at_intervals ~base ~interval] creates an incremental that fires at regular intervals. *)

  val snapshot : 'a t -> at:Time.t -> before:'a -> 'a t
  (** [snapshot value_at ~at ~before] creates a snapshot of a value at a specific time, with an initial value [before]. *)

  val step_function : init:'a -> (Time.t * 'a) list -> 'a t
  (** [step_function ~init steps] creates an incremental that changes value at specific times. *)

  val advance_clock : to_:Time.t -> unit
  (** [advance_clock ~to_] advances the clock to the specified time. (Also available in Clock module) *)
end

module Make () : S
(** Functor to create an isolated incremental instance. *)