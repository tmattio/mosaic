(** Executor - runs tape commands and produces event log *)

val run :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  Tape_lang.Ast.tape ->
  (Event.log * Vte.t * Config.t, Error.t) result
(** Run a tape script and produce an event log
    @param sw Eio switch for resource management
    @param env Eio environment
    @param tape List of tape commands to execute
    @return (event_log, final_vte_state, config) or error *)
