(** VCR - Terminal recording from tape scripts *)

module Event = Event
module Sampler = Sampler
module Executor = Executor
module Error = Error
module Timing = Vcr_common.Timing

val run :
  ?timing:Timing.t -> Tape_lang.Ast.command list -> string option -> unit
(** [run ?timing tape output_path] executes the tape script and generates
    output. If output_path is None, it looks for an Output command in the tape.
    @param timing Optional timing tracker for performance monitoring *)
