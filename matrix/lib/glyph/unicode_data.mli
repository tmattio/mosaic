(* WARNING: Do not edit. This file was automatically generated.

   Unicode version 17.0.0.
   Generated using matrix/support/gen_unicode_data.ml
*)

[@@@ocamlformat "disable"]

(** TTY width hint. Returns (starts, ends, values). Default is 1, -1 for controls. *)
val tty_width_hint: int array * int array * int array

(** Grapheme cluster break property (UAX #29). Default is 16 (XX). *)
val grapheme_cluster_break: string array array

(** Indic conjunct break property (UAX #29 GB9c). Default is 3 (None). *)
val indic_conjunct_break: string array array

(** Extended_Pictographic property (UAX #29 GB11). Default is false. *)
val extended_pictographic: string array array
