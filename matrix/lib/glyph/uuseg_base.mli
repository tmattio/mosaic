[@@@ocamlformat "disable"]
(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Segmenter commonalities. *)

(** {1 Common} *)

type ret = [ `Await | `Boundary | `End | `Uchar of Uchar.t ]
(** See {!Uuseg.ret}. *)

val pp_ret : Format.formatter -> [< ret ] -> unit
(** See {!Uuseg.pp_ret}. *)

val err_exp_await : [< ret] -> 'a
(** See {!Uuseg.err_exp_await}. *)

val err_ended : [< ret] -> 'a
(** See {!Uuseg.err_ended}. *)

(** {1:tid Type identifiers} *)

(** Type introspection.

    {b Note.} Available in 5.1. *)
module Type : sig

  type (_, _) eq = Equal : ('a, 'a) eq (** *)
  (** The type for type quality testing. *)

  module Id : sig

    (** {1:typeids Type identifiers} *)

    type 'a t
    (** The type for type identifiers for a type ['a]. *)

    val make : unit -> 'a t
    (** [make ()] is a new type identifier. *)

    val provably_equal : 'a t -> 'b t -> ('a, 'b) eq option
    (** [provably_equal id0 id1] determines if [id0] and [id1] are equal. *)

    val uid : 'a t -> int
    (** [uid id] is a runtime unique identifier for [id]. *)
  end
end
