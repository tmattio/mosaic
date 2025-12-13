[@@@ocamlformat "disable"]
(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type ret = [ `Await | `Boundary | `End | `Uchar of Uchar.t ]

let pp_ret ppf v = match (v :> ret) with
| `Await -> Format.fprintf ppf "`Await"
| `Boundary -> Format.fprintf ppf "`Boundary"
| `End -> Format.fprintf ppf "`End"
| `Uchar u -> Format.fprintf ppf "`Uchar U+%04X" (Uchar.to_int u)

let err_exp_await add =
  invalid_arg (Format.asprintf "can't add %a, expected `Await" pp_ret add)

let err_ended add =
  invalid_arg (Format.asprintf "can't add %a, `End already added" pp_ret add)

(* Type identifiers *)

module Type = struct
  (* See http://alan.petitepomme.net/cwn/2015.03.24.html#1
     In the stdlib since 5.1. *)

  type ('a, 'b) eq = Equal : ('a, 'a) eq

  module Id = struct
    type _ id = ..
    module type ID = sig
      type t
      type _ id += Id : t id
    end

    type 'a t = (module ID with type t = 'a)

    let make (type a) () : a t =
      (module struct type t = a type _ id += Id : t id end)

    let provably_equal
        (type a b) ((module A) : a t) ((module B) : b t) : (a, b) eq option
      =
      match A.Id with B.Id -> Some Equal | _ -> None

    let uid (type a) ((module A) : a t) =
      Obj.Extension_constructor.id (Obj.Extension_constructor.of_val A.Id)
  end
end
