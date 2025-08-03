open Ansi

type t =
  | Empty
  | Glyph of { text : string; width : int; style : Style.t }
  | Continuation of { style : Style.t }

let empty = Empty

let make_glyph ~style ?link ~east_asian_context text =
  if String.length text = 0 then Empty
  else
    let width = Ucwidth.string_width ~east_asian:east_asian_context text in
    if width = 0 then Empty
    else
      (* Link handling is done in Grid.Storage via Style link ID bits *)
      let _ = link in
      (* Suppress unused warning for now *)
      Glyph { text; width; style }

let make_continuation ~style = Continuation { style }

(** Cell accessors *)

let width = function
  | Empty -> 0
  | Glyph { width; _ } -> width
  | Continuation _ -> 0

let get_style = function
  | Empty -> Style.default
  | Glyph { style; _ } -> style
  | Continuation { style; _ } -> style

let get_text = function
  | Empty -> ""
  | Glyph { text; _ } -> text
  | Continuation _ -> ""

(** Compute hash of a cell for comparison *)
let hash = function
  | Empty -> 0
  | Continuation { style } ->
      (* Hash style for continuation cells *)
      Style.hash style
  | Glyph { text; style; _ } ->
      let glyph_hash = Hashtbl.hash text in
      let style_hash = Style.hash style in
      glyph_hash lxor style_hash

(** Cell type checks *)

let is_empty = function Empty -> true | _ -> false
let is_glyph = function Glyph _ -> true | _ -> false
let is_continuation = function Continuation _ -> true | _ -> false

(* Pretty-printing *)

let pp ppf = function
  | Empty -> Fmt.string ppf "Empty"
  | Glyph { text; width; style } ->
      Fmt.pf ppf "@[<h>Glyph {text=%S; width=%d; style=%a}@]" text width
        Style.pp style
  | Continuation { style } ->
      Fmt.pf ppf "@[<h>Continuation {style=%a}@]" Style.pp style

(* Equality *)

let equal c1 c2 =
  match (c1, c2) with
  | Empty, Empty -> true
  | Glyph g1, Glyph g2 ->
      g1.text = g2.text && g1.width = g2.width && Style.equal g1.style g2.style
  | Continuation s1, Continuation s2 -> Style.equal s1.style s2.style
  | _ -> false
