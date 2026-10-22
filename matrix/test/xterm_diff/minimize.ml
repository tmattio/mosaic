(* Delta debugging (Zeller and Hildebrandt, "Simplifying and isolating
   failure-inducing input", 2002) over a case's tokens, then a pass that
   replaces each token by its simpler variants, repeated to a fixpoint.

   [fails tokens] must be deterministic. [budget] bounds the number of calls. *)

let split l n =
  let len = List.length l in
  let rec go l i acc =
    if i = n then List.rev acc
    else
      let size = (len * (i + 1) / n) - (len * i / n) in
      let chunk = List.filteri (fun j _ -> j < size) l in
      let rest = List.filteri (fun j _ -> j >= size) l in
      go rest (i + 1) (chunk :: acc)
  in
  go l 0 []

let run ?(budget = 400) ~fails tokens =
  let calls = ref 0 in
  let test l =
    if !calls >= budget || l = [] then false
    else (
      incr calls;
      fails l)
  in
  let rec ddmin l n =
    let len = List.length l in
    if len <= 1 then l
    else
      let chunks = split l n in
      match List.find_opt test chunks with
      | Some c -> ddmin c 2
      | None -> (
          let complements =
            List.mapi
              (fun i _ -> List.concat (List.filteri (fun j _ -> j <> i) chunks))
              chunks
          in
          match if n > 2 then List.find_opt test complements else None with
          | Some c -> ddmin c (max (n - 1) 2)
          | None -> if n < len then ddmin l (min len (2 * n)) else l)
  in
  let rec simplify l =
    let changed = ref false in
    let l =
      List.mapi (fun i _ -> i) l
      |> List.fold_left
           (fun l i ->
             let t = List.nth l i in
             let with_ b =
               List.mapi
                 (fun j x ->
                   if j = i then { x with Gen.bytes = b; simpler = [] } else x)
                 l
             in
             match List.find_opt (fun b -> test (with_ b)) t.Gen.simpler with
             | Some b ->
                 changed := true;
                 with_ b
             | None -> l)
           l
    in
    if !changed && !calls < budget then simplify (ddmin l 2) else l
  in
  let l = ddmin tokens 2 in
  let l = simplify l in
  (l, !calls)
