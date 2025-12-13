(* Generate minimal Unicode data for matrix.glyph.

   Adapted from notty's generator (Copyright (c) 2020 David Kaloper Meršinjak).

   Usage: dune exec matrix/support/gen_unicode_data.exe
*)

let filter p seq i = seq (fun x -> if p x then i x)
let map f seq i = seq (fun x -> i (f x))

let uchars it =
  let rec go it u =
    it u;
    go it (Uchar.succ u)
  in
  try go it Uchar.min with Invalid_argument _ -> ()

let to_list seq =
  let xs = ref [] in
  seq (fun x -> xs := x :: !xs);
  List.rev !xs

let intervals_kv seq i =
  let s = ref None in
  let f (x, v) =
    match !s with
    | None -> s := Some (x, x, v)
    | Some (a, b, v0) when v = v0 && x = Uchar.succ b -> s := Some (a, x, v0)
    | Some e ->
        i e;
        s := Some (x, x, v)
  in
  seq f;
  match !s with Some e -> i e | _ -> ()

(* Condenses code points into continuous range. *)
let pack_u u =
  let i = Uchar.to_int u in
  if i > 0xd7ff then i - 0x800 else i

let unpack_u i = Uchar.of_int (if i < 0xd800 then i else i + 0x800)

(* 12-6-6-bit (0xfff-0x3f-0x3f) trie, 3 levels, array-array-string. *)
let trie ~default f =
  let xs =
    List.init ((pack_u Uchar.max lsr 12) + 1) @@ fun b0 ->
    let mask = b0 lsl 12 in
    let arr =
      Array.init 0x40 @@ fun b1 ->
      let mask = mask lor (b1 lsl 6) in
      let v b2 =
        match unpack_u (mask lor b2) with
        | x -> f x
        | exception Invalid_argument _ -> default
      in
      match
        for b2 = 0 to 0x3f do
          if v b2 <> default then raise Exit
        done
      with
      | exception Exit -> String.init 0x40 (fun b2 -> Char.chr (v b2))
      | () -> ""
    in
    if Array.for_all (( = ) "") arr then [||] else arr
  in
  let rec trim = function [||] :: xs -> trim xs | xs -> xs in
  List.rev (trim (List.rev xs)) |> Array.of_list

(* Boolean trie - pack 8 bools per byte *)
let trie_bool ~default f =
  let xs =
    List.init ((pack_u Uchar.max lsr 12) + 1) @@ fun b0 ->
    let mask = b0 lsl 12 in
    let arr =
      Array.init 0x40 @@ fun b1 ->
      let mask = mask lor (b1 lsl 6) in
      let v b2 =
        match unpack_u (mask lor b2) with
        | x -> f x
        | exception Invalid_argument _ -> default
      in
      (* Check if all values are default *)
      match
        for b2 = 0 to 0x3f do
          if v b2 <> default then raise Exit
        done
      with
      | exception Exit ->
          (* Pack 8 bools per byte *)
          String.init 8 (fun byte_idx ->
              let byte = ref 0 in
              for bit = 0 to 7 do
                let b2 = (byte_idx * 8) + bit in
                if v b2 then byte := !byte lor (1 lsl bit)
              done;
              Char.chr !byte)
      | () -> ""
    in
    if Array.for_all (( = ) "") arr then [||] else arr
  in
  let rec trim = function [||] :: xs -> trim xs | xs -> xs in
  List.rev (trim (List.rev xs)) |> Array.of_list

let pf = Format.fprintf
let strf = Format.sprintf

let pp_iter ?(sep = fun _ _ -> ()) iter pp ppf x =
  let fst = ref true in
  let f x =
    if !fst then fst := false else sep ppf ();
    pp ppf x
  in
  iter f x

let pp_u ppf u = pf ppf "0x%04x" (Uchar.to_int u)

let pp_as_array iter pp ppf x =
  let sep ppf () = pf ppf ";@ " in
  pf ppf "@[<2>[|%a|]@]" (pp_iter ~sep iter pp) x

let intern ppf_ml iter =
  let t = Hashtbl.create 16 in
  let n = ref 0 in
  iter (fun s ->
      if not (Hashtbl.mem t s) then begin
        let name = strf "s%03d" !n in
        Hashtbl.add t s name;
        incr n;
        pf ppf_ml "let %s = %S@." name s
      end);
  pf ppf_ml "@.";
  fun ppf s ->
    match Hashtbl.find_opt t s with
    | Some name -> pf ppf "%s" name
    | None -> pf ppf "%S" s

let dump_interval_map (ppf_mli, ppf_ml) ~name ~desc seq =
  pf ppf_mli "(** %s *)@.val %s: int array * int array * int array@.@." desc
    name;
  let xs = to_list (intervals_kv seq) in
  let aa = List.map (fun (a, _, _) -> a) xs
  and bb = List.map (fun (_, b, _) -> b) xs
  and cc = List.map (fun (_, _, c) -> c) xs in
  let pp_arr pp = pp_as_array List.iter pp in
  let pp_arr_u = pp_arr pp_u and pp_arr_i = pp_arr Format.pp_print_int in
  pf ppf_ml "@[<2>let %s =@ @[<1>(%a,@ %a,@ %a)@]@]@.@." name pp_arr_u aa
    pp_arr_u bb pp_arr_i cc

let dump_trie_map (ppf_mli, ppf_ml) ~name ~desc ~default f =
  pf ppf_mli "(** %s *)@.val %s: string array array@.@." desc name;
  let xs = trie ~default f in
  let pp_s =
    intern ppf_ml
      Array.(
        fun i ->
          i "";
          iter (iter i) xs)
  in
  pf ppf_ml "@[<2>let %s =@ %a@]@.@." name
    Array.(pp_as_array iter (pp_as_array iter pp_s))
    xs

let dump_trie_bool_map (ppf_mli, ppf_ml) ~name ~desc ~default f =
  pf ppf_mli "(** %s *)@.val %s: string array array@.@." desc name;
  let xs = trie_bool ~default f in
  let pp_s =
    intern ppf_ml
      Array.(
        fun i ->
          i "";
          iter (iter i) xs)
  in
  pf ppf_ml "@[<2>let %s =@ %a@]@.@." name
    Array.(pp_as_array iter (pp_as_array iter pp_s))
    xs

let pp_header ppf =
  Format.fprintf ppf
    "(* WARNING: Do not edit. This file was automatically generated.\n\n\
    \   Unicode version %s.\n\
    \   Generated using matrix/support/gen_unicode_data.ml\n\
     *)\n\n\
     [@@@@@@ocamlformat \"disable\"]\n\n"
    Uucp.unicode_version

let extract ((ppmli, ppml) as ppfs) =
  pp_header ppmli;
  pp_header ppml;

  (* tty_width_hint: interval map since most values are 1 *)
  dump_interval_map ppfs ~name:"tty_width_hint"
    ~desc:
      "TTY width hint. Returns (starts, ends, values). Default is 1, -1 for \
       controls."
    (uchars
    |> map (fun u -> (u, Uucp.Break.tty_width_hint u))
    |> filter (fun (_, w) -> w <> -1 && w <> 1));

  (* grapheme_cluster_break: trie map, default is XX (16) *)
  dump_trie_map ppfs ~name:"grapheme_cluster_break"
    ~desc:"Grapheme cluster break property (UAX #29). Default is 16 (XX)."
    ~default:16 Uucp.Break.Low.grapheme_cluster;

  (* indic_conjunct_break: trie map, default is None (3) *)
  dump_trie_map ppfs ~name:"indic_conjunct_break"
    ~desc:"Indic conjunct break property (UAX #29 GB9c). Default is 3 (None)."
    ~default:3 Uucp.Break.Low.indic_conjunct_break;

  (* extended_pictographic: boolean trie, default is false *)
  dump_trie_bool_map ppfs ~name:"extended_pictographic"
    ~desc:"Extended_Pictographic property (UAX #29 GB11). Default is false."
    ~default:false Uucp.Emoji.is_extended_pictographic;

  ()

let file = "matrix/lib/glyph/unicode_data"

let with_new name f =
  let o = open_out_gen [ Open_trunc; Open_creat; Open_wronly ] 0o664 name in
  let ppf = Format.formatter_of_out_channel o in
  f ppf;
  Format.pp_print_flush ppf ();
  close_out o

let () =
  Format.printf "Dumping Unicode v%s data to %s.@." Uucp.unicode_version file;
  with_new (file ^ ".mli") @@ fun ppmli ->
  with_new (file ^ ".ml") @@ fun ppml -> extract (ppmli, ppml)
