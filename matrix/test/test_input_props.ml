(** Property tests for the input parser over untrusted bytes.

    The parser consumes bytes read from a terminal, so every byte string and
    every chunking of it is a valid input. Two laws are checked here:

    - {b Split equivalence}: feeding a byte string whole or in arbitrary chunks
      (at the same instant, draining expired deadlines at the end) yields the
      same stream of events and responses.
    - {b Total safety}: no input may raise, leave the parser with unbounded
      buffered data, or require unbounded draining; bracketed paste payloads are
      preserved exactly. *)

module Input = Matrix_input
open Windtrap
module Gen = Windtrap_prop.Gen

(* Generators ------------------------------------------------------------- *)

(* Byte strings biased toward escape-sequence shapes: complete sequences,
   truncated prefixes, paste markers, UTF-8 fragments and high bytes, plus
   plain text and pure random bytes. *)
let fragment_gen =
  Gen.frequency
    [
      ( 5,
        Gen.oneofl
          [
            "\x1b[A";
            "\x1b[1;5H";
            "\x1b[Z";
            "\x1bOP";
            "\x1bOa";
            "\x1b[3$";
            "\x1b[5^";
            "\x1b[8$";
            "\x1b[2;1$y";
            "\x1b[?1016;2$y";
            "\x1b[<0;10;20M";
            "\x1b[<32;3;4m";
            "\x1b[<0;5;7m";
            "\x1b[M !!";
            "\x1b[97;2u";
            "\x1b[27;5;13~";
            "\x1b[3~";
            "\x1b[8;24;80t";
            "\x1b[?1u";
            "\x1b[?62;4c";
            "\x1b[10;25R";
            "\x1b[I";
            "\x1b[O";
            "\x1b[[A";
            "\x1b[200~";
            "\x1b[201~";
            "\x1b]0;t\x07";
            "\x1b]52;c;aGVsbG8=\x1b\\";
            "\x1bP>|xy\x1b\\";
            "\x1bP+q5465\x1b\\";
            "\x1b_Gi=31337;OK\x1b\\";
            "\x1b\x1b";
            "\x1b";
          ] );
      ( 3,
        Gen.oneofl
          [
            "\x1b[";
            "\x1bO";
            "\x1b]";
            "\x1bP";
            "\x1b_";
            "\x1b[<";
            "\x1b[<35;2";
            "\x1b[20";
            "\x1b[200";
            "\x1b[?";
            "\x1b[?1016;2$";
            "\x1b[M";
            "\x1b[M!";
            "\x1b[[";
            "\x1b]0;t\x1b";
            "\x1bP+q\x1b";
          ] );
      ( 3,
        Gen.oneofl
          [
            "\xC3\xA9";
            "\xE2\x82\xAC";
            "\xF0\x9F\x98\x80";
            "\xC3";
            "\xE2";
            "\xE2\x82";
            "\xF0\x9F\x98";
            "\x80";
            "\xBF";
            "\xC0";
            "\xE0\x80";
            "\xF4\x90\x80\x80";
            "\xFF";
            "\xFE";
          ] );
      (4, Gen.string_size (Gen.int_range 0 5) (Gen.char_range 'a' 'z'));
      (2, Gen.string_size (Gen.int_range 0 6) Gen.char);
      (1, Gen.oneofl [ "\x01"; "\x07"; "\x09"; "\x0a"; "\x0d"; "\x7f" ]);
    ]

let input_gen =
  let open Gen in
  let+ frags = list_size (int_range 0 12) fragment_gen in
  String.concat "" frags

(* A byte string together with arbitrary cut points. *)
let case_gen =
  let open Gen in
  let* s = input_gen in
  let+ cuts = list_size (int_range 0 8) (int_range 0 (String.length s)) in
  (s, cuts)

let chunks_of s cuts =
  let len = String.length s in
  let cuts = List.sort_uniq compare (0 :: len :: cuts) in
  let rec slices = function
    | a :: (b :: _ as rest) -> String.sub s a (b - a) :: slices rest
    | _ -> []
  in
  slices cuts

let pp_case fmt (s, cuts) =
  Format.fprintf fmt "%S cut at [%s]" s
    (String.concat ";" (List.map string_of_int cuts))

let case = testable ~pp:pp_case ~gen:case_gen ()

(* Harness ---------------------------------------------------------------- *)

type item = User of Input.t | Response of Input.Response.t

let pp_item fmt = function
  | User e -> Format.fprintf fmt "User(%a)" Input.pp e
  | Response r -> Format.fprintf fmt "Response(%a)" Input.Response.pp r

let equal_item a b =
  match (a, b) with
  | User a, User b -> Input.equal a b
  | Response a, Response b -> Input.Response.equal a b
  | (User _ | Response _), _ -> false

let item = testable ~pp:pp_item ~equal:equal_item ()
let max_drain_steps = 32

(* Feeds [chunks] at the same instant, then drains every expired deadline and
   finishes the stream. The number of drains needed to quiesce is bounded;
   exceeding the bound means the parser re-arms deadlines forever. *)
let run_stream ?max_paste_bytes chunks =
  let parser = Input.Parser.create ?max_paste_bytes () in
  let acc = ref [] in
  let on_event e = acc := User e :: !acc in
  let on_response r = acc := Response r :: !acc in
  List.iter
    (fun chunk ->
      let bytes = Bytes.of_string chunk in
      Input.Parser.feed parser bytes 0 (Bytes.length bytes) ~now:0.0 ~on_event
        ~on_response)
    chunks;
  let rec drain_all now steps =
    match Input.Parser.deadline parser with
    | None -> ()
    | Some deadline ->
        if steps >= max_drain_steps then
          failf "drain did not quiesce after %d steps" steps;
        let now = Float.max (now +. 1e-6) (deadline +. 1e-6) in
        Input.Parser.drain parser ~now ~on_event ~on_response;
        drain_all now (steps + 1)
  in
  drain_all 0.0 0;
  Input.Parser.finish parser ~on_event;
  List.rev !acc

(* Properties ------------------------------------------------------------- *)

let prop_split_equivalence (s, cuts) =
  equal ~msg:"whole and split feeds emit the same stream" (list item)
    (run_stream ~max_paste_bytes:64 [ s ])
    (run_stream ~max_paste_bytes:64 (chunks_of s cuts))

let prop_total_safety (s, cuts) =
  let parser = Input.Parser.create ~max_paste_bytes:64 () in
  let on_event _ = () in
  let on_response _ = () in
  List.iter
    (fun chunk ->
      let bytes = Bytes.of_string chunk in
      Input.Parser.feed parser bytes 0 (Bytes.length bytes) ~now:0.0 ~on_event
        ~on_response;
      let buffered = Bytes.length (Input.Parser.pending parser) in
      is_true
        ~msg:(Printf.sprintf "pending stays bounded (%d bytes)" buffered)
        (buffered <= 4096))
    (chunks_of s cuts);
  let rec drain_all now steps =
    match Input.Parser.deadline parser with
    | None -> ()
    | Some deadline ->
        if steps >= max_drain_steps then
          failf "drain did not quiesce after %d steps" steps;
        let now = Float.max (now +. 1e-6) (deadline +. 1e-6) in
        Input.Parser.drain parser ~now ~on_event ~on_response;
        drain_all now (steps + 1)
  in
  drain_all 0.0 0;
  Input.Parser.finish parser ~on_event

(* Paste payloads below the size limit must be delivered verbatim, whatever
   the chunking. Payloads containing the end marker terminate early by
   definition and are discarded. *)
let paste_case_gen =
  let open Gen in
  let* payload = input_gen in
  let framed = "\x1b[200~" ^ payload ^ "\x1b[201~" in
  let+ cuts = list_size (int_range 0 4) (int_range 0 (String.length framed)) in
  (payload, cuts)

let paste_case =
  testable
    ~pp:(fun fmt (payload, cuts) ->
      Format.fprintf fmt "payload %S cut at [%s]" payload
        (String.concat ";" (List.map string_of_int cuts)))
    ~gen:paste_case_gen ()

let contains_substring s sub =
  let len_s = String.length s and len_sub = String.length sub in
  let rec outer i =
    if i + len_sub > len_s then false
    else if String.sub s i len_sub = sub then true
    else outer (i + 1)
  in
  len_sub = 0 || outer 0

let prop_paste_exactness (payload, cuts) =
  assume (not (contains_substring payload "\x1b[201~"));
  let framed = "\x1b[200~" ^ payload ^ "\x1b[201~" in
  equal ~msg:"paste payload is preserved exactly" (list item)
    [ User (Input.Paste payload) ]
    (run_stream (chunks_of framed cuts))

let props =
  let config =
    { Windtrap_prop.Prop.default_config with count = 1000; max_gen = 1500 }
  in
  [
    prop' ~config "split equivalence" case prop_split_equivalence;
    prop' ~config "total safety" case prop_total_safety;
    prop' ~config "paste payload exactness" paste_case prop_paste_exactness;
  ]

let () = run "matrix.input.props" [ group "parser" props ]
