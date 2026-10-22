(* A weighted, seeded generator of short terminal inputs.

   A case is a list of tokens. Each token is one lexical unit (a run of text, a
   control, a whole escape sequence or string) with a label naming its
   grammar rule, which the triage groups by, and simpler variants of itself,
   which the minimizer tries. Cases are short so that a difference shrinks to
   a few tokens. *)

type token = { label : string; bytes : string; simpler : string list }

type config = {
  rows : int;
  cols : int;
  exclude : string list;
  max_tokens : int;  (** The longest case, in tokens. *)
}

let tok ?(simpler = []) label bytes = { label; bytes; simpler }

let utf8 cp =
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b (Uchar.of_int cp);
  Buffer.contents b

let pick st l = List.nth l (Random.State.int st (List.length l))

(* [weighted st l] draws from [(weight, x)] pairs. *)
let weighted st l =
  let total = List.fold_left (fun acc (w, _) -> acc + w) 0 l in
  let r = Random.State.int st total in
  let rec go r = function
    | (w, x) :: rest -> if r < w then x else go (r - w) rest
    | [] -> assert false
  in
  go r l

(* {1 Parameters}

   Edge ranges: absent, 0, 1, the screen's dimensions and their neighbours,
   and values beyond any 16, 31 or 32-bit bound. *)

let param st { rows; cols; _ } =
  weighted st
    [
      (15, "");
      (10, "0");
      (15, "1");
      (8, "2");
      (10, string_of_int (3 + Random.State.int st 7));
      (5, string_of_int rows);
      (3, string_of_int (rows - 1));
      (3, string_of_int (rows + 1));
      (5, string_of_int cols);
      (3, string_of_int (cols - 1));
      (3, string_of_int (cols + 1));
      (2, "65535");
      (1, "65536");
      (2, "99999");
      (2, "2147483647");
      (1, "4294967296");
      (2, "99999999999999999999");
    ]

(* A CSI token. Its simpler variants drop parameters or set them to 1. *)
let csi ?(marker = "") ?(inter = "") label params final =
  let build ps =
    "\027[" ^ marker ^ String.concat ";" ps ^ inter ^ String.make 1 final
  in
  let bytes = build params in
  let n = List.length params in
  let simpler =
    (if n > 0 then [ build [] ] else [])
    @ List.concat
        (List.init n (fun i ->
             let with_i v =
               List.mapi (fun j p -> if i = j then v else p) params
             in
             let take = List.filteri (fun j _ -> j < i) params in
             [ build take; build (with_i ""); build (with_i "1") ]))
  in
  tok label bytes
    ~simpler:(List.sort_uniq compare simpler |> List.filter (( <> ) bytes))

(* {1 Text}

   The alphabet: printable ASCII, Latin-1, wide CJK, combining marks, emoji
   with VS15/VS16, skin tones and ZWJ, Prepend, regional indicators, Hangul
   jamo, a conjunct. [alphabet] lists every non-ASCII scalar so that the
   driver can measure xterm's width of each. *)

let cjk = [ 0x4E00; 0x4E2D; 0x30A2; 0xD55C; 0xFF21; 0x3000 ]
let combining = [ 0x0301; 0x0308; 0x0323; 0x20DD; 0x0345; 0x1AB0 ]
let emoji = [ 0x1F600; 0x1F44D; 0x1F468; 0x1F469; 0x1F467; 0x1F308 ]
let emoji_vs = [ 0x2764; 0x263A; 0x2702; 0x1F3F3; 0x0023 ]
let skin = [ 0x1F3FB; 0x1F3FD ]
let prepend = [ 0x0600; 0x0601; 0x06DD; 0x070F; 0x0D4E; 0x110BD ]
let regional = [ 0x1F1EB; 0x1F1F7 ]
let jamo = [ 0x1100; 0x1161; 0x11A8 ]

let other =
  [
    0x00E9;
    0x03A9;
    0x00AD;
    0x200B;
    0x200C;
    0x200D;
    0xFE0E;
    0xFE0F;
    0xFFFD;
    0x0915;
    0x094D;
    0x0937;
    0x2588;
  ]

let alphabet =
  List.concat
    [ cjk; combining; emoji; emoji_vs; skin; prepend; regional; jamo; other ]

let ascii_run st =
  let n = 1 + Random.State.int st 8 in
  let s = String.init n (fun _ -> Char.chr (0x21 + Random.State.int st 94)) in
  let s = if Random.State.int st 4 = 0 then " " ^ s else s in
  tok "text.ascii" s
    ~simpler:(if String.length s > 1 then [ "a"; String.sub s 0 1 ] else [])

let text st cfg =
  let one l = utf8 (pick st l) in
  weighted st
    [
      (30, fun () -> ascii_run st);
      ( 4,
        fun () ->
          let n = cfg.cols + Random.State.int st 3 - 1 in
          tok "text.line"
            (String.init (max 1 n) (fun i -> Char.chr (0x41 + (i mod 26))))
            ~simpler:[ "a" ] );
      (10, fun () -> tok "text.wide" (one cjk) ~simpler:[ "a" ]);
      (6, fun () -> tok "text.combining" (one combining));
      ( 3,
        fun () ->
          tok "text.base+combining"
            ("e" ^ one combining
            ^ if Random.State.bool st then one combining else "")
            ~simpler:[ "e" ] );
      (4, fun () -> tok "text.emoji" (one emoji) ~simpler:[ "a" ]);
      ( 3,
        fun () ->
          tok "text.emoji+vs16" (one emoji_vs ^ utf8 0xFE0F) ~simpler:[ "a" ] );
      ( 2,
        fun () ->
          tok "text.emoji+vs15" (one emoji_vs ^ utf8 0xFE0E) ~simpler:[ "a" ] );
      ( 2,
        fun () -> tok "text.emoji+skin" (one emoji ^ one skin) ~simpler:[ "a" ]
      );
      ( 3,
        fun () ->
          tok "text.zwj"
            (utf8 0x1F468 ^ utf8 0x200D ^ utf8 0x1F469
            ^ if Random.State.bool st then utf8 0x200D ^ utf8 0x1F467 else "")
            ~simpler:[ utf8 0x1F468 ] );
      ( 1,
        fun () ->
          tok "text.zwj"
            (utf8 0x1F3F3 ^ utf8 0xFE0F ^ utf8 0x200D ^ utf8 0x1F308)
            ~simpler:[ utf8 0x1F3F3 ] );
      (2, fun () -> tok "text.zwj-alone" (utf8 0x200D));
      ( 3,
        fun () ->
          tok "text.prepend"
            (one prepend ^ if Random.State.bool st then "1" else "")
            ~simpler:[ one prepend ] );
      ( 2,
        fun () ->
          tok "text.regional"
            (utf8 0x1F1EB ^ utf8 0x1F1F7)
            ~simpler:[ utf8 0x1F1EB ] );
      (1, fun () -> tok "text.regional" (utf8 0x1F1EB));
      ( 2,
        fun () ->
          tok "text.jamo"
            (utf8 0x1100 ^ utf8 0x1161
            ^ if Random.State.bool st then utf8 0x11A8 else "")
            ~simpler:[ utf8 0x1100 ] );
      ( 1,
        fun () ->
          tok "text.conjunct"
            (utf8 0x0915 ^ utf8 0x094D ^ utf8 0x0937)
            ~simpler:[ utf8 0x0915 ] );
      (4, fun () -> tok "text.other" (one other));
    ]
    ()

(* {1 Controls} *)

let c0 st =
  weighted st
    [
      (6, ("c0.BS", "\b"));
      (6, ("c0.HT", "\t"));
      (6, ("c0.LF", "\n"));
      (2, ("c0.VT", "\x0b"));
      (2, ("c0.FF", "\x0c"));
      (6, ("c0.CR", "\r"));
      (2, ("c0.SO", "\x0e"));
      (2, ("c0.SI", "\x0f"));
      (1, ("c0.BEL", "\x07"));
      (1, ("c0.NUL", "\x00"));
      (1, ("c0.ENQ", "\x05"));
      (1, ("c0.CAN", "\x18"));
      (1, ("c0.SUB", "\x1a"));
      (1, ("c0.DEL", "\x7f"));
      (1, ("c0.ESC", "\027"));
      ( 1,
        ( "c0.other",
          String.make 1
            (pick st
               [
                 '\x01';
                 '\x02';
                 '\x03';
                 '\x04';
                 '\x06';
                 '\x10';
                 '\x11';
                 '\x1c';
                 '\x1f';
               ]) ) );
    ]
  |> fun (l, b) -> tok l b

let sgr_code st =
  weighted st
    [
      (8, "0");
      (3, "");
      (6, "1");
      (3, "2");
      (3, "3");
      (6, "4");
      (2, "5");
      (6, "7");
      (2, "8");
      (3, "9");
      (2, "21");
      (3, "22");
      (2, "23");
      (3, "24");
      (2, "25");
      (3, "27");
      (2, "28");
      (2, "29");
      (2, "53");
      (1, "51");
      (1, "4:3");
      (1, "4:0");
      (8, string_of_int (30 + Random.State.int st 8));
      (8, string_of_int (40 + Random.State.int st 8));
      (3, string_of_int (90 + Random.State.int st 8));
      (3, string_of_int (100 + Random.State.int st 8));
      (2, "39");
      (2, "49");
      (3, Printf.sprintf "38;5;%d" (pick st [ 0; 3; 9; 15; 16; 100; 231; 255 ]));
      (3, Printf.sprintf "48;5;%d" (pick st [ 0; 3; 9; 15; 16; 100; 231; 255 ]));
      ( 2,
        Printf.sprintf "38;2;%d;%d;%d" (Random.State.int st 256)
          (Random.State.int st 256) (Random.State.int st 256) );
      ( 2,
        Printf.sprintf "48;2;%d;%d;%d" (Random.State.int st 256)
          (Random.State.int st 256) (Random.State.int st 256) );
      (1, Printf.sprintf "38:5:%d" (Random.State.int st 256));
      ( 1,
        Printf.sprintf "48:2::%d:%d:%d" (Random.State.int st 256)
          (Random.State.int st 256) (Random.State.int st 256) );
    ]

let sgr st =
  let n = 1 + Random.State.int st 3 in
  let codes = List.init n (fun _ -> sgr_code st) in
  let bytes = "\027[" ^ String.concat ";" codes ^ "m" in
  tok "SGR" bytes
    ~simpler:
      (List.map (fun c -> "\027[" ^ c ^ "m") codes |> List.filter (( <> ) bytes))

let dec_modes = [ 1; 6; 7; 25; 42; 45; 47; 1047; 1048; 1049; 2004; 12; 1004 ]

let csi_fn st cfg =
  let p () = param st cfg in
  let ps n = List.init n (fun _ -> p ()) in
  let one label final = csi label (ps 1) final in
  weighted st
    [
      (4, fun () -> one "CUU" 'A');
      (4, fun () -> one "CUD" 'B');
      (4, fun () -> one "CUF" 'C');
      (2, fun () -> one "HPR" 'a');
      (4, fun () -> one "CUB" 'D');
      (2, fun () -> one "CNL" 'E');
      (2, fun () -> one "CPL" 'F');
      (5, fun () -> one "CHA" 'G');
      (2, fun () -> one "HPA" '`');
      (3, fun () -> one "VPA" 'd');
      (2, fun () -> one "VPR" 'e');
      (6, fun () -> csi "CUP" (ps (Random.State.int st 3)) 'H');
      (2, fun () -> csi "HVP" (ps (Random.State.int st 3)) 'f');
      (3, fun () -> one "CHT" 'I');
      (3, fun () -> one "CBT" 'Z');
      (2, fun () -> csi "TBC" [ pick st [ ""; "0"; "3"; "2" ] ] 'g');
      (4, fun () -> csi "ED" [ pick st [ ""; "0"; "1"; "2"; "3"; "4" ] ] 'J');
      (4, fun () -> csi "EL" [ pick st [ ""; "0"; "1"; "2"; "3" ] ] 'K');
      ( 2,
        fun () -> csi ~marker:"?" "DECSED" [ pick st [ ""; "0"; "1"; "2" ] ] 'J'
      );
      ( 2,
        fun () -> csi ~marker:"?" "DECSEL" [ pick st [ ""; "0"; "1"; "2" ] ] 'K'
      );
      ( 3,
        fun () ->
          csi ~inter:"\"" "DECSCA" [ pick st [ ""; "0"; "1"; "2"; "3" ] ] 'q' );
      (3, fun () -> one "ECH" 'X');
      (3, fun () -> one "IL" 'L');
      (3, fun () -> one "DL" 'M');
      (4, fun () -> one "DCH" 'P');
      (4, fun () -> one "ICH" '@');
      (2, fun () -> csi ~inter:" " "SL" (ps 1) '@');
      (2, fun () -> csi ~inter:" " "SR" (ps 1) 'A');
      (2, fun () -> csi ~inter:"'" "DECIC" (ps 1) '}');
      (2, fun () -> csi ~inter:"'" "DECDC" (ps 1) '~');
      (5, fun () -> csi "DECSTBM" (ps (Random.State.int st 3)) 'r');
      (3, fun () -> one "SU" 'S');
      (3, fun () -> one "SD" 'T');
      (4, fun () -> one "REP" 'b');
      (12, fun () -> sgr st);
      (2, fun () -> csi "DSR" [ pick st [ "5"; "6" ] ] 'n');
      (1, fun () -> csi ~marker:"?" "DECXCPR" [ "6" ] 'n');
      (1, fun () -> csi "DA" (pick st [ []; [ "0" ] ]) 'c');
      (1, fun () -> csi ~marker:">" "DA2" (pick st [ []; [ "0" ] ]) 'c');
      (2, fun () -> csi "SCOSC" [] 's');
      (2, fun () -> csi "SCORC" [] 'u');
      (1, fun () -> csi ~inter:"!" "DECSTR" [] 'p');
      (3, fun () -> csi "SM" [ pick st [ "4"; "4"; "2"; "34" ] ] 'h');
      (3, fun () -> csi "RM" [ pick st [ "4"; "4"; "2"; "34" ] ] 'l');
      ( 6,
        fun () ->
          csi ~marker:"?" "DECSET" [ string_of_int (pick st dec_modes) ] 'h' );
      ( 5,
        fun () ->
          csi ~marker:"?" "DECRST" [ string_of_int (pick st dec_modes) ] 'l' );
    ]
    ()

let charset_finals =
  [
    'B';
    'B';
    '0';
    '0';
    'A';
    'K';
    'R';
    'Q';
    'Y';
    'E';
    '6';
    'Z';
    'H';
    '7';
    '=';
    '4';
    'C';
    '5';
    '9';
    'f';
    '`';
    '<';
    '>';
    '1';
    '2';
  ]

let esc_fn st =
  weighted st
    [
      (4, fun () -> tok "DECSC" "\0277");
      (4, fun () -> tok "DECRC" "\0278");
      (3, fun () -> tok "IND" "\027D");
      (3, fun () -> tok "NEL" "\027E");
      (2, fun () -> tok "HTS" "\027H");
      (4, fun () -> tok "RI" "\027M");
      (1, fun () -> tok "RIS" "\027c");
      (2, fun () -> tok "DECALN" "\027#8");
      ( 6,
        fun () ->
          let g = pick st [ '('; '('; ')'; '*'; '+' ] in
          tok "SCS"
            (Printf.sprintf "\027%c%c" g (pick st charset_finals))
            ~simpler:[ Printf.sprintf "\027%c0" g ] );
      (2, fun () -> tok "LS2" "\027n");
      (2, fun () -> tok "LS3" "\027o");
      (2, fun () -> tok "SS2" "\027N");
      (2, fun () -> tok "SS3" "\027O");
      (1, fun () -> tok "DECKPAM" "\027=");
      (1, fun () -> tok "DECKPNM" "\027>");
    ]
    ()

let strings st =
  let term () =
    weighted st [ (4, "\027\\"); (3, "\x07"); (1, "\x18"); (1, "") ]
  in
  let body () =
    pick st [ ""; "x"; "title"; "a;b"; "\xc3\xa9t\xc3\xa9"; "1;2;3" ]
  in
  weighted st
    [
      ( 3,
        fun () ->
          let t = term () in
          tok "OSC"
            ("\027]"
            ^ pick st [ "0"; "2"; "1"; "8;;http://e" ]
            ^ ";" ^ body () ^ t) );
      ( 1,
        fun () ->
          tok "OSC.hyperlink" "\027]8;;http://e\027\\link\027]8;;\027\\"
            ~simpler:[ "link" ] );
      ( 2,
        fun () ->
          tok "DCS"
            (* Not [$q]: DECRQSS, outside matrix's functions. *)
            ("\027P" ^ pick st [ "q"; "1;2|x"; "" ] ^ body () ^ term ()) );
      (1, fun () -> tok "APC" ("\027_" ^ body () ^ term ()));
      (1, fun () -> tok "PM" ("\027^" ^ body () ^ term ()));
      (1, fun () -> tok "SOS" ("\027X" ^ body () ^ term ()));
    ]
    ()

(* Bytes that are not well-formed text, and C1 controls in both encodings. *)
let encoding st =
  weighted st
    [
      (3, fun () -> tok "c1.utf8" (utf8 (0x80 + Random.State.int st 32)));
      ( 2,
        fun () ->
          tok "c1.utf8" (utf8 (pick st [ 0x9B; 0x9D; 0x90; 0x85; 0x84; 0x8D ]))
      );
      ( 2,
        fun () ->
          tok "c1.raw"
            (String.make 1 (Char.chr (0x80 + Random.State.int st 32))) );
      ( 2,
        fun () ->
          tok "utf8.bad"
            (pick st
               [
                 "\xe4\xb8";
                 "\xc0\x80";
                 "\xff";
                 "\xed\xa0\x80";
                 "\xf4\x90\x80\x80";
                 "\xe4";
                 "\xbf";
               ]) );
    ]
    ()

(* Syntax that a parser must recover from. *)
let malformed st cfg =
  let p () = param st cfg in
  weighted st
    [
      ( 2,
        fun () ->
          tok "bad.c0-in-csi"
            ("\027[" ^ p ()
            ^ pick st [ "\n"; "\r"; "\b"; "\t"; "\x0e" ]
            ^ p ()
            ^ pick st [ "A"; "C"; "H"; "@"; "K" ]) );
      ( 1,
        fun () ->
          tok "bad.esc-in-csi"
            ("\027[" ^ p () ^ "\027" ^ pick st [ "[C"; "7"; "D" ]) );
      ( 1,
        fun () ->
          tok "bad.can-in-csi"
            ("\027[" ^ p () ^ pick st [ "\x18"; "\x1a" ] ^ "A") );
      (* Not U+00E9: xterm reads it as [i], and [CSI 5 i] turns printer
         controller mode on, which swallows the driver's probe. *)
      ( 1,
        fun () ->
          tok "bad.utf8-in-csi"
            ("\027[" ^ p ()
            ^ utf8 (pick st [ 0xC1; 0xE8; 0xED; 0x100; 0x4E00 ])
            ^ "A") );
      (* Not [m]: [CSI ? Ps m] and [CSI > Ps m] are xterm's XTQMODKEYS and
         XTMODKEYS, outside matrix's functions. *)
      ( 1,
        fun () ->
          tok "bad.marker"
            ("\027["
            ^ pick st [ "<"; "="; ">"; "?" ]
            ^ p ()
            ^ pick st [ "A"; "H"; "J"; "@" ]) );
      ( 1,
        fun () ->
          tok "bad.intermediate"
            ("\027[" ^ p ()
            ^ pick st [ "!"; "$"; "#"; "%" ]
            ^ pick st [ "A"; "H"; "J"; "m" ]) );
      ( 1,
        fun () ->
          tok "bad.many-params"
            ("\027["
            ^ String.concat ";"
                (List.init
                   (pick st [ 16; 17; 32; 255; 256; 300 ])
                   (fun _ -> "1"))
            ^ pick st [ "m"; "H"; "C" ]) );
      ( 1,
        fun () ->
          tok "bad.colon"
            ("\027["
            ^ pick st [ "1:2"; "2:3H"; ":"; "1::" ]
            ^ pick st [ "H"; "C"; "m" ]) );
      ( 1,
        fun () ->
          tok "bad.esc-intermediate"
            (* Not [#3] to [#6]: line attributes (DECDHL, DECSWL, DECDWL),
               outside matrix's functions. *)
            ("\027" ^ pick st [ " F"; " G"; "%G"; "%@"; "#0"; "#9"; "(%5" ]) );
    ]
    ()

let token st cfg =
  weighted st
    [
      (40, fun () -> text st cfg);
      (10, fun () -> c0 st);
      (36, fun () -> csi_fn st cfg);
      (9, fun () -> esc_fn st);
      (2, fun () -> strings st);
      (2, fun () -> encoding st);
      (2, fun () -> malformed st cfg);
    ]
    ()

let excluded cfg t =
  List.exists (fun p -> String.starts_with ~prefix:p t.label) cfg.exclude

(* [case cfg ~seed i] is the [i]th case of the run seeded with [seed]: cases
   are independent, so a case is reproducible from its index alone. A token
   that [allow ~prev ~last] refuses ([prev] are the tokens before it, the
   last first; [last] whether it ends the case) is drawn again. *)
let case ?(allow = fun ~prev:_ ~last:_ _ -> true) cfg ~seed i =
  let st = Random.State.make [| seed; i |] in
  let len =
    weighted st
      ([
         (4, 1 + Random.State.int st 4);
         (4, 4 + Random.State.int st 12);
         (2, 15 + Random.State.int st 26);
       ]
      @
      if cfg.max_tokens > 40 then
        [ (2, 41 + Random.State.int st (cfg.max_tokens - 40)) ]
      else [])
  in
  let len = min len cfg.max_tokens in
  let rec draw acc n =
    if n = 0 then List.rev acc
    else
      let t = token st cfg in
      if excluded cfg t || not (allow ~prev:acc ~last:(n = 1) t) then draw acc n
      else draw (t :: acc) (n - 1)
  in
  draw [] len

let bytes tokens = String.concat "" (List.map (fun t -> t.bytes) tokens)
