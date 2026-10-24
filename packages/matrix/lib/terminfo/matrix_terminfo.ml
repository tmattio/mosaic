(* The standard capabilities' names, in the order of the compiled format,
   which is term.h's (ncurses 6.5: 44 booleans, 39 numbers, 414 strings). *)

let booleans =
  [|
    "bw";
    "am";
    "xsb";
    "xhp";
    "xenl";
    "eo";
    "gn";
    "hc";
    "km";
    "hs";
    "in";
    "da";
    "db";
    "mir";
    "msgr";
    "os";
    "eslok";
    "xt";
    "hz";
    "ul";
    "xon";
    "nxon";
    "mc5i";
    "chts";
    "nrrmc";
    "npc";
    "ndscr";
    "ccc";
    "bce";
    "hls";
    "xhpa";
    "crxm";
    "daisy";
    "xvpa";
    "sam";
    "cpix";
    "lpix";
    "OTbs";
    "OTns";
    "OTnc";
    "OTMT";
    "OTNL";
    "OTpt";
    "OTxr";
  |]

let numbers =
  [|
    "cols";
    "it";
    "lines";
    "lm";
    "xmc";
    "pb";
    "vt";
    "wsl";
    "nlab";
    "lh";
    "lw";
    "ma";
    "wnum";
    "colors";
    "pairs";
    "ncv";
    "bufsz";
    "spinv";
    "spinh";
    "maddr";
    "mjump";
    "mcs";
    "mls";
    "npins";
    "orc";
    "orl";
    "orhi";
    "orvi";
    "cps";
    "widcs";
    "btns";
    "bitwin";
    "bitype";
    "OTug";
    "OTdC";
    "OTdN";
    "OTdB";
    "OTdT";
    "OTkn";
  |]

let strings =
  [|
    "cbt";
    "bel";
    "cr";
    "csr";
    "tbc";
    "clear";
    "el";
    "ed";
    "hpa";
    "cmdch";
    "cup";
    "cud1";
    "home";
    "civis";
    "cub1";
    "mrcup";
    "cnorm";
    "cuf1";
    "ll";
    "cuu1";
    "cvvis";
    "dch1";
    "dl1";
    "dsl";
    "hd";
    "smacs";
    "blink";
    "bold";
    "smcup";
    "smdc";
    "dim";
    "smir";
    "invis";
    "prot";
    "rev";
    "smso";
    "smul";
    "ech";
    "rmacs";
    "sgr0";
    "rmcup";
    "rmdc";
    "rmir";
    "rmso";
    "rmul";
    "flash";
    "ff";
    "fsl";
    "is1";
    "is2";
    "is3";
    "if";
    "ich1";
    "il1";
    "ip";
    "kbs";
    "ktbc";
    "kclr";
    "kctab";
    "kdch1";
    "kdl1";
    "kcud1";
    "krmir";
    "kel";
    "ked";
    "kf0";
    "kf1";
    "kf10";
    "kf2";
    "kf3";
    "kf4";
    "kf5";
    "kf6";
    "kf7";
    "kf8";
    "kf9";
    "khome";
    "kich1";
    "kil1";
    "kcub1";
    "kll";
    "knp";
    "kpp";
    "kcuf1";
    "kind";
    "kri";
    "khts";
    "kcuu1";
    "rmkx";
    "smkx";
    "lf0";
    "lf1";
    "lf10";
    "lf2";
    "lf3";
    "lf4";
    "lf5";
    "lf6";
    "lf7";
    "lf8";
    "lf9";
    "rmm";
    "smm";
    "nel";
    "pad";
    "dch";
    "dl";
    "cud";
    "ich";
    "indn";
    "il";
    "cub";
    "cuf";
    "rin";
    "cuu";
    "pfkey";
    "pfloc";
    "pfx";
    "mc0";
    "mc4";
    "mc5";
    "rep";
    "rs1";
    "rs2";
    "rs3";
    "rf";
    "rc";
    "vpa";
    "sc";
    "ind";
    "ri";
    "sgr";
    "hts";
    "wind";
    "ht";
    "tsl";
    "uc";
    "hu";
    "iprog";
    "ka1";
    "ka3";
    "kb2";
    "kc1";
    "kc3";
    "mc5p";
    "rmp";
    "acsc";
    "pln";
    "kcbt";
    "smxon";
    "rmxon";
    "smam";
    "rmam";
    "xonc";
    "xoffc";
    "enacs";
    "smln";
    "rmln";
    "kbeg";
    "kcan";
    "kclo";
    "kcmd";
    "kcpy";
    "kcrt";
    "kend";
    "kent";
    "kext";
    "kfnd";
    "khlp";
    "kmrk";
    "kmsg";
    "kmov";
    "knxt";
    "kopn";
    "kopt";
    "kprv";
    "kprt";
    "krdo";
    "kref";
    "krfr";
    "krpl";
    "krst";
    "kres";
    "ksav";
    "kspd";
    "kund";
    "kBEG";
    "kCAN";
    "kCMD";
    "kCPY";
    "kCRT";
    "kDC";
    "kDL";
    "kslt";
    "kEND";
    "kEOL";
    "kEXT";
    "kFND";
    "kHLP";
    "kHOM";
    "kIC";
    "kLFT";
    "kMSG";
    "kMOV";
    "kNXT";
    "kOPT";
    "kPRV";
    "kPRT";
    "kRDO";
    "kRPL";
    "kRIT";
    "kRES";
    "kSAV";
    "kSPD";
    "kUND";
    "rfi";
    "kf11";
    "kf12";
    "kf13";
    "kf14";
    "kf15";
    "kf16";
    "kf17";
    "kf18";
    "kf19";
    "kf20";
    "kf21";
    "kf22";
    "kf23";
    "kf24";
    "kf25";
    "kf26";
    "kf27";
    "kf28";
    "kf29";
    "kf30";
    "kf31";
    "kf32";
    "kf33";
    "kf34";
    "kf35";
    "kf36";
    "kf37";
    "kf38";
    "kf39";
    "kf40";
    "kf41";
    "kf42";
    "kf43";
    "kf44";
    "kf45";
    "kf46";
    "kf47";
    "kf48";
    "kf49";
    "kf50";
    "kf51";
    "kf52";
    "kf53";
    "kf54";
    "kf55";
    "kf56";
    "kf57";
    "kf58";
    "kf59";
    "kf60";
    "kf61";
    "kf62";
    "kf63";
    "el1";
    "mgc";
    "smgl";
    "smgr";
    "fln";
    "sclk";
    "dclk";
    "rmclk";
    "cwin";
    "wingo";
    "hup";
    "dial";
    "qdial";
    "tone";
    "pulse";
    "hook";
    "pause";
    "wait";
    "u0";
    "u1";
    "u2";
    "u3";
    "u4";
    "u5";
    "u6";
    "u7";
    "u8";
    "u9";
    "op";
    "oc";
    "initc";
    "initp";
    "scp";
    "setf";
    "setb";
    "cpi";
    "lpi";
    "chr";
    "cvr";
    "defc";
    "swidm";
    "sdrfq";
    "sitm";
    "slm";
    "smicm";
    "snlq";
    "snrmq";
    "sshm";
    "ssubm";
    "ssupm";
    "sum";
    "rwidm";
    "ritm";
    "rlm";
    "rmicm";
    "rshm";
    "rsubm";
    "rsupm";
    "rum";
    "mhpa";
    "mcud1";
    "mcub1";
    "mcuf1";
    "mvpa";
    "mcuu1";
    "porder";
    "mcud";
    "mcub";
    "mcuf";
    "mcuu";
    "scs";
    "smgb";
    "smgbp";
    "smglp";
    "smgrp";
    "smgt";
    "smgtp";
    "sbim";
    "scsd";
    "rbim";
    "rcsd";
    "subcs";
    "supcs";
    "docr";
    "zerom";
    "csnm";
    "kmous";
    "minfo";
    "reqmp";
    "getm";
    "setaf";
    "setab";
    "pfxl";
    "devt";
    "csin";
    "s0ds";
    "s1ds";
    "s2ds";
    "s3ds";
    "smglr";
    "smgtb";
    "birep";
    "binel";
    "bicr";
    "colornm";
    "defbi";
    "endbi";
    "setcolor";
    "slines";
    "dispc";
    "smpch";
    "rmpch";
    "smsc";
    "rmsc";
    "pctrm";
    "scesc";
    "scesa";
    "ehhlm";
    "elhlm";
    "elohlm";
    "erhlm";
    "ethlm";
    "evhlm";
    "sgr1";
    "slength";
    "OTi2";
    "OTrs";
    "OTnl";
    "OTbc";
    "OTko";
    "OTma";
    "OTG2";
    "OTG3";
    "OTG1";
    "OTG4";
    "OTGR";
    "OTGL";
    "OTGU";
    "OTGD";
    "OTGH";
    "OTGV";
    "OTGC";
    "meml";
    "memu";
    "box1";
  |]

module Names = Map.Make (String)

type t = {
  names : string list;
  flags : string list;
  numbers : (string * int) list;
  strings : (string * string) list;
  flag_set : unit Names.t;
  number_map : int Names.t;
  string_map : string Names.t;
}

(* Reading a compiled entry *)

exception Invalid of string

let invalid fmt = Printf.ksprintf (fun m -> raise (Invalid m)) fmt

let parse s =
  let length = String.length s in
  let need pos n what =
    if pos < 0 || n < 0 || pos + n > length then
      invalid "the entry ends at byte %d, inside its %s" length what
  in
  let short pos = String.get_int16_le s pos in
  let count pos what =
    let n = short pos in
    if n < 0 then invalid "a negative count of %s" what;
    n
  in
  (* A NUL-terminated string at [pos], within [limit]. *)
  let c_string pos limit what =
    match String.index_from_opt s pos '\000' with
    | Some e when e < limit -> String.sub s pos (e - pos)
    | _ -> invalid "a string of the %s is not ended" what
  in
  let even pos = pos + (pos land 1) in
  need 0 12 "header";
  let number_size =
    match String.get_uint16_le s 0 with
    | 0o432 -> 2
    | 0o1036 -> 4
    | magic -> invalid "magic number 0o%o, not a compiled terminfo entry" magic
  in
  let number pos =
    if number_size = 2 then short pos
    else Int32.to_int (String.get_int32_le s pos)
  in
  let names_size = count 2 "name bytes" in
  let booleans_count = count 4 "booleans" in
  let numbers_count = count 6 "numbers" in
  let strings_count = count 8 "strings" in
  let table_size = count 10 "string table bytes" in
  let flags = ref [] and numbers_ = ref [] and strings_ = ref [] in
  let pos = 12 in
  need pos names_size "names";
  let names =
    if names_size = 0 then []
    else String.split_on_char '|' (c_string pos (pos + names_size) "names")
  in
  let pos = pos + names_size in
  need pos booleans_count "booleans";
  for i = 0 to booleans_count - 1 do
    if s.[pos + i] = '\001' && i < Array.length booleans then
      flags := booleans.(i) :: !flags
  done;
  let pos = even (pos + booleans_count) in
  need pos (numbers_count * number_size) "numbers";
  for i = 0 to numbers_count - 1 do
    let v = number (pos + (i * number_size)) in
    (* -1 is absent, -2 cancelled. *)
    if v >= 0 && i < Array.length numbers then
      numbers_ := (numbers.(i), v) :: !numbers_
  done;
  let pos = pos + (numbers_count * number_size) in
  need pos (strings_count * 2) "string offsets";
  let table = pos + (strings_count * 2) in
  need table table_size "string table";
  for i = 0 to strings_count - 1 do
    let offset = short (pos + (i * 2)) in
    if offset >= 0 then (
      if offset >= table_size then invalid "a string offset past its table";
      if i < Array.length strings then
        strings_ :=
          (strings.(i), c_string (table + offset) (table + table_size) "table")
          :: !strings_)
    else if offset < -2 then invalid "a string offset of %d" offset
  done;
  (* ncurses' extended section, when the entry goes on past its string
     table: user-defined capabilities, with their names. *)
  let pos = even (table + table_size) in
  if pos + 10 <= length then begin
    let ext_booleans = count pos "extended booleans" in
    let ext_numbers = count (pos + 2) "extended numbers" in
    let ext_strings = count (pos + 4) "extended strings" in
    let ext_table_size = count (pos + 8) "extended string table bytes" in
    let pos = pos + 10 in
    need pos ext_booleans "extended booleans";
    let booleans_at = pos in
    let pos = even (pos + ext_booleans) in
    need pos (ext_numbers * number_size) "extended numbers";
    let numbers_at = pos in
    let pos = pos + (ext_numbers * number_size) in
    let names_count = ext_booleans + ext_numbers + ext_strings in
    need pos ((ext_strings + names_count) * 2) "extended string offsets";
    let table = pos + ((ext_strings + names_count) * 2) in
    need table ext_table_size "extended string table";
    let limit = table + ext_table_size in
    (* The values come first in the table, then the names, whose offsets
       count from the end of the last value. *)
    let values =
      Array.init ext_strings (fun i ->
          let offset = short (pos + (i * 2)) in
          if offset >= 0 then (
            if offset >= ext_table_size then
              invalid "an extended string offset past its table";
            Some (c_string (table + offset) limit "extended table"))
          else if offset < -2 then invalid "a string offset of %d" offset
          else None)
    in
    let names_base =
      let base = ref 0 in
      Array.iteri
        (fun i v ->
          match v with
          | Some v ->
              let offset = short (pos + (i * 2)) in
              base := max !base (offset + String.length v + 1)
          | None -> ())
        values;
      table + !base
    in
    let name j =
      let offset = short (pos + ((ext_strings + j) * 2)) in
      if offset < 0 || names_base + offset >= limit then
        invalid "an extended capability without a name";
      c_string (names_base + offset) limit "extended names"
    in
    for i = 0 to ext_booleans - 1 do
      if s.[booleans_at + i] = '\001' then flags := name i :: !flags
    done;
    for i = 0 to ext_numbers - 1 do
      let v = number (numbers_at + (i * number_size)) in
      if v >= 0 then numbers_ := (name (ext_booleans + i), v) :: !numbers_
    done;
    Array.iteri
      (fun i v ->
        match v with
        | Some v ->
            strings_ := (name (ext_booleans + ext_numbers + i), v) :: !strings_
        | None -> ())
      values
  end;
  let flags = List.rev !flags
  and numbers = List.rev !numbers_
  and strings = List.rev !strings_ in
  let map l = List.fold_left (fun m (k, v) -> Names.add k v m) Names.empty l in
  {
    names;
    flags;
    numbers;
    strings;
    flag_set = map (List.map (fun f -> (f, ())) flags);
    number_map = map numbers;
    string_map = map strings;
  }

let of_string s = try Ok (parse s) with Invalid m -> Error m

(* Capabilities *)

let names t = t.names
let flag t name = Names.mem name t.flag_set
let number t name = Names.find_opt name t.number_map
let string t name = Names.find_opt name t.string_map
let flags t = t.flags
let numbers t = t.numbers
let strings t = t.strings

(* Parameters *)

type param = Int of int | String of string
type vars = int array

let vars () = Array.make 26 0

(* A value on the stack; a number popped where a string is wanted is "",
   and a string where a number is wanted is 0, as in ncurses. *)
type value = Num of int | Str of string

(* C ints: arithmetic wraps at 32 bits, and %o, %x and %X print them
   unsigned. *)
let to_int32 n = Int32.to_int (Int32.of_int n)

(* printf(3)'s flags, width and precision before a conversion, as tparm
   reads them: [#] and space, [-] only after a [:], digits, one [.]. A width
   or precision over 10000 drops them all. *)
type spec = {
  left : bool;
  space : bool;
  alt : bool;
  zero : bool;
  width : int;
  precision : int option;
}

let no_spec =
  {
    left = false;
    space = false;
    alt = false;
    zero = false;
    width = 0;
    precision = None;
  }

let is_digit c = c >= '0' && c <= '9'

(* The spec from [i], just after a [%], and the index of the character that
   ends it. *)
let parse_spec s i =
  let n = String.length s in
  let rec go i spec ~colon ~dot ~value ~digits ~err =
    let finish i =
      let spec =
        if err then no_spec
        else if dot then { spec with precision = Some value }
        else { spec with width = value }
      in
      (i, spec)
    in
    if i >= n then finish i
    else
      match s.[i] with
      | 'c' | 'd' | 'o' | 'x' | 'X' | 's' -> finish i
      | '.' ->
          let spec = if dot then spec else { spec with width = value } in
          go (i + 1) spec ~colon ~dot:true ~value:0 ~digits:false
            ~err:(err || dot)
      | '#' ->
          go (i + 1) { spec with alt = true } ~colon ~dot ~value ~digits ~err
      | ' ' ->
          go (i + 1) { spec with space = true } ~colon ~dot ~value ~digits ~err
      | ':' -> go (i + 1) spec ~colon:true ~dot ~value ~digits ~err
      | '-' when colon ->
          go (i + 1) { spec with left = true } ~colon ~dot ~value ~digits ~err
      | c when is_digit c ->
          (* A 0 that starts the width is printf's zero flag. *)
          let spec =
            if c = '0' && (not digits) && not dot then { spec with zero = true }
            else spec
          in
          let value = (value * 10) + Char.code c - 48 in
          go (i + 1) spec ~colon ~dot ~value ~digits:true
            ~err:(err || value > 10000)
      | _ -> finish i
  in
  go i no_spec ~colon:false ~dot:false ~value:0 ~digits:false ~err:false

let pad buf spec ~prefix body =
  let n = String.length prefix + String.length body in
  let fill = max 0 (spec.width - n) in
  if spec.left then (
    Buffer.add_string buf prefix;
    Buffer.add_string buf body;
    Buffer.add_string buf (String.make fill ' '))
  else if spec.zero && spec.precision = None then (
    Buffer.add_string buf prefix;
    Buffer.add_string buf (String.make fill '0');
    Buffer.add_string buf body)
  else (
    Buffer.add_string buf (String.make fill ' ');
    Buffer.add_string buf prefix;
    Buffer.add_string buf body)

let format_int buf spec conv n =
  let unsigned = n land 0xFFFF_FFFF in
  let digits =
    match conv with
    | 'd' -> string_of_int (abs n)
    | 'o' -> Printf.sprintf "%o" unsigned
    | 'x' -> Printf.sprintf "%x" unsigned
    | _ -> Printf.sprintf "%X" unsigned
  in
  let digits =
    match spec.precision with
    | Some 0 when n = 0 -> ""
    | Some p when String.length digits < p ->
        String.make (p - String.length digits) '0' ^ digits
    | _ -> digits
  in
  let prefix =
    match conv with
    | 'd' -> if n < 0 then "-" else if spec.space then " " else ""
    | 'o' when spec.alt && not (String.starts_with ~prefix:"0" digits) -> "0"
    | 'x' when spec.alt && n <> 0 -> "0x"
    | 'X' when spec.alt && n <> 0 -> "0X"
    | _ -> ""
  in
  pad buf spec ~prefix digits

let format_string buf spec s =
  let s =
    match spec.precision with
    | Some p when p < String.length s -> String.sub s 0 p
    | _ -> s
  in
  pad buf { spec with zero = false } ~prefix:"" s

(* How tparm reads a string with no [%p] code, as termcap wrote them
   ([\E[%d;%dH]): the highest [%p] number, and the parameters that string
   pops, which are pushed for it, at most two. *)
let analyze s =
  let n = String.length s in
  let highest = ref 0 and number = ref 0 in
  let level = ref (-1) and lastpop = ref (-1) in
  let bump () = if !level < 0 && !number < 2 then incr number in
  let rec go i =
    if i < n then
      if s.[i] <> '%' then go (i + 1)
      else
        let i, _ = parse_spec s (i + 1) in
        let next =
          if i >= n then i
          else
            match s.[i] with
            | 'd' | 'o' | 'x' | 'X' | 'c' ->
                if !lastpop <= 0 then bump ();
                decr level;
                lastpop := -1;
                i
            | 'l' | 's' ->
                if !lastpop > 0 then decr level;
                bump ();
                i
            | 'p' ->
                if i + 1 < n && is_digit s.[i + 1] then (
                  incr level;
                  lastpop := Char.code s.[i + 1] - 48;
                  highest := max !highest !lastpop);
                i + 1
            | 'P' -> i + 1
            | 'g' ->
                incr level;
                i + 1
            | '\'' ->
                incr level;
                lastpop := -1;
                i + 2
            | '{' ->
                incr level;
                let rec digits j =
                  if j < n && is_digit s.[j] then digits (j + 1) else j
                in
                digits (i + 1)
            | '+' | '-' | '*' | '/' | 'm' | 'A' | 'O' | '&' | '|' | '^' | '='
            | '<' | '>' ->
                bump ();
                decr level;
                lastpop := -1;
                i
            | '!' | '~' ->
                bump ();
                lastpop := -1;
                i
            | _ -> i
        in
        go (next + 1)
  in
  go 0;
  (!highest, !number)

let expand ?vars:(statics = vars ()) s params =
  let n = String.length s in
  let params =
    Array.init 9 (fun i ->
        match List.nth_opt params i with
        | Some (Int n) -> Num (to_int32 n)
        | Some (String s) -> Str s
        | None -> Num 0)
  in
  let dynamics = Array.make 26 0 in
  let stack = ref (Array.make 16 (Num 0)) and sp = ref 0 in
  let push v =
    if !sp = Array.length !stack then (
      let a = Array.make (2 * !sp) (Num 0) in
      Array.blit !stack 0 a 0 !sp;
      stack := a);
    !stack.(!sp) <- v;
    incr sp
  in
  let pop () =
    if !sp > 0 then (
      decr sp;
      Some !stack.(!sp))
    else None
  in
  let pop_int () = match pop () with Some (Num n) -> n | _ -> 0 in
  let pop_string () = match pop () with Some (Str s) -> s | _ -> "" in
  (* A string with no [%p] has its parameters pushed for it, the first on
     top. *)
  let termcap =
    let highest, number = analyze s in
    highest = 0
    && begin
      for k = number - 1 downto 0 do
        push params.(k)
      done;
      true
    end
  in
  let buf = Buffer.create (String.length s) in
  let incremented = ref false in
  (* From a [%t] that failed, past the [%e] or [%;] that ends its part; from
     an [%e], past the [%;]. *)
  let skip i ~at_else =
    let rec go i level =
      if i >= n then i
      else if s.[i] = '%' then
        if i + 1 >= n then i + 1
        else
          match s.[i + 1] with
          | '?' -> go (i + 2) (level + 1)
          | ';' -> if level = 0 then i + 1 else go (i + 2) (level - 1)
          | 'e' when at_else && level = 0 -> i + 1
          | _ -> go (i + 2) level
      else go (i + 1) level
    in
    go i 0
  in
  let binary f =
    let y = pop_int () in
    let x = pop_int () in
    push (Num (to_int32 (f x y)))
  in
  (* [i] is at a code's last character; the next code starts after it. *)
  let rec go i =
    if i < n then
      if s.[i] <> '%' then (
        Buffer.add_char buf s.[i];
        go (i + 1))
      else
        let i, spec = parse_spec s (i + 1) in
        if i < n then go (code i spec + 1)
  and code i spec =
    let next = i + 1 in
    match s.[i] with
    | '%' ->
        Buffer.add_char buf '%';
        i
    | ('d' | 'o' | 'x' | 'X') as conv ->
        format_int buf spec conv (pop_int ());
        i
    | 'c' ->
        (* A zero is sent as 0200, which a C string can hold. *)
        let v = pop_int () in
        Buffer.add_char buf (Char.chr (if v = 0 then 0o200 else v land 0xFF));
        i
    | 'l' ->
        push (Num (String.length (pop_string ())));
        i
    | 's' ->
        format_string buf spec (pop_string ());
        i
    | 'p' ->
        if next < n && s.[next] >= '1' && s.[next] <= '9' then
          push params.(Char.code s.[next] - 49);
        next
    | ('P' | 'g') as c ->
        (if next < n then
           let v = s.[next] in
           let store =
             if v >= 'a' && v <= 'z' then Some (dynamics, Char.code v - 97)
             else if v >= 'A' && v <= 'Z' then Some (statics, Char.code v - 65)
             else None
           in
           match store with
           | Some (a, k) ->
               if c = 'P' then a.(k) <- pop_int () else push (Num a.(k))
           | None -> ());
        next
    | '\'' ->
        (* The character, then one more, the closing quote. *)
        if next < n then push (Num (Char.code s.[next]));
        next + 1
    | '{' ->
        (* Digits, then one more character, the closing brace. *)
        let rec digits j v =
          if j < n && is_digit s.[j] then
            digits (j + 1) (to_int32 ((v * 10) + Char.code s.[j] - 48))
          else (j, v)
        in
        let j, v = digits next 0 in
        push (Num v);
        j
    | '+' ->
        binary ( + );
        i
    | '-' ->
        binary ( - );
        i
    | '*' ->
        binary ( * );
        i
    | '/' ->
        binary (fun x y -> if y = 0 then 0 else x / y);
        i
    | 'm' ->
        binary (fun x y -> if y = 0 then 0 else x mod y);
        i
    | '&' ->
        binary ( land );
        i
    | '|' ->
        binary ( lor );
        i
    | '^' ->
        binary ( lxor );
        i
    | '=' ->
        binary (fun x y -> Bool.to_int (x = y));
        i
    | '>' ->
        binary (fun x y -> Bool.to_int (x > y));
        i
    | '<' ->
        binary (fun x y -> Bool.to_int (x < y));
        i
    | 'A' ->
        binary (fun x y -> Bool.to_int (x <> 0 && y <> 0));
        i
    | 'O' ->
        binary (fun x y -> Bool.to_int (x <> 0 || y <> 0));
        i
    | '!' ->
        push (Num (Bool.to_int (pop_int () = 0)));
        i
    | '~' ->
        push (Num (to_int32 (lnot (pop_int ()))));
        i
    | 'i' ->
        (* Once, however often it is written. A string with no [%p] gets
           its two first stack slots set too, from the bottom, as ncurses
           sets them. *)
        if not !incremented then (
          incremented := true;
          for k = 0 to 1 do
            match params.(k) with
            | Num v ->
                params.(k) <- Num (to_int32 (v + 1));
                if termcap && k < !sp then !stack.(k) <- params.(k)
            | Str _ -> ()
          done);
        i
    | 't' -> if pop_int () = 0 then skip next ~at_else:true else i
    | 'e' -> skip next ~at_else:false
    | _ -> i
  in
  go 0;
  Buffer.contents buf

(* Padding *)

type piece =
  | Text of string
  | Delay of { ms : float; per_line : bool; mandatory : bool }

let pieces s =
  let n = String.length s in
  let out = ref [] and start = ref 0 in
  let text upto =
    if upto > !start then
      out := Text (String.sub s !start (upto - !start)) :: !out
  in
  (* A delay at [i], "$<" digits, at most one decimal, then "*" and "/" in
     either order, then ">": its end and value, else [None]. *)
  let delay i =
    let rec digits j =
      if j < n && s.[j] >= '0' && s.[j] <= '9' then digits (j + 1) else j
    in
    let j = digits (i + 2) in
    if j = i + 2 then None
    else
      let j, decimal =
        if j < n && s.[j] = '.' then
          let k = digits (j + 1) in
          if k - (j + 1) > 1 then (j, false) else (k, true)
        else (j, true)
      in
      if not decimal then None
      else
        let ms = float_of_string (String.sub s (i + 2) (j - i - 2)) in
        let rec marks j per_line mandatory =
          if j < n && s.[j] = '*' && not per_line then
            marks (j + 1) true mandatory
          else if j < n && s.[j] = '/' && not mandatory then
            marks (j + 1) per_line true
          else (j, per_line, mandatory)
        in
        let j, per_line, mandatory = marks j false false in
        if j < n && s.[j] = '>' then
          Some (j + 1, Delay { ms; per_line; mandatory })
        else None
  in
  let rec go i =
    if i + 1 >= n then ()
    else if s.[i] = '$' && s.[i + 1] = '<' then
      match delay i with
      | Some (j, d) ->
          text i;
          out := d :: !out;
          start := j;
          go j
      | None -> go (i + 1)
    else go (i + 1)
  in
  go 0;
  text n;
  List.rev !out
