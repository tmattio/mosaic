(* A list of standard terminfo directories to search. *)
let terminfo_dirs =
  [
    "/usr/share/terminfo";
    "/lib/terminfo";
    "/etc/terminfo";
    (try Filename.concat (Unix.getenv "HOME") ".terminfo" with Not_found -> "");
  ]
  |> List.filter (fun s -> s <> "")

(* Standard capability names, used to map indices in the file to names. *)
module Caps = struct
  let bool_caps =
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

  let num_caps =
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
      "spinh";
      "spinv";
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
      "OTlC";
      "OTln";
      "OTpC";
      "OTpn";
      "OTsc";
      "OTsn";
      "OTws";
      "OTvr";
      "OTkn";
      "OTmC";
    |]

  let str_caps =
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
      "kEOL";
      "kEOS";
      "kext";
      "kind";
      "kri";
      "kled";
      "kcuf1";
      "kBEG";
      "kCAN";
      "kCMD";
      "kCPY";
      "kCRT";
      "kDC";
      "kDL";
      "kEXT";
      "kFND";
      "kHLP";
      "kMRK";
      "kMOV";
      "kMSG";
      "kPRT";
      "kRDO";
      "kRPL";
      "kRES";
      "kSAV";
      "kSPD";
      "kUND";
      "kPRV";
      "kNXT";
      "kHOM";
      "kIC";
      "kLFT";
      "kEND";
      "kSOH";
      "kSDC";
      "kSDL";
      "kSLF";
      "kSRG";
      "kSTB";
      "kSDE";
      "kSHM";
      "kSIC";
      "kSL";
      "kSR";
      "kSRT";
      "kSGR";
      "kST";
      "kSVS";
      "kDC3";
      "kDC4";
      "kDC5";
      "kDC6";
      "kDC7";
      "kIC3";
      "kIC4";
      "kIC5";
      "kIC6";
      "kIC7";
      "kLFT3";
      "kLFT4";
      "kLFT5";
      "kLFT6";
      "kLFT7";
      "kRIT3";
      "kRIT4";
      "kRIT5";
      "kRIT6";
      "kRIT7";
      "kUP3";
      "kUP4";
      "kUP5";
      "kUP6";
      "kUP7";
      "kDN3";
      "kDN4";
      "kDN5";
      "kDN6";
      "kDN7";
      "kHOM3";
      "kHOM4";
      "kHOM5";
      "kHOM6";
      "kHOM7";
      "kEND3";
      "kEND4";
      "kEND5";
      "kEND6";
      "kEND7";
      "kUP";
      "kDN";
      "kRIT";
      "kPRT";
      "kHLP";
      "kUND";
      "kRDO";
      "kCAN";
      "kmous";
      "mc0";
      "mc4";
      "mc5";
      "meml";
      "memu";
      "op";
      "oc";
      "initc";
      "initp";
      "scp";
      "rcp";
      "setf";
      "setb";
      "setaf";
      "setab";
      "pfx";
      "pfkey";
      "pfloc";
      "pfxl";
      "pln";
      "mc_s1";
      "mc_s2";
      "mc_s3";
      "reqmp";
      "scanc";
      "smpch";
      "smpch_end";
      "rmpch";
      "scs";
      "smsc";
      "smsc_end";
      "rmsc";
      "scr";
      "indn";
      "rin";
      "tsl";
      "wind";
      "ht";
      "hts";
      "mgc";
      "smgl";
      "smgr";
      "smgt";
      "smgb";
      "smgtb";
      "uc";
      "hu";
      "iprog";
      "ka1";
      "ka3";
      "kb2";
      "kc1";
      "kc3";
      "mc_all";
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
      "sgr0";
      "sgr";
      "ssub_char";
      "ssup_char";
      "sub_char";
      "sup_char";
      "dch";
      "dl";
      "ich";
      "il";
      "cud";
      "cuf";
      "cup";
      "cuu";
      "rmup";
      "smup";
      "ind";
      "ri";
      "devt";
      "sgr_end";
      "ssub_char_end";
      "ssup_char_end";
      "el1";
      "u6";
      "u7";
      "u8";
      "u9";
      "u0";
      "u1";
      "u2";
      "u3";
      "u4";
      "u5";
      "acsc";
      "smam";
      "rmam";
      "enacs";
      "kent";
      "cuf";
      "cub";
      "cuu";
      "cud";
      "setcolor";
      "smxon";
      "rmxon";
      "smcup";
      "rmcup";
      "smm";
      "rmm";
      "getm";
      "kUP";
      "kDN";
      "kLFT";
      "kRIT";
      "kEND";
      "kHOM";
      "kIC";
      "kDC";
      "kPRV";
      "kNXT";
      "kcbt";
      "kS";
      "khlp";
      "kprt";
      "kbeg";
      "kcan";
      "kclo";
      "kcmd";
      "kcpy";
      "kcrt";
      "kdl";
      "kslt";
      "kmsg";
      "kmov";
      "knxt";
      "kopn";
      "kopt";
      "kprv";
      "kprt";
      "krdo";
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
      "el";
      "mgc";
      "smgl";
      "smgr";
      "smgt";
      "smgb";
      "smgtb";
      "sclk";
      "dclk";
      "wind";
      "sbim";
      "scsd";
      "rbim";
      "rcsd";
      "subcs";
      "supcs";
      "ext";
      "rep";
      "binel";
      "set_hyperlink";
      "set_spacing";
      "box_chars_1";
    |]
end

(* Terminal capability with its return type encoded in the type parameter *)
type _ cap =
  (* Boolean capabilities *)
  | Auto_left_margin : bool cap
  | Auto_right_margin : bool cap
  | Back_color_erase : bool cap
  | Can_change : bool cap
  | Eat_newline_glitch : bool cap
  | Has_colors : bool cap
  | Has_meta_key : bool cap
  | Insert_null_glitch : bool cap
  | Move_insert_mode : bool cap
  | Move_standout_mode : bool cap
  | Over_strike : bool cap
  | Transparent_underline : bool cap
  | Xon_xoff : bool cap
  (* Numeric capabilities *)
  | Columns : int cap
  | Lines : int cap
  | Max_colors : int cap
  | Max_pairs : int cap
  | Max_attributes : int cap
  | Init_tabs : int cap
  | Virtual_terminal : int cap
  (* String capabilities - non-parameterized *)
  | Bell : string cap
  | Carriage_return : string cap
  | Clear_screen : string cap
  | Clear_to_eol : string cap
  | Clear_to_eos : string cap
  | Cursor_down : string cap
  | Cursor_home : string cap
  | Cursor_invisible : string cap
  | Cursor_left : string cap
  | Cursor_normal : string cap
  | Cursor_right : string cap
  | Cursor_up : string cap
  | Cursor_visible : string cap
  | Delete_character : string cap
  | Delete_line : string cap
  | Enter_alt_charset : string cap
  | Enter_blink_mode : string cap
  | Enter_bold_mode : string cap
  | Enter_dim_mode : string cap
  | Enter_insert_mode : string cap
  | Enter_reverse_mode : string cap
  | Enter_standout_mode : string cap
  | Enter_underline_mode : string cap
  | Exit_alt_charset : string cap
  | Exit_attribute_mode : string cap
  | Exit_insert_mode : string cap
  | Exit_standout_mode : string cap
  | Exit_underline_mode : string cap
  | Flash_screen : string cap
  | Insert_character : string cap
  | Insert_line : string cap
  | Keypad_local : string cap
  | Keypad_xmit : string cap
  | Newline : string cap
  | Reset_1string : string cap
  | Reset_2string : string cap
  | Restore_cursor : string cap
  | Save_cursor : string cap
  | Scroll_forward : string cap
  | Scroll_reverse : string cap
  | Tab : string cap
  (* Parameterized capabilities *)
  | Column_address : (int -> string) cap
  | Cursor_position : (int * int -> string) cap
  | Delete_chars : (int -> string) cap
  | Delete_lines : (int -> string) cap
  | Insert_chars : (int -> string) cap
  | Insert_lines : (int -> string) cap
  | Parm_down_cursor : (int -> string) cap
  | Parm_left_cursor : (int -> string) cap
  | Parm_right_cursor : (int -> string) cap
  | Parm_up_cursor : (int -> string) cap
  | Repeat_char : (char * int -> string) cap
  | Row_address : (int -> string) cap
  | Set_background : (int -> string) cap
  | Set_foreground : (int -> string) cap

type t = {
  bools : (string, bool) Hashtbl.t;
  nums : (string, int) Hashtbl.t;
  strs : (string, string) Hashtbl.t;
}

let create () =
  {
    bools = Hashtbl.create (Array.length Caps.bool_caps);
    nums = Hashtbl.create (Array.length Caps.num_caps);
    strs = Hashtbl.create (Array.length Caps.str_caps);
  }

module Parser = struct
  let get_byte = Bytes.get
  let get_i16_le = Bytes.get_int16_le

  let read_string_at bytes offset =
    let len = ref 0 in
    while get_byte bytes (offset + !len) <> '\x00' do
      incr len
    done;
    Bytes.sub_string bytes offset !len

  let parse bytes =
    let ti = create () in
    let len = Bytes.length bytes in
    if len < 12 then Error "Header too short"
    else
      let magic = get_i16_le bytes 0 in
      if magic <> 0x011a then Error "Invalid magic number"
      else
        let name_size = get_i16_le bytes 2 in
        let bool_count = get_i16_le bytes 4 in
        let num_count = get_i16_le bytes 6 in
        let str_count = get_i16_le bytes 8 in
        let _str_tbl_size = get_i16_le bytes 10 in

        let pos = ref 12 in

        (* Skip names section for now, we don't use it *)
        pos := !pos + name_size;

        (* Bools *)
        for i = 0 to bool_count - 1 do
          if !pos < len then (
            let value = get_byte bytes !pos = '\x01' in
            if value && i < Array.length Caps.bool_caps then
              Hashtbl.add ti.bools Caps.bool_caps.(i) true;
            incr pos)
        done;

        (* Align to short *)
        if !pos mod 2 <> 0 then incr pos;

        (* Numbers *)
        for i = 0 to num_count - 1 do
          if !pos + 1 < len then (
            let value = get_i16_le bytes !pos in
            if value <> -1 && i < Array.length Caps.num_caps then
              Hashtbl.add ti.nums Caps.num_caps.(i) value;
            pos := !pos + 2)
        done;

        (* Strings *)
        let str_table_offset = !pos + (str_count * 2) in
        for i = 0 to str_count - 1 do
          if !pos + 1 < len then
            let offset = get_i16_le bytes !pos in
            if offset <> -1 && i < Array.length Caps.str_caps then (
              let str_val = read_string_at bytes (str_table_offset + offset) in
              Hashtbl.add ti.strs Caps.str_caps.(i) str_val;
              pos := !pos + 2)
        done;
        Ok ti
end

let find_terminfo_file term =
  if String.length term = 0 then None
  else
    let first_char = term.[0] in
    (* Try both single-char and hex-based subdirectories *)
    let subdirs =
      [
        String.sub term 0 1;
        (* e.g., "x" for xterm *)
        Printf.sprintf "%02x" (Char.code first_char);
        (* e.g., "78" for 'x' *)
      ]
    in
    let check_path dir subdir =
      let path = Filename.concat (Filename.concat dir subdir) term in
      if Sys.file_exists path then Some path else None
    in
    let check_in_dir dir = List.find_map (check_path dir) subdirs in
    List.find_map check_in_dir terminfo_dirs

let load ?term () =
  try
    let term_name =
      match term with Some t -> t | None -> Unix.getenv "TERM"
    in
    match find_terminfo_file term_name with
    | None -> Error `Not_found
    | Some path -> (
        let ic = open_in_bin path in
        let len = in_channel_length ic in
        let bytes = Bytes.create len in
        really_input ic bytes 0 len;
        close_in ic;
        match Parser.parse bytes with
        | Ok ti -> Ok ti
        | Error msg -> Error (`Parse_error msg))
  with Not_found -> Error `Not_found

let get_bool ti name = Hashtbl.find_opt ti.bools name
let get_number ti name = Hashtbl.find_opt ti.nums name
let get_string ti name = Hashtbl.find_opt ti.strs name

type param_value = Int of int | Char of char

let parm cap params =
  let out = Buffer.create (String.length cap) in
  let len = String.length cap in
  let i = ref 0 in
  let stack = Stack.create () in
  let dyn_vars = Array.make 26 0 in
  let static_vars = Array.make 26 0 in

  let get_param n =
    try match List.nth params (n - 1) with Int v -> v | Char c -> Char.code c
    with _ -> 0
  in
  let pop () = try Stack.pop stack with Stack.Empty -> 0 in
  let push v = Stack.push v stack in

  while !i < len do
    if cap.[!i] <> '%' then Buffer.add_char out cap.[!i]
    else (
      incr i;
      if !i < len then
        match cap.[!i] with
        | '%' -> Buffer.add_char out '%'
        | 'c' -> Buffer.add_char out (Char.chr (pop ()))
        | 's' ->
            Buffer.add_string out (string_of_int (pop ()))
            (* Terminfo doesn't have string stack items *)
        | 'd' -> Buffer.add_string out (string_of_int (pop ()))
        | 'o' -> Buffer.add_string out (Printf.sprintf "%o" (pop ()))
        | 'x' -> Buffer.add_string out (Printf.sprintf "%x" (pop ()))
        | 'X' -> Buffer.add_string out (Printf.sprintf "%X" (pop ()))
        | 'i' ->
            (* Increment first two parameters by 1 (1-based indexing) *)
            ()
        | 'p' ->
            incr i;
            if !i < len then
              let n = Char.code cap.[!i] - Char.code '0' in
              push (get_param n)
        | 'l' -> push (String.length (string_of_int (pop ())))
        | '+' ->
            let v2, v1 = (pop (), pop ()) in
            push (v1 + v2)
        | '-' ->
            let v2, v1 = (pop (), pop ()) in
            push (v1 - v2)
        | '*' ->
            let v2, v1 = (pop (), pop ()) in
            push (v1 * v2)
        | '/' ->
            let v2, v1 = (pop (), pop ()) in
            if v2 <> 0 then push (v1 / v2) else push 0
        | 'm' ->
            let v2, v1 = (pop (), pop ()) in
            if v2 <> 0 then push (v1 mod v2) else push 0
        | '&' ->
            let v2, v1 = (pop (), pop ()) in
            push (v1 land v2)
        | '|' ->
            let v2, v1 = (pop (), pop ()) in
            push (v1 lor v2)
        | '^' ->
            let v2, v1 = (pop (), pop ()) in
            push (v1 lxor v2)
        | '=' ->
            let v2, v1 = (pop (), pop ()) in
            push (if v1 = v2 then 1 else 0)
        | '>' ->
            let v2, v1 = (pop (), pop ()) in
            push (if v1 > v2 then 1 else 0)
        | '<' ->
            let v2, v1 = (pop (), pop ()) in
            push (if v1 < v2 then 1 else 0)
        | '!' -> push (if pop () = 0 then 1 else 0)
        | '~' -> push (lnot (pop ()))
        | '?' -> () (* Start of if, handled by %t and %e *)
        | 't' ->
            if pop () = 0 then
              let level = ref 1 in
              while !level > 0 && !i < len - 1 do
                incr i;
                if !i < len - 1 && cap.[!i] = '%' then
                  match cap.[!i + 1] with
                  | '?' -> incr level
                  | ';' -> decr level
                  | 'e' when !level = 1 ->
                      level := 0;
                      decr i (* stop before else *)
                  | _ -> ()
              done
        | 'e' ->
            let level = ref 1 in
            while !level > 0 && !i < len - 1 do
              incr i;
              if !i < len - 1 && cap.[!i] = '%' then
                match cap.[!i + 1] with
                | '?' -> incr level
                | ';' -> decr level
                | _ -> ()
            done
        | ';' -> () (* End of if *)
        | 'P' ->
            incr i;
            let var = Char.code cap.[!i] in
            if var >= Char.code 'a' && var <= Char.code 'z' then
              dyn_vars.(var - Char.code 'a') <- pop ()
            else if var >= Char.code 'A' && var <= Char.code 'Z' then
              static_vars.(var - Char.code 'A') <- pop ()
        | 'g' ->
            incr i;
            let var = Char.code cap.[!i] in
            if var >= Char.code 'a' && var <= Char.code 'z' then
              push dyn_vars.(var - Char.code 'a')
            else if var >= Char.code 'A' && var <= Char.code 'Z' then
              push static_vars.(var - Char.code 'A')
        | _ -> () (* Ignore unknown format specifiers *));
    incr i
  done;
  Buffer.contents out

(* The main get function with GADT pattern matching *)
let get : type a. t -> a cap -> a option =
 fun ti cap ->
  match cap with
  (* Boolean capabilities *)
  | Auto_left_margin -> get_bool ti "bw"
  | Auto_right_margin -> get_bool ti "am"
  | Back_color_erase -> get_bool ti "bce"
  | Can_change -> get_bool ti "ccc"
  | Eat_newline_glitch -> get_bool ti "xenl"
  | Has_colors -> (
      (* Special case: check if colors > 0 *)
      match get_number ti "colors" with
      | Some n when n > 0 -> Some true
      | _ -> Some false)
  | Has_meta_key -> get_bool ti "km"
  | Insert_null_glitch -> get_bool ti "in"
  | Move_insert_mode -> get_bool ti "mir"
  | Move_standout_mode -> get_bool ti "msgr"
  | Over_strike -> get_bool ti "os"
  | Transparent_underline -> get_bool ti "ul"
  | Xon_xoff -> get_bool ti "xon"
  (* Numeric capabilities *)
  | Columns -> get_number ti "cols"
  | Lines -> get_number ti "lines"
  | Max_colors -> get_number ti "colors"
  | Max_pairs -> get_number ti "pairs"
  | Max_attributes -> get_number ti "ma"
  | Init_tabs -> get_number ti "it"
  | Virtual_terminal -> get_number ti "vt"
  (* String capabilities - non-parameterized *)
  | Bell -> get_string ti "bel"
  | Carriage_return -> get_string ti "cr"
  | Clear_screen -> get_string ti "clear"
  | Clear_to_eol -> get_string ti "el"
  | Clear_to_eos -> get_string ti "ed"
  | Cursor_down -> get_string ti "cud1"
  | Cursor_home -> get_string ti "home"
  | Cursor_invisible -> get_string ti "civis"
  | Cursor_left -> get_string ti "cub1"
  | Cursor_normal -> get_string ti "cnorm"
  | Cursor_right -> get_string ti "cuf1"
  | Cursor_up -> get_string ti "cuu1"
  | Cursor_visible -> get_string ti "cvvis"
  | Delete_character -> get_string ti "dch1"
  | Delete_line -> get_string ti "dl1"
  | Enter_alt_charset -> get_string ti "smacs"
  | Enter_blink_mode -> get_string ti "blink"
  | Enter_bold_mode -> get_string ti "bold"
  | Enter_dim_mode -> get_string ti "dim"
  | Enter_insert_mode -> get_string ti "smir"
  | Enter_reverse_mode -> get_string ti "rev"
  | Enter_standout_mode -> get_string ti "smso"
  | Enter_underline_mode -> get_string ti "smul"
  | Exit_alt_charset -> get_string ti "rmacs"
  | Exit_attribute_mode -> get_string ti "sgr0"
  | Exit_insert_mode -> get_string ti "rmir"
  | Exit_standout_mode -> get_string ti "rmso"
  | Exit_underline_mode -> get_string ti "rmul"
  | Flash_screen -> get_string ti "flash"
  | Insert_character -> get_string ti "ich1"
  | Insert_line -> get_string ti "il1"
  | Keypad_local -> get_string ti "rmkx"
  | Keypad_xmit -> get_string ti "smkx"
  | Newline -> get_string ti "nel"
  | Reset_1string -> get_string ti "rs1"
  | Reset_2string -> get_string ti "rs2"
  | Restore_cursor -> get_string ti "rc"
  | Save_cursor -> get_string ti "sc"
  | Scroll_forward -> get_string ti "ind"
  | Scroll_reverse -> get_string ti "ri"
  | Tab -> get_string ti "ht"
  (* Parameterized capabilities *)
  | Column_address -> (
      match get_string ti "hpa" with
      | Some fmt -> Some (fun col -> parm fmt [ Int col ])
      | None -> None)
  | Cursor_position -> (
      match get_string ti "cup" with
      | Some fmt ->
          Some
            (fun (row, col) ->
              (* cup uses 0-based coordinates but some terminals expect 1-based *)
              let fmt_has_incr = String.contains fmt 'i' in
              if fmt_has_incr then parm fmt [ Int row; Int col ]
              else parm fmt [ Int row; Int col ])
      | None -> None)
  | Delete_chars -> (
      match get_string ti "dch" with
      | Some fmt -> Some (fun n -> parm fmt [ Int n ])
      | None -> None)
  | Delete_lines -> (
      match get_string ti "dl" with
      | Some fmt -> Some (fun n -> parm fmt [ Int n ])
      | None -> None)
  | Insert_chars -> (
      match get_string ti "ich" with
      | Some fmt -> Some (fun n -> parm fmt [ Int n ])
      | None -> None)
  | Insert_lines -> (
      match get_string ti "il" with
      | Some fmt -> Some (fun n -> parm fmt [ Int n ])
      | None -> None)
  | Parm_down_cursor -> (
      match get_string ti "cud" with
      | Some fmt -> Some (fun n -> parm fmt [ Int n ])
      | None -> None)
  | Parm_left_cursor -> (
      match get_string ti "cub" with
      | Some fmt -> Some (fun n -> parm fmt [ Int n ])
      | None -> None)
  | Parm_right_cursor -> (
      match get_string ti "cuf" with
      | Some fmt -> Some (fun n -> parm fmt [ Int n ])
      | None -> None)
  | Parm_up_cursor -> (
      match get_string ti "cuu" with
      | Some fmt -> Some (fun n -> parm fmt [ Int n ])
      | None -> None)
  | Repeat_char -> (
      match get_string ti "rep" with
      | Some fmt -> Some (fun (ch, n) -> parm fmt [ Char ch; Int n ])
      | None -> None)
  | Row_address -> (
      match get_string ti "vpa" with
      | Some fmt -> Some (fun row -> parm fmt [ Int row ])
      | None -> None)
  | Set_background -> (
      match get_string ti "setab" with
      | Some fmt -> Some (fun color -> parm fmt [ Int color ])
      | None -> None)
  | Set_foreground -> (
      match get_string ti "setaf" with
      | Some fmt -> Some (fun color -> parm fmt [ Int color ])
      | None -> None)
