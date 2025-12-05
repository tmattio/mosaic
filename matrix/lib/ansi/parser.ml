let max_escape_length = 256
let max_osc_length = 8192

type control =
  | CUU of int
  | CUD of int
  | CUF of int
  | CUB of int
  | CNL of int
  | CPL of int
  | CHA of int
  | VPA of int
  | CUP of int * int
  | ED of int
  | EL of int
  | IL of int
  | DL of int
  | DCH of int
  | ICH of int
  | OSC of int * string
  | Hyperlink of ((string * string) list * string) option
  | Reset
  | DECSC
  | DECRC
  | Unknown of string

type sgr_attr =
  [ `Reset
  | `Bold
  | `Dim
  | `Italic
  | `Underline
  | `Double_underline
  | `Blink
  | `Reverse
  | `Conceal
  | `Strikethrough
  | `Overline
  | `Framed
  | `Encircled
  | `No_bold
  | `No_dim
  | `No_italic
  | `No_underline
  | `No_blink
  | `No_reverse
  | `No_conceal
  | `No_strikethrough
  | `No_overline
  | `No_framed
  | `No_encircled
  | `Fg of Color.t
  | `Bg of Color.t ]

type token = Text of string | SGR of sgr_attr list | Control of control

let is_final_byte c =
  let code = Char.code c in
  code >= 0x40 && code <= 0x7e

let int_of_string_opt s =
  match int_of_string s with v -> Some v | exception _ -> None

let sgr_code_to_color n =
  match n with
  | 0 -> Color.black
  | 1 -> Color.red
  | 2 -> Color.green
  | 3 -> Color.yellow
  | 4 -> Color.blue
  | 5 -> Color.magenta
  | 6 -> Color.cyan
  | 7 -> Color.white
  | 8 -> Color.bright_black
  | 9 -> Color.bright_red
  | 10 -> Color.bright_green
  | 11 -> Color.bright_yellow
  | 12 -> Color.bright_blue
  | 13 -> Color.bright_magenta
  | 14 -> Color.bright_cyan
  | 15 -> Color.bright_white
  | _ -> Color.default

let parse_sgr_params params =
  let acc = ref [] in
  let push x = acc := x :: !acc in
  let i = ref 0 in
  while !i < Array.length params do
    (match params.(!i) with
    | 0 -> push `Reset
    | 1 -> push `Bold
    | 2 -> push `Dim
    | 3 -> push `Italic
    | 4 -> push `Underline
    | 21 -> push `Double_underline
    | 5 -> push `Blink
    | 7 -> push `Reverse
    | 8 -> push `Conceal
    | 9 -> push `Strikethrough
    | 53 -> push `Overline
    | 51 -> push `Framed
    | 52 -> push `Encircled
    | 22 ->
        push `No_bold;
        push `No_dim
    | 23 -> push `No_italic
    | 24 -> push `No_underline
    | 25 -> push `No_blink
    | 27 -> push `No_reverse
    | 28 -> push `No_conceal
    | 29 -> push `No_strikethrough
    | 54 ->
        push `No_framed;
        push `No_encircled
    | 55 -> push `No_overline
    | 39 -> push (`Fg Color.default)
    | 49 -> push (`Bg Color.default)
    | n when 30 <= n && n <= 37 -> push (`Fg (sgr_code_to_color (n - 30)))
    | n when 90 <= n && n <= 97 -> push (`Fg (sgr_code_to_color (n - 90 + 8)))
    | n when 40 <= n && n <= 47 -> push (`Bg (sgr_code_to_color (n - 40)))
    | n when 100 <= n && n <= 107 ->
        push (`Bg (sgr_code_to_color (n - 100 + 8)))
    | (38 | 48) as first -> (
        let bg = first = 48 in
        if !i + 1 < Array.length params then
          match params.(!i + 1) with
          | 5 when !i + 2 < Array.length params ->
              let idx = params.(!i + 2) in
              push
                (if bg then `Bg (Color.of_palette_index idx)
                 else `Fg (Color.of_palette_index idx));
              i := !i + 2
          | 2 when !i + 4 < Array.length params ->
              let r = params.(!i + 2) in
              let g = params.(!i + 3) in
              let b = params.(!i + 4) in
              push
                (if bg then `Bg (Color.of_rgb r g b)
                 else `Fg (Color.of_rgb r g b));
              i := !i + 4
          | _ -> ())
    | _ -> ());
    incr i
  done;
  List.rev !acc

let parse_csi body final : token option =
  let len = String.length body in
  let params = Array.make 16 (-1) in
  (* Fixed small array for parameters *)
  let param_count = ref 0 in
  let rec parse_param i current =
    if i = len then (
      if !param_count < 16 then params.(!param_count) <- current;
      incr param_count)
    else
      match body.[i] with
      | ';' ->
          if !param_count < 16 then params.(!param_count) <- current;
          incr param_count;
          parse_param (i + 1) 0
      | '0' .. '9' as c ->
          parse_param (i + 1) ((current * 10) + (Char.code c - 48))
      | _ -> () (* Skip invalid characters *)
  in
  if len > 0 then parse_param 0 0;

  (* Convert -1 to 0 for compatibility with existing code *)
  for i = 0 to !param_count - 1 do
    if params.(i) = -1 then params.(i) <- 0
  done;

  let get n = if n < !param_count then params.(n) else 1 in
  let get_default default n =
    if n < !param_count then params.(n) else default
  in
  match final with
  | 'A' -> Some (Control (CUU (get 0)))
  | 'B' -> Some (Control (CUD (get 0)))
  | 'C' -> Some (Control (CUF (get 0)))
  | 'D' -> Some (Control (CUB (get 0)))
  | 'E' -> Some (Control (CNL (get 0)))
  | 'F' -> Some (Control (CPL (get 0)))
  | 'G' -> Some (Control (CHA (get 0)))
  | 'd' -> Some (Control (VPA (get 0)))
  | 'H' | 'f' -> Some (Control (CUP (get 0, get 1)))
  | 'J' -> Some (Control (ED (get_default 0 0)))
  | 'K' -> Some (Control (EL (get_default 0 0)))
  | 'L' -> Some (Control (IL (get 0)))
  | 'M' -> Some (Control (DL (get 0)))
  | 'P' -> Some (Control (DCH (get 0)))
  | '@' -> Some (Control (ICH (get 0)))
  | 'm' ->
      let params_arr = Array.sub params 0 !param_count in
      let attrs =
        if !param_count = 0 then [ `Reset ] else parse_sgr_params params_arr
      in
      Some (SGR attrs)
  | _ -> Some (Control (Unknown (Printf.sprintf "CSI[%s%c" body final)))

let parse_hyperlink data : control option =
  let rec find_semicolon i =
    if i >= String.length data then -1
    else if data.[i] = ';' then i
    else find_semicolon (i + 1)
  in
  let parts =
    match find_semicolon 0 with
    | -1 -> [ data ]
    | idx1 -> (
        let part1 = String.sub data 0 idx1 in
        match find_semicolon (idx1 + 1) with
        | -1 ->
            [
              part1; String.sub data (idx1 + 1) (String.length data - idx1 - 1);
            ]
        | _ -> String.split_on_char ';' data)
  in
  match parts with
  | [ ""; "" ] -> Some (Hyperlink None)
  | [ ""; link ] -> Some (Hyperlink (Some ([], link)))
  | [ params; link ] ->
      let kv_pairs =
        params |> String.split_on_char ':'
        |> List.filter_map (fun s ->
            match String.split_on_char '=' s with
            | [ k; v ] -> Some (k, v)
            | _ -> None)
      in
      Some (Hyperlink (Some (kv_pairs, link)))
  | _ -> None

let parse_osc body : token =
  match String.index_opt body ';' with
  | None -> Control (Unknown ("OSC " ^ body))
  | Some semi -> (
      let code_str = String.sub body 0 semi in
      let data = String.sub body (semi + 1) (String.length body - semi - 1) in
      match int_of_string_opt code_str with
      | Some 8 -> (
          match parse_hyperlink data with
          | Some c -> Control c
          | None -> Control (Unknown ("OSC8 " ^ data)))
      | Some code -> Control (OSC (code, data))
      | None -> Control (Unknown ("OSC " ^ body)))

type state =
  | Normal
  | Esc
  | Csi of Buffer.t
  | Osc of Buffer.t
  | Osc_esc of Buffer.t

type t = {
  mutable buf : bytes;
  mutable len : int;
  text : Buffer.t;
  decoded_text : Buffer.t;
  mutable decoder : Uutf.decoder;
  mutable st : state;
  csi_buf : Buffer.t;
  osc_buf : Buffer.t;
}

let create_decoder () = Uutf.decoder ~encoding:`UTF_8 `Manual

let create () =
  {
    buf = Bytes.create 4096;
    len = 0;
    text = Buffer.create 128;
    decoded_text = Buffer.create 128;
    decoder = create_decoder ();
    st = Normal;
    csi_buf = Buffer.create 32;
    osc_buf = Buffer.create 64;
  }

let reset p =
  p.len <- 0;
  p.st <- Normal;
  Buffer.clear p.text;
  Buffer.clear p.decoded_text;
  p.decoder <- create_decoder ()

let pending p = Bytes.sub p.buf 0 p.len

let rec drain_decoder p =
  match Uutf.decode p.decoder with
  | `Uchar u ->
      Uutf.Buffer.add_utf_8 p.decoded_text u;
      drain_decoder p
  | `Malformed bytes ->
      for i = 0 to String.length bytes - 1 do
        let code = Char.code bytes.[i] in
        Uutf.Buffer.add_utf_8 p.decoded_text (Uchar.of_int code)
      done;
      drain_decoder p
  | `Await | `End -> ()

let decode_pending_text ?(eof = false) p =
  if Buffer.length p.text > 0 then (
    let raw = Buffer.contents p.text in
    Buffer.clear p.text;
    Uutf.Manual.src p.decoder (Bytes.unsafe_of_string raw) 0 (String.length raw);
    drain_decoder p);
  if eof then (
    Uutf.Manual.src p.decoder Bytes.empty 0 0;
    drain_decoder p)

let flush_decoded_text p acc =
  if Buffer.length p.decoded_text = 0 then acc
  else
    let t = Text (Buffer.contents p.decoded_text) in
    Buffer.clear p.decoded_text;
    t :: acc

let flush_text ?(eof = false) p acc =
  decode_pending_text ~eof p;
  flush_decoded_text p acc

let overshoot_csi buf = Buffer.length buf > max_escape_length
let overshoot_osc buf = Buffer.length buf > max_osc_length

exception Stop of token list

let feed p (src : bytes) off len : token list =
  let eof = len = 0 in
  if p.len + len > Bytes.length p.buf then (
    let nbuf = Bytes.create (max (p.len + len) (2 * Bytes.length p.buf)) in
    Bytes.blit p.buf 0 nbuf 0 p.len;
    p.buf <- nbuf);
  Bytes.blit src off p.buf p.len len;
  p.len <- p.len + len;

  let rec loop tokens pos =
    if pos >= p.len then (tokens, pos)
    else
      match p.st with
      | Normal ->
          let rec scan_text start =
            if start >= p.len then start
            else if Bytes.get p.buf start = '\x1b' then start
            else scan_text (start + 1)
          in
          let next_esc = scan_text pos in
          if next_esc > pos then (
            Buffer.add_subbytes p.text p.buf pos (next_esc - pos);
            if next_esc < p.len then (
              let tokens = flush_text p tokens in
              p.st <- Esc;
              loop tokens (next_esc + 1))
            else (tokens, next_esc))
          else if next_esc = pos && pos < p.len then (
            let tokens = flush_text p tokens in
            p.st <- Esc;
            loop tokens (pos + 1))
          else (tokens, pos)
      | Esc -> (
          if pos >= p.len then (tokens, pos)
          else
            let c = Bytes.get p.buf pos in
            match c with
            | '[' ->
                Buffer.clear p.csi_buf;
                p.st <- Csi p.csi_buf;
                loop tokens (pos + 1)
            | ']' ->
                Buffer.clear p.osc_buf;
                p.st <- Osc p.osc_buf;
                loop tokens (pos + 1)
            | 'c' ->
                let tokens = Control Reset :: flush_text p tokens in
                p.st <- Normal;
                loop tokens (pos + 1)
            | '7' ->
                let tokens = Control DECSC :: flush_text p tokens in
                p.st <- Normal;
                loop tokens (pos + 1)
            | '8' ->
                let tokens = Control DECRC :: flush_text p tokens in
                p.st <- Normal;
                loop tokens (pos + 1)
            | '%' ->
                if pos + 1 < p.len then (
                  p.st <- Normal;
                  loop tokens (pos + 2))
                else (tokens, pos - 1)
            | '(' | ')' | '*' | '+' ->
                if pos + 1 < p.len then (
                  p.st <- Normal;
                  loop tokens (pos + 2))
                else (tokens, pos - 1)
            | _ ->
                Buffer.add_char p.text '\x1b';
                p.st <- Normal;
                loop tokens pos)
      | Csi buf ->
          if pos >= p.len then (tokens, pos)
          else
            let c = Bytes.get p.buf pos in
            Buffer.add_char buf c;
            if overshoot_csi buf then (
              Buffer.clear buf;
              p.st <- Normal;
              Buffer.clear p.text;
              Buffer.clear p.decoded_text;
              p.decoder <- create_decoder ();
              p.len <- 0;
              raise (Stop (Control (Unknown "ESCAPE_TOO_LONG") :: tokens)))
            else if is_final_byte c then (
              let body = Buffer.sub buf 0 (Buffer.length buf - 1) in
              let tok =
                Option.value ~default:(Control (Unknown "")) (parse_csi body c)
              in
              p.st <- Normal;
              loop (tok :: tokens) (pos + 1))
            else loop tokens (pos + 1)
      | Osc buf ->
          if pos >= p.len then (tokens, pos)
          else
            let c = Bytes.get p.buf pos in
            if c = '\x07' then (
              let tok = parse_osc (Buffer.contents buf) in
              p.st <- Normal;
              loop (tok :: tokens) (pos + 1))
            else if c = '\x1b' then (
              p.st <- Osc_esc buf;
              loop tokens (pos + 1))
            else (
              Buffer.add_char buf c;
              if overshoot_osc buf then (
                let tok = Control (Unknown "ESCAPE_TOO_LONG") in
                p.st <- Normal;
                loop (tok :: tokens) (pos + 1))
              else loop tokens (pos + 1))
      | Osc_esc buf ->
          if pos >= p.len then (tokens, pos)
          else
            let c = Bytes.get p.buf pos in
            if c = '\\' then (
              let tok = parse_osc (Buffer.contents buf) in
              p.st <- Normal;
              loop (tok :: tokens) (pos + 1))
            else (
              Buffer.add_char buf '\x1b';
              p.st <- Osc buf;
              loop tokens pos)
  in
  let tokens, consumed = try loop [] 0 with Stop tokens -> (tokens, p.len) in
  if consumed > 0 then (
    let rem = p.len - consumed in
    if rem > 0 then Bytes.blit p.buf consumed p.buf 0 rem;
    p.len <- rem);
  List.rev (flush_text ~eof p tokens)

let parse s =
  let p = create () in
  let t1 = feed p (Bytes.of_string s) 0 (String.length s) in
  t1 @ feed p Bytes.empty 0 0
