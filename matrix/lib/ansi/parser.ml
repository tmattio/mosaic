open Internal

let max_escape_length = 20 (* matches historical mobi‑CI code *)

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
  | OSC of int * string
  | Hyperlink of ((string * string) list * string) option  (** OSC 8 *)
  | Reset  (** ESC c (RIS) *)
  | Unknown of string

type token = Text of string | SGR of attr list | Control of control

let is_final_byte c =
  let code = Char.code c in
  code >= 0x40 && code <= 0x7e

let int_of_string_opt s =
  match int_of_string s with v -> Some v | exception _ -> None

let utf8_len b =
  let n = Char.code b in
  if n < 0x80 then 1
  else if n < 0xC0 then 1
  else if n < 0xE0 then 2
  else if n < 0xF0 then 3
  else if n < 0xF8 then 4
  else 1

let parse_sgr_params params =
  let acc = ref [] in
  let push x = acc := x :: !acc in
  let rec loop i =
    if i >= Array.length params then ()
    else
      match params.(i) with
      | 0 ->
          push `Reset;
          loop (i + 1)
      | 1 ->
          push `Bold;
          loop (i + 1)
      | 2 ->
          push `Dim;
          loop (i + 1)
      | 3 ->
          push `Italic;
          loop (i + 1)
      | 4 ->
          push `Underline;
          loop (i + 1)
      | 21 ->
          push `Double_underline;
          loop (i + 1)
      | 5 ->
          push `Blink;
          loop (i + 1)
      | 7 ->
          push `Reverse;
          loop (i + 1)
      | 8 ->
          push `Conceal;
          loop (i + 1)
      | 9 ->
          push `Strikethrough;
          loop (i + 1)
      | 53 ->
          push `Overline;
          loop (i + 1)
      | 51 ->
          push `Framed;
          loop (i + 1)
      | 52 ->
          push `Encircled;
          loop (i + 1)
      | 22 | 23 | 24 | 25 | 27 | 28 | 29 | 55 | 54 ->
          push `Reset;
          loop (i + 1)
      | n when (30 <= n && n <= 37) || (90 <= n && n <= 97) ->
          let idx = if n < 90 then n - 30 else n - 90 + 8 in
          push (`Fg (Index idx));
          loop (i + 1)
      | n when (40 <= n && n <= 47) || (100 <= n && n <= 107) ->
          let idx = if n < 100 then n - 40 else n - 100 + 8 in
          push (`Bg (Index idx));
          loop (i + 1)
      | (38 | 48) as first -> (
          let bg = first = 48 in
          if i + 1 < Array.length params then
            match params.(i + 1) with
            | 5 when i + 2 < Array.length params ->
                push
                  (if bg then `Bg (Index params.(i + 2))
                   else `Fg (Index params.(i + 2)));
                loop (i + 3)
            | 2 when i + 4 < Array.length params ->
                let r, g, b =
                  (params.(i + 2), params.(i + 3), params.(i + 4))
                in
                push (if bg then `Bg (RGB (r, g, b)) else `Fg (RGB (r, g, b)));
                loop (i + 5)
            | _ -> loop (i + 1))
      | _ -> loop (i + 1)
  in
  loop 0;
  List.rev !acc

let parse_csi body final : token option =
  let ints =
    if body = "" then [||]
    else
      Array.of_list
        (List.filter_map int_of_string_opt (String.split_on_char ';' body))
  in
  let get n = if n < Array.length ints then ints.(n) else 1 in
  let get_default default n = if n < Array.length ints then ints.(n) else default in
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
  | 'm' -> Some (SGR (parse_sgr_params ints))
  | _ -> Some (Control (Unknown (Printf.sprintf "CSI[%s%c" body final)))

let parse_hyperlink data : control option =
  let parts = String.split_on_char ';' data in
  match parts with
  | [ ""; "" ] -> Some (Hyperlink None) (* close tag *)
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
  mutable st : state;
}

let create () =
  { buf = Bytes.create 4096; len = 0; text = Buffer.create 128; st = Normal }

let reset p =
  p.len <- 0;
  p.st <- Normal;
  Buffer.clear p.text

let pending p = Bytes.sub p.buf 0 p.len

let flush_text p acc =
  if Buffer.length p.text = 0 then acc
  else
    let t = Text (Buffer.contents p.text) in
    Buffer.clear p.text;
    t :: acc

let overshoot buf = Buffer.length buf > max_escape_length

let feed p (src : bytes) off len : token list =
  (* grow buffer if necessary *)
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
          let b = Bytes.get p.buf pos in
          if b = '\x1b' then (
            let tokens = flush_text p tokens in
            p.st <- Esc;
            loop tokens (pos + 1))
          else
            let need = utf8_len b in
            if pos + need > p.len then (tokens, pos)
            else (
              Buffer.add_subbytes p.text p.buf pos need;
              loop tokens (pos + need))
      | Esc -> (
          if pos >= p.len then (tokens, pos)
          else
            let c = Bytes.get p.buf pos in
            match c with
            | '[' ->
                p.st <- Csi (Buffer.create 32);
                loop tokens (pos + 1)
            | ']' ->
                p.st <- Osc (Buffer.create 64);
                loop tokens (pos + 1)
            | 'c' ->
                (*   ESC c : RIS   *)
                let tokens = Control Reset :: flush_text p tokens in
                p.st <- Normal;
                loop tokens (pos + 1)
            | _ ->
                Buffer.add_char p.text '\x1b';
                p.st <- Normal;
                loop tokens pos)
      | Csi buf ->
          if pos >= p.len then (tokens, pos)
          else
            let c = Bytes.get p.buf pos in
            Buffer.add_char buf c;
            if overshoot buf then (
              let tokens = Control (Unknown "ESCAPE_TOO_LONG") :: tokens in
              Buffer.clear buf;
              p.st <- Normal;
              loop tokens (pos + 1))
            else if is_final_byte c then (
              let body = Buffer.sub buf 0 (Buffer.length buf - 1) in
              let tok =
                Option.value ~default:(Control (Unknown "")) (parse_csi body c)
              in
              Buffer.clear buf;
              p.st <- Normal;
              loop (tok :: tokens) (pos + 1))
            else loop tokens (pos + 1)
      | Osc buf ->
          if pos >= p.len then (tokens, pos)
          else
            let c = Bytes.get p.buf pos in
            if c = '\x07' then (* BEL terminator *)
              (
              let tok = parse_osc (Buffer.contents buf) in
              Buffer.clear buf;
              p.st <- Normal;
              loop (tok :: tokens) (pos + 1))
            else if c = '\x1b' then (
              p.st <- Osc_esc buf;
              loop tokens (pos + 1))
            else (
              Buffer.add_char buf c;
              if overshoot buf then (
                let tok = Control (Unknown "ESCAPE_TOO_LONG") in
                Buffer.clear buf;
                p.st <- Normal;
                loop (tok :: tokens) (pos + 1))
              else loop tokens (pos + 1))
      | Osc_esc buf ->
          if pos >= p.len then (tokens, pos)
          else
            let c = Bytes.get p.buf pos in
            if c = '\\' then (* ST terminator *)
              (
              let tok = parse_osc (Buffer.contents buf) in
              Buffer.clear buf;
              p.st <- Normal;
              loop (tok :: tokens) (pos + 1))
            else (
              Buffer.add_char buf '\x1b';
              p.st <- Osc buf;
              loop tokens pos)
  in
  let tokens, consumed = loop [] 0 in
  if consumed > 0 then (
    let rem = p.len - consumed in
    if rem > 0 then Bytes.blit p.buf consumed p.buf 0 rem;
    p.len <- rem);
  List.rev (flush_text p tokens)

let parse s =
  let p = create () in
  let t1 = feed p (Bytes.of_string s) 0 (String.length s) in
  t1 @ feed p Bytes.empty 0 0
