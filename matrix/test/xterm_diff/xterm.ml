(* The xterm side, run as the program xterm executes ([xterm -e]): the
   process's standard input and output are xterm's pty.

   One xterm runs every case. For each case the driver writes, in one write:

   - RIS, returning xterm to its power-up state;
   - the case and the trailer [ESC \] (ST), which ends any sequence or
     string the case left open (both sides receive the trailer; CAN would not
     do, since xterm, as a VT100, shows it as a checkerboard);
   - CPR ([CSI 6 n]): the cursor after the case;
   - media copy ([CSI i]): xterm prints the screen into the FIFO named by
     [printerCommand], ending with a form feed;
   - [#] then CPR again: where the next character goes, which tells a pending
     wrap (the last column flag) from none;
   - XTVERSION ([CSI > 0 q]), whose reply ends the case's replies: the
     generator never produces it.

   xterm processes its input in order, so when the XTVERSION reply arrives the
   print has been written. *)

type t = {
  tty_in : Unix.file_descr;
  tty_out : Unix.file_descr;
  fifo : Unix.file_descr;
  tty_pending : Buffer.t;  (** Bytes read past the last terminator. *)
  fifo_pending : Buffer.t;
}

let trailer = "\027\\"
let probe = "\027[6n\027[i#\027[6n\027[>0q"
let terminator_prefix = "\027P>|"

exception Timeout of string

(* [XDIFF_TRACE=FILE] logs every write and read. (xterm removes [XTERM_*]
   variables from its child's environment.) *)
let trace =
  Option.map
    (fun f -> open_out_gen [ Open_wronly; Open_append; Open_creat ] 0o644 f)
    (Sys.getenv_opt "XDIFF_TRACE")

let tracef fmt =
  match trace with
  | Some oc -> Printf.kfprintf (fun oc -> flush oc) oc fmt
  | None -> Printf.ifprintf stdout fmt

let write_all fd s =
  tracef "%.4f write %s\n" (Unix.gettimeofday ()) (Screen.escape s);
  let b = Bytes.unsafe_of_string s in
  let rec go off =
    if off < Bytes.length b then
      go (off + Unix.write fd b off (Bytes.length b - off))
  in
  go 0

(* Reads from [fd] until [until buf] is [Some n]: the first [n] bytes are the
   result, and the rest stays in [pending] for the next call. *)
let read_until ?(timeout = 2.0) fd pending ~what until =
  let chunk = Bytes.create 4096 in
  let deadline = Unix.gettimeofday () +. timeout in
  let rec go () =
    let s = Buffer.contents pending in
    match until s with
    | Some n ->
        Buffer.clear pending;
        Buffer.add_substring pending s n (String.length s - n);
        String.sub s 0 n
    | None ->
        let left = deadline -. Unix.gettimeofday () in
        if left <= 0. then raise (Timeout (what ^ ": " ^ Screen.escape s));
        (match Unix.select [ fd ] [] [] left with
        | [], _, _ -> ()
        | _ ->
            let n = Unix.read fd chunk 0 (Bytes.length chunk) in
            tracef "%.4f read %s %s\n" (Unix.gettimeofday ()) what
              (Screen.escape (Bytes.sub_string chunk 0 n));
            Buffer.add_subbytes pending chunk 0 n);
        go ()
  in
  go ()

let find_sub ?(from = 0) s sub =
  let n = String.length s and m = String.length sub in
  let rec go i =
    if i + m > n then None
    else if String.sub s i m = sub then Some i
    else go (i + 1)
  in
  go from

(* The tty's replies up to and excluding the XTVERSION reply, which is
   consumed. *)
let read_replies t =
  let s =
    read_until t.tty_in t.tty_pending ~what:"replies" (fun s ->
        match find_sub s terminator_prefix with
        | Some i -> (
            match find_sub ~from:i s "\027\\" with
            | Some j -> Some (j + 2)
            | None -> None)
        | None -> None)
  in
  String.sub s 0 (Option.get (find_sub s terminator_prefix))

let read_dump t =
  read_until t.fifo t.fifo_pending ~what:"dump" (fun s ->
      Option.map (fun i -> i + 1) (String.index_opt s '\x0c'))

let open_ ~fifo =
  let tty_in = Unix.stdin and tty_out = Unix.stdout in
  let tio = Unix.tcgetattr tty_in in
  Unix.tcsetattr tty_in Unix.TCSANOW
    {
      tio with
      c_icanon = false;
      c_echo = false;
      c_isig = false;
      c_ixon = false;
      c_icrnl = false;
      c_inlcr = false;
      c_opost = false;
      c_vmin = 1;
      c_vtime = 0;
    };
  (* Opened read-write so that it never reads end of file between prints. *)
  let fifo = Unix.openfile fifo [ Unix.O_RDWR ] 0 in
  let t =
    {
      tty_in;
      tty_out;
      fifo;
      tty_pending = Buffer.create 256;
      fifo_pending = Buffer.create 4096;
    }
  in
  (* Open the printer once, through printer controller mode ([CSI 5 i], a
     form feed for the printer, [CSI 4 i]), and keep it open: a print with no
     printer open forks a printer process and closes it afterwards, and the
     SIGCHLD handler of xterm 398 can then block in wait() on this process,
     wedging xterm (seen every few hundred prints). *)
  write_all t.tty_out "\027[5i\x0c\027[4i";
  ignore (read_dump t);
  t

(* Splits the replies into those to the case plus the first CPR, and the
   second CPR (the last CPR reply). *)
let split_last_cpr s =
  let n = String.length s in
  let rec back i =
    if i < 0 then None
    else if s.[i] = '\027' && i + 1 < n && s.[i + 1] = '[' then Some i
    else back (i - 1)
  in
  if n > 0 && s.[n - 1] = 'R' then
    match back (n - 1) with
    | Some i -> (String.sub s 0 i, String.sub s i (n - i))
    | None -> (s, "")
  else (s, "")

type result = { replies : string; cpr2 : string; dump : string }

let run t bytes =
  write_all t.tty_out ("\027c" ^ bytes ^ trailer ^ probe);
  let replies = read_replies t in
  let dump = read_dump t in
  let replies, cpr2 = split_last_cpr replies in
  { replies; cpr2; dump }

(* After a timeout: drop whatever is in flight, end whatever the case left
   open, reset, and wait for a fresh print and terminator. *)
let recover t =
  let chunk = Bytes.create 4096 in
  let rec drain fd =
    match Unix.select [ fd ] [] [] 0.5 with
    | [], _, _ -> ()
    | _ ->
        ignore (Unix.read fd chunk 0 (Bytes.length chunk));
        drain fd
  in
  (* [CSI 4 i] first: a case may have turned printer controller mode on, in
     which xterm passes everything else to the printer. *)
  write_all t.tty_out "\027[4i\027\\\027c";
  drain t.tty_in;
  drain t.fifo;
  Buffer.clear t.tty_pending;
  Buffer.clear t.fifo_pending;
  write_all t.tty_out "\027[i\027[>0q";
  ignore (read_replies t);
  ignore (read_dump t)

(* [calibrate t cps] is xterm's width of each of [cps]: 0, 1 or 2. Each one is
   written after an [A] at the start of a cleared line and measured by CPR. *)
let calibrate t cps =
  let b = Buffer.create 1024 in
  Buffer.add_string b "\027c";
  List.iter
    (fun cp ->
      Buffer.add_string b "\r\027[2KA";
      Buffer.add_utf_8_uchar b (Uchar.of_int cp);
      Buffer.add_string b "\027[6n")
    cps;
  Buffer.add_string b "\027[>0q";
  write_all t.tty_out (Buffer.contents b);
  let replies = read_replies t in
  let cols =
    String.split_on_char 'R' replies
    |> List.filter (fun s -> s <> "")
    |> List.map (fun s -> Scanf.sscanf s "\027[%d;%d" (fun _ c -> c))
  in
  List.map2 (fun cp col -> (cp, col - 2)) cps cols
