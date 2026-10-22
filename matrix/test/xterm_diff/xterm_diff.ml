(* Differential fuzzer: matrix's VTE against xterm. See README.md. *)

(* {1 xterm's configuration}

   Every resource that matters, so that xterm's power-up state is matrix's
   where nothing decides otherwise. *)
let resources ~fifo ~decid =
  [
    (* Identity: DA1 [CSI ? 1 ; 2 c], a VT100 with advanced video. *)
    ("decTerminalID", string_of_int decid);
    (* UTF-8 in and out ([-u8] also sets it). *)
    ("utf8", "true");
    (* Decode ill-formed UTF-8 as the web does (one U+FFFD per maximal
       subpart, Unicode §3.9) for overlong forms, surrogates and code points
       beyond U+10FFFF, as matrix does. (A lone continuation byte followed by
       ASCII still reads as a Latin-1 or C1 character: ptydata.c.) *)
    ("utf8Weblike", "true");
    (* Keep combining characters as written: xterm composes them to NFC by
       default, matrix never does. *)
    ("precompose", "false");
    (* Store more combining characters per cell than the default 2 (xterm
       clamps this to 5). *)
    ("combiningChars", "10");
    (* xterm's own width table, not the C library's, so that widths do not
       depend on the host. *)
    ("mkWidth", "true");
    ("cjkWidth", "false");
    (* Power-up modes as matrix: autowrap on, reverse wrap off, no alternate
       screen inhibition. *)
    ("autoWrap", "true");
    ("reverseWrap", "false");
    ("titeInhibit", "false");
    (* Attributes stay attributes: bold, underline and blink are not drawn as
       colours, and bold does not brighten the colour. *)
    ("boldColors", "false");
    ("colorBDMode", "false");
    ("colorULMode", "false");
    ("colorBLMode", "false");
    ("colorRVMode", "false");
    ("colorITMode", "false");
    ("colorAttrMode", "false");
    ("colorMode", "true");
    ("directColor", "true");
    (* Scroll at once: with jump scroll, xterm defers the redraw of the rows
       a scroll brings in, and while that is pending EL 1 and DECSEL 1 skip
       the Last Column Flag and protection (util.c, ClearLeft: AddToVisible). *)
    ("jumpScroll", "false");
    (* No scroll bar, no logging, no login shell, no utmp entry. *)
    ("scrollBar", "false");
    ("logging", "false");
    ("loginShell", "false");
    ("utmpInhibit", "true");
    ("saveLines", "0");
    (* No window operations: the generator sends none, and a resize would
       change the geometry under the comparison. allowC1Printable keeps its
       default (false): it changes nothing in UTF-8 mode (measured). *)
    ("allowWindowOps", "false");
    ("allowC1Printable", "false");
    ("allowColumnMode", "false");
    ("answerbackString", "");
    (* Print-screen ([CSI i]) as the dump: the whole screen whatever the
       margins (printerExtent), with SGR and colours (printAttributes 2), every
       cell as stored (printRawChars: U+FFFF marks the second cell of a wide
       character, U+FFFD prints as itself), a form feed after each screen
       (printerFormFeed, which also flushes the pipe), a newline after every
       row (printerNewLine), and one printer process for the session. *)
    ("printerCommand", "cat > " ^ Filename.quote fifo);
    ("printerExtent", "true");
    ("printAttributes", "2");
    ("printRawChars", "true");
    ("printerFormFeed", "true");
    ("printerNewLine", "true");
    ("printerAutoClose", "false");
    ("printModeImmediate", "0");
    ("printOptsImmediate", "0");
  ]

(* {1 Options} *)

let rows = ref 6
let cols = ref 12
let seed = ref 1
let count = ref 1000
let start = ref 0
let out = ref "xterm_diff_out"
let decid = ref 100
let exclude = ref []

let xterm =
  ref (Option.value (Sys.getenv_opt "XTERM_DIFF_XTERM") ~default:"xterm")

let budget = ref 400
let no_min = ref false
let strict = ref []
let max_tokens = ref 40
let mode = ref ""
let input = ref ""

let spec =
  Arg.align
    [
      ("--rows", Arg.Set_int rows, "N screen rows (default 6)");
      ("--cols", Arg.Set_int cols, "N screen columns (default 12)");
      ("--seed", Arg.Set_int seed, "N run seed (default 1)");
      ("--count", Arg.Set_int count, "N number of cases (default 1000)");
      ("--start", Arg.Set_int start, "N index of the first case (default 0)");
      ( "--out",
        Arg.Set_string out,
        "DIR output directory (default xterm_diff_out)" );
      ("--decid", Arg.Set_int decid, "N xterm's decTerminalID (default 100)");
      ( "--exclude",
        Arg.String (fun s -> exclude := !exclude @ String.split_on_char ',' s),
        "L1,L2 never generate tokens whose label starts with one of these" );
      ( "--xterm",
        Arg.Set_string xterm,
        "PATH xterm binary (default $XTERM_DIFF_XTERM or xterm)" );
      ("--budget", Arg.Set_int budget, "N runs per minimization (default 400)");
      ("--no-minimize", Arg.Set no_min, " do not minimize failing cases");
      ( "--strict",
        Arg.String (fun s -> strict := !strict @ String.split_on_char ',' s),
        "N1,N2 turn these normalisations off ('all' for every one; see \
         'xterm_diff list')" );
      ( "--max-tokens",
        Arg.Set_int max_tokens,
        "N the longest case, in tokens (default 40)" );
    ]

let usage =
  "xterm_diff (fuzz | case BYTES | case @FILE | matrix BYTES | gen | list) \
   [options]\n\
   BYTES is escaped: \\e, \\xHH, \\u{HHHH}, \\n, \\r, \\t, \\\\."

let anon a =
  if !mode = "" then mode := a
  else if !input = "" then input := a
  else raise (Arg.Bad ("unexpected " ^ a))

let config () =
  {
    Gen.rows = !rows;
    cols = !cols;
    exclude = !exclude;
    max_tokens = !max_tokens;
  }

let enabled name = not (List.mem "all" !strict || List.mem name !strict)

let context ~width =
  {
    Norm.decid = !decid;
    rows = !rows;
    cols = !cols;
    width;
    enabled;
    fired = [];
  }

(* Characters outside the measured alphabet reach the screen only through the
   character sets (narrow) or as U+FFFD (narrow). *)
let default_width cp =
  if cp < 0x20 then 0
  else Matrix_text.uchar_width ~tab_width:1 (Uchar.of_int cp)

let case_tokens ~width i =
  let ctx = context ~width in
  let tokens = Gen.case ~allow:(Norm.allow ctx) (config ()) ~seed:!seed i in
  (tokens, ctx.fired)

(* {1 One comparison} *)

type outcome = {
  diffs : Screen.diff list;
  report : string;
  fired : string list;  (** The normalisations that fired. *)
}

let with_alarm secs f =
  let old =
    Sys.signal Sys.sigalrm
      (Sys.Signal_handle (fun _ -> failwith "matrix timed out"))
  in
  ignore (Unix.alarm secs);
  Fun.protect
    ~finally:(fun () ->
      ignore (Unix.alarm 0);
      Sys.set_signal Sys.sigalrm old)
    f

let matrix_side ?width_method bytes =
  with_alarm 5 (fun () ->
      let m = Vte_adapter.create ?width_method ~rows:!rows ~cols:!cols () in
      let r1 = Vte_adapter.feed m (bytes ^ Xterm.trailer ^ "\027[6n") in
      let screen = Vte_adapter.screen m in
      let r2 = Vte_adapter.feed m "#\027[6n" in
      let replies, cpr1 = Xterm.split_last_cpr r1 in
      (replies, cpr1, r2, screen))

let compare_case session ~width ?(fired = []) bytes =
  let ctx = context ~width in
  ctx.fired <- fired;
  let rw = Norm.rewrite ctx bytes in
  let x = Xterm.run session ~decid:!decid rw.xterm in
  let xs = Screen.decode ~width ~rows:!rows ~cols:!cols x.dump in
  let x_replies, x_cpr1 = Xterm.split_last_cpr x.replies in
  let x_replies = Norm.replies ctx x_replies in
  let mismatch kind where a b =
    if a = b then []
    else
      [
        {
          Screen.kind;
          where;
          xterm = Screen.escape a;
          matrix = Screen.escape b;
        };
      ]
  in
  let differences ~xterm:xs (m_replies, m_cpr1, m_cpr2, ms) =
    let xs, ms =
      Norm.screens ctx ~orphans:rw.orphans ~erased:rw.erased ~cut:rw.cut
        ~lost:rw.lost ~xterm:xs ~matrix:ms
    in
    let diffs =
      Screen.compare_screens ~xterm:xs ~matrix:ms
      @ mismatch Cursor "cursor after the case" x_cpr1 m_cpr1
      @ mismatch Cursor "cursor after one more character" x.cpr2 m_cpr2
      @ mismatch Reply "replies" x_replies m_replies
    in
    (diffs, xs, ms)
  in
  match matrix_side bytes with
  | exception e ->
      let msg = Printexc.to_string e in
      {
        diffs = [ { kind = Crash; where = "matrix"; xterm = ""; matrix = msg } ];
        report = "matrix raised " ^ msg ^ "\n\nxterm:\n" ^ Screen.render xs;
        fired = ctx.fired;
      }
  | unicode ->
      let saved = ctx.fired in
      let diffs, xs', ms = differences ~xterm:xs unicode in
      (* graphemes: when matrix's two width methods disagree, the one whose
         units are xterm's. *)
      let (_, m_cpr1, m_cpr2, _), diffs, xs', ms =
        if diffs = [] || not (enabled "graphemes") then (unicode, diffs, xs', ms)
        else
          match matrix_side ~width_method:`Wcwidth bytes with
          | exception _ -> (unicode, diffs, xs', ms)
          | wcwidth ->
              if unicode = wcwidth then (unicode, diffs, xs', ms)
              else (
                ctx.fired <- saved;
                Norm.fire ctx "graphemes";
                let diffs, xs', ms = differences ~xterm:xs wcwidth in
                (wcwidth, diffs, xs', ms))
      in
      let report =
        Printf.sprintf
          "xterm (cursor %s, then %s):\n%smatrix (cursor %s, then %s):\n%s"
          (Screen.escape x_cpr1) (Screen.escape x.cpr2) (Screen.render xs')
          (Screen.escape m_cpr1) (Screen.escape m_cpr2) (Screen.render ms)
      in
      let report =
        if rw.xterm = bytes then report
        else Printf.sprintf "xterm got: %s\n%s" (Screen.escape rw.xterm) report
      in
      let cells name l =
        if l = [] then ""
        else
          Printf.sprintf "%s cells: %s\n" name
            (String.concat " "
               (List.map (fun (r, c) -> Printf.sprintf "%d,%d" r c) l))
      in
      let report =
        cells "lost" rw.lost ^ cells "cut" rw.cut ^ cells "erased" rw.erased
        ^ report
      in
      { diffs; report; fired = List.sort_uniq compare ctx.fired }

let primary diffs =
  List.find_opt
    (fun k -> List.exists (fun d -> d.Screen.kind = k) diffs)
    Screen.[ Crash; Text; Cursor; Attr; Reply; Line ]

let kinds diffs =
  List.sort_uniq compare (List.map (fun d -> d.Screen.kind) diffs)
  |> List.map Screen.kind_name |> String.concat ","

let describe_diffs diffs =
  String.concat ""
    (List.map
       (fun d ->
         Printf.sprintf "  %s at %s: xterm %s, matrix %s\n"
           (Screen.kind_name d.Screen.kind)
           d.where d.xterm d.matrix)
       diffs)

let describe_tokens tokens =
  String.concat " "
    (List.map
       (fun t -> Printf.sprintf "%s[%s]" t.Gen.label (Screen.pretty t.bytes))
       tokens)

(* {1 The agent: runs inside xterm} *)

let rec agent () =
  (* xterm waits for its printer process on exit, which waits for xterm to
     close the pipe it reads: xterm never exits by itself after the agent. *)
  at_exit (fun () ->
      try Unix.kill (Unix.getppid ()) Sys.sigkill with Unix.Unix_error _ -> ());
  let fifo = Filename.concat !out "printer.fifo" in
  let log =
    open_out_gen
      [ Open_wronly; Open_append; Open_creat ]
      0o644
      (Filename.concat !out "log.txt")
  in
  let logf fmt = Printf.kfprintf (fun oc -> flush oc) log fmt in
  List.iter
    (fun (signal, name) ->
      Sys.set_signal signal
        (Sys.Signal_handle
           (fun _ ->
             logf "agent: %s\n" name;
             exit 5)))
    [
      (Sys.sighup, "SIGHUP: xterm went away");
      (Sys.sigterm, "SIGTERM");
      (Sys.sigpipe, "SIGPIPE");
    ];
  Printexc.register_printer (function
    | Xterm.Timeout w -> Some ("Timeout " ^ w)
    | _ -> None);
  try agent_body ~fifo ~log
  with e ->
    logf "agent failed: %s\n%s" (Printexc.to_string e)
      (Printexc.get_backtrace ());
    exit 4

and agent_body ~fifo ~log =
  let logf fmt = Printf.kfprintf (fun oc -> flush oc) log fmt in
  let session = Xterm.open_ ~fifo in
  (* xterm's widths, measured once per output directory on a screen wide
     enough to measure them (see [launch]). *)
  let file = Filename.concat !out "widths.txt" in
  let widths =
    if Sys.file_exists file then
      In_channel.with_open_bin file In_channel.input_lines
      |> List.map (fun l -> Scanf.sscanf l "%x %d" (fun cp w -> (cp, w)))
    else
      let widths = Xterm.calibrate session Gen.alphabet in
      Out_channel.with_open_bin file (fun oc ->
          List.iter (fun (cp, w) -> Printf.fprintf oc "%x %d\n" cp w) widths);
      widths
  in
  if !mode = "calibrate" then exit 0;
  let table = Hashtbl.create 64 in
  List.iter (fun (cp, w) -> Hashtbl.replace table cp w) widths;
  logf "calibration: %s\n"
    (String.concat " "
       (List.map (fun (cp, w) -> Printf.sprintf "U+%04X:%d" cp w) widths));
  (* Characters outside the alphabet reach the print only through the
     character sets (narrow) or as U+FFFD (narrow). *)
  let width cp =
    match Hashtbl.find_opt table cp with
    | Some w -> w
    | None -> default_width cp
  in
  (* The case being run, which the parent skips if xterm cannot recover. *)
  let next = Filename.concat !out "next" in
  let set_next i =
    Out_channel.with_open_bin next (fun oc ->
        output_string oc (string_of_int i))
  in
  let compare ?fired bytes =
    try Some (compare_case session ~width ?fired bytes)
    with Xterm.Timeout what ->
      logf "timeout (%s) on %s\n" what (Screen.escape bytes);
      (try Xterm.recover session
       with Xterm.Timeout _ ->
         logf "xterm did not recover\n";
         exit 3);
      None
  in
  match !mode with
  | "case" ->
      (* [@FILE] runs every line of FILE. *)
      let inputs =
        if String.starts_with ~prefix:"@" !input then
          In_channel.with_open_bin
            (String.sub !input 1 (String.length !input - 1))
            In_channel.input_lines
          |> List.filter (fun l -> l <> "" && l.[0] <> '#')
        else [ !input ]
      in
      let oc = open_out (Filename.concat !out "case.txt") in
      List.iter
        (fun input ->
          let bytes = Screen.unescape input in
          match compare bytes with
          | Some o ->
              Printf.fprintf oc "case: %s\n%s%s%s\n" (Screen.escape bytes)
                (if o.fired = [] then ""
                 else "normalised: " ^ String.concat " " o.fired ^ "\n")
                (describe_diffs o.diffs) o.report
          | None ->
              Printf.fprintf oc "case: %s\nxterm timed out\n\n"
                (Screen.escape bytes))
        inputs;
      close_out oc
  | _ ->
      let results =
        open_out_gen
          [ Open_wronly; Open_append; Open_creat ]
          0o644
          (Filename.concat !out "results.tsv")
      in
      let dir = Filename.concat !out "diffs" in
      (try Unix.mkdir dir 0o755 with Unix.Unix_error (EEXIST, _, _) -> ());
      let t0 = Unix.gettimeofday () in
      let runs = ref 0 and failing = ref 0 in
      for i = !start to !start + !count - 1 do
        set_next i;
        let tokens, restricted = case_tokens ~width i in
        let bytes = Gen.bytes tokens in
        incr runs;
        (match compare ~fired:restricted bytes with
        | None -> Printf.fprintf results "%d\ttimeout\n%!" i
        | Some o when o.diffs = [] ->
            Printf.fprintf results "%d\tsame\t%s\n%!" i
              (String.concat "," o.fired)
        | Some o -> begin
            incr failing;
            let p = primary o.diffs in
            (* A smaller case must still be one the generator could make. *)
            let valid l =
              let ctx = context ~width in
              let rec go prev = function
                | [] -> true
                | t :: rest ->
                    Norm.allow ctx ~prev ~last:(rest = []) t
                    && go (t :: prev) rest
              in
              go [] l
            in
            let fails l =
              valid l
              && begin
                incr runs;
                match compare (Gen.bytes l) with
                | Some o -> o.diffs <> [] && primary o.diffs = p
                | None -> false
              end
            in
            let min, calls =
              if !no_min then (tokens, 0)
              else Minimize.run ~budget:!budget ~fails tokens
            in
            let mo = Option.value (compare (Gen.bytes min)) ~default:o in
            let signature =
              String.concat " " (List.map (fun t -> t.Gen.label) min)
            in
            Printf.fprintf results "%d\tdiff\t%s\t%s\t%s\t%s\n%!" i
              (kinds mo.diffs) signature
              (Screen.escape (Gen.bytes min))
              (String.concat "," o.fired);
            let oc =
              open_out (Filename.concat dir (Printf.sprintf "%06d.txt" i))
            in
            Printf.fprintf oc
              "case %d (seed %d, %dx%d, decTerminalID %d)\n\
               original: %s\n\
               tokens:   %s\n\
               original differences:\n\
               %s\n\
               normalised: %s\n\
               minimized (%d runs): %s\n\
               tokens:   %s\n\
               differences:\n\
               %s\n\
               %s"
              i !seed !cols !rows !decid (Screen.escape bytes)
              (describe_tokens tokens) (describe_diffs o.diffs)
              (String.concat " " mo.fired)
              calls
              (Screen.escape (Gen.bytes min))
              (describe_tokens min) (describe_diffs mo.diffs) mo.report;
            close_out oc
          end);
        if (i - !start + 1) mod 100 = 0 then
          logf "%d cases, %d differ, %d runs, %.1f runs/s\n"
            (i - !start + 1)
            !failing !runs
            (float !runs /. (Unix.gettimeofday () -. t0))
      done;
      set_next (!start + !count);
      let dt = Unix.gettimeofday () -. t0 in
      logf "done: %d cases, %d differ, %d runs in %.1fs (%.1f runs/s)\n" !count
        !failing !runs dt
        (float !runs /. dt);
      close_out results

(* {1 The parent: starts xterm on the agent} *)

(* Groups results.tsv by the kinds of difference and the labels of the
   minimized case, largest group first, with the first case of each. *)
let summarize () =
  let rows =
    In_channel.with_open_bin
      (Filename.concat !out "results.tsv")
      In_channel.input_lines
    |> List.map (String.split_on_char '\t')
  in
  let count v =
    List.length (List.filter (fun r -> List.nth_opt r 1 = Some v) rows)
  in
  let groups = Hashtbl.create 64 in
  let fired = Hashtbl.create 32 in
  let note names =
    List.iter
      (fun n ->
        if n <> "" then
          Hashtbl.replace fired n
            (1 + Option.value (Hashtbl.find_opt fired n) ~default:0))
      (String.split_on_char ',' names)
  in
  List.iter
    (function
      | i :: "diff" :: kinds :: signature :: bytes :: rest ->
          note (String.concat "" rest);
          let key = (kinds, signature) in
          let n, first =
            Option.value (Hashtbl.find_opt groups key) ~default:(0, (i, bytes))
          in
          Hashtbl.replace groups key (n + 1, first)
      | _ :: "same" :: names -> note (String.concat "" names)
      | _ -> ())
    rows;
  let groups =
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) groups []
    |> List.sort (fun (_, (a, _)) (_, (b, _)) -> compare b a)
  in
  Out_channel.with_open_bin (Filename.concat !out "summary.txt") (fun oc ->
      Printf.fprintf oc "%d cases: %d same, %d timed out\n" (List.length rows)
        (count "same") (count "timeout");
      Printf.fprintf oc "%d unexplained differences\n\n" (count "diff");
      Printf.fprintf oc "normalisations (cases in which each fired):\n";
      List.iter
        (fun (i : Norm.info) ->
          let off = not (enabled i.name) in
          match Hashtbl.find_opt fired i.name with
          | Some n -> Printf.fprintf oc "  %-20s %7d\n" i.name n
          | None when off -> Printf.fprintf oc "  %-20s     off\n" i.name
          | None -> Printf.fprintf oc "  %-20s       0\n" i.name)
        Norm.all;
      if groups <> [] then (
        Printf.fprintf oc
          "\ncount\tkinds\tminimized tokens\tfirst case\tits minimized bytes\n";
        List.iter
          (fun ((kinds, signature), (n, (i, bytes))) ->
            Printf.fprintf oc "%d\t%s\t%s\t%s\t%s\n" n kinds signature i bytes)
          groups))

let list () =
  List.iter
    (fun (i : Norm.info) ->
      Printf.printf "%s (%s, %s)\n  %s\n" i.name (Norm.kind_name i.kind) i.how
        i.why)
    Norm.all

let launch () =
  (try Unix.mkdir !out 0o755 with Unix.Unix_error (EEXIST, _, _) -> ());
  if Sys.getenv_opt "DISPLAY" = None then (
    prerr_endline "xterm_diff: DISPLAY is not set (see README.md)";
    exit 2);
  let fifo = Filename.concat !out "printer.fifo" in
  let next = Filename.concat !out "next" in
  (try Unix.unlink next with Unix.Unix_error _ -> ());
  let stop = !start + !count in
  (* One xterm runs every case; if it wedges beyond recovery, the agent exits
     and a new xterm resumes after the case that wedged it. *)
  let rec go ?(mode = !mode) ?(rows = !rows) ?(cols = !cols) first =
    (try Unix.unlink fifo with Unix.Unix_error _ -> ());
    Unix.mkfifo fifo 0o600;
    let xrm =
      List.concat_map
        (fun (k, v) -> [ "-xrm"; Printf.sprintf "XTerm*%s: %s" k v ])
        (resources ~fifo ~decid:!decid)
    in
    let own =
      [
        "agent:" ^ mode;
        !input;
        "--rows";
        string_of_int rows;
        "--cols";
        string_of_int cols;
        "--seed";
        string_of_int !seed;
        "--start";
        string_of_int first;
        "--count";
        string_of_int (stop - first);
        "--out";
        !out;
        "--decid";
        string_of_int !decid;
        "--budget";
        string_of_int !budget;
      ]
      @ (if !exclude = [] then []
         else [ "--exclude"; String.concat "," !exclude ])
      @ (if !no_min then [ "--no-minimize" ] else [])
      @ (if !strict = [] then [] else [ "--strict"; String.concat "," !strict ])
      @ [ "--max-tokens"; string_of_int !max_tokens ]
    in
    let args =
      [
        !xterm;
        "-u8";
        "+sb";
        "-ut";
        "-geometry";
        Printf.sprintf "%dx%d" cols rows;
      ]
      @ xrm
      @ ("-e" :: Sys.executable_name :: own)
    in
    let pid =
      Unix.create_process !xterm (Array.of_list args) Unix.stdin Unix.stdout
        Unix.stderr
    in
    let status =
      match snd (Unix.waitpid [] pid) with
      | Unix.WEXITED n -> Printf.sprintf "exited %d" n
      | WSIGNALED n -> Printf.sprintf "killed by signal %d" n
      | WSTOPPED n -> Printf.sprintf "stopped by signal %d" n
    in
    let reached =
      try
        int_of_string
          (String.trim (In_channel.with_open_bin next In_channel.input_all))
      with _ -> first
    in
    if mode = "fuzz" && reached < stop then (
      Printf.eprintf "xterm_diff: case %d wedged xterm (%s), skipping it\n%!"
        reached status;
      go (reached + 1))
  in
  (* The calibration writes a character after an A and reads where the
     cursor went: below 4 columns it runs on a screen of its own. *)
  if !cols < 4 && not (Sys.file_exists (Filename.concat !out "widths.txt")) then
    go ~mode:"calibrate" ~rows:6 ~cols:12 !start;
  go !start;
  if !mode = "fuzz" then summarize ();
  let show f =
    let f = Filename.concat !out f in
    if Sys.file_exists f then
      In_channel.with_open_bin f In_channel.input_all |> print_string
  in
  if !mode = "case" then show "case.txt" else show "summary.txt"

let matrix_only () =
  let bytes = Screen.unescape !input in
  let replies, cpr1, cpr2, screen = matrix_side bytes in
  let ctx = context ~width:default_width in
  let rw = Norm.rewrite ctx bytes in
  Printf.printf "xterm would get %s (%s)\nreplies %s, cursor %s, then %s\n%s"
    (Screen.escape rw.xterm)
    (String.concat " " ctx.fired)
    (Screen.escape replies) (Screen.escape cpr1) (Screen.escape cpr2)
    (Screen.render screen)

let () =
  Arg.parse spec anon usage;
  match !mode with
  | "fuzz" | "case" -> launch ()
  | "agent:fuzz" ->
      mode := "fuzz";
      agent ()
  | "agent:case" ->
      mode := "case";
      agent ()
  | "agent:calibrate" ->
      mode := "calibrate";
      agent ()
  | "matrix" -> matrix_only ()
  | "list" -> list ()
  | "gen" ->
      for i = !start to !start + !count - 1 do
        let tokens, _ = case_tokens ~width:default_width i in
        Printf.printf "%d\t%s\t%s\n" i
          (Screen.escape (Gen.bytes tokens))
          (describe_tokens tokens)
      done
  | _ ->
      Arg.usage spec usage;
      exit 2
