(* input_diff: the runtime's input against a real xterm.

   Not a test: it needs xterm and an X server, which CI lacks. See README.md.

   [input_diff caps --decid N] starts xterm with that decTerminalID on an agent
   that creates a {!Matrix} application, so that the runtime sends its real
   startup probe and derives the capabilities from xterm's answers. The agent
   writes the capabilities, and the raw replies to the same probe, and the
   parent checks the capabilities against what xterm 398 is documented and
   measured to do.

   [input_diff keys --decid N --driver drive.py --chords FILE] starts the
   agent in a running application, presses each chord of FILE in xterm with
   XTest, and checks the keystroke the runtime decoded for each. *)

let xterm =
  ref (Option.value (Sys.getenv_opt "XTERM_DIFF_XTERM") ~default:"xterm")

let decid = ref 100
let out = ref "/tmp/input_diff"
let driver = ref ""
let chords = ref ""
let xtst = ref "libXtst.so.6"
let duration = ref 20.

(* {1 The agent: runs inside xterm} *)

let escape s =
  String.concat ""
    (List.map
       (fun c ->
         if c = '\027' then "\\e"
         else if c < ' ' || c > '~' then Printf.sprintf "\\x%02x" (Char.code c)
         else String.make 1 c)
       (List.of_seq (String.to_seq s)))

let show_caps (c : Matrix.Terminal.capabilities) =
  let b = string_of_bool in
  let opt f = function None -> "none" | Some v -> f v in
  [
    ("rgb", b c.rgb);
    ("kitty_keyboard", opt string_of_int c.kitty_keyboard);
    ("kitty_graphics", b c.kitty_graphics);
    ("bracketed_paste", b c.bracketed_paste);
    ("focus_tracking", b c.focus_tracking);
    ( "unicode_width",
      match c.unicode_width with
      | `Unicode -> "unicode"
      | `Unicode_wide -> "unicode_wide"
      | `Wcwidth -> "wcwidth" );
    ("sgr_pixels", b c.sgr_pixels);
    ("color_scheme_updates", b c.color_scheme_updates);
    ("explicit_width", b c.explicit_width);
    ("explicit_cursor_positioning", b c.explicit_cursor_positioning);
    ("scaled_text", b c.scaled_text);
    ("sixel", b c.sixel);
    ("sync", b c.sync);
    ("hyperlinks", b c.hyperlinks);
    ("meta_sends_escape", opt b c.meta_sends_escape);
    ( "multiplexer",
      opt (function `Tmux -> "tmux" | `Screen -> "screen") c.multiplexer );
    ("identity", opt (fun (n, v) -> n ^ " " ^ v) c.identity);
  ]

let write_file path s =
  Out_channel.with_open_bin (path ^ ".tmp") (fun oc -> output_string oc s);
  Unix.rename (path ^ ".tmp") path

(* The replies xterm sends to the runtime's probe, read raw for a second. *)
let agent_raw path =
  let saved = Matrix.Terminal.set_raw Unix.stdin in
  let env = Matrix.Terminal.Caps.env Sys.getenv_opt ~win32:false in
  let probe, _ =
    Matrix.Terminal.Caps.ask (Matrix.Terminal.Caps.evidence env) Probe
  in
  ignore (Unix.write_substring Unix.stdout probe 0 (String.length probe));
  let buf = Bytes.create 4096 and got = Buffer.create 256 in
  let deadline = Unix.gettimeofday () +. 1. in
  while Unix.gettimeofday () < deadline do
    match Unix.select [ Unix.stdin ] [] [] 0.05 with
    | [], _, _ -> ()
    | _ -> Buffer.add_subbytes got buf 0 (Unix.read Unix.stdin buf 0 4096)
  done;
  Matrix.Terminal.restore Unix.stdin saved;
  write_file path (escape (Buffer.contents got) ^ "\n")

let agent_caps path =
  let caps = Matrix.run Matrix.capabilities in
  write_file path
    (String.concat ""
       (List.map (fun (k, v) -> Printf.sprintf "%s=%s\n" k v) (show_caps caps)))

(* Records each event the runtime delivers, with the time, until [duration]
   has passed. *)
let agent_keys path =
  let oc = open_out_bin path in
  write_file (path ^ ".ready") "";
  Matrix.run (fun app ->
      let stop =
        Matrix.at app (Matrix.now app + Float.to_int (!duration *. 1e9))
      in
      let rec loop () =
        match Matrix.next app with
        | Stopped -> ()
        | Timer t when t = stop -> ()
        | Event event ->
            Printf.fprintf oc "%.3f\t%s\n%!" (Unix.gettimeofday ())
              (Format.asprintf "%a" Matrix.pp_event event);
            loop ()
        | Frame ->
            Matrix.draw app ignore;
            loop ()
        | Timer _ | Posted _ -> loop ()
      in
      loop ());
  close_out oc

(* {1 The parent: starts xterm on the agent} *)

(* The environment of a terminal started fresh, directly attached: none of
   the variables that would describe another terminal or a multiplexer. *)
let clean_env () =
  let keep = [ "DISPLAY"; "PATH"; "HOME"; "LD_LIBRARY_PATH"; "USER" ] in
  Array.of_list
    ("LANG=C.UTF-8" :: "LC_ALL=C.UTF-8"
    :: List.filter_map
         (fun k -> Option.map (fun v -> k ^ "=" ^ v) (Sys.getenv_opt k))
         keep)

let run_xterm mode path =
  let args =
    [
      !xterm;
      "-u8";
      "+sb";
      "-ut";
      "-geometry";
      "80x24+0+0";
      "-xrm";
      Printf.sprintf "XTerm*decTerminalID: %d" !decid;
      "-xrm";
      "XTerm*utf8: true";
      "-e";
      Sys.executable_name;
      "agent:" ^ mode;
      path;
      "--duration";
      string_of_float !duration;
    ]
  in
  Unix.create_process_env !xterm (Array.of_list args) (clean_env ()) Unix.stdin
    Unix.stdout Unix.stderr

let wait pid = ignore (Unix.waitpid [] pid)
let read_lines path = In_channel.with_open_bin path In_channel.input_lines

(* What the runtime must derive from xterm 398 (measured with this build:
   see README.md): XTVERSION [XTerm(398)]; DECRQM answers 1004, 1016, 2004
   and 1036 reset and 2026, 2027, 2031 not recognized; no kitty keyboard, no
   kitty graphics, no OSC 66. *)
let expected_caps ~decid =
  ignore decid;
  [
    ("rgb", "false");
    ("kitty_keyboard", "none");
    ("kitty_graphics", "false");
    ("bracketed_paste", "true");
    ("focus_tracking", "true");
    ("unicode_width", "unicode");
    ("sgr_pixels", "true");
    ("color_scheme_updates", "false");
    ("explicit_width", "false");
    ("explicit_cursor_positioning", "false");
    ("scaled_text", "false");
    ("sixel", "false");
    ("sync", "false");
    ("hyperlinks", "false");
    ("meta_sends_escape", "false");
    ("multiplexer", "none");
    ("identity", "XTerm 398");
  ]

let check name expected got =
  let failures = ref 0 in
  List.iter
    (fun (k, v) ->
      match List.assoc_opt k got with
      | Some g when g = v -> ()
      | g ->
          incr failures;
          Printf.printf "  %s %s: expected %s, got %s\n" name k v
            (Option.value g ~default:"(missing)"))
    expected;
  !failures

let caps () =
  (try Unix.mkdir !out 0o755 with Unix.Unix_error (EEXIST, _, _) -> ());
  let raw = Filename.concat !out (Printf.sprintf "raw-%d.txt" !decid) in
  let caps = Filename.concat !out (Printf.sprintf "caps-%d.txt" !decid) in
  wait (run_xterm "raw" raw);
  wait (run_xterm "caps" caps);
  Printf.printf "decTerminalID %d\n  replies: %s" !decid
    (String.concat "\n" (read_lines raw));
  print_newline ();
  let got =
    List.filter_map
      (fun l ->
        match String.index_opt l '=' with
        | Some i ->
            Some
              (String.sub l 0 i, String.sub l (i + 1) (String.length l - i - 1))
        | None -> None)
      (read_lines caps)
  in
  List.iter (fun (k, v) -> Printf.printf "  %s=%s\n" k v) got;
  let failures = check "caps" (expected_caps ~decid:!decid) got in
  Printf.printf "%d unexpected\n" failures;
  exit (if failures = 0 then 0 else 1)

(* The keystroke each chord decodes to: xterm 398 with the modes the runtime
   sets for a terminal without the kitty protocol, modifyOtherKeys 1 and
   metaSendsEscape (measured byte by byte with this driver beforehand). A
   chord that sends ESC alone, or ESC before an introducer, is decided by
   silence. *)
let expected_keys =
  [
    ("a", "Key 'a' text \"a\"");
    ("Shift_L+a", "Key 'A' text \"A\"");
    ("Shift_L+1", "Key '!' text \"!\"");
    ("Alt_L+a", "Key Alt+'a'");
    ("Alt_L+Shift_L+a", "Key Alt+'A'");
    ("Alt_L+Shift_L+1", "Key Alt+'!'");
    ("Alt_L+bracketleft", "Key Alt+'['");
    ("Control_L+a", "Key Ctrl+'a'");
    ("Control_L+Shift_L+a", "Key Ctrl+'a'");
    ("Control_L+s", "Key Ctrl+'s'");
    ("Control_L+Shift_L+1", "Key Ctrl+'!'");
    ("Control_L+Return", "Key Ctrl+Enter");
    ("Shift_L+Return", "Key Shift+Enter");
    ("Control_L+i", "Key Tab");
    ("Control_L+j", "Key Ctrl+'j'");
    ("Control_L+2", "Key Ctrl+U+0020");
    ("Control_L+minus", "Key Ctrl+'-'");
    ("Control_L+Alt_L+a", "Key Ctrl+Alt+'a'");
    ("Shift_L+F3", "Key Shift+F 3");
    ("Control_L+F1", "Key Ctrl+F 1");
    ("Shift_L+Up", "Key Shift+Up");
    ("KP_5", "Key Begin keypad");
    ("KP_Enter", "Key Enter");
    ("Escape", "Key Escape");
    ("Alt_L+Escape", "Key Alt+Escape");
  ]

let keys () =
  (try Unix.mkdir !out 0o755 with Unix.Unix_error (EEXIST, _, _) -> ());
  let events = Filename.concat !out (Printf.sprintf "keys-%d.txt" !decid) in
  let log = Filename.concat !out (Printf.sprintf "drive-%d.txt" !decid) in
  (try Sys.remove (events ^ ".ready") with Sys_error _ -> ());
  let pid = run_xterm "keys" events in
  while not (Sys.file_exists (events ^ ".ready")) do
    Unix.sleepf 0.05
  done;
  Unix.sleepf 0.5;
  let drive =
    Unix.create_process "python3"
      [| "python3"; !driver; !xtst; log; !chords |]
      Unix.stdin Unix.stdout Unix.stderr
  in
  wait drive;
  wait pid;
  let split l =
    match String.index_opt l '\t' with
    | Some i ->
        ( float_of_string (String.sub l 0 i),
          String.sub l (i + 1) (String.length l - i - 1) )
    | None -> (0., l)
  in
  let pressed = List.map split (read_lines log) in
  let decoded = List.map split (read_lines events) in
  (* The events after a chord was pressed and before the next one are its. *)
  let rec align = function
    | [] -> []
    | (t, chord) :: rest ->
        let until = match rest with (t', _) :: _ -> t' | [] -> infinity in
        let mine =
          List.filter_map
            (fun (at, e) ->
              if
                at >= t && at < until
                && not (String.starts_with ~prefix:"Focus" e)
              then Some e
              else None)
            decoded
        in
        (chord, mine) :: align rest
  in
  let failures = ref 0 and seen = Hashtbl.create 16 in
  List.iter
    (fun (chord, got) ->
      let got = String.concat "; " got in
      (* The second time a chord is pressed, Caps Lock is on. *)
      let caps = Hashtbl.mem seen chord in
      Hashtbl.replace seen chord ();
      let expected =
        if chord = "Caps_Lock" then Some ""
        else if caps then
          List.assoc_opt chord
            [
              ("a", "Key 'A' text \"A\"");
              ("Alt_L+a", "Key Alt+'A'");
              ("Control_L+s", "Key Ctrl+'s'");
              ("Alt_L+Shift_L+a", "Key Alt+'a'");
            ]
        else List.assoc_opt chord expected_keys
      in
      match expected with
      | Some e when e = got -> Printf.printf "  ok   %-22s %s\n" chord got
      | Some e ->
          incr failures;
          Printf.printf "  FAIL %-22s expected %s, got %s\n" chord e got
      | None -> Printf.printf "  ?    %-22s %s\n" chord got)
    (align pressed);
  Printf.printf "%d unexpected\n" !failures;
  exit (if !failures = 0 then 0 else 1)

let () =
  let mode = ref "" and path = ref "" in
  let spec =
    [
      ("--decid", Arg.Set_int decid, "xterm's decTerminalID (default 100)");
      ("--out", Arg.Set_string out, "the output directory");
      ("--xterm", Arg.Set_string xterm, "xterm's path");
      ("--driver", Arg.Set_string driver, "the XTest driver (drive.py)");
      ("--chords", Arg.Set_string chords, "the chords to press");
      ("--xtst", Arg.Set_string xtst, "libXtst's path");
      ("--duration", Arg.Set_float duration, "seconds the key agent runs");
    ]
  in
  Arg.parse spec
    (fun a -> if !mode = "" then mode := a else path := a)
    "input_diff (caps|keys) [options]";
  match !mode with
  | "caps" -> caps ()
  | "keys" -> keys ()
  | "agent:raw" -> agent_raw !path
  | "agent:caps" -> agent_caps !path
  | "agent:keys" -> agent_keys !path
  | m ->
      prerr_endline ("input_diff: unknown mode " ^ m);
      exit 2
