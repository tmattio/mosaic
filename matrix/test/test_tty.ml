open Windtrap

let with_pty ?winsize f =
  let master, slave = Pty.open_pty ?winsize () in
  Fun.protect
    ~finally:(fun () ->
      Pty.close master;
      Pty.close slave)
    (fun () -> f master (Pty.file_descr slave))

let with_pipe f =
  let r, w = Unix.pipe ~cloexec:true () in
  Fun.protect
    ~finally:(fun () -> List.iter Unix.close [ r; w ])
    (fun () -> f r w)

(* The device *)

let test_is_tty () =
  with_pty (fun _ fd -> is_true ~msg:"a pseudo-terminal" (Matrix_tty.is_tty fd));
  with_pipe (fun r _ -> is_false ~msg:"a pipe" (Matrix_tty.is_tty r))

(* A pseudo-terminal the process opened is not the one its job runs in. *)
let test_is_controlling () =
  with_pty (fun _ fd ->
      is_false ~msg:"an opened pseudo-terminal" (Matrix_tty.is_controlling fd));
  with_pipe (fun r _ -> is_false ~msg:"a pipe" (Matrix_tty.is_controlling r))

let size = Testable.(option (pair int int))

(* The window as the system has it, and no guess where it has none. *)
let test_window () =
  with_pty ~winsize:{ rows = 30; cols = 100; xpixel = 0; ypixel = 0 }
    (fun master fd ->
      equal ~msg:"the size set" size (Some (100, 30)) (Matrix_tty.window fd);
      Pty.resize master ~rows:12 ~cols:40;
      equal ~msg:"a resize" size (Some (40, 12)) (Matrix_tty.window fd));
  with_pty ~winsize:{ rows = 0; cols = 0; xpixel = 0; ypixel = 0 } (fun _ fd ->
      equal ~msg:"a size never set" size None (Matrix_tty.window fd));
  with_pipe (fun r _ -> equal ~msg:"a pipe" size None (Matrix_tty.window r))

let test_now () =
  let a = Matrix_tty.now () in
  Unix.sleepf 0.01;
  let b = Matrix_tty.now () in
  is_true ~msg:"10 ms later, by at least 10 ms" (b - a >= 10_000_000)

(* Settings *)

(* [Unix.terminal_io] cannot express IEXTEN: the stub reads it. *)
external get_iexten : Unix.file_descr -> bool = "matrix_tty_get_iexten"

let test_raw_and_restore () =
  with_pty (fun _ fd ->
      (* A fresh line discipline has IEXTEN set: the kernel then takes
         VDISCARD (^O) and VLNEXT (^V) even with ICANON off. *)
      is_true ~msg:"IEXTEN set at first" (get_iexten fd);
      let saved = Matrix_tty.raw fd in
      let t = Unix.tcgetattr fd in
      is_false ~msg:"no echo" t.c_echo;
      is_false ~msg:"no line editing" t.c_icanon;
      is_false ~msg:"no signal characters" t.c_isig;
      is_false ~msg:"no IEXTEN" (get_iexten fd);
      Matrix_tty.restore fd saved;
      let t = Unix.tcgetattr fd in
      is_true ~msg:"echo again" t.c_echo;
      is_true ~msg:"line editing again" t.c_icanon;
      is_true ~msg:"IEXTEN again" (get_iexten fd))

let test_raw_not_a_terminal () =
  with_pipe (fun r _ ->
      match Matrix_tty.raw r with
      | _ -> fail "a pipe has no settings"
      | exception Unix.Unix_error _ -> ())

(* Terminal descriptions *)

let fixture name =
  In_channel.with_open_bin ("data/terminfo/" ^ name) In_channel.input_all

(* The process's environment for [f], each variable set or emptied (an
   empty variable is unset), then put back. *)
let with_env vars f =
  let saved = List.map (fun (v, _) -> (v, Sys.getenv_opt v)) vars in
  List.iter (fun (v, x) -> Unix.putenv v (Option.value x ~default:"")) vars;
  Fun.protect
    ~finally:(fun () ->
      List.iter (fun (v, x) -> Unix.putenv v (Option.value x ~default:"")) saved)
    f

let rec remove path =
  if Sys.is_directory path then (
    Array.iter (fun e -> remove (Filename.concat path e)) (Sys.readdir path);
    Sys.rmdir path)
  else Sys.remove path

(* A database in a fresh directory, with [entries] as (leaf directory,
   name, bytes). *)
let with_database entries f =
  let dir = Filename.temp_dir "matrix-terminfo" "" in
  List.iter
    (fun (leaf, name, bytes) ->
      let d = Filename.concat dir leaf in
      if not (Sys.file_exists d) then Sys.mkdir d 0o755;
      Out_channel.with_open_bin (Filename.concat d name) (fun oc ->
          Out_channel.output_string oc bytes))
    entries;
  Fun.protect ~finally:(fun () -> remove dir) (fun () -> f dir)

let found what = function
  | Ok t -> List.hd (Matrix_terminfo.names t)
  | Error `Not_found -> failf "%s: not found" what
  | Error (`Invalid e) -> failf "%s: invalid: %s" what e

let not_found what = function
  | Error `Not_found -> ()
  | Ok _ -> failf "%s: found" what
  | Error (`Invalid e) -> failf "%s: invalid: %s" what e

let name = "matrix-test-terminal"

(* TERMINFO, then ~/.terminfo, then TERMINFO_DIRS, then the system. *)
let test_terminfo_order () =
  with_database [ ("m", name, fixture "d/dumb") ] @@ fun terminfo ->
  with_database [] @@ fun home ->
  with_database [ ("m", name, fixture "x/xterm") ] @@ fun dirs ->
  let dot = Filename.concat home ".terminfo" in
  Sys.mkdir dot 0o755;
  Sys.mkdir (Filename.concat dot "m") 0o755;
  Out_channel.with_open_bin
    (Filename.concat (Filename.concat dot "m") name)
    (fun oc -> Out_channel.output_string oc (fixture "l/linux"));
  let look vars = with_env vars (fun () -> Matrix_tty.terminfo name) in
  let dirs = ("TERMINFO_DIRS", Some ("/nonexistent::" ^ dirs)) in
  equal ~msg:"TERMINFO first" string "dumb"
    (found "all"
       (look [ ("TERMINFO", Some terminfo); ("HOME", Some home); dirs ]));
  equal ~msg:"then ~/.terminfo" string "linux"
    (found "home" (look [ ("TERMINFO", None); ("HOME", Some home); dirs ]));
  equal ~msg:"then TERMINFO_DIRS" string "xterm"
    (found "dirs" (look [ ("TERMINFO", None); ("HOME", None); dirs ]));
  not_found "none of them"
    (look [ ("TERMINFO", None); ("HOME", None); ("TERMINFO_DIRS", None) ])

(* A database on a filesystem that ignores case names its directories by
   the first character's code. *)
let test_terminfo_hex_directory () =
  with_database [ ("6d", name, fixture "d/dumb") ] @@ fun dir ->
  with_env [ ("TERMINFO", Some dir) ] @@ fun () ->
  equal ~msg:"6d for m" string "dumb" (found "hex" (Matrix_tty.terminfo name))

let hex s =
  String.concat ""
    (List.map
       (fun c -> Printf.sprintf "%02X" (Char.code c))
       (List.of_seq (String.to_seq s)))

(* As tic -Q 2 writes it: [-_] for [+/], no padding. *)
let b64 s =
  let alphabet =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
  in
  let b = Buffer.create 64 and n = String.length s in
  let byte i = if i < n then Char.code s.[i] else 0 in
  let i = ref 0 in
  while !i < n do
    let v = (byte !i lsl 16) lor (byte (!i + 1) lsl 8) lor byte (!i + 2) in
    let chars = if n - !i >= 3 then 4 else n - !i + 1 in
    for k = 0 to chars - 1 do
      Buffer.add_char b alphabet.[(v lsr (18 - (6 * k))) land 63]
    done;
    i := !i + 3
  done;
  Buffer.contents b

(* TERMINFO may hold the description itself, used for a name it has. *)
let test_terminfo_encoded () =
  let dumb = fixture "d/dumb" in
  List.iter
    (fun (form, value) ->
      with_env
        [ ("TERMINFO", Some value); ("HOME", None); ("TERMINFO_DIRS", None) ]
      @@ fun () ->
      equal ~msg:form string "dumb" (found form (Matrix_tty.terminfo "dumb"));
      equal
        ~msg:(form ^ ", by another of its names")
        string "dumb"
        (found form (Matrix_tty.terminfo "80-column dumb tty"));
      not_found (form ^ " for another name") (Matrix_tty.terminfo name))
    [ ("hex", "hex:" ^ hex dumb); ("b64", "b64:" ^ b64 dumb) ]

(* The first description that reads; a broken one is skipped. *)
let test_terminfo_invalid () =
  with_database [ ("m", name, "not an entry") ] @@ fun broken ->
  with_database [ ("m", name, fixture "d/dumb") ] @@ fun good ->
  with_env
    [ ("TERMINFO", Some broken); ("HOME", None); ("TERMINFO_DIRS", Some good) ]
    (fun () ->
      equal ~msg:"the next one" string "dumb"
        (found "skip" (Matrix_tty.terminfo name)));
  with_env
    [ ("TERMINFO", Some broken); ("HOME", None); ("TERMINFO_DIRS", None) ]
    (fun () ->
      match Matrix_tty.terminfo name with
      | Error (`Invalid _) -> ()
      | _ -> fail "only a broken description: invalid")

let test_terminfo_names () =
  List.iter
    (fun n -> not_found n (Matrix_tty.terminfo n))
    [ ""; "."; ".."; "../x/xterm"; "x/xterm" ]

(* Signals *)

module Signal = Matrix_tty.Signal

(* Linux's view of what the process catches, from /proc/self/status:
   [Sys.signal] reads a handler installed in C as the default, and putting
   that back would remove it. *)
let linux = Sys.file_exists "/proc/self/status"

let caught signo =
  let line =
    In_channel.with_open_text "/proc/self/status" In_channel.input_lines
    |> List.find (String.starts_with ~prefix:"SigCgt:")
  in
  let mask =
    Int64.of_string
      ("0x" ^ String.trim (String.sub line 7 (String.length line - 7)))
  in
  Int64.logand mask (Int64.shift_left 1L (signo - 1)) <> 0L

let sighup = 1

let with_handler s f =
  let called = ref 0 in
  let previous = Sys.signal s (Sys.Signal_handle (fun _ -> incr called)) in
  Fun.protect
    ~finally:(fun () -> ignore (Sys.signal s previous : Sys.signal_behavior))
    (fun () -> f called)

let with_holder ?output ~controlling f =
  let h = Signal.hold ~controlling ~output in
  Fun.protect ~finally:(fun () -> Signal.release h) (fun () -> f h)

let readable_now fd =
  match Unix.select [ fd ] [] [] 0. with [], _, _ -> false | _ -> true

let readable fd =
  match Unix.select [ fd ] [] [] 1. with [], _, _ -> false | _ -> true

let drain fd =
  let buf = Bytes.create 64 in
  let rec go () =
    match Unix.read fd buf 0 64 with
    | 0 -> ()
    | _ -> go ()
    | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) -> ()
  in
  go ()

(* Which signals a holder takes: the window and job control only on the
   controlling terminal. *)
let test_signals () =
  let names = List.map string_of_int in
  let ending =
    [ Sys.sigabrt; Sys.sigint; Sys.sigterm; Sys.sighup; Sys.sigquit ]
  in
  equal ~msg:"another terminal" (list string) (names ending)
    (names (Signal.signals ~controlling:false));
  equal ~msg:"the controlling terminal" (list string)
    (names ([ Sys.sigwinch; Sys.sigcont; Sys.sigtstp ] @ ending))
    (names (Signal.signals ~controlling:true))

(* A signal that ends the process is taken only at its default action, and
   given back to it. *)
let test_take_at_default () =
  if linux && not (caught sighup) then (
    is_true ~msg:"taken at its default" (Signal.take Sys.sighup);
    is_true ~msg:"caught" (caught sighup);
    is_true ~msg:"a second holder takes it too" (Signal.take Sys.sighup);
    Signal.give_back Sys.sighup;
    is_true ~msg:"still caught for the other" (caught sighup);
    Signal.give_back Sys.sighup;
    is_false ~msg:"the default again" (caught sighup);
    with_handler Sys.sighup (fun _ ->
        is_false ~msg:"the program's own action is kept"
          (Signal.take Sys.sighup)))

(* SIGWINCH is taken whatever its action, and the handler installed before
   is still called. *)
let test_take_always () =
  if Sys.unix then
    with_handler Sys.sigwinch (fun called ->
        with_holder ~controlling:true (fun h ->
            is_true ~msg:"taken over a handler" (Signal.take Sys.sigwinch);
            Unix.kill (Unix.getpid ()) Sys.sigwinch;
            is_true ~msg:"the holder is woken" (readable (Signal.wake_fd h));
            drain (Signal.wake_fd h);
            is_true ~msg:"received" (Signal.received h Sys.sigwinch);
            is_false ~msg:"received once" (Signal.received h Sys.sigwinch);
            Signal.give_back Sys.sigwinch;
            (* The OCaml handler runs at the next safe point. *)
            Unix.sleepf 0.01;
            equal ~msg:"the handler before was called" int 1 !called))

let test_take_outside_the_policy () =
  if Sys.unix then
    match Signal.take Sys.sigusr1 with
    | _ -> fail "SIGUSR1 is no terminal's signal"
    | exception Invalid_argument _ -> ()

(* The window signal concerns the controlling terminal's holders alone; a
   signal that ends the process concerns every holder, and says so. *)
let test_holders_concerned () =
  if linux && not (caught sighup) then
    with_holder ~controlling:false (fun served ->
        with_holder ~controlling:true (fun own ->
            ignore (Signal.take Sys.sigwinch : bool);
            ignore (Signal.take Sys.sighup : bool);
            Fun.protect
              ~finally:(fun () ->
                Signal.give_back Sys.sigwinch;
                Signal.give_back Sys.sighup)
              (fun () ->
                Unix.kill (Unix.getpid ()) Sys.sigwinch;
                is_true ~msg:"woken" (readable (Signal.wake_fd own));
                is_true ~msg:"the controlling terminal's"
                  (Signal.received own Sys.sigwinch);
                is_false ~msg:"not a served one's"
                  (Signal.received served Sys.sigwinch);
                Unix.kill (Unix.getpid ()) Sys.sighup;
                is_true ~msg:"woken" (readable (Signal.wake_fd served));
                is_true ~msg:"terminating" (Signal.terminating served);
                is_true ~msg:"received by every holder"
                  (Signal.received served Sys.sighup
                  && Signal.received own Sys.sighup);
                is_false ~msg:"taken" (Signal.terminating served))))

let test_wake () =
  if Sys.unix then
    with_holder ~controlling:false (fun h ->
        is_false ~msg:"quiet" (readable_now (Signal.wake_fd h));
        Signal.wake h;
        is_true ~msg:"woken" (readable (Signal.wake_fd h)))

(* A signal that ends the process cuts a write the terminal is not taking
   for good: the write returns, the rest unwritten. *)
let test_write_cut_by_termination () =
  if linux && not (caught sighup) then
    with_holder ~controlling:false (fun h ->
        with_pipe (fun r w ->
            ignore (Signal.take Sys.sighup : bool);
            let mask = Thread.sigmask SIG_BLOCK [ Sys.sighup ] in
            Fun.protect
              ~finally:(fun () ->
                ignore (Thread.sigmask SIG_SETMASK mask : int list);
                Signal.give_back Sys.sighup)
              (fun () ->
                let n = 200_000 in
                let writer =
                  Thread.create
                    (fun () ->
                      ignore
                        (Thread.sigmask SIG_UNBLOCK [ Sys.sighup ] : int list);
                      Signal.write h w (Bytes.make n 'x') 0 n)
                    ()
                in
                Thread.delay 0.2;
                Unix.kill (Unix.getpid ()) Sys.sighup;
                Thread.join writer;
                Unix.set_nonblock r;
                let got = ref 0 and buf = Bytes.create 65536 in
                (try
                   while true do
                     got := !got + Unix.read r buf 0 65536
                   done
                 with Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) -> ());
                equal ~msg:"what the pipe took, and no more" int 65536 !got)))

(* What an abort writes and puts back, for a holder whose process ends where
   no OCaml code runs. *)
let test_restore_all () =
  if Sys.unix then
    with_pty (fun _ input ->
        with_pipe (fun r output ->
            with_holder ~output ~controlling:false (fun h ->
                Signal.keep_settings h ~input;
                let _ = Matrix_tty.raw input in
                Signal.set_reset h "\027[?25h";
                Signal.restore_all ();
                let buf = Bytes.create 64 in
                let n = Unix.read r buf 0 64 in
                equal ~msg:"the reset bytes" string "\027[?25h"
                  (Bytes.sub_string buf 0 n);
                is_true ~msg:"cooked again" (Unix.tcgetattr input).c_icanon;
                Signal.set_reset h "";
                Signal.forget_settings h)))

(* Reset bytes have no length but the terminal's: a holder's teardown can
   be as long as what it set. *)
let test_restore_all_long () =
  if Sys.unix then
    with_pty (fun _ input ->
        with_pipe (fun r output ->
            with_holder ~output ~controlling:false (fun h ->
                Signal.keep_settings h ~input;
                let reset =
                  String.init 20_000 (fun i -> Char.chr (65 + (i mod 26)))
                in
                Signal.set_reset h reset;
                Signal.set_reset h (reset ^ reset);
                Signal.restore_all ();
                let got = Buffer.create 40_000 and buf = Bytes.create 65536 in
                while Buffer.length got < 40_000 do
                  let n = Unix.read r buf 0 65536 in
                  Buffer.add_subbytes got buf 0 n
                done;
                equal ~msg:"the last reset bytes, whole" string (reset ^ reset)
                  (Buffer.contents got);
                Signal.set_reset h "")))

let () =
  run "matrix.tty"
    [
      group "device"
        [
          test "is_tty" test_is_tty;
          test "is_controlling" test_is_controlling;
          test "window" test_window;
          test "now" test_now;
        ];
      group "settings"
        [
          test "raw and restore" test_raw_and_restore;
          test "raw on a pipe" test_raw_not_a_terminal;
        ];
      group "descriptions"
        [
          test "the order of the databases" test_terminfo_order;
          test "hexadecimal directories" test_terminfo_hex_directory;
          test "a description in TERMINFO" test_terminfo_encoded;
          test "a broken description" test_terminfo_invalid;
          test "names that are no file" test_terminfo_names;
        ];
      group "signals"
        [
          test "which a holder takes" test_signals;
          test "taken at the default" test_take_at_default;
          test "taken over a handler" test_take_always;
          test "outside the policy" test_take_outside_the_policy;
          test "the holders a signal concerns" test_holders_concerned;
          test "wake" test_wake;
          test "a write cut by termination" test_write_cut_by_termination;
          test "restore_all" test_restore_all;
          test "restore_all with long reset bytes" test_restore_all_long;
        ];
    ]
