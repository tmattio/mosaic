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
    ]
