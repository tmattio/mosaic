(* The examples that run a program from another event loop, each in a
   pseudo-terminal that answers the queries a terminal answers: the program
   shows its title, ends on [q], and gives the terminal back; one that takes
   [--for] also ends when its event loop cancels it, and gives the terminal
   back before the program writes again.

   The example is $EXAMPLE, its title $TITLE, and $CANCEL is set for one
   that takes [--for]. *)

open Windtrap

type session = { pty : Pty.t; pid : int; output : Buffer.t }

let find ?(from = 0) haystack sub =
  let n = String.length sub in
  let rec go i =
    if i + n > String.length haystack then None
    else if String.sub haystack i n = sub then Some i
    else go (i + 1)
  in
  go from

let send s bytes =
  ignore (Pty.write_string s.pty bytes 0 (String.length bytes) : int)

(* Reads what is ready, answering device attributes and cursor queries. *)
let pump s =
  let buf = Bytes.create 65536 in
  match Unix.select [ Pty.in_fd s.pty ] [] [] 0.01 with
  | [], _, _ -> ()
  | _ -> (
      match Pty.read s.pty buf 0 65536 with
      | n when n > 0 ->
          let data = Bytes.sub_string buf 0 n in
          Buffer.add_string s.output data;
          let count sub =
            let rec go i acc =
              match find ~from:i data sub with
              | Some j -> go (j + 1) (acc + 1)
              | None -> acc
            in
            go 0 0
          in
          for _ = 1 to count "\027[6n" do
            send s "\027[1;1R"
          done;
          for _ = 1 to count "\027[c" do
            send s "\027[?62;22c"
          done
      | _ -> ()
      | exception Unix.Unix_error _ -> ())

(* The timeouts guard against a program that never does what is waited for;
   they measure nothing. *)
let await ?(from = 0) s sub =
  let deadline = Unix.gettimeofday () +. 60. in
  let rec go () =
    match find ~from (Buffer.contents s.output) sub with
    | Some i -> i
    | None ->
        if Unix.gettimeofday () > deadline then
          failf "timed out waiting for %S in %S" sub (Buffer.contents s.output)
        else (
          pump s;
          go ())
  in
  go ()

let wait_exit s =
  let deadline = Unix.gettimeofday () +. 60. in
  let rec go () =
    match Unix.waitpid [ Unix.WNOHANG ] s.pid with
    | 0, _ ->
        if Unix.gettimeofday () > deadline then (
          Unix.kill s.pid Sys.sigkill;
          fail "the program did not end")
        else (
          pump s;
          go ())
    | _, status ->
        for _ = 1 to 5 do
          pump s
        done;
        status
  in
  go ()

let cooked s =
  let t = Unix.tcgetattr (Pty.file_descr s.pty) in
  t.c_icanon && t.c_echo && t.c_isig

let session exe args f =
  let pty =
    Pty.spawn ~prog:exe ~args
      ~winsize:Pty.{ rows = 24; cols = 80; xpixel = 0; ypixel = 0 }
      ()
  in
  Pty.set_nonblock pty;
  let s =
    { pty; pid = Option.get (Pty.pid pty); output = Buffer.create 4096 }
  in
  Fun.protect
    ~finally:(fun () -> try Pty.close pty with _ -> ())
    (fun () -> f s)

(* The terminal is given back: cooked, the alternate screen left after it
   was entered. *)
let given_back s =
  is_true ~msg:"cooked" (cooked s);
  let out = Buffer.contents s.output in
  match (find out "\027[?1049h", find out "\027[?1049l") with
  | Some entered, Some left ->
      is_true ~msg:"the alternate screen left after it was entered"
        (left > entered)
  | _ -> fail "the alternate screen was never entered and left"

let quits exe title () =
  session exe [] @@ fun s ->
  ignore (await s title : int);
  send s "q";
  (match wait_exit s with
  | WEXITED 0 -> ()
  | _ -> fail "expected a normal exit");
  given_back s

let cancelled exe title () =
  session exe [ "--for"; "1" ] @@ fun s ->
  ignore (await s title : int);
  (match wait_exit s with
  | WEXITED 0 -> ()
  | _ -> fail "expected a normal exit");
  given_back s;
  let out = Buffer.contents s.output in
  match
    (find out "\027[?1049l", find out "The terminal is the shell's again.")
  with
  | Some left, Some line ->
      is_true ~msg:"the program writes once the terminal is given back"
        (line > left)
  | _, None -> ()
  | None, _ -> fail "the terminal was not given back"

let () =
  let exe = Sys.getenv "EXAMPLE" and title = Sys.getenv "TITLE" in
  run
    ("mosaic.examples " ^ Filename.basename (Filename.dirname exe))
    [
      group "Example"
        (test "quits on q" (quits exe title)
        ::
        (if Sys.getenv_opt "CANCEL" = None then []
         else [ test "cancelled by its event loop" (cancelled exe title) ]));
    ]
