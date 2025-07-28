open Cmdliner

(* Set up logging *)
let setup_logs style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let run_tape input_file output_file =
  let start_time = Unix.gettimeofday () in
  (* Read from stdin if input_file is "-" or use the file *)
  let ic, should_close =
    if input_file = "-" then (stdin, false)
    else (
      Printf.printf "File: %s\n" input_file;
      flush stdout;
      (open_in input_file, true))
  in
  let result =
    Fun.protect
      ~finally:(fun () -> if should_close then close_in_noerr ic)
      (fun () -> Tape_lang.from_channel ic)
  in
  let parse_time = Unix.gettimeofday () in
  Printf.eprintf "[TIMING] Parse tape: %.3fs\n" (parse_time -. start_time);
  match result with
  | Error msg -> `Error (false, "Failed to parse tape file: " ^ msg)
  | Ok tape -> (
      (* Extract output path from tape if Output command is present *)
      let output_path =
        match
          List.find_opt
            (function Tape_lang.Ast.Output _ -> true | _ -> false)
            tape
        with
        | Some (Tape_lang.Ast.Output path) ->
            if input_file <> "-" then (
              Printf.printf "Output %s\n" path;
              flush stdout);
            Some path
        | _ ->
            (* Only use output if explicitly provided via -o flag *)
            if output_file = "" then None
              (* No output by default, matching VHS behavior *)
            else
              let path = output_file in
              if input_file <> "-" && path <> "-" then (
                Printf.printf "Output %s\n" path;
                flush stdout);
              Some path
      in
      (* Print all settings and commands from the tape only if not reading from stdin *)
      if input_file <> "-" then
        List.iter
          (function
            | Tape_lang.Ast.Require cmd -> Printf.printf "Require %s\n" cmd
            | Tape_lang.Ast.Set (Tape_lang.Ast.Shell, Tape_lang.Ast.String s) ->
                Printf.printf "Set Shell %s\n" s
            | Tape_lang.Ast.Set (Tape_lang.Ast.FontSize, Tape_lang.Ast.Float f)
              ->
                Printf.printf "Set FontSize %d\n" (int_of_float f)
            | Tape_lang.Ast.Set (Tape_lang.Ast.Width, Tape_lang.Ast.Float f) ->
                Printf.printf "Set Width %d\n" (int_of_float f)
            | Tape_lang.Ast.Set (Tape_lang.Ast.Height, Tape_lang.Ast.Float f) ->
                Printf.printf "Set Height %d\n" (int_of_float f)
            | Tape_lang.Ast.Type { text; _ } -> Printf.printf "Type %s\n" text
            | Tape_lang.Ast.Sleep t -> Printf.printf "Sleep %gms\n" (t *. 1000.0)
            | Tape_lang.Ast.KeyPress { key = Tape_lang.Ast.Enter; count; _ } ->
                Printf.printf "Enter %d\n" count
            | _ -> ())
          tape;
      flush stdout;
      match output_path with
      | Some path ->
          if input_file <> "-" && path <> "-" then (
            Printf.printf "Creating %s...\n" path;
            flush stdout);
          let run_start = Unix.gettimeofday () in
          Vcr.run tape (Some path);
          let run_end = Unix.gettimeofday () in
          Printf.eprintf "[TIMING] Total Vcr.run: %.3fs\n" (run_end -. run_start);
          Printf.eprintf "[TIMING] Total execution: %.3fs\n"
            (run_end -. start_time);
          `Ok ()
      | None ->
          (* No output requested, just execute the tape for side effects like screenshots *)
          let run_start = Unix.gettimeofday () in
          Vcr.run tape None;
          let run_end = Unix.gettimeofday () in
          Printf.eprintf "[TIMING] Total Vcr.run: %.3fs\n" (run_end -. run_start);
          Printf.eprintf "[TIMING] Total execution: %.3fs\n"
            (run_end -. start_time);
          `Ok ())

let input_arg =
  let doc = "The input .tape file to execute. Use '-' to read from stdin." in
  Arg.(value & pos 0 string "-" & info [] ~docv:"INPUT" ~doc)

let output_arg =
  let doc =
    "The path for the output file. If not specified and no Output command in \
     the tape file, no output will be generated (matching VHS behavior). Use \
     '-' to write to stdout."
  in
  Arg.(value & opt string "" & info [ "o"; "output" ] ~docv:"OUTPUT" ~doc)

let setup_logs_arg =
  let env = Cmd.Env.info "VCR_VERBOSITY" in
  Logs_cli.level ~docs:Cmdliner.Manpage.s_common_options ~env ()

let cmd =
  let doc = "Generate terminal recordings from a declarative script." in
  let info = Cmd.info "vcr" ~doc in
  let term =
    Term.(
      ret
        (const (fun level output input ->
             setup_logs None level;
             run_tape input output)
        $ setup_logs_arg $ output_arg $ input_arg))
  in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
