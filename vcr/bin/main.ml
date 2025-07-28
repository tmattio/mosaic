open Cmdliner

let run_tape input_file output_file =
  (* Read from stdin if input_file is "-" or use the file *)
  let ic, should_close =
    if input_file = "-" then (stdin, false)
    else (
      Printf.printf "File: %s\n" input_file;
      (open_in input_file, true))
  in
  let result =
    Fun.protect
      ~finally:(fun () -> if should_close then close_in_noerr ic)
      (fun () -> Tape_lang.from_channel ic)
  in
  match result with
  | Error msg -> `Error (false, "Failed to parse tape file: " ^ msg)
  | Ok tape ->
      (* Extract output path from tape if Output command is present *)
      let output_path =
        match
          List.find_opt
            (function Tape_lang.Ast.Output _ -> true | _ -> false)
            tape
        with
        | Some (Tape_lang.Ast.Output path) ->
            if input_file <> "-" then Printf.printf "Output %s\n" path;
            Some path
        | _ ->
            (* Only use output if explicitly provided via -o flag *)
            if output_file = "" then
              None  (* No output by default, matching VHS behavior *)
            else
              let path = output_file in
              if input_file <> "-" && path <> "-" then
                Printf.printf "Output %s\n" path;
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
      match output_path with
      | Some path ->
          if input_file <> "-" && path <> "-" then
            Printf.printf "Creating %s...\n" path;
          Vcr.run tape (Some path);
          `Ok ()
      | None ->
          (* No output requested, just execute the tape for side effects like screenshots *)
          Vcr.run tape None;
          `Ok ()

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

let cmd =
  let doc = "Generate terminal recordings from a declarative script." in
  let info = Cmd.info "vcr" ~doc in
  Cmd.v info Term.(ret (const run_tape $ input_arg $ output_arg))

let () = exit (Cmd.eval cmd)
