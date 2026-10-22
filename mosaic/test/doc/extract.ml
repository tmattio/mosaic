(* [extract file name] prints the OCaml code blocks of the Markdown [file] as
   one compilation unit. Each block becomes a module [Block_n], where [n] is
   its line in [file], and starts with a line directive naming [name], so a
   type error points into the document. *)

let () =
  let file = Sys.argv.(1) and name = Sys.argv.(2) in
  let lines = In_channel.with_open_text file In_channel.input_lines in
  print_string "include Doc_prelude\n";
  let rec scan number block = function
    | [] -> if block > 0 then failwith (name ^ ": unterminated code block")
    | line :: rest ->
        let trimmed = String.trim line in
        if block = 0 && String.equal trimmed "```ocaml" then begin
          let block = number + 1 in
          Printf.printf "\nmodule Block_%d = struct\n# %d %S\n" block
            (number + 2) name;
          scan (number + 1) block rest
        end
        else if block > 0 && String.equal trimmed "```" then begin
          print_string "end\n";
          scan (number + 1) 0 rest
        end
        else begin
          if block > 0 then print_endline line;
          scan (number + 1) block rest
        end
  in
  scan 0 0 lines
