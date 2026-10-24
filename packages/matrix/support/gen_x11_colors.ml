(* Generate the X11 color names table for matrix.input.

   Reads X.Org's rgb.txt, whose lines are "red green blue<TAB>name", and
   writes each name lowercased and without spaces, in increasing order, with
   its color. Xlib looks a name up so: "Color strings are case-insensitive"
   (Xlib, ch. 6), and Xcms removes the spaces of a name before it looks it up
   (libX11, src/xcms/cmsColNm.c, "Now, remove spaces."). rgb.txt lists names
   with spaces beside their forms without, such as "ghost white" and
   "GhostWhite", for the same color, so no two names collide once
   normalized; the generator checks it.

   Usage: dune exec packages/matrix/support/gen_x11_colors.exe -- \
       RGB_TXT VERSION SHA256
     > packages/matrix/lib/input/x11_colors.ml *)

let normalize name =
  String.lowercase_ascii name
  |> String.to_seq
  |> Seq.filter (fun c -> not (String.contains " \t\n\011\012\r" c))
  |> String.of_seq

let read path =
  let ic = open_in_bin path in
  let rec go acc =
    match input_line ic with
    | exception End_of_file ->
        close_in ic;
        acc
    | line when String.length line = 0 || line.[0] = '!' -> go acc
    | line -> (
        match
          Scanf.sscanf line " %d %d %d %[^\n]" (fun r g b n -> (r, g, b, n))
        with
        | r, g, b, name ->
            go
              ((normalize (String.trim name), (r lsl 16) lor (g lsl 8) lor b)
              :: acc))
  in
  go []

let () =
  match Sys.argv with
  | [| _; path; version; sha256 |] ->
      let table = Hashtbl.create 1024 in
      List.iter
        (fun (name, rgb) ->
          match Hashtbl.find_opt table name with
          | Some rgb' when rgb' <> rgb ->
              failwith (Printf.sprintf "%s names two colors" name)
          | _ -> Hashtbl.replace table name rgb)
        (read path);
      let entries =
        List.sort compare (Hashtbl.fold (fun n c acc -> (n, c) :: acc) table [])
      in
      Printf.printf
        "(* WARNING: Do not edit. This file was automatically generated.\n\n\
        \   X.Org's rgb.txt, rgb %s:\n\
        \   https://gitlab.freedesktop.org/xorg/app/rgb/-/raw/rgb-%s/rgb.txt\n\
        \   sha256 %s\n\
        \   Generated using packages/matrix/support/gen_x11_colors.ml\n\
         *)\n\n\
         [@@@ocamlformat \"disable\"]\n\n\
         let names = [|\n"
        version version sha256;
      List.iter (fun (n, _) -> Printf.printf "  %S;\n" n) entries;
      print_string "|]\n\nlet rgb = [|\n";
      List.iter (fun (_, c) -> Printf.printf "  0x%06x;\n" c) entries;
      print_string "|]\n"
  | _ ->
      prerr_endline "usage: gen_x11_colors RGB_TXT VERSION SHA256";
      exit 2
