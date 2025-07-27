open Mosaic

let ocaml_code =
  {|
(* OCaml syntax highlighting demo *)
let rec factorial n =
  if n <= 1 then 1
  else n * factorial (n - 1)

type 'a tree =
  | Leaf of 'a
  | Node of 'a tree * 'a * 'a tree

let rec sum_tree = function
  | Leaf x -> x
  | Node (left, x, right) -> sum_tree left + x + sum_tree right
|}

let dune_code =
  {|
(library
 (name my_lib)
 (public_name myproject.lib)
 (libraries base stdio)
 (preprocess
  (pps ppx_jane)))

(executable
 (public_name myproject)
 (name main)
 (libraries my_lib))
|}

let shell_code =
  {|
#!/bin/bash
# Shell script demo

echo "Building project..."
dune build @all

if [ $? -eq 0 ]; then
    echo "Build successful!"
    ./myproject.exe --help
else
    echo "Build failed!" >&2
    exit 1
fi
|}

let diff_code =
  {|
--- a/src/main.ml
+++ b/src/main.ml
@@ -1,5 +1,6 @@
 let () =
   print_endline "Hello, World!";
-  let result = factorial 5 in
+  (* Calculate factorial of 10 instead *)
+  let result = factorial 10 in
   Printf.printf "Result: %d\n" result
|}

let render_example ~title ~(lang : Mosaic_syntax.lang) ~code =
  let open Ui in
  let lang_str =
    match lang with
    | `OCaml -> "OCaml"
    | `OCaml_interface -> "OCaml Interface"
    | `Dune -> "Dune"
    | `Shell -> "Shell"
    | `Diff -> "Diff"
    | `Custom _ -> "Custom"
  in
  vbox
    [
      hbox
        [
          text ~style:Style.(fg Blue ++ bold) (title ^ " (");
          text ~style:Style.(fg Yellow) lang_str;
          text ~style:Style.(fg Blue ++ bold) ")";
        ];
      text "";
      (match Mosaic_syntax.highlight ~theme:`Solarized_dark ~lang code with
      | Ok element -> element
      | Error (`Unknown_lang l) ->
          text ~style:Style.(fg Red) (Printf.sprintf "Unknown language: %s" l));
      text "";
      text "";
    ]

let main () =
  let open Ui in
  let examples =
    vbox
      [
        text
          ~style:Style.(fg Cyan ++ bold ++ underline)
          "Mosaic Syntax Highlighting Demo";
        text "";
        render_example ~title:"OCaml Example" ~lang:`OCaml ~code:ocaml_code;
        render_example ~title:"Dune Example" ~lang:`Dune ~code:dune_code;
        render_example ~title:"Shell Example" ~lang:`Shell ~code:shell_code;
        render_example ~title:"Diff Example" ~lang:`Diff ~code:diff_code;
      ]
  in

  (* Print to terminal *)
  Ui.print examples

let () = main ()
