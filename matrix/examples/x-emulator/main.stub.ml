(* Built in place of main.sdl.ml when tsdl or tsdl-ttf is not installed. *)

let () =
  prerr_endline "x-emulator needs the tsdl and tsdl-ttf libraries.";
  prerr_endline "Install them with: opam install tsdl tsdl-ttf";
  exit 1
