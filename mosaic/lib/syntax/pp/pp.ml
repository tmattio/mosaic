let file = try Array.get Sys.argv 1 with _ -> failwith "You must pass a path"

let () =
  let json = Yojson.Basic.from_file file in
  let yojson = Yojson.Basic.show json in
  print_endline yojson
