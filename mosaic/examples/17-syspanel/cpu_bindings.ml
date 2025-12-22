(** OCaml bindings for macOS CPU statistics using host_statistics *)

external get_cpu_load : unit -> int64 * int64 * int64 * int64 = "ocaml_get_cpu_load"

let get_cpu_load () = get_cpu_load ()

