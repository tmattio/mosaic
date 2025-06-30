type t = Unix of Unix_source.t | Windows of Windows_source.t

let is_windows = Sys.os_type = "Win32"

let create ?(mouse = false) terminal =
  if is_windows && Terminal.is_tty (Terminal.input_fd terminal) then
    Windows (Windows_source.create ~mouse terminal)
  else
    (* Unix source doesn't need the mouse flag - mouse is controlled via terminal escape codes *)
    Unix (Unix_source.create terminal)

let read t ~sw ~clock ~timeout =
  match t with
  | Unix u -> Unix_source.read u ~sw ~clock ~timeout
  | Windows w -> Windows_source.read w ~sw ~clock ~timeout
