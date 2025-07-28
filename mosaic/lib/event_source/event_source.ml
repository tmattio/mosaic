type t = Input.event Eio.Stream.t

let is_windows = Sys.os_type = "Win32"

let create ~sw ~env ?(mouse = false) ?(paste_threshold = 0.01)
    ?(paste_min_chars = 3) terminal =
  if is_windows then
    Windows_source.create ~sw ~env ~mouse ~paste_threshold ~paste_min_chars
      terminal
  else Unix_source.create ~sw ~env ~mouse terminal

let read t ~clock ~timeout =
  match timeout with
  | None -> `Event (Eio.Stream.take t)
  | Some s -> (
      try
        Eio.Time.with_timeout_exn clock s (fun () -> `Event (Eio.Stream.take t))
      with Eio.Time.Timeout -> `Timeout)
