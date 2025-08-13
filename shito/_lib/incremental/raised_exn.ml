type t =
  { exn : exn
  ; backtrace : Printexc.raw_backtrace option
  }

let create exn = 
  { exn
  ; backtrace = 
      if Printexc.backtrace_status () then 
        Some (Printexc.get_raw_backtrace ()) 
      else 
        None 
  }

let reraise_with_message { exn; backtrace } msg =
  let new_exn = Failure (msg ^ ": " ^ Printexc.to_string exn) in
  match backtrace with
  | Some bt -> Printexc.raise_with_backtrace new_exn bt
  | None -> raise new_exn

let reraise { exn; backtrace } = 
  match backtrace with
  | Some bt -> Printexc.raise_with_backtrace exn bt
  | None -> raise exn
