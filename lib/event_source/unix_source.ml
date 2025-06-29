type t = {
  terminal : Terminal.t;
  parser : Input.parser;
  mutable event_queue : Input.event list;
}

let create terminal = { terminal; parser = Input.create (); event_queue = [] }

let rec read t ~timeout =
  (* First check if we have queued events *)
  match t.event_queue with
  | event :: rest ->
      t.event_queue <- rest;
      `Event event
  | [] -> (
      (* No queued events, read more input *)
      match Terminal.read_input t.terminal ~timeout with
      | `Input (bytes, n) -> (
          let events = Input.feed t.parser bytes 0 n in
          match events with
          | [] ->
              (* No complete events yet, try reading more *)
              read t ~timeout:(Some 0.0)
              (* Non-blocking check *)
          | event :: rest ->
              t.event_queue <- rest;
              `Event event)
      | `Timeout -> `Timeout
      | `Eof -> `Eof)
