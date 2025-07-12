type 'msg action = Handle of 'msg | Pass_through | Insert

type 'msg config = {
  bindings : (Mosaic.Input.key_event * 'msg action) list;
  default : 'msg action;
}

let default_config = { bindings = []; default = Insert }

let pass_through_ctrl_keys config keys =
  let ctrl_bindings =
    List.map
      (fun c ->
        ( {
            Mosaic.Input.key = Char (Uchar.of_char c);
            modifier = { ctrl = true; alt = false; shift = false };
          },
          Pass_through ))
      keys
  in
  { config with bindings = config.bindings @ ctrl_bindings }

let key_event_equal e1 e2 =
  e1.Mosaic.Input.key = e2.Mosaic.Input.key
  && e1.modifier.ctrl = e2.modifier.ctrl
  && e1.modifier.alt = e2.modifier.alt
  && e1.modifier.shift = e2.modifier.shift

let find_binding event config =
  List.find_opt
    (fun (key_event, _) -> key_event_equal key_event event)
    config.bindings
