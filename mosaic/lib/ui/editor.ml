type t = {
  buffer : Edit_buffer.t;
  revision : unit ref;
  base : unit ref option;
  preferred_col : int option;
}

let of_string ?(max_length = max_int) text =
  {
    buffer = Edit_buffer.create ~max_length text;
    revision = ref ();
    base = None;
    preferred_col = None;
  }

let empty = of_string ""
let text t = Edit_buffer.text t.buffer
let length t = Edit_buffer.length t.buffer
let cursor t = Edit_buffer.cursor t.buffer
let caret t = Edit_buffer.caret t.buffer
let selection t = Edit_buffer.selection t.buffer
let selected_text t = Edit_buffer.selected_text t.buffer
let source t = Edit_buffer.source t.buffer

let accept ~current proposal =
  match proposal.base with
  | Some base when base != current.revision -> current
  | _ -> proposal

let changed ~base buffer preferred_col =
  if Edit_buffer.same base.buffer buffer && base.preferred_col = preferred_col
  then base
  else begin
    (* Undo restores a string snapshot. Index it once before publishing so
       every subsequent cursor-only branch shares the restored source. *)
    ignore (Edit_buffer.source buffer : Matrix.Text.Layout.Source.t);
    { buffer; revision = ref (); base = Some base.revision; preferred_col }
  end

let transform f t =
  let buffer = Edit_buffer.copy t.buffer in
  f buffer;
  changed ~base:t buffer None

let insert text =
  transform (fun buffer -> ignore (Edit_buffer.insert buffer text : bool))

let clear = transform (fun buffer -> ignore (Edit_buffer.clear buffer : bool))

let set_cursor ?(select = false) pos =
  transform (fun buffer -> Edit_buffer.set_cursor_offset ~select buffer pos)

let set_caret ?(select = false) caret =
  transform (fun buffer -> Edit_buffer.set_caret ~select buffer caret)

let select ~anchor ~cursor =
  transform (fun buffer ->
      Edit_buffer.set_cursor buffer anchor;
      Edit_buffer.set_cursor_offset ~select:true buffer cursor)

let clear_selection = transform Edit_buffer.clear_selection
let select_all = transform Edit_buffer.select_all

let delete_backward =
  transform (fun buffer -> ignore (Edit_buffer.delete_backward buffer : bool))

let delete_forward =
  transform (fun buffer -> ignore (Edit_buffer.delete_forward buffer : bool))

let delete_word_backward =
  transform (fun buffer ->
      ignore (Edit_buffer.delete_word_backward buffer : bool))

let delete_word_forward =
  transform (fun buffer ->
      ignore (Edit_buffer.delete_word_forward buffer : bool))

let delete_to_line_start =
  transform (fun buffer ->
      ignore (Edit_buffer.delete_to_line_start buffer : bool))

let delete_to_line_end =
  transform (fun buffer ->
      ignore (Edit_buffer.delete_to_line_end buffer : bool))

let delete_line =
  transform (fun buffer -> ignore (Edit_buffer.delete_line buffer : bool))

let undo = transform (fun buffer -> ignore (Edit_buffer.undo buffer : bool))
let redo = transform (fun buffer -> ignore (Edit_buffer.redo buffer : bool))

let move_left ?(select = false) =
  transform (fun buffer -> ignore (Edit_buffer.move_left ~select buffer : bool))

let move_right ?(select = false) =
  transform (fun buffer ->
      ignore (Edit_buffer.move_right ~select buffer : bool))

let move_word_backward ?(select = false) =
  transform (fun buffer ->
      ignore (Edit_buffer.move_word_backward ~select buffer : bool))

let move_word_forward ?(select = false) =
  transform (fun buffer ->
      ignore (Edit_buffer.move_word_forward ~select buffer : bool))

let move_home ?(select = false) =
  transform (fun buffer -> ignore (Edit_buffer.move_home ~select buffer : bool))

let move_end ?(select = false) =
  transform (fun buffer -> ignore (Edit_buffer.move_end ~select buffer : bool))

let move_line_start ?(select = false) =
  transform (fun buffer ->
      ignore (Edit_buffer.move_line_start ~select buffer : bool))

let move_line_end ?(select = false) =
  transform (fun buffer ->
      ignore (Edit_buffer.move_line_end ~select buffer : bool))

module Private = struct
  let buffer t = Edit_buffer.copy t.buffer
  let preferred_col t = t.preferred_col

  let propose ~base ~preferred_col buffer =
    if Edit_buffer.same base.buffer buffer && base.preferred_col = preferred_col
    then base
    else
      (* Never publish the mutable scratch buffer retained by a widget. *)
      changed ~base (Edit_buffer.copy buffer) preferred_col
end
