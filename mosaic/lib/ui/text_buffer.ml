module Source = Matrix.Text.Layout.Source

(* ───── Spans ───── *)

type span = { text : string; style : Ansi.Style.t }

(* ───── Highlights ───── *)

module Highlight = struct
  type t = {
    start_offset : int;
    end_offset : int;
    style : Ansi.Style.t;
    priority : int;
    ref_id : int;
  }

  let make ~start_offset ~end_offset ~style ?(priority = 0) ~ref_id () =
    { start_offset; end_offset; style; priority; ref_id }

  let start_offset h = h.start_offset
  let end_offset h = h.end_offset
  let style h = h.style
  let priority h = h.priority
  let ref_id h = h.ref_id
end

(* ───── Buffer ───── *)

type t = {
  mutable spans : span list;
  mutable spans_rev : bool;
  mutable default_style : Ansi.Style.t;
  mutable highlights : Highlight.t list;
  mutable tab_width : int;
  mutable width_method : Matrix.Text.width_method;
  mutable cached_plain_text : string option;
  mutable cached_max_line_width : int option;
  mutable source : Source.t option;
  mutable paint : (int * int * Ansi.Style.t) array option;
  mutable version : int;
}

let create ?(default_style = Ansi.Style.default) ?(width_method = `Unicode)
    ?(tab_width = 2) () =
  {
    spans = [];
    spans_rev = false;
    default_style;
    highlights = [];
    tab_width = max 1 tab_width;
    width_method;
    cached_plain_text = None;
    cached_max_line_width = None;
    source = None;
    paint = None;
    version = 0;
  }

(* ───── Invalidation ───── *)

let invalidate t =
  t.cached_plain_text <- None;
  t.cached_max_line_width <- None;
  t.source <- None;
  t.paint <- None;
  t.version <- t.version + 1

(* ───── Span Order ───── *)

let ensure_span_order t =
  if t.spans_rev then begin
    t.spans <- List.rev t.spans;
    t.spans_rev <- false
  end

let ensure_spans_rev t =
  if not t.spans_rev then begin
    t.spans <- List.rev t.spans;
    t.spans_rev <- true
  end

(* ───── Content ───── *)

let set_text t s =
  let unchanged =
    match t.cached_plain_text with
    | Some text -> String.equal text s
    | None -> false
  in
  t.spans <- [ { text = s; style = t.default_style } ];
  t.spans_rev <- false;
  if unchanged then t.paint <- None else invalidate t;
  t.cached_plain_text <- Some s

let styled_text t =
  ensure_span_order t;
  t.spans

let set_styled_text t spans =
  let unchanged =
    match t.cached_plain_text with
    | None -> false
    | Some text ->
        let length = String.length text in
        let rec matches offset = function
          | [] -> offset = length
          | span :: rest ->
              let size = String.length span.text in
              if size > length - offset then false
              else
                let i = ref 0 in
                while
                  !i < size
                  && String.unsafe_get text (offset + !i)
                     = String.unsafe_get span.text !i
                do
                  incr i
                done;
                !i = size && matches (offset + size) rest
        in
        matches 0 spans
  in
  t.spans <- spans;
  t.spans_rev <- false;
  if unchanged then t.paint <- None else invalidate t

let append t s =
  ensure_spans_rev t;
  t.spans <- { text = s; style = t.default_style } :: t.spans;
  invalidate t

let append_styled t new_spans =
  ensure_spans_rev t;
  t.spans <- List.rev_append new_spans t.spans;
  invalidate t

let clear t =
  t.spans <- [];
  t.spans_rev <- false;
  t.highlights <- [];
  invalidate t

let plain_text t =
  match t.cached_plain_text with
  | Some s -> s
  | None ->
      ensure_span_order t;
      let s =
        match t.spans with
        | [] -> ""
        | [ s ] -> s.text
        | spans ->
            let buf = Buffer.create 256 in
            List.iter (fun s -> Buffer.add_string buf s.text) spans;
            Buffer.contents buf
      in
      t.cached_plain_text <- Some s;
      s

let source t =
  match t.source with
  | Some source -> source
  | None ->
      let source = Source.of_string (plain_text t) in
      t.source <- Some source;
      source

let set_source t source =
  match t.source with
  | Some current when current == source -> ()
  | _ ->
      if not (String.equal (plain_text t) (Source.text source)) then
        set_text t (Source.text source);
      t.source <- Some source;
      t.version <- t.version + 1

let grapheme_count t = Source.length (source t)

(* ───── Default Style ───── *)

let default_style t = t.default_style
let set_default_style t s = t.default_style <- s

(* ───── Line Info ───── *)

let line_count t = Source.line_count (source t)

let line_width t line =
  let src = source t in
  if line < 0 || line >= Source.line_count src then 0
  else
    Source.columns ~width_method:t.width_method ~tab_width:t.tab_width src
      ~first:(Source.line_start src line)
      ~last:(Source.line_end src line)

let max_line_width t =
  match t.cached_max_line_width with
  | Some width -> width
  | None ->
      let width = ref 0 in
      for line = 0 to line_count t - 1 do
        width := max !width (line_width t line)
      done;
      t.cached_max_line_width <- Some !width;
      !width

(* ───── Text In Range ───── *)

let text_in_range t ~start ~len =
  if len <= 0 then ""
  else Source.slice (source t) ~first:start ~last:(start + len)

(* ───── Highlights ───── *)

let add_highlight t h =
  t.paint <- None;
  t.highlights <- h :: t.highlights

let remove_highlights_by_ref t ref_id =
  t.paint <- None;
  t.highlights <-
    List.filter (fun h -> Highlight.ref_id h <> ref_id) t.highlights

let clear_highlights t =
  t.paint <- None;
  t.highlights <- []

let highlights_in_range t ~start ~len =
  let end_offset = start + len in
  t.highlights
  |> List.filter (fun h ->
      Highlight.start_offset h < end_offset && Highlight.end_offset h > start)
  |> List.sort (fun a b ->
      compare (Highlight.priority a) (Highlight.priority b))

(* ───── Tab Width ───── *)

let tab_width t = t.tab_width

let set_tab_width t w =
  let w = max 1 w in
  if t.tab_width <> w then begin
    t.tab_width <- w;
    t.cached_max_line_width <- None;
    t.version <- t.version + 1
  end

(* ───── Width Method ───── *)

let width_method t = t.width_method

let set_width_method t m =
  if t.width_method <> m then begin
    t.width_method <- m;
    t.cached_max_line_width <- None;
    t.version <- t.version + 1
  end

(* ───── Versioning ───── *)

let version t = t.version

(* Sorting is O(E log E); partial-style resolution is O(E * A), where A is
   simultaneous overlays. This preparation never repeats on unchanged paint. *)
let paint_runs t =
  match t.paint with
  | Some runs -> runs
  | None ->
      ensure_span_order t;
      let src = source t in
      let n = Source.length src in
      let events = ref [] and byte = ref 0 in
      List.iter
        (fun (span : span) ->
          let g = Source.grapheme_of_byte src ~affinity:Downstream !byte in
          if g < n && String.length span.text > 0 then
            events := (g, `Base span.style) :: !events;
          byte := !byte + String.length span.text)
        t.spans;
      let highlights =
        List.stable_sort
          (fun a b -> compare (Highlight.priority a) (Highlight.priority b))
          t.highlights
      in
      List.iteri
        (fun id hl ->
          let first = max 0 hl.Highlight.start_offset in
          let last = min n hl.Highlight.end_offset in
          if first < last then begin
            events := (first, `Start (id, hl.Highlight.style)) :: !events;
            events := (last, `Stop id) :: !events
          end)
        highlights;
      let events =
        List.stable_sort (fun (a, _) (b, _) -> compare a b) (List.rev !events)
      in
      let active = ref [] and base = ref t.default_style in
      let runs = Dynarray.create () in
      let rec sweep pos events =
        let rec apply = function
          | (g, event) :: rest when g = pos ->
              (match event with
              | `Base style -> base := style
              | `Start entry ->
                  active :=
                    List.merge
                      (fun (a, _) (b, _) -> compare a b)
                      [ entry ] !active
              | `Stop id ->
                  active := List.filter (fun (key, _) -> key <> id) !active);
              apply rest
          | rest -> rest
        in
        let rest = apply events in
        let last = match rest with (g, _) :: _ -> g | [] -> n in
        if last > pos then begin
          let style =
            List.fold_left
              (fun base (_, overlay) -> Ansi.Style.merge ~base ~overlay)
              !base !active
          in
          Dynarray.add_last runs (pos, last, style)
        end;
        if last < n then sweep last rest
      in
      if n > 0 then sweep 0 events;
      let runs = Dynarray.to_array runs in
      t.paint <- Some runs;
      runs

let style_at t g =
  let runs = paint_runs t in
  let n = Array.length runs in
  if n = 0 then (t.default_style, max_int)
  else begin
    let lo = ref 0 and hi = ref (n - 1) in
    while !lo < !hi do
      let mid = !lo + ((!hi - !lo + 1) / 2) in
      let first, _, _ = runs.(mid) in
      if first <= g then lo := mid else hi := mid - 1
    done;
    let _, last, style = runs.(!lo) in
    (style, last)
  end
