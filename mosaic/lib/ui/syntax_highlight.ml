type meta = {
  is_injection : bool;
  contains_injection : bool;
  conceal : string option;
  conceal_lines : bool;
}

let default_meta =
  {
    is_injection = false;
    contains_injection = false;
    conceal = None;
    conceal_lines = false;
  }

type range = { start_byte : int; end_byte : int; scope : string; meta : meta }

(* A run of visible bytes that one set of scopes styles. [scopes] indexes the
   projection's [scope_sets], so a theme resolves each distinct set once. *)
type segment = { start_byte : int; end_byte : int; scopes : int }

type projection = {
  content : string;
  segments : segment array;
  scope_sets : string list array;
}

type t = {
  content : string;
  ranges : range list;
  plain : projection option Atomic.t;
  concealed : projection option Atomic.t;
}

let range ?(meta = default_meta) ~start_byte ~end_byte ~scope () =
  if start_byte < 0 || end_byte < start_byte then
    invalid_arg
      (Printf.sprintf "Syntax_highlight.range: invalid range (%d, %d)"
         start_byte end_byte);
  { start_byte; end_byte; scope; meta }

let start_byte (r : range) = r.start_byte
let end_byte (r : range) = r.end_byte
let scope r = r.scope
let meta r = r.meta

let specificity scope =
  let n = ref 1 in
  String.iter (fun c -> if c = '.' then incr n) scope;
  !n

let should_suppress_in_injection range =
  (not range.meta.is_injection) && String.equal range.scope "markup.raw.block"

let validate ~content (ranges : range list) =
  let len = String.length content in
  List.iter
    (fun (r : range) ->
      if r.start_byte < 0 || r.end_byte > len || r.start_byte > r.end_byte then
        invalid_arg
          (Printf.sprintf
             "Syntax_highlight.make: range (%d, %d) out of bounds for content \
              of length %d"
             r.start_byte r.end_byte len))
    ranges

let conceal_replacement range =
  match range.meta.conceal with
  | Some text -> Some text
  | None ->
      if
        String.equal range.scope "conceal"
        || String.starts_with ~prefix:"conceal." range.scope
      then
        if String.equal range.scope "conceal.with.space" then Some " "
        else Some ""
      else None

(* A range open at the sweep's position, with its declaration index. *)
type open_range = { index : int; specificity : int; range : range }

(* [insert before x l] puts [x] into [l], kept ordered by [before]. *)
let insert before x l =
  let rec go = function
    | y :: rest when not (before x y) -> y :: go rest
    | l -> x :: l
  in
  go l

(* Scopes apply least specific first, then in declaration order. *)
let applies_before a b =
  a.specificity < b.specificity
  || (a.specificity = b.specificity && a.index < b.index)

(* The concealing range that wins is the latest to start, then the first
   declared. *)
let conceals_before a b =
  a.range.start_byte > b.range.start_byte
  || (a.range.start_byte = b.range.start_byte && a.index < b.index)

(* [prepare] sweeps the ranges once in start order. Between two consecutive
   range boundaries the open ranges are constant, so each such segment gets
   one set of scopes, or the replacement of the concealing range that wins. *)
let prepare ~conceal content ranges =
  let len = String.length content in
  let ranges : range array = Array.of_list ranges in
  let order =
    Array.of_list
      (List.filter
         (fun i -> ranges.(i).start_byte < ranges.(i).end_byte)
         (List.init (Array.length ranges) Fun.id))
  in
  (* Producers usually list ranges by start already; sort only if not. *)
  let by_start i j = Int.compare ranges.(i).start_byte ranges.(j).start_byte in
  let sorted = ref true in
  for k = 1 to Array.length order - 1 do
    if by_start order.(k - 1) order.(k) > 0 then sorted := false
  done;
  if not !sorted then Array.stable_sort by_start order;
  let scope_sets = Dynarray.create () and set_ids = Hashtbl.create 64 in
  let scope_set scopes =
    match Hashtbl.find_opt set_ids scopes with
    | Some id -> id
    | None ->
        let id = Dynarray.length scope_sets in
        Dynarray.add_last scope_sets scopes;
        Hashtbl.add set_ids scopes id;
        id
  in
  let unstyled = scope_set [] in
  let active = ref [] and concealing = ref [] in
  let segments = Dynarray.create () and current = ref 0 in
  (* Allocate output storage only when concealment changes the source. *)
  let output = ref None and written = ref 0 and visible = ref 0 in
  let output_buffer () =
    match !output with
    | Some buffer -> buffer
    | None ->
        let buffer = Buffer.create len in
        Buffer.add_substring buffer content 0 !written;
        output := Some buffer;
        buffer
  in
  let emit first last scopes replacement =
    let size = last - first in
    let visible_size =
      match replacement with
      | Some text ->
          Buffer.add_string (output_buffer ()) text;
          String.length text
      | None ->
          (match !output with
          | None -> ()
          | Some buffer -> Buffer.add_substring buffer content first size);
          size
    in
    if visible_size > 0 then
      Dynarray.add_last segments
        { start_byte = !visible; end_byte = !visible + visible_size; scopes };
    visible := !visible + visible_size;
    written := last
  in
  let flush_until pos =
    if pos > !current then begin
      let replacement =
        match !concealing with
        | open_range :: _ -> conceal_replacement open_range.range
        | [] -> None
      in
      let scopes =
        match replacement with
        | Some _ -> unstyled
        | None ->
            let inside_injection =
              List.exists
                (fun open_range -> open_range.range.meta.contains_injection)
                !active
            in
            scope_set
              (List.filter_map
                 (fun { range; _ } ->
                   if inside_injection && should_suppress_in_injection range
                   then None
                   else Some range.scope)
                 !active)
      in
      emit !current pos scopes replacement;
      current := pos
    end
  in
  let skip_char_at pos c =
    if pos < len && Char.equal (String.unsafe_get content pos) c then begin
      ignore (output_buffer () : Buffer.t);
      current := pos + 1;
      written := pos + 1
    end
  in
  let close pos range =
    if conceal then begin
      if range.meta.conceal_lines then skip_char_at pos '\n';
      match range.meta.conceal with
      | Some " " -> skip_char_at pos ' '
      | Some ""
        when String.equal range.scope "conceal" && not range.meta.is_injection
        ->
          skip_char_at pos ' '
      | _ -> ()
    end
  in
  let next = ref 0 in
  let rec sweep () =
    let next_start =
      if !next < Array.length order then ranges.(order.(!next)).start_byte
      else max_int
    in
    let next_end =
      List.fold_left
        (fun m open_range -> min m open_range.range.end_byte)
        max_int !active
    in
    let pos = min next_start next_end in
    if pos < max_int then begin
      flush_until pos;
      if next_end = pos then begin
        let ends open_range = open_range.range.end_byte = pos in
        let closing = List.filter ends !active in
        active := List.filter (fun r -> not (ends r)) !active;
        concealing := List.filter (fun r -> not (ends r)) !concealing;
        List.iter (fun open_range -> close pos open_range.range) closing
      end;
      while
        !next < Array.length order && ranges.(order.(!next)).start_byte = pos
      do
        let index = order.(!next) in
        let range = ranges.(index) in
        let open_range =
          { index; specificity = specificity range.scope; range }
        in
        active := insert applies_before open_range !active;
        if conceal && conceal_replacement range <> None then
          concealing := insert conceals_before open_range !concealing;
        incr next
      done;
      sweep ()
    end
  in
  sweep ();
  if !current < len then emit !current len unstyled None;
  let content =
    match !output with None -> content | Some buffer -> Buffer.contents buffer
  in
  {
    content;
    segments = Dynarray.to_array segments;
    scope_sets = Dynarray.to_array scope_sets;
  }

let make ~content ranges =
  validate ~content ranges;
  let plain = Atomic.make None in
  let has_conceal =
    List.exists
      (fun range ->
        range.meta.conceal_lines || conceal_replacement range <> None)
      ranges
  in
  let concealed = if has_conceal then Atomic.make None else plain in
  { content; ranges; plain; concealed }

let of_triples ~content triples =
  make ~content
    (List.map
       (fun (start_byte, end_byte, scope) ->
         range ~start_byte ~end_byte ~scope ())
       triples)

let content t = t.content
let ranges t = t.ranges

let projection ~conceal t =
  let cache = if conceal then t.concealed else t.plain in
  match Atomic.get cache with
  | Some projection -> projection
  | None ->
      let projection = prepare ~conceal t.content t.ranges in
      if Atomic.compare_and_set cache None (Some projection) then projection
      else Option.get (Atomic.get cache)

let to_annotations ?(conceal = false) ~style t =
  let projection = projection ~conceal t in
  let base_style = Syntax_style.base style in
  let styles =
    Array.map
      (List.fold_left
         (fun base scope ->
           Ansi.Style.merge ~base
             ~overlay:(Syntax_style.resolve_overlay style scope))
         base_style)
      projection.scope_sets
  in
  let ranges =
    Array.fold_right
      (fun (segment : segment) ranges ->
        {
          Text_buffer.Annotations.start_byte = segment.start_byte;
          end_byte = segment.end_byte;
          style = styles.(segment.scopes);
        }
        :: ranges)
      projection.segments []
  in
  Text_buffer.Annotations.make ~base_style ~content:projection.content ranges

let to_spans ?(conceal = true) ~style t =
  Text_buffer.Annotations.to_spans (to_annotations ~conceal ~style t)
