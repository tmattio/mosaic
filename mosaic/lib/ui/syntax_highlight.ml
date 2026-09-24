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
type segment = { start_byte : int; end_byte : int; scopes : string list }
type projection = { content : string; segments : segment list }

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

type indexed_range = { range : range; index : int }
type boundary = Start of int * int | End of int * int

let boundary_pos = function Start (pos, _) | End (pos, _) -> pos

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

let index_ranges ranges =
  ranges
  |> List.mapi (fun index range -> { range; index })
  |> List.filter (fun item -> item.range.start_byte <> item.range.end_byte)

let range_table ranges =
  let table = Hashtbl.create (List.length ranges) in
  List.iter (fun item -> Hashtbl.add table item.index item.range) ranges;
  table

let collect_injection_containers ranges =
  List.filter_map
    (fun item ->
      if item.range.meta.contains_injection then
        Some (item.range.start_byte, item.range.end_byte)
      else None)
    ranges

let active_ranges table active =
  List.map (fun id -> (id, Hashtbl.find table id)) active

let inside_injection_container containers pos =
  List.exists
    (fun (start_byte, end_byte) -> pos >= start_byte && pos < end_byte)
    containers

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

let concealed_text ~conceal ranges =
  if conceal then
    List.find_map
      (fun (_, range) ->
        match conceal_replacement range with
        | Some replacement -> Some replacement
        | None -> None)
      ranges
  else None

let active_scopes ~injection_containers ~pos ranges =
  let inside_injection = inside_injection_container injection_containers pos in
  let ranges =
    ranges
    |> List.filter (fun (_, range) ->
        (not inside_injection) || not (should_suppress_in_injection range))
    |> List.sort (fun (index_a, range_a) (index_b, range_b) ->
        let c =
          Int.compare (specificity range_a.scope) (specificity range_b.scope)
        in
        if c <> 0 then c else Int.compare index_a index_b)
  in
  List.map (fun (_, range) -> range.scope) ranges

let sorted_boundaries ranges =
  let boundaries = ref [] in
  List.iter
    (fun item ->
      if item.range.start_byte <> item.range.end_byte then begin
        boundaries := Start (item.range.start_byte, item.index) :: !boundaries;
        boundaries := End (item.range.end_byte, item.index) :: !boundaries
      end)
    ranges;
  List.sort
    (fun a b ->
      let pos_a = boundary_pos a and pos_b = boundary_pos b in
      if pos_a <> pos_b then Int.compare pos_a pos_b
      else
        match (a, b) with End _, Start _ -> -1 | Start _, End _ -> 1 | _ -> 0)
    !boundaries

let prepare ~conceal content ranges =
  let len = String.length content in
  let indexed = index_ranges ranges in
  let by_index = range_table indexed in
  let injection_containers = collect_injection_containers indexed in
  let boundaries = sorted_boundaries indexed in
  let active = ref [] and segments = ref [] and current = ref 0 in
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
      segments :=
        { start_byte = !visible; end_byte = !visible + visible_size; scopes }
        :: !segments;
    visible := !visible + visible_size;
    written := last
  in
  let flush_until pos =
    if pos > !current then begin
      let ranges = active_ranges by_index !active in
      let replacement = concealed_text ~conceal ranges in
      let scopes =
        match replacement with
        | Some _ -> []
        | None -> active_scopes ~injection_containers ~pos:!current ranges
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
  List.iter
    (fun boundary ->
      let pos = boundary_pos boundary in
      flush_until pos;
      match boundary with
      | Start (_, id) -> active := id :: !active
      | End (_, id) ->
          active := List.filter (fun active_id -> active_id <> id) !active;
          if conceal then begin
            let range = Hashtbl.find by_index id in
            if range.meta.conceal_lines then skip_char_at pos '\n';
            match range.meta.conceal with
            | Some " " -> skip_char_at pos ' '
            | Some ""
              when String.equal range.scope "conceal"
                   && not range.meta.is_injection ->
                skip_char_at pos ' '
            | _ -> ()
          end)
    boundaries;
  if !current < len then emit !current len [] None;
  let content =
    match !output with None -> content | Some buffer -> Buffer.contents buffer
  in
  { content; segments = List.rev !segments }

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
  let ranges =
    List.map
      (fun segment ->
        let style =
          List.fold_left
            (fun base scope ->
              Ansi.Style.merge ~base
                ~overlay:(Syntax_style.resolve_overlay style scope))
            base_style segment.scopes
        in
        {
          Text_buffer.Annotations.start_byte = segment.start_byte;
          end_byte = segment.end_byte;
          style;
        })
      projection.segments
  in
  Text_buffer.Annotations.make ~base_style ~content:projection.content ranges

let to_spans ?(conceal = true) ~style t =
  Text_buffer.Annotations.to_spans (to_annotations ~conceal ~style t)
