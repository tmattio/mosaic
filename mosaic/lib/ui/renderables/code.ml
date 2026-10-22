(* Highlighters *)

module Highlighter = struct
  type request = { content : string; language : string }
  type result = (Syntax_highlight.t, exn) Stdlib.result
  type job = { poll : unit -> result option; cancel : unit -> unit }

  type t =
    | Sync of (request -> Syntax_highlight.t)
    | Async of (request -> notify:(unit -> unit) -> job)

  let job ~poll ~cancel = { poll; cancel }
  let sync f = Sync (fun { language; content } -> f ~language ~content)
  let async f = Async f

  type work = {
    request : request;
    notify : unit -> unit;
    mutable cancelled : bool;
    mutable outcome : result option;
  }

  let threaded f =
    let mutex = Mutex.create () in
    let pending = Queue.create () in
    let running = ref false in
    let rec work () =
      match
        Mutex.protect mutex (fun () ->
            match Queue.take_opt pending with
            | None ->
                running := false;
                None
            | Some task -> Some task)
      with
      | None -> ()
      | Some task ->
          let outcome =
            try
              Ok
                (f ~language:task.request.language ~content:task.request.content)
            with exn -> Error exn
          in
          Mutex.protect mutex (fun () ->
              if not task.cancelled then begin
                task.outcome <- Some outcome;
                (* Notification only schedules a render; it never polls the job. *)
                try task.notify () with _ -> ()
              end);
          work ()
    in
    async (fun request ~notify ->
        let task = { request; notify; cancelled = false; outcome = None } in
        Mutex.protect mutex (fun () ->
            Queue.add task pending;
            if not !running then begin
              running := true;
              try ignore (Thread.create work () : Thread.t)
              with exn ->
                running := false;
                Queue.clear pending;
                raise exn
            end);
        job
          ~poll:(fun () ->
            Mutex.protect mutex (fun () ->
                let result = task.outcome in
                task.outcome <- None;
                result))
          ~cancel:(fun () ->
            Mutex.protect mutex (fun () ->
                task.cancelled <- true;
                task.outcome <- None;
                (* Remove cancelled input immediately, preserving the live FIFO. *)
                for _ = 1 to Queue.length pending do
                  let queued = Queue.take pending in
                  if queued != task then Queue.add queued pending
                done)))

  let completed outcome =
    let pending = ref (Some outcome) in
    job
      ~poll:(fun () ->
        let result = !pending in
        pending := None;
        result)
      ~cancel:(fun () -> pending := None)

  let start t request ~notify =
    match t with
    | Async f -> ( try f request ~notify with exn -> completed (Error exn))
    | Sync f -> completed (try Ok (f request) with exn -> Error exn)
end

(* Syntax *)

type source = Ranges of Syntax_highlight.t | Highlighter of Highlighter.t

type syntax = {
  language : string option;
  style : Syntax_style.t;
  source : source;
  conceal : bool;
}

let syntax ?language ?(style = Syntax_style.default) ?(conceal = true)
    highlights =
  { language; style; source = Ranges highlights; conceal }

let with_highlighter ~language ?(style = Syntax_style.default) ?(conceal = true)
    highlighter =
  { language = Some language; style; source = Highlighter highlighter; conceal }

let source_equal a b =
  match (a, b) with
  | Ranges a, Ranges b ->
      a == b
      || String.equal (Syntax_highlight.content a) (Syntax_highlight.content b)
         && Syntax_highlight.ranges a = Syntax_highlight.ranges b
  | Highlighter a, Highlighter b -> a == b
  | _ -> false

let syntax_equal a b =
  Option.equal String.equal a.language b.language
  && Syntax_style.equal a.style b.style
  && source_equal a.source b.source
  && Bool.equal a.conceal b.conceal

(* Two settings with the same producer yield the same captures for the same
   content; they may differ only in presentation. *)
let same_producer a b =
  Option.equal String.equal a.language b.language
  && source_equal a.source b.source

(* Props *)

module Props = struct
  type t = {
    content : string;
    syntax : syntax option;
    text_style : Ansi.Style.t;
    wrap : Text_surface.wrap;
    tab_width : int;
    truncate : bool;
    selectable : bool;
    selection_bg : Ansi.Color.t option;
    selection_fg : Ansi.Color.t option;
  }

  let make ?(content = "") ?syntax ?(text_style = Ansi.Style.default)
      ?(wrap = `None) ?(tab_width = 4) ?(truncate = false) ?(selectable = true)
      ?selection_bg ?selection_fg () =
    {
      content;
      syntax;
      text_style;
      wrap;
      tab_width;
      truncate;
      selectable;
      selection_bg;
      selection_fg;
    }

  let default = make ()

  let equal a b =
    String.equal a.content b.content
    && Option.equal syntax_equal a.syntax b.syntax
    && Ansi.Style.equal a.text_style b.text_style
    && a.wrap = b.wrap && a.tab_width = b.tab_width && a.truncate = b.truncate
    && a.selectable = b.selectable
    && Option.equal Ansi.Color.equal a.selection_bg b.selection_bg
    && Option.equal Ansi.Color.equal a.selection_fg b.selection_fg
end

(* Types *)

type t = {
  text : Text_renderable.t;
  mutable props : Props.t;
  mutable generation : int;
  mutable highlighting : bool;
  mutable job : running_job option;
  mutable pending : (string * syntax * Highlighter.t) option;
  mutable latest : Syntax_highlight.t option;
      (* The latest captures of the current producer. While a result is
         pending they describe an earlier content. *)
  mutable on_rows_change : (unit -> unit) option;
}

and running_job = {
  generation : int;
  content : string;
  syntax : syntax;
  job : Highlighter.job;
}

(* Accessors *)

let node t = Text_renderable.node t.text
let buffer t = Text_renderable.buffer t.text
let surface t = Text_renderable.surface t.text
let set_on_selection t h = Text_renderable.set_on_selection t.text h
let set_on_rows_change t h = t.on_rows_change <- h
let notify_rows_change t = Option.iter (fun f -> f ()) t.on_rows_change

let pending_work t =
  if t.highlighting then
    let label = Option.bind t.props.syntax (fun syntax -> syntax.language) in
    Some (Renderable.Pending.make ?label ~kind:"code.highlight" ())
  else None

(* Rendering *)

let request_render t = Renderable.request_render (Text_renderable.node t.text)

let clear_pending t =
  t.pending <- None;
  t.highlighting <- false

let cancel_job t =
  clear_pending t;
  match t.job with
  | None -> ()
  | Some running ->
      (try running.job.cancel () with _ -> ());
      t.job <- None;
      t.highlighting <- false

let apply_plain t content = Text_renderable.set_text t.text content

(* [paint t content syntax captures] shows [content] styled by [captures], or
   plain when [captures] describe other bytes. *)
let paint t content syntax captures =
  try
    let captured = Syntax_highlight.content captures in
    if content != captured && not (String.equal content captured) then
      invalid_arg "Code: highlights do not match source";
    let annotations =
      Syntax_highlight.to_annotations ~conceal:syntax.conceal
        ~style:syntax.style captures
    in
    Text_renderable.set_annotations t.text annotations;
    true
  with _ ->
    apply_plain t content;
    false

let apply_result t content syntax captures =
  if paint t content syntax captures then t.latest <- Some captures

let common_prefix a b =
  let n = min (String.length a) (String.length b) in
  let rec loop i = if i < n && a.[i] = b.[i] then loop (i + 1) else i in
  loop 0

let common_suffix a b ~limit =
  let la = String.length a and lb = String.length b in
  let rec loop i =
    if i < limit && a.[la - 1 - i] = b.[lb - 1 - i] then loop (i + 1) else i
  in
  loop 0

(* [carry captures content] moves [captures] onto [content]. Ranges that lie
   in the bytes [content] shares with their source, as a common prefix or a
   common suffix, keep their scope; ranges that touch changed bytes are
   dropped, so captures never style bytes they were not computed for. *)
let carry captures content =
  let source = Syntax_highlight.content captures in
  if String.equal source content then captures
  else
    let prefix = common_prefix source content in
    let limit = min (String.length source) (String.length content) - prefix in
    let suffix = common_suffix source content ~limit in
    let suffix_start = String.length source - suffix in
    let shift = String.length content - String.length source in
    let keep range =
      let start_byte = Syntax_highlight.start_byte range in
      let end_byte = Syntax_highlight.end_byte range in
      if end_byte <= prefix then Some range
      else if start_byte >= suffix_start then
        Some
          (Syntax_highlight.range
             ~meta:(Syntax_highlight.meta range)
             ~start_byte:(start_byte + shift) ~end_byte:(end_byte + shift)
             ~scope:(Syntax_highlight.scope range)
             ())
      else None
    in
    Syntax_highlight.make ~content
      (List.filter_map keep (Syntax_highlight.ranges captures))

(* While a result is pending, [content] shows with the latest captures carried
   onto it. *)
let show_pending t content syntax =
  match t.latest with
  | Some captures ->
      ignore (paint t content syntax (carry captures content) : bool)
  | None -> apply_plain t content

let finish_job t (running : running_job) outcome =
  if running.generation = t.generation then begin
    t.highlighting <- false;
    (match (outcome, t.props.syntax) with
    | Ok captures, Some syntax -> apply_result t running.content syntax captures
    | Ok _, None | Error _, _ -> apply_plain t running.content);
    notify_rows_change t;
    request_render t
  end
  else
    (* A result for content that has since changed still describes the bytes
       the current content shares with it. *)
    match (outcome, t.props.syntax) with
    | Ok captures, Some syntax
      when t.highlighting
           && same_producer running.syntax syntax
           && String.equal (Syntax_highlight.content captures) running.content
      ->
        t.latest <- Some captures;
        show_pending t t.props.content syntax;
        request_render t
    | _ -> ()

let launch_job t content syntax highlighter =
  let language = Option.value syntax.language ~default:"" in
  let generation = t.generation in
  let request = { Highlighter.content; language } in
  let job =
    Highlighter.start highlighter request ~notify:(fun () -> request_render t)
  in
  t.job <- Some { generation; content; syntax; job }

let rec poll_job t =
  match t.job with
  | None -> ()
  | Some running -> (
      match try running.job.poll () with exn -> Some (Error exn) with
      | None -> ()
      | Some outcome -> (
          t.job <- None;
          finish_job t running outcome;
          match t.pending with
          | None -> ()
          | Some (content, syntax, highlighter) ->
              t.pending <- None;
              launch_job t content syntax highlighter;
              poll_job t))

let start_highlighting t content syntax highlighter =
  t.highlighting <- true;
  (match t.job with
  | None -> launch_job t content syntax highlighter
  | Some _ -> t.pending <- Some (content, syntax, highlighter));
  poll_job t;
  if t.highlighting then show_pending t content syntax

let render_new_content t (props : Props.t) =
  t.generation <- t.generation + 1;
  match props.syntax with
  | Some ({ source = Highlighter highlighter; _ } as syntax)
    when not (String.equal props.content "") ->
      start_highlighting t props.content syntax highlighter
  | Some ({ source = Ranges captures; _ } as syntax) ->
      clear_pending t;
      apply_result t props.content syntax captures;
      notify_rows_change t
  | Some { source = Highlighter _; _ } | None ->
      clear_pending t;
      apply_plain t props.content;
      notify_rows_change t

let render_content t (props : Props.t) =
  let previous = t.props in
  t.props <- props;
  let same_producer =
    match (previous.syntax, props.syntax) with
    | Some old, Some syntax -> same_producer old syntax
    | _ -> false
  in
  if not same_producer then t.latest <- None;
  match props.syntax with
  | Some syntax
    when same_producer && String.equal previous.content props.content -> (
      if
        (* Only the presentation changed: restyle what is shown. *)
        t.highlighting
      then show_pending t props.content syntax
      else
        match t.latest with
        | Some captures ->
            ignore (paint t props.content syntax captures : bool);
            notify_rows_change t
        | None -> render_new_content t props)
  | _ -> render_new_content t props

let render_before t _node _grid ~delta:_ = poll_job t

(* Construction *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?content ?syntax
    ?text_style ?wrap ?tab_width ?truncate ?selectable ?selection_bg
    ?selection_fg ?on_selection () =
  let props =
    Props.make ?content ?syntax ?text_style ?wrap ?tab_width ?truncate
      ?selectable ?selection_bg ?selection_fg ()
  in
  let text =
    Text_renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity
      ~text_style:props.text_style ~wrap:props.wrap ~tab_width:props.tab_width
      ~truncate:props.truncate ~selectable:props.selectable
      ?selection_bg:props.selection_bg ?selection_fg:props.selection_fg
      ?on_selection ()
  in
  let t =
    {
      text;
      props;
      generation = 0;
      highlighting = false;
      job = None;
      pending = None;
      latest = None;
      on_rows_change = None;
    }
  in
  Renderable.set_render_before
    (Text_renderable.node text)
    (Some (render_before t));
  Renderable.set_pending_provider
    (Text_renderable.node text)
    (Some (fun () -> pending_work t));
  Renderable.Private.set_cleanup (node t) (fun () -> cancel_job t);
  render_content t props;
  t

(* Content *)

let set_content t s =
  let props = { t.props with content = s } in
  render_content t props

let set_syntax t syntax =
  let props = { t.props with syntax } in
  render_content t props

(* Apply Props *)

let apply_props t (props : Props.t) =
  let style_changed =
    not (Ansi.Style.equal t.props.text_style props.text_style)
  in
  let metrics_changed = ref false in
  if style_changed then Text_renderable.set_text_style t.text props.text_style;
  let content_changed =
    (not (String.equal t.props.content props.content))
    || (not (Option.equal syntax_equal t.props.syntax props.syntax))
    || style_changed
  in

  (* Wrap mode *)
  if t.props.wrap <> props.wrap then begin
    Text_renderable.set_wrap t.text props.wrap;
    metrics_changed := true
  end;
  (* Tab width *)
  if t.props.tab_width <> props.tab_width then begin
    Text_renderable.set_tab_width t.text props.tab_width;
    metrics_changed := true
  end;
  (* Truncate *)
  if t.props.truncate <> props.truncate then begin
    Text_renderable.set_truncate t.text props.truncate;
    metrics_changed := true
  end;
  (* Selection colors *)
  if not (Option.equal Ansi.Color.equal t.props.selection_bg props.selection_bg)
  then Text_renderable.set_selection_bg t.text props.selection_bg;
  if not (Option.equal Ansi.Color.equal t.props.selection_fg props.selection_fg)
  then Text_renderable.set_selection_fg t.text props.selection_fg;
  (* Selectable *)
  if t.props.selectable <> props.selectable then
    Text_renderable.set_selectable t.text props.selectable;
  if content_changed then render_content t props else t.props <- props;
  if !metrics_changed then notify_rows_change t

(* Query *)

let is_highlighting t = t.highlighting
let line_count t = Text_renderable.line_count t.text
let display_line_count t = Text_renderable.display_line_count t.text
