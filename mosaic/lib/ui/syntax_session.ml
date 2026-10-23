(* Highlighters *)

module Highlighter = struct
  type edit = { start : int; removed : int; inserted : int }
  type 'state previous = { content : string; state : 'state; edit : edit }

  type 'state request = {
    content : string;
    language : string;
    previous : 'state previous option;
  }

  type 'state result = (Syntax_highlight.t * 'state, exn) Stdlib.result

  type 'state job = {
    poll : unit -> 'state result option;
    cancel : unit -> unit;
  }

  (* The state's type is the highlighter's own: a view never looks at it.
     [off_loop] runs work off the rendering thread, on the renderer's
     executor. *)
  type t =
    | Highlighter :
        ('state request ->
        notify:(unit -> unit) ->
        off_loop:((unit -> unit) -> unit) ->
        'state job)
        -> t

  let job ~poll ~cancel = { poll; cancel }

  let completed outcome =
    let pending = ref (Some outcome) in
    job
      ~poll:(fun () ->
        let result = !pending in
        pending := None;
        result)
      ~cancel:(fun () -> pending := None)

  let sync_incremental f =
    Highlighter
      (fun request ~notify:_ ~off_loop:_ ->
        completed (try Ok (f request) with exn -> Error exn))

  (* A highlighter that keeps nothing has [()] as its state. *)
  let stateless f { language; content; previous = _ } =
    (f ~language ~content, ())

  let sync f = sync_incremental (stateless f)

  let async f =
    Highlighter
      (fun request ~notify ~off_loop:_ ->
        try f request ~notify with exn -> completed (Error exn))

  type 'state work = {
    request : 'state request;
    notify : unit -> unit;
    mutable cancelled : bool;
    mutable outcome : 'state result option;
  }

  let threaded_incremental f =
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
          let outcome = try Ok (f task.request) with exn -> Error exn in
          Mutex.protect mutex (fun () ->
              if not task.cancelled then begin
                task.outcome <- Some outcome;
                (* Notification only schedules a render; it never polls the job. *)
                try task.notify () with _ -> ()
              end);
          work ()
    in
    (* The worker runs on the renderer's executor, started outside the lock,
       which an executor that runs it at once takes. *)
    Highlighter
      (fun request ~notify ~off_loop ->
        let task = { request; notify; cancelled = false; outcome = None } in
        let start =
          Mutex.protect mutex (fun () ->
              Queue.add task pending;
              let start = not !running in
              running := true;
              start)
        in
        match if start then off_loop work with
        | exception exn ->
            Mutex.protect mutex (fun () ->
                running := false;
                Queue.clear pending);
            completed (Error exn)
        | () ->
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

  let threaded f = threaded_incremental (stateless f)
end

(* [edit ~source content] is the one span in which [content] differs from
   [source]: the longest common prefix, then the longest common suffix that
   does not overlap it. *)
let edit ~source content : Highlighter.edit =
  let source_length = String.length source in
  let length = String.length content in
  let shorter = min source_length length in
  let prefix = ref 0 in
  while !prefix < shorter && source.[!prefix] = content.[!prefix] do
    incr prefix
  done;
  let suffix = ref 0 in
  while
    !suffix < shorter - !prefix
    && source.[source_length - 1 - !suffix] = content.[length - 1 - !suffix]
  do
    incr suffix
  done;
  {
    start = !prefix;
    removed = source_length - !prefix - !suffix;
    inserted = length - !prefix - !suffix;
  }

(* A result accepted by a view, with the request that builds on it. The
   request holds the highlighter's state, so the view need not know its type. *)
type latest = {
  captures : Syntax_highlight.t;
  next : content:string -> notify:(unit -> unit) -> job;
}

and job = {
  poll : unit -> (latest, exn) Stdlib.result option;
  cancel : unit -> unit;
}

let rec request : type state.
    (state Highlighter.request ->
    notify:(unit -> unit) ->
    state Highlighter.job) ->
    language:string ->
    state Highlighter.previous option ->
    content:string ->
    notify:(unit -> unit) ->
    job =
 fun start ~language previous ~content ~notify ->
  let job = start { content; language; previous } ~notify in
  let accept (captures, state) =
    let next ~content ~notify =
      let source = Syntax_highlight.content captures in
      let previous =
        { Highlighter.content = source; state; edit = edit ~source content }
      in
      request start ~language (Some previous) ~content ~notify
    in
    { captures; next }
  in
  {
    poll = (fun () -> Option.map (Result.map accept) (job.poll ()));
    cancel = job.cancel;
  }

let first_request (Highlighter.Highlighter start) ~off_loop ~language ~content
    ~notify =
  request
    (fun request ~notify -> start request ~notify ~off_loop)
    ~language None ~content ~notify

(* Syntax settings *)

type source = Ranges of Syntax_highlight.t | Highlighter of Highlighter.t

type syntax = {
  language : string option;
  style : Syntax_style.t;
  source : source;
}

let syntax ?language ?(style = Syntax_style.default) captures =
  { language; style; source = Ranges captures }

let with_highlighter ~language ?(style = Syntax_style.default) highlighter =
  { language = Some language; style; source = Highlighter highlighter }

let language syntax = syntax.language
let style syntax = syntax.style

let source_equal a b =
  match (a, b) with
  | Ranges a, Ranges b ->
      a == b
      || String.equal (Syntax_highlight.content a) (Syntax_highlight.content b)
         && Syntax_highlight.ranges a = Syntax_highlight.ranges b
  | Highlighter a, Highlighter b -> a == b
  | _ -> false

(* Two settings with the same producer yield the same captures for the same
   content; they may differ only in presentation. *)
let same_producer a b =
  Option.equal String.equal a.language b.language
  && source_equal a.source b.source

let equal a b = same_producer a b && Syntax_style.equal a.style b.style

(* Sessions *)

type running = {
  generation : int;
  content : string;
  syntax : syntax;
  job : job;
}

type t = {
  notify : unit -> unit;
  off_loop : (unit -> unit) -> unit;
  mutable content : string;
  mutable syntax : syntax option;
  mutable generation : int;
      (* Changes of content or producer, so a result knows if it is current. *)
  mutable highlighting : bool;
  mutable running : running option;
  mutable waiting : (string * syntax * Highlighter.t) option;
  mutable latest : latest option;
      (* The latest result of the current producer. While a result is pending
         it describes an earlier content. *)
  mutable result : Syntax_highlight.t option;
      (* Once no result is pending, the result for the content, or [None] if
         it failed. *)
}

let create ~notify ~off_loop =
  {
    notify;
    off_loop;
    content = "";
    syntax = None;
    generation = 0;
    highlighting = false;
    running = None;
    waiting = None;
    latest = None;
    result = None;
  }

let is_highlighting t = t.highlighting

let bound captures content =
  String.equal (Syntax_highlight.content captures) content

let launch t content syntax highlighter =
  let language = Option.value syntax.language ~default:"" in
  let job =
    match t.latest with
    | Some latest -> latest.next ~content ~notify:t.notify
    | None ->
        first_request highlighter ~off_loop:t.off_loop ~language ~content
          ~notify:t.notify
  in
  t.running <- Some { generation = t.generation; content; syntax; job }

(* [finish t running outcome] is [true] iff it changes what [t] shows. A
   result for content that has since changed still describes the bytes the
   current content shares with it, so it becomes the latest. *)
let finish t (running : running) outcome =
  let accepted =
    match outcome with
    | Ok latest when bound latest.captures running.content -> Some latest
    | Ok _ | Error _ -> None
  in
  if running.generation = t.generation then begin
    t.highlighting <- false;
    Option.iter (fun latest -> t.latest <- Some latest) accepted;
    t.result <- Option.map (fun latest -> latest.captures) accepted;
    true
  end
  else
    match (accepted, t.syntax) with
    | Some latest, Some syntax
      when t.highlighting && same_producer running.syntax syntax ->
        t.latest <- Some latest;
        true
    | _ -> false

let rec poll t =
  match t.running with
  | None -> false
  | Some running -> (
      match try running.job.poll () with exn -> Some (Error exn) with
      | None -> false
      | Some outcome ->
          t.running <- None;
          let changed = finish t running outcome in
          (match t.waiting with
          | None -> ()
          | Some (content, syntax, highlighter) ->
              t.waiting <- None;
              launch t content syntax highlighter);
          poll t || changed)

let set t ~content syntax =
  let same_producer =
    match (t.syntax, syntax) with
    | Some old, Some syntax -> same_producer old syntax
    | _ -> false
  in
  let same_content = String.equal t.content content in
  t.content <- content;
  t.syntax <- syntax;
  if not same_producer then t.latest <- None;
  if not (same_producer && same_content) then begin
    t.generation <- t.generation + 1;
    t.waiting <- None;
    t.highlighting <- false;
    t.result <- None;
    match syntax with
    | Some ({ source = Highlighter highlighter; _ } as syntax)
      when not (String.equal content "") ->
        t.highlighting <- true;
        (match t.running with
        | None -> launch t content syntax highlighter
        | Some _ -> t.waiting <- Some (content, syntax, highlighter));
        ignore (poll t : bool)
    | Some _ | None -> ()
  end

let cancel t =
  t.waiting <- None;
  t.highlighting <- false;
  match t.running with
  | None -> ()
  | Some running -> (
      t.running <- None;
      try running.job.cancel () with _ -> ())

(* [carry ~conceal captures content] moves [captures] onto [content], which
   differs from their source in one span. A range keeps its scope over its
   bytes outside that span, which is plain, so captures never style bytes they
   were not computed for. *)
let carry ~conceal captures content =
  let source = Syntax_highlight.content captures in
  if String.equal source content then captures
  else
    let { Highlighter.start; removed; inserted } = edit ~source content in
    let removed_end = start + removed and inserted_end = start + inserted in
    let shift = inserted - removed in
    (* Ranges accumulate in reverse declaration order. *)
    let part range ~start_byte ~end_byte moved =
      if start_byte < end_byte then
        Syntax_highlight.range
          ~meta:(Syntax_highlight.meta range)
          ~start_byte ~end_byte
          ~scope:(Syntax_highlight.scope range)
          ()
        :: moved
      else moved
    in
    let move moved range =
      let start_byte = Syntax_highlight.start_byte range in
      let end_byte = Syntax_highlight.end_byte range in
      if end_byte <= start then range :: moved
      else if start_byte >= removed_end then
        part range ~start_byte:(start_byte + shift) ~end_byte:(end_byte + shift)
          moved
      else if conceal && Syntax_highlight.conceals range then moved
      else
        let moved =
          part range ~start_byte ~end_byte:(min end_byte start) moved
        in
        part range ~start_byte:inserted_end ~end_byte:(end_byte + shift) moved
    in
    Syntax_highlight.make ~content
      (List.rev (List.fold_left move [] (Syntax_highlight.ranges captures)))

let shown t ~conceal =
  match t.syntax with
  | None -> None
  | Some { source = Ranges captures; _ } ->
      if bound captures t.content then Some captures else None
  | Some { source = Highlighter _; _ } ->
      if t.highlighting then
        Option.map
          (fun { captures; _ } -> carry ~conceal captures t.content)
          t.latest
      else t.result
