let src = Logs.Src.create "vcr" ~doc:"VCR recording"

module Log = (val Logs.src_log src : Logs.LOG)
module Event = Event
module Sampler = Sampler
module Executor = Executor
module Error = Error
module Timing = Vcr_common.Timing

(** Main entry point - run tape and generate output *)
let run ?(timing = Timing.create ()) tape output_path =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (* 1. Execute tape and produce event log *)
  Timing.start timing "Execute tape";
  match Executor.run ~sw ~env tape with
  | Error err ->
      Printf.eprintf "Error: %s\n" (Error.to_string err);
      exit 1
  | Ok (events, final_vte, config) -> (
      ignore (Timing.stop timing "Execute tape");
      (* Debug: log event count and types *)
      Log.info (fun m -> m "Generated %d events" (List.length events));
      List.iter
        (fun event -> Log.info (fun m -> m "Event: %s" (Event.to_string event)))
        events;

      (* 2. Determine output format and create renderer config *)
      let output_path =
        match output_path with
        | Some path -> path
        | None -> (
            (* Look for Output command in tape *)
            match
              List.find_opt
                (function Tape_lang.Ast.Output _ -> true | _ -> false)
                tape
            with
            | Some (Tape_lang.Ast.Output path) -> path
            | _ ->
                Printf.eprintf "Error: No output path specified\n";
                exit 1)
      in

      match Filename.extension output_path with
      | ".gif" ->
          (* For GIF, we sample frames and render them *)
          let initial_rows = Vte.rows final_vte in
          let initial_cols = Vte.cols final_vte in

          (* Sample frames from events *)
          let frames =
            Timing.with_timing timing "Sample frames" (fun () ->
                Sampler.sample
                  ~fps:(float_of_int config.framerate)
                  ~strategy:Sampler.Drop ~initial_cols ~initial_rows events)
          in
          Printf.eprintf "[INFO] Sampled %d frames at %d fps\n"
            (List.length frames) config.framerate;

          (* Calculate delays and convert to renderer frames *)
          let frames_with_delays =
            Timing.with_timing timing "Calculate delays" (fun () ->
                Sampler.calculate_delays frames)
          in

          (* Convert sampler frames to Renderer.Frame.t *)
          let frames =
            Timing.with_timing timing "Convert frames" (fun () ->
                List.map
                  (fun (frame, delay_cs) ->
                    let renderer_frame =
                      Renderer.Frame.create ~grid:frame.Sampler.grid
                        ~cursor_row:frame.Sampler.cursor_row
                        ~cursor_col:frame.Sampler.cursor_col
                        ~cursor_visible:frame.Sampler.cursor_visible ~delay_cs
                        ~dirty_regions:frame.Sampler.dirty_regions
                        ~cursor_moved:frame.Sampler.cursor_moved
                    in
                    renderer_frame)
                  frames_with_delays)
          in

          (* Calculate character dimensions *)
          let char_width = max 6 (config.font_size * 6 / 10) in
          let char_height = max 8 (config.font_size * 12 / 10) in

          let renderer_config =
            Renderer.Gif_renderer.
              {
                char_width;
                char_height;
                theme = Renderer.Gif_renderer.default_theme;
                font_path = config.font_family;
                font_size = config.font_size;
                target_width = Some config.width;
                target_height = Some config.height;
                padding = config.padding;
              }
          in

          (* Render using streaming interface *)
          let out_channel =
            if output_path = "-" then stdout else open_out_bin output_path
          in
          (* Get dimensions from first frame *)
          let rows, cols =
            match frames with
            | [] -> (24, 80) (* Default terminal size *)
            | frame :: _ ->
                (Renderer.Frame.rows frame, Renderer.Frame.cols frame)
          in
          let renderer =
            Renderer.Gif_renderer.create ~rows ~cols renderer_config
          in
          (* Create buffered writer function for the output channel *)
          let buffer = Buffer.create (64 * 1024) in
          (* 64KB buffer *)
          let writer bytes offset length =
            Buffer.add_subbytes buffer bytes offset length;
            (* Flush if buffer gets too large *)
            if Buffer.length buffer > 256 * 1024 then (
              output_string out_channel (Buffer.contents buffer);
              Buffer.clear buffer)
          in

          (* Write each frame *)
          Timing.with_timing timing "Render GIF" (fun () ->
              (* Set global timing for renderer to use *)
              Renderer.Gif_renderer.set_timing (Some timing);
              List.iter
                (fun frame ->
                  Renderer.Gif_renderer.write_frame renderer frame
                    ~incremental:true ~writer)
                frames;

              (* Finalize the GIF *)
              Renderer.Gif_renderer.finalize renderer ~writer;
              Renderer.Gif_renderer.set_timing None;

              (* Flush remaining buffer contents *)
              if Buffer.length buffer > 0 then
                output_string out_channel (Buffer.contents buffer);

              if output_path <> "-" then close_out out_channel)
      | ".txt" | ".ascii" ->
          (* For text output, just dump final terminal state *)
          let content = Vte.to_string_grid final_vte in
          if output_path = "-" then print_string content
          else
            let oc = open_out output_path in
            output_string oc content;
            close_out oc
      | "" ->
          (* No extension - default to GIF format *)
          failwith
            (Printf.sprintf
               "Unsupported output format for '%s'. Supported formats: .gif, \
                .svg, .txt, .ascii"
               output_path)
      | ext ->
          Printf.eprintf "Error: Unsupported output format: %s\n" ext;
          exit 1)
