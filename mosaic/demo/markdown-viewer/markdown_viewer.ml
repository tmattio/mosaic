open Mosaic

(* Sample markdown content *)
let sample_markdown =
  "# Mosaic Markdown Viewer\n\n\
   Welcome to the **Mosaic Markdown Viewer** demo! This is a terminal-based \
   markdown viewer built with the Mosaic React API.\n\n\
   ## Features\n\n\
   This viewer demonstrates several key features of the Mosaic framework:\n\n\
   - **Scrollable content** with keyboard navigation\n\
   - **Table of Contents** sidebar for quick navigation  \n\
   - **Syntax highlighting** for code blocks\n\
   - **Responsive layout** that adapts to terminal size\n\n\
   ### Navigation\n\n\
   You can navigate the document using:\n\n\
   - **Arrow keys** to scroll line by line\n\
   - **Page Up/Down** for faster scrolling\n\
   - **Home/End** to jump to beginning or end\n\
   - **j/k** to navigate TOC entries\n\
   - **Enter** to jump to selected heading\n\
   - **q** to quit\n\n\
   ## Code Example\n\n\
   Here's a simple OCaml function:\n\n\
   ```ocaml\n\
   let fibonacci n =\n\
  \  let rec fib n a b =\n\
  \    if n = 0 then a\n\
  \    else fib (n - 1) b (a + b)\n\
  \  in\n\
  \  fib n 0 1\n\
   ```\n\n\
   ## Lists\n\n\
   ### Unordered List\n\n\
   - First item\n\
   - Second item with **bold text**\n\
   - Third item with `inline code`\n\n\
   ### Ordered List\n\n\
   1. First step\n\
   2. Second step\n\
   3. Third step\n\n\
   ---\n\n\
   ## Blockquotes\n\n\
   > This is a blockquote. It can be used to highlight important information \
   or quotes from other sources.\n\n\
   ## Tables\n\n\
   | Feature | Description | Status |\n\
   |---------|-------------|--------|\n\
   | Scrolling | Keyboard-based scrolling | ✓ |\n\
   | TOC | Table of contents sidebar | ✓ |\n\
   | Syntax | Code highlighting | ✓ |\n\
   | Responsive | Adapts to terminal size | ✓ |\n\n\
   ## More Sections\n\n\
   ### Section 3.1\n\n\
   Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod \
   tempor incididunt ut labore et dolore magna aliqua.\n\n\
   ### Section 3.2\n\n\
   Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut \
   aliquip ex ea commodo consequat.\n\n\
   ### Section 3.3\n\n\
   Duis aute irure dolor in reprehenderit in voluptate velit esse cillum \
   dolore eu fugiat nulla pariatur.\n\n\
   ## Advanced Features\n\n\
   This markdown viewer could be extended with:\n\n\
   1. **Link handling** - Open URLs in browser\n\
   2. **Search functionality** - Find text in document  \n\
   3. **Multiple files** - Browse directory of markdown files\n\
   4. **Themes** - Switch between color schemes\n\
   5. **Export** - Save as HTML or PDF\n\n\
   ## Technical Details\n\n\
   The viewer is built using:\n\n\
   - **Mosaic.react** for declarative UI\n\
   - **Cmarkit** for markdown parsing\n\
   - **Mosaic.markdown** for rendering\n\
   - **React-style hooks** for state management\n\
   - **Subscriptions** for keyboard and window events\n\n\
   ---\n\n\
   ## Conclusion\n\n\
   This demo shows how Mosaic can be used to build sophisticated terminal \
   applications with modern React-style patterns.\n\n\
   Thank you for trying the Mosaic Markdown Viewer!"

(* Extract headings from markdown for TOC *)
let extract_headings markdown_text =
  let doc = Cmarkit.Doc.of_string markdown_text in
  let headings = ref [] in
  let folder =
    Cmarkit.Folder.make
      ~block:(fun _f _acc b ->
        match b with
        | Cmarkit.Block.Heading (h, _) ->
            let level = Cmarkit.Block.Heading.level h in
            let text =
              Cmarkit.Block.Heading.inline h
              |> Cmarkit.Inline.to_plain_text ~break_on_soft:false
              |> List.map (String.concat "")
              |> String.concat ""
            in
            headings := (level, text) :: !headings;
            Cmarkit.Folder.default
        | _ -> Cmarkit.Folder.default)
      ()
  in
  ignore (Cmarkit.Folder.fold_block folder () (Cmarkit.Doc.block doc));
  List.rev !headings

(* Scrollable markdown viewer component - no longer needed, using scroll_view directly *)

(* Table of Contents component *)
let toc_component ~headings ~selected =
  let open Ui in
  vbox ~gap:(`Cells 1)
    [
      text ~style:Style.(fg (Index 39) ++ bold) "TABLE OF CONTENTS";
      text ~style:Style.(fg (Index 238)) "-----------------";
      vbox
        (List.mapi
           (fun i (level, heading_text) ->
             let indent = String.make ((level - 1) * 2) ' ' in
             let is_selected = i = selected in
             let style =
               if is_selected then
                 Style.(bg (Index 237) ++ fg (Index 214) ++ bold)
               else Style.(fg (Index 250))
             in
             text ~style
               (Printf.sprintf "%s%s %s"
                  (if is_selected then ">" else " ")
                  indent heading_text))
           headings);
    ]

(* Main markdown viewer app *)
let markdown_viewer_app () =
  let open Ui in
  (* Use the new scroll hook with momentum enabled *)
  let scroll_offset, scroll_by, set_scroll_offset =
    use_scroll ~initial:0 ~min_offset:0 ()
  in
  let selected_heading, _set_selected_heading, update_selected_heading =
    use_state 0
  in
  let window_height, set_window_height, _ = use_state 24 in
  let window_width, set_window_width, _ = use_state 80 in

  (* Extract headings for TOC *)
  let headings = extract_headings sample_markdown in

  (* Subscribe to keyboard events for scrolling *)
  use_subscription
    (Sub.keyboard_filter (fun event ->
         match event.Input.key with
         | Input.Down ->
             scroll_by 1;
             Some ()
         | Input.Up ->
             scroll_by (-1);
             Some ()
         | Input.Page_down ->
             scroll_by 10;
             Some ()
         | Input.Page_up ->
             scroll_by (-10);
             Some ()
         | Input.Home ->
             set_scroll_offset 0;
             Some ()
         | Input.Char c when Uchar.to_int c = 0x6A ->
             (* 'j' *)
             update_selected_heading (fun x ->
                 min (List.length headings - 1) (x + 1));
             Some ()
         | Input.Char c when Uchar.to_int c = 0x6B ->
             (* 'k' *)
             update_selected_heading (fun x -> max 0 (x - 1));
             Some ()
         | Input.Enter ->
             (* Jump to selected heading - simplified for now *)
             let jump_offset = selected_heading * 5 in
             set_scroll_offset jump_offset;
             Some ()
         | Input.Char c when Uchar.to_int c = 0x71 ->
             (* 'q' *)
             exit 0
         (* dispatch_cmd doesn't work in subscriptions outside render context *)
         (* Some () *)
         | _ -> None));

  (* Subscribe to mouse scroll events *)
  use_subscription
    (Sub.mouse_filter (fun event ->
         match event with
         | Input.Button_press (_, _, Input.Wheel_down, _) ->
             scroll_by 3;
             Some ()
         | Input.Button_press (_, _, Input.Wheel_up, _) ->
             scroll_by (-3);
             Some ()
         | _ -> None));

  (* Subscribe to window resize *)
  use_subscription
    (Sub.window (fun size ->
         set_window_height size.height;
         set_window_width size.width;
         ()));

  (* Render markdown content *)
  let content_width = window_width - 28 in
  (* Leave space for TOC and separator *)
  let rendered_content =
    Mosaic_markdown.render ~width:content_width
      ~style:Mosaic_markdown.Style.default sample_markdown
  in

  (* Layout: TOC on left, content on right *)
  let toc_width = 25 in

  vbox
    [
      (* Header *)
      hbox
        ~style:Style.(bg (Index 235) ++ fg (Index 214) ++ bold)
        [
          text " [MD] Markdown Viewer ";
          spacer ~flex_grow:1. ();
          text ~style:Style.(fg (Index 244)) " [arrows/jk] Navigate [q] Quit ";
        ];
      (* Main content area *)
      hbox
        [
          (* TOC sidebar *)
          vbox ~min_width:(`Cells toc_width)
            ~min_height:(`Cells (window_height - 3))
            ~style:Style.(bg (Index 233))
            [
              toc_component ~headings ~selected:selected_heading;
              spacer ~flex_grow:1. () (* Fill remaining vertical space *);
            ];
          (* Separator *)
          vbox ~min_width:(`Cells 1)
            ~min_height:(`Cells (window_height - 3))
            ~style:Style.(bg (Index 233) ++ fg (Index 238))
            [ text "|"; spacer ~flex_grow:1. () ];
          (* Content area with scrolling using scroll_view *)
          scroll_view ~min_width:(`Cells content_width)
            ~min_height:(`Cells (window_height - 3))
            ~h_offset:0 ~v_offset:scroll_offset
            ~style:Style.(bg (Index 234))
            rendered_content;
        ];
      (* Status bar *)
      hbox
        ~style:Style.(bg (Index 236) ++ fg (Index 250))
        [
          text
            (Printf.sprintf " Scroll: %d | Heading: %d/%d " scroll_offset
               selected_heading (List.length headings));
          spacer ~flex_grow:1. ();
          text (Printf.sprintf " %dx%d " window_width window_height);
        ];
    ]

(* Main entry point *)
let () =
  Eio_main.run @@ fun _env ->
  Mosaic.run ~alt_screen:true ~mouse:true ~debug:"mosaic-markdown-viewer.log"
    markdown_viewer_app
