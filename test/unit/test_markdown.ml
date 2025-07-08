open Mosaic
open Test_utils
open Alcotest

let render_to_string ui =
  let buffer = Render.create 100 50 in
  Ui.render buffer ui;
  let width, height = Render.dimensions buffer in
  let buf = Buffer.create ((width + 1) * height) in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let cell = Render.get buffer x y in
      match cell.Render.chars with
      | [] -> Buffer.add_char buf ' '
      | ch :: _ -> Buffer.add_utf_8_uchar buf ch
    done;
    if y < height - 1 then Buffer.add_char buf '\n'
  done;
  Buffer.contents buf |> String.trim

let test_paragraphs () =
  let md = "This is a simple paragraph." in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "This is a simple paragraph.";
  
  let md = "First paragraph.\n\nSecond paragraph." in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "First paragraph.";
  assert_output_contains output "Second paragraph."

let test_headings () =
  let test_heading level =
    let md = String.make level '#' ^ " Heading " ^ string_of_int level in
    let ui = Mosaic_markdown.render md in
    let output = render_to_string ui in
    assert_output_contains output ("Heading " ^ string_of_int level)
  in
  List.iter test_heading [1; 2; 3; 4; 5; 6]

let test_emphasis () =
  let md = "This has *emphasis* and **strong emphasis**." in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "emphasis";
  assert_output_contains output "strong emphasis";
  
  let md = "Also works with _underscore_ and __double underscore__." in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "underscore";
  assert_output_contains output "double underscore"

let test_inline_code () =
  let md = "Use `code` for inline code." in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output " code";
  
  let md = "Multiple backticks: ``code with ` backtick``" in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output " code with ` backtick"

let test_links () =
  let md = "Check out [this link](https://example.com)." in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "this link";
  
  let md = "![Alt text](image.png)" in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "[Image: Alt text <image.png>]"

let test_unordered_lists () =
  let md = "- First item\n- Second item\n- Third item" in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "• First item";
  assert_output_contains output "• Second item";
  assert_output_contains output "• Third item";
  
  let md = "* Using asterisks\n* Also works\n  * Nested item" in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "• Using asterisks";
  assert_output_contains output "• Nested item"

let test_ordered_lists () =
  let md = "1. First\n2. Second\n3. Third" in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "1. First";
  assert_output_contains output "2. Second";
  assert_output_contains output "3. Third";
  
  let md = "1. Starting at one\n1. All ones\n1. Still increments" in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "1. Starting at one";
  assert_output_contains output "2. All ones";
  assert_output_contains output "3. Still increments"

let test_code_blocks () =
  let md = "```\nplain code block\nwith multiple lines\n```" in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "```";
  assert_output_contains output "plain code block";
  assert_output_contains output "with multiple lines";
  
  let md = "```ocaml\nlet x = 42\nlet y = x + 1\n```" in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "```ocaml";
  assert_output_contains output "let x = 42"

let test_blockquotes () =
  let md = "> This is a quote\n> with multiple lines" in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  (* In CommonMark, consecutive lines form a single paragraph *)
  assert_output_contains output "│";
  assert_output_contains output "This is a quote";
  
  let md = "> Level 1\n> > Level 2 nested" in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "│";
  assert_output_contains output "Level 1";
  assert_output_contains output "Level 2 nested"

let test_horizontal_rules () =
  let md = "---" in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "----";
  
  let md = "***" in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "----"

let test_mixed_content () =
  let md = {|# Main Title

This is a paragraph with **bold** and *italic* text.

## Features

- First feature with `code`
- Second feature with [link](https://example.com)
  - Nested item

```ocaml
let greet name = 
  Printf.printf "Hello, %s!" name
```

> Important note: This is a blockquote.

---

Final paragraph.|} in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  
  assert_output_contains output "# Main Title";
  assert_output_contains output "bold";
  assert_output_contains output "italic";
  assert_output_contains output "## Features";
  assert_output_contains output "• First feature";
  assert_output_contains output " code ";
  assert_output_contains output "link";
  assert_output_contains output "let greet name";
  assert_output_contains output "│ Important note";
  assert_output_contains output "----";
  assert_output_contains output "Final paragraph."

let test_custom_style () =
  let custom_style = 
    let open Mosaic_markdown.Style in
    { default with
      paragraph = { default.paragraph with margin_bottom = 3 };
      h1 = { default.h1 with style = Mosaic.Style.(bold ++ fg Red) };
      list = { default.list with item_prefix = "→" };
    }
  in
  
  let md = "# Red Heading\n\nParagraph\n\n- Custom bullet" in
  let ui = Mosaic_markdown.render ~style:custom_style md in
  let output = render_to_string ui in
  
  assert_output_contains output "# Red Heading";
  assert_output_contains output "→ Custom bullet"

let test_empty_markdown () =
  let md = "" in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  (* Empty markdown should produce empty output *)
  check string "Empty markdown should produce empty output" "" output

let test_width_parameter () =
  let md = "This is a test" in
  let ui1 = Mosaic_markdown.render ~width:40 md in
  let ui2 = Mosaic_markdown.render ~width:80 md in
  (* Both should work without errors *)
  let _ = render_to_string ui1 in
  let _ = render_to_string ui2 in
  ()

let test_special_characters () =
  let md = "Special: < > & \" ' chars" in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "Special: < > & \" ' chars";
  
  let md = "Escaped: \\* \\_ \\` \\[" in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "* _ ` ["

let test_nested_emphasis () =
  let md = "***bold and italic*** and **bold with *nested italic***" in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "bold and italic";
  assert_output_contains output "nested italic"

let test_complex_lists () =
  let md = {|1. First ordered item
   - Nested unordered
   - Another nested
2. Second ordered item
   1. Nested ordered
   2. Another nested ordered
3. Third with paragraph

   This is a paragraph in the list.
   
   - And a nested list after paragraph|} in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "First ordered item";
  assert_output_contains output "• Nested unordered";
  assert_output_contains output "Second ordered item";
  assert_output_contains output "Nested ordered";
  assert_output_contains output "This is a paragraph in the list."

let test_string_of_inlines () =
  (* Test internal function through link text *)
  let md = "[Complex *emphasized* and **strong** text](url)" in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "Complex emphasized and strong text"

let test_reference_links () =
  let md = "[reference link][ref]\n\n[ref]: https://example.com" in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "reference link"

let test_autolinks () =
  (* Skip autolinks test for now - they might not be parsed as autolinks 
     by default in Cmarkit without extensions *)
  ()

let test_line_breaks () =
  let md = "Line one\\  \nLine two" in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "Line one";
  assert_output_contains output "Line two";
  
  let md = "Soft\nbreak" in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "Soft";
  assert_output_contains output "break"

let () =
  run "Markdown"
    [
      ("Basic elements", [
        test_case "paragraphs" `Quick test_paragraphs;
        test_case "headings" `Quick test_headings;
        test_case "horizontal rules" `Quick test_horizontal_rules;
      ]);
      ("Inline elements", [
        test_case "emphasis" `Quick test_emphasis;
        test_case "inline code" `Quick test_inline_code;
        test_case "links" `Quick test_links;
        test_case "reference links" `Quick test_reference_links;
        test_case "autolinks" `Quick test_autolinks;
        test_case "line breaks" `Quick test_line_breaks;
      ]);
      ("Block elements", [
        test_case "unordered lists" `Quick test_unordered_lists;
        test_case "ordered lists" `Quick test_ordered_lists;
        test_case "code blocks" `Quick test_code_blocks;
        test_case "blockquotes" `Quick test_blockquotes;
      ]);
      ("Advanced features", [
        test_case "mixed content" `Quick test_mixed_content;
        test_case "custom style" `Quick test_custom_style;
        test_case "nested emphasis" `Quick test_nested_emphasis;
        test_case "complex lists" `Quick test_complex_lists;
      ]);
      ("Edge cases", [
        test_case "empty markdown" `Quick test_empty_markdown;
        test_case "width parameter" `Quick test_width_parameter;
        test_case "special characters" `Quick test_special_characters;
        test_case "string of inlines" `Quick test_string_of_inlines;
      ]);
    ]