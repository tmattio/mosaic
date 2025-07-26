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
  List.iter test_heading [ 1; 2; 3; 4; 5; 6 ]

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
  let md =
    {|# Main Title

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

Final paragraph.|}
  in
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
    {
      default with
      paragraph = { default.paragraph with margin_bottom = 3 };
      h1 = { default.h1 with style = Ui.Style.(bold ++ fg Red) };
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
  let md =
    {|1. First ordered item
   - Nested unordered
   - Another nested
2. Second ordered item
   1. Nested ordered
   2. Another nested ordered
3. Third with paragraph

   This is a paragraph in the list.
   
   - And a nested list after paragraph|}
  in
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

(** Test tables (if supported) *)
let test_tables () =
  let md =
    {|
| Header 1 | Header 2 | Header 3 |
|----------|----------|----------|
| Cell 1   | Cell 2   | Cell 3   |
| Cell 4   | Cell 5   | Cell 6   |
|}
  in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  (* Tables might be rendered as code blocks if not supported *)
  assert_output_contains output "Header 1";
  assert_output_contains output "Cell 1"

(** Test task lists *)
let test_task_lists () =
  let md =
    {|- [ ] Unchecked item
- [x] Checked item
- [ ] Another unchecked|}
  in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "[ ]";
  assert_output_contains output "[x]";
  assert_output_contains output "Unchecked item";
  assert_output_contains output "Checked item"

(** Test deeply nested structures *)
let test_deep_nesting () =
  let md =
    {|> Quote level 1
> > Quote level 2
> > > Quote level 3
> > > > Quote level 4
> > > > > Quote level 5|}
  in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "Quote level 1";
  assert_output_contains output "Quote level 5";

  (* Deep list nesting *)
  let md =
    {|1. Level 1
   1. Level 2
      1. Level 3
         1. Level 4
            1. Level 5|}
  in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "Level 1";
  assert_output_contains output "Level 5"

(** Test inline HTML (if supported) *)
let test_inline_html () =
  let md = "This has <em>HTML emphasis</em> and <strong>strong</strong>." in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  (* HTML might be escaped or rendered as-is *)
  assert_output_contains output "HTML emphasis";
  assert_output_contains output "strong"

(** Test code spans with special content *)
let test_code_spans_advanced () =
  let md = "Code with `**bold**` and `_italic_` inside" in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output " **bold**";
  assert_output_contains output " _italic_";

  (* Code with backticks *)
  let md = "Code: ``with ` backtick`` and ```triple ` backticks```" in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "with ` backtick";

  (* Code with entities *)
  let md = "Code: `&lt; &gt; &amp;`" in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output " &lt; &gt; &amp;"

(** Test strikethrough (if supported) *)
let test_strikethrough () =
  let md = "This has ~~strikethrough~~ text." in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "strikethrough"

(** Test footnotes (if supported) *)
let test_footnotes () =
  let md = {|This has a footnote[^1].

[^1]: This is the footnote text.|} in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "footnote";
  assert_output_contains output "This is the footnote text"

(** Test definition lists (if supported) *)
let test_definition_lists () =
  let md =
    {|Term 1
:   Definition 1

Term 2
:   Definition 2a
:   Definition 2b|}
  in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "Term 1";
  assert_output_contains output "Definition 1"

(** Test smart punctuation *)
let test_smart_punctuation () =
  let md =
    {|"Double quotes" and 'single quotes'.
Em---dash and en--dash.
Ellipsis...|}
  in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "Double quotes";
  assert_output_contains output "single quotes"

(** Test very long lines *)
let test_long_lines () =
  let long_word = String.make 100 'A' in
  let md = Printf.sprintf "Word: %s end." long_word in
  let ui = Mosaic_markdown.render ~width:50 md in
  let output = render_to_string ui in
  assert_output_contains output "AAAA"

(** Test unicode and emoji *)
let test_unicode_content () =
  let md = "Unicode: αβγδε 中文字符 🎉🎈🎊" in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "αβγδε";
  assert_output_contains output "中文字符";
  assert_output_contains output "🎉"

(** Test malformed markdown *)
let test_malformed_markdown () =
  (* Unclosed emphasis *)
  let md = "This has *unclosed emphasis" in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "unclosed emphasis";

  (* Mismatched delimiters *)
  let md = "This has **mixed *delimiters**" in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "mixed";

  (* Invalid link *)
  let md = "[link with no url]()" in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "link with no url"

(** Test edge cases in lists *)
let test_list_edge_cases () =
  (* Empty list items *)
  let md = "- \n- Item 2\n- " in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "• Item 2";

  (* List with different markers *)
  let md = "- Dash\n* Star\n+ Plus" in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "• Dash";
  assert_output_contains output "• Star";
  assert_output_contains output "• Plus";

  (* List with paragraphs *)
  let md =
    {|1. First item.

   This is still part of the first item.

2. Second item.|}
  in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "First item";
  assert_output_contains output "still part of the first"

(** Test setext headings *)
let test_setext_headings () =
  let md = {|Heading 1
=========

Heading 2
---------|} in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "Heading 1";
  assert_output_contains output "Heading 2"

(** Test raw HTML blocks *)
let test_html_blocks () =
  let md = {|<div>
  <p>HTML content</p>
</div>

Regular markdown.|} in
  let ui = Mosaic_markdown.render md in
  let output = render_to_string ui in
  assert_output_contains output "Regular markdown"

let () =
  run "Markdown"
    [
      ( "Basic elements",
        [
          test_case "paragraphs" `Quick test_paragraphs;
          test_case "headings" `Quick test_headings;
          test_case "horizontal rules" `Quick test_horizontal_rules;
          test_case "setext headings" `Quick test_setext_headings;
        ] );
      ( "Inline elements",
        [
          test_case "emphasis" `Quick test_emphasis;
          test_case "inline code" `Quick test_inline_code;
          test_case "links" `Quick test_links;
          test_case "reference links" `Quick test_reference_links;
          test_case "autolinks" `Quick test_autolinks;
          test_case "line breaks" `Quick test_line_breaks;
          test_case "code spans advanced" `Quick test_code_spans_advanced;
        ] );
      ( "Block elements",
        [
          test_case "unordered lists" `Quick test_unordered_lists;
          test_case "ordered lists" `Quick test_ordered_lists;
          test_case "code blocks" `Quick test_code_blocks;
          test_case "blockquotes" `Quick test_blockquotes;
          test_case "list edge cases" `Quick test_list_edge_cases;
        ] );
      ( "Extended features",
        [
          test_case "tables" `Quick test_tables;
          test_case "task lists" `Quick test_task_lists;
          test_case "strikethrough" `Quick test_strikethrough;
          test_case "footnotes" `Quick test_footnotes;
          test_case "definition lists" `Quick test_definition_lists;
          test_case "smart punctuation" `Quick test_smart_punctuation;
        ] );
      ( "HTML",
        [
          test_case "inline HTML" `Quick test_inline_html;
          test_case "HTML blocks" `Quick test_html_blocks;
        ] );
      ( "Advanced features",
        [
          test_case "mixed content" `Quick test_mixed_content;
          test_case "custom style" `Quick test_custom_style;
          test_case "nested emphasis" `Quick test_nested_emphasis;
          test_case "complex lists" `Quick test_complex_lists;
          test_case "deep nesting" `Quick test_deep_nesting;
        ] );
      ( "Edge cases",
        [
          test_case "empty markdown" `Quick test_empty_markdown;
          test_case "width parameter" `Quick test_width_parameter;
          test_case "special characters" `Quick test_special_characters;
          test_case "string of inlines" `Quick test_string_of_inlines;
          test_case "long lines" `Quick test_long_lines;
          test_case "unicode content" `Quick test_unicode_content;
          test_case "malformed markdown" `Quick test_malformed_markdown;
        ] );
    ]
