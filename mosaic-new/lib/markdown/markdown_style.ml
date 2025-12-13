let palette idx = Ansi.Color.of_palette_index idx

type block = {
  text_style : Ansi.Style.t;
  margin_top : int;
  margin_bottom : int;
  padding_left : int;
  padding_right : int;
}

type list_block = {
  block : block;
  item_prefix : string;
  item_prefix_style : Ansi.Style.t;
  item_gap : int;
  level_indent : int;
  task_style : Ansi.Style.t;
  checked_style : Ansi.Style.t;
}

type code_block = {
  block : block;
  lang_style : Ansi.Style.t;
  fence_style : Ansi.Style.t;
  show_fences : bool;
}

type table_block = {
  block : block;
  header_style : Ansi.Style.t;
  cell_style : Ansi.Style.t;
  separator_style : Ansi.Style.t * string;
  box_style : Mosaic_ui.Table.box_style;
}

type t = {
  document : block;
  paragraph : block;
  heading : block;
  heading_prefix : Ansi.Style.t;
  h1 : block;
  h2 : block;
  h3 : block;
  h4 : block;
  h5 : block;
  h6 : block;
  block_quote : block;
  code_block : code_block;
  horizontal_rule : Ansi.Style.t * string;
  list : list_block;
  table : table_block;
  emph : Ansi.Style.t;
  strong : Ansi.Style.t;
  code : Ansi.Style.t;
  link : Ansi.Style.t;
  image : Ansi.Style.t;
  html : Ansi.Style.t;
  strike : Ansi.Style.t;
}

let block ?(text_style = Ansi.Style.default) ?(margin_top = 0)
    ?(margin_bottom = 0) ?(padding_left = 0) ?(padding_right = 0) () =
  { text_style; margin_top; margin_bottom; padding_left; padding_right }

let default =
  let heading_colors = [ 228; 220; 214; 208; 202; 196 ] |> List.map palette in
  let make_heading color =
    block
      ~text_style:(Ansi.Style.make ~bold:true ~fg:color ())
      ~margin_bottom:1 ()
  in
  let list_block =
    {
      block = block ~margin_bottom:1 ();
      item_prefix = "•";
      item_prefix_style = Ansi.Style.make ~fg:(palette 39) ();
      item_gap = 1;
      level_indent = 0;
      task_style = Ansi.Style.make ~fg:(palette 244) ();
      checked_style = Ansi.Style.make ~fg:(palette 39) ();
    }
  in
  let table_block =
    {
      block = block ~margin_bottom:1 ();
      header_style = Ansi.Style.make ~bold:true ();
      cell_style = Ansi.Style.default;
      separator_style = (Ansi.Style.make ~fg:(palette 240) (), "-");
      box_style = Mosaic_ui.Table.Heavy_head;
    }
  in
  {
    document = block ();
    paragraph = block ~margin_bottom:1 ();
    heading =
      block ~text_style:(Ansi.Style.make ~bold:true ~fg:(palette 39) ()) ();
    heading_prefix = Ansi.Style.make ~fg:(palette 244) ();
    h1 = make_heading (List.nth heading_colors 0);
    h2 = make_heading (List.nth heading_colors 1);
    h3 = make_heading (List.nth heading_colors 2);
    h4 = make_heading (List.nth heading_colors 3);
    h5 = make_heading (List.nth heading_colors 4);
    h6 = make_heading (List.nth heading_colors 5);
    block_quote =
      block ~text_style:Ansi.Style.default ~padding_left:1 ~margin_bottom:1 ();
    code_block =
      {
        block =
          block ~padding_left:0 ~padding_right:0 ~margin_bottom:1
            ~text_style:(Ansi.Style.make ~fg:(palette 252) ())
            ();
        lang_style = Ansi.Style.make ~fg:(palette 244) ();
        fence_style = Ansi.Style.make ~fg:(palette 240) ();
        show_fences = true;
      };
    horizontal_rule = (Ansi.Style.make ~fg:(palette 240) (), "─");
    list = list_block;
    table = table_block;
    emph = Ansi.Style.make ~italic:true ();
    strong = Ansi.Style.make ~bold:true ();
    code = Ansi.Style.make ~fg:(palette 203) ~bg:(palette 236) ();
    link = Ansi.Style.make ~fg:(palette 30) ~underline:true ();
    image = Ansi.Style.make ~fg:(palette 212) ();
    html = Ansi.Style.make ~fg:(palette 244) ~italic:true ();
    strike = Ansi.Style.make ~strikethrough:true ();
  }
