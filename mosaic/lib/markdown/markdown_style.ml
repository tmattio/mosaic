open Ui

type block = {
  style : Style.t;
  margin_top : int;
  margin_bottom : int;
  padding_left : int;
  padding_right : int;
}

type list_block = {
  block : block;
  item_prefix : string;
  item_prefix_style : Style.t;
  item_gap : int;
  level_indent : int;
  task_style : Style.t;
  checked_style : Style.t;
}

type code_block = { block : block; lang_style : Style.t; fence_style : Style.t }

type table_block = {
  block : block;
  header_style : Style.t;
  separator_style : Style.t * string;
}

type t = {
  document : block;
  paragraph : block;
  heading : block;
  heading_prefix : Style.t;
  h1 : block;
  h2 : block;
  h3 : block;
  h4 : block;
  h5 : block;
  h6 : block;
  block_quote : block;
  code_block : code_block;
  horizontal_rule : Style.t * string;
  list : list_block;
  table : table_block;
  emph : Style.t;
  strong : Style.t;
  code : Style.t;
  link : Style.t;
  image : Style.t;
  html : Style.t;
  strike : Style.t;
}

let default_block =
  {
    style = Style.empty;
    margin_top = 0;
    margin_bottom = 0;
    padding_left = 0;
    padding_right = 0;
  }

let default =
  {
    document = { default_block with margin_bottom = 0 };
    paragraph = { default_block with margin_bottom = 0 };
    heading =
      {
        default_block with
        margin_bottom = 0;
        style = Style.(bold ++ fg (Index 39));
      };
    heading_prefix = Style.(fg (Index 244));
    h1 =
      {
        default_block with
        margin_bottom = 0;
        style = Style.(bold ++ fg (Index 228));
      };
    h2 =
      {
        default_block with
        margin_bottom = 0;
        style = Style.(bold ++ fg (Index 220));
      };
    h3 =
      {
        default_block with
        margin_bottom = 0;
        style = Style.(bold ++ fg (Index 214));
      };
    h4 =
      {
        default_block with
        margin_bottom = 0;
        style = Style.(bold ++ fg (Index 208));
      };
    h5 =
      {
        default_block with
        margin_bottom = 0;
        style = Style.(bold ++ fg (Index 202));
      };
    h6 =
      {
        default_block with
        margin_bottom = 0;
        style = Style.(bold ++ fg (Index 196));
      };
    block_quote =
      {
        default_block with
        margin_bottom = 0;
        padding_left = 2;
        style = Style.(fg (Index 244));
      };
    code_block =
      {
        block =
          {
            default_block with
            margin_bottom = 0;
            padding_left = 2;
            padding_right = 2;
          };
        lang_style = Style.(fg (Index 244));
        fence_style = Style.(fg (Index 240));
      };
    horizontal_rule = (Style.(fg (Index 240)), "-");
    list =
      {
        block = { default_block with margin_bottom = 0 };
        item_prefix = "•";
        item_prefix_style = Style.(fg (Index 39));
        item_gap = 1;
        level_indent = 2;
        task_style = Style.(fg (Index 244));
        checked_style = Style.(fg (Index 39));
      };
    table =
      {
        block = { default_block with margin_bottom = 0 };
        header_style = Style.bold;
        separator_style = (Style.(fg (Index 240)), "-");
      };
    emph = Style.italic;
    strong = Style.bold;
    code = Style.(fg (Index 203) ++ bg (Index 236));
    link = Style.(underline ++ fg (Index 30));
    image = Style.(fg (Index 212));
    html = Style.(fg (Index 244) ++ italic);
    strike = Style.strikethrough;
  }
