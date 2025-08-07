(* Use qualified names instead of open to avoid name conflicts *)
module E = Element

type justify = [ `Left | `Center | `Right | `Full ]
type vertical_align = [ `Top | `Middle | `Bottom ]
type overflow = [ `Ellipsis | `Crop | `Fold ]

type column = {
  header : string;
  footer : string option;
  header_style : Style.t;
  footer_style : Style.t;
  style : Style.t;
  justify : justify;
  vertical : vertical_align;
  overflow : overflow;
  width : int option;
  min_width : int option;
  max_width : int option;
  ratio : int option;
  no_wrap : bool;
}

type padding = int * int * int * int (* top, right, bottom, left *)

type box_style =
  | NoBox
  | Simple
  | Rounded
  | Heavy
  | HeavyHead
  | Double
  | DoubleEdge
  | Ascii
  | MinimalHeavyHead
  | MinimalDoubleHead
  | Minimal
  | Square
  | SquareDoubleHead

type measurement = { min : int; max : int }

type box_chars = {
  head_left : string;
  head_right : string;
  head_vertical : string;
  head_horizontal : string;
  row_left : string;
  row_right : string;
  row_vertical : string;
  row_horizontal : string;
  mid_left : string;
  mid_right : string;
  mid_vertical : string;
  mid_horizontal : string;
  foot_left : string;
  foot_right : string;
  foot_vertical : string;
  foot_horizontal : string;
  vertical : string; (* for straight vertical borders *)
}

let get_box_chars box_style safe_box =
  let ascii_fallback = safe_box in
  match box_style with
  | NoBox ->
      {
        head_left = "";
        head_right = "";
        head_vertical = "";
        head_horizontal = "";
        (* all empty *) row_left = "";
        row_right = "";
        row_vertical = "";
        row_horizontal = "";
        mid_left = "";
        mid_right = "";
        mid_vertical = "";
        mid_horizontal = "";
        foot_left = "";
        foot_right = "";
        foot_vertical = "";
        foot_horizontal = "";
        vertical = "";
      }
  | HeavyHead ->
      if ascii_fallback then
        {
          head_left = "+";
          head_right = "+";
          head_vertical = "+";
          head_horizontal = "-";
          row_left = "+";
          row_right = "+";
          row_vertical = "+";
          row_horizontal = "-";
          mid_left = "+";
          mid_right = "+";
          mid_vertical = "+";
          mid_horizontal = "-";
          foot_left = "+";
          foot_right = "+";
          foot_vertical = "+";
          foot_horizontal = "-";
          vertical = "|";
        }
      else
        {
          head_left = "┏";
          head_right = "┓";
          head_vertical = "┳";
          head_horizontal = "━";
          row_left = "├";
          row_right = "┤";
          row_vertical = "┼";
          row_horizontal = "─";
          mid_left = "├";
          mid_right = "┤";
          mid_vertical = "┼";
          mid_horizontal = "─";
          foot_left = "└";
          foot_right = "┘";
          foot_vertical = "┴";
          foot_horizontal = "─";
          vertical = "│";
        }
  | Simple ->
      if ascii_fallback then
        {
          head_left = "";
          head_right = "";
          head_vertical = "";
          head_horizontal = "-";
          row_left = "";
          row_right = "";
          row_vertical = "";
          row_horizontal = "-";
          mid_left = "";
          mid_right = "";
          mid_vertical = "";
          mid_horizontal = "-";
          foot_left = "";
          foot_right = "";
          foot_vertical = "";
          foot_horizontal = "-";
          vertical = "|";
        }
      else
        {
          head_left = "";
          head_right = "";
          head_vertical = "";
          head_horizontal = "─";
          row_left = "";
          row_right = "";
          row_vertical = "";
          row_horizontal = "─";
          mid_left = "";
          mid_right = "";
          mid_vertical = "";
          mid_horizontal = "─";
          foot_left = "";
          foot_right = "";
          foot_vertical = "";
          foot_horizontal = "─";
          vertical = "│";
        }
  | Rounded ->
      if ascii_fallback then
        {
          head_left = "/";
          head_right = "\\";
          head_vertical = "-";
          head_horizontal = "-";
          row_left = "|";
          row_right = "|";
          row_vertical = "+";
          row_horizontal = "-";
          mid_left = "|";
          mid_right = "|";
          mid_vertical = "+";
          mid_horizontal = "-";
          foot_left = "\\";
          foot_right = "/";
          foot_vertical = "-";
          foot_horizontal = "-";
          vertical = "|";
        }
      else
        {
          head_left = "╭";
          head_right = "╮";
          head_vertical = "┬";
          head_horizontal = "─";
          row_left = "│";
          row_right = "│";
          row_vertical = "┼";
          row_horizontal = "─";
          mid_left = "├";
          mid_right = "┤";
          mid_vertical = "┼";
          mid_horizontal = "─";
          foot_left = "╰";
          foot_right = "╯";
          foot_vertical = "┴";
          foot_horizontal = "─";
          vertical = "│";
        }
  | Heavy ->
      if ascii_fallback then
        {
          head_left = "+";
          head_right = "+";
          head_vertical = "+";
          head_horizontal = "=";
          row_left = "+";
          row_right = "+";
          row_vertical = "+";
          row_horizontal = "=";
          mid_left = "+";
          mid_right = "+";
          mid_vertical = "+";
          mid_horizontal = "=";
          foot_left = "+";
          foot_right = "+";
          foot_vertical = "+";
          foot_horizontal = "=";
          vertical = "|";
        }
      else
        {
          head_left = "┏";
          head_right = "┓";
          head_vertical = "┳";
          head_horizontal = "━";
          row_left = "┣";
          row_right = "┫";
          row_vertical = "╋";
          row_horizontal = "━";
          mid_left = "┣";
          mid_right = "┫";
          mid_vertical = "╋";
          mid_horizontal = "━";
          foot_left = "┗";
          foot_right = "┛";
          foot_vertical = "┻";
          foot_horizontal = "━";
          vertical = "┃";
        }
  | Double ->
      if ascii_fallback then
        {
          head_left = "+";
          head_right = "+";
          head_vertical = "+";
          head_horizontal = "=";
          row_left = "+";
          row_right = "+";
          row_vertical = "+";
          row_horizontal = "=";
          mid_left = "+";
          mid_right = "+";
          mid_vertical = "+";
          mid_horizontal = "=";
          foot_left = "+";
          foot_right = "+";
          foot_vertical = "+";
          foot_horizontal = "=";
          vertical = "|";
        }
      else
        {
          head_left = "╔";
          head_right = "╗";
          head_vertical = "╦";
          head_horizontal = "═";
          row_left = "╠";
          row_right = "╣";
          row_vertical = "╬";
          row_horizontal = "═";
          mid_left = "╠";
          mid_right = "╣";
          mid_vertical = "╬";
          mid_horizontal = "═";
          foot_left = "╚";
          foot_right = "╝";
          foot_vertical = "╩";
          foot_horizontal = "═";
          vertical = "║";
        }
  | Ascii ->
      {
        head_left = "+";
        head_right = "+";
        head_vertical = "+";
        head_horizontal = "-";
        row_left = "+";
        row_right = "+";
        row_vertical = "+";
        row_horizontal = "-";
        mid_left = "+";
        mid_right = "+";
        mid_vertical = "+";
        mid_horizontal = "-";
        foot_left = "+";
        foot_right = "+";
        foot_vertical = "+";
        foot_horizontal = "-";
        vertical = "|";
      }
  | Minimal ->
      {
        head_left = "";
        head_right = "";
        head_vertical = "";
        head_horizontal = " ";
        row_left = "";
        row_right = "";
        row_vertical = "";
        row_horizontal = " ";
        mid_left = "";
        mid_right = "";
        mid_vertical = "";
        mid_horizontal = " ";
        foot_left = "";
        foot_right = "";
        foot_vertical = "";
        foot_horizontal = " ";
        vertical = " ";
      }
  | DoubleEdge ->
      if ascii_fallback then
        {
          head_left = "+";
          head_right = "+";
          head_vertical = "+";
          head_horizontal = "=";
          row_left = "|";
          row_right = "|";
          row_vertical = "|";
          row_horizontal = "-";
          mid_left = "|";
          mid_right = "|";
          mid_vertical = "|";
          mid_horizontal = "-";
          foot_left = "+";
          foot_right = "+";
          foot_vertical = "+";
          foot_horizontal = "=";
          vertical = "|";
        }
      else
        {
          head_left = "╔";
          head_right = "╗";
          head_vertical = "╤";
          head_horizontal = "═";
          row_left = "│";
          row_right = "│";
          row_vertical = "┼";
          row_horizontal = "─";
          mid_left = "├";
          mid_right = "┤";
          mid_vertical = "┼";
          mid_horizontal = "─";
          foot_left = "╚";
          foot_right = "╝";
          foot_vertical = "╧";
          foot_horizontal = "═";
          vertical = "║";
        }
  | MinimalHeavyHead ->
      if ascii_fallback then
        {
          head_left = "";
          head_right = "";
          head_vertical = " ";
          head_horizontal = "-";
          row_left = "";
          row_right = "";
          row_vertical = " ";
          row_horizontal = " ";
          mid_left = "";
          mid_right = "";
          mid_vertical = " ";
          mid_horizontal = " ";
          foot_left = "";
          foot_right = "";
          foot_vertical = " ";
          foot_horizontal = " ";
          vertical = " ";
        }
      else
        {
          head_left = "";
          head_right = "";
          head_vertical = " ";
          head_horizontal = "━";
          row_left = "";
          row_right = "";
          row_vertical = " ";
          row_horizontal = " ";
          mid_left = "";
          mid_right = "";
          mid_vertical = " ";
          mid_horizontal = " ";
          foot_left = "";
          foot_right = "";
          foot_vertical = " ";
          foot_horizontal = " ";
          vertical = " ";
        }
  | MinimalDoubleHead ->
      if ascii_fallback then
        {
          head_left = "";
          head_right = "";
          head_vertical = " ";
          head_horizontal = "=";
          row_left = "";
          row_right = "";
          row_vertical = " ";
          row_horizontal = " ";
          mid_left = "";
          mid_right = "";
          mid_vertical = " ";
          mid_horizontal = " ";
          foot_left = "";
          foot_right = "";
          foot_vertical = " ";
          foot_horizontal = " ";
          vertical = " ";
        }
      else
        {
          head_left = "";
          head_right = "";
          head_vertical = " ";
          head_horizontal = "═";
          row_left = "";
          row_right = "";
          row_vertical = " ";
          row_horizontal = " ";
          mid_left = "";
          mid_right = "";
          mid_vertical = " ";
          mid_horizontal = " ";
          foot_left = "";
          foot_right = "";
          foot_vertical = " ";
          foot_horizontal = " ";
          vertical = " ";
        }
  | Square ->
      if ascii_fallback then
        {
          head_left = "+";
          head_right = "+";
          head_vertical = "+";
          head_horizontal = "-";
          row_left = "|";
          row_right = "|";
          row_vertical = "|";
          row_horizontal = "-";
          mid_left = "|";
          mid_right = "|";
          mid_vertical = "|";
          mid_horizontal = "-";
          foot_left = "+";
          foot_right = "+";
          foot_vertical = "+";
          foot_horizontal = "-";
          vertical = "|";
        }
      else
        {
          head_left = "┌";
          head_right = "┐";
          head_vertical = "┬";
          head_horizontal = "─";
          row_left = "│";
          row_right = "│";
          row_vertical = "┼";
          row_horizontal = "─";
          mid_left = "├";
          mid_right = "┤";
          mid_vertical = "┼";
          mid_horizontal = "─";
          foot_left = "└";
          foot_right = "┘";
          foot_vertical = "┴";
          foot_horizontal = "─";
          vertical = "│";
        }
  | SquareDoubleHead ->
      if ascii_fallback then
        {
          head_left = "+";
          head_right = "+";
          head_vertical = "+";
          head_horizontal = "=";
          row_left = "|";
          row_right = "|";
          row_vertical = "|";
          row_horizontal = "-";
          mid_left = "|";
          mid_right = "|";
          mid_vertical = "|";
          mid_horizontal = "-";
          foot_left = "+";
          foot_right = "+";
          foot_vertical = "+";
          foot_horizontal = "-";
          vertical = "|";
        }
      else
        {
          head_left = "╔";
          head_right = "╗";
          head_vertical = "╦";
          head_horizontal = "═";
          row_left = "║";
          row_right = "║";
          row_vertical = "╬";
          row_horizontal = "─";
          mid_left = "╟";
          mid_right = "╢";
          mid_vertical = "╫";
          mid_horizontal = "─";
          foot_left = "╚";
          foot_right = "╝";
          foot_vertical = "╩";
          foot_horizontal = "─";
          vertical = "║";
        }

let default_column ~header =
  {
    header;
    footer = None;
    header_style = Style.empty;
    footer_style = Style.empty;
    style = Style.empty;
    justify = `Left;
    vertical = `Top;
    overflow = `Ellipsis;
    width = None;
    min_width = None;
    max_width = None;
    ratio = None;
    no_wrap = false;
  }

let ratio_distribute amount ratios mins =
  let total_r = List.fold_left ( + ) 0 ratios in
  if total_r = 0 then List.map (fun m -> m) mins
  else
    let parts = List.map (fun r -> r * amount / total_r) ratios in
    let sum_parts = List.fold_left ( + ) 0 parts in
    let remain = amount - sum_parts in
    let parts = List.mapi (fun i p -> if i < remain then p + 1 else p) parts in
    List.map2 max parts mins

let ratio_reduce amount ratios maxs values =
  let values = List.map2 min values maxs in
  let total = List.fold_left ( + ) 0 values in
  let total_r = List.fold_left ( + ) 0 ratios in
  if total_r = 0 then values
  else
    let rec loop values excess =
      if excess <= 0 then values
      else
        let total = List.fold_left ( + ) 0 values in
        let excess = total - amount in
        if excess <= 0 then values
        else
          let deductions = List.map (fun r -> r * excess / total_r) ratios in
          let new_excess = ref excess in
          let new_values =
            List.mapi
              (fun i v ->
                let d = List.nth deductions i in
                if d > 0 then (
                  let new_v = max 0 (v - d) in
                  new_excess := !new_excess - (v - new_v);
                  new_v)
                else v)
              values
          in
          loop new_values !new_excess
    in
    loop values (total - amount)

let collapse_widths widths wrapable max_width =
  let total = List.fold_left ( + ) 0 widths in
  let excess = total - max_width in
  let rec loop excess widths =
    if excess <= 0 || not (List.exists (fun x -> x) wrapable) then widths
    else
      let max_col =
        List.fold_left2
          (fun acc w a -> if a then max acc w else acc)
          0 widths wrapable
      in
      let second_max =
        List.fold_left2
          (fun acc w a -> if a && w <> max_col then max acc w else acc)
          0 widths wrapable
      in
      let col_diff = max_col - second_max in
      if col_diff <= 0 then widths
      else
        let ratios =
          List.map2
            (fun w a -> if a && w = max_col then 1 else 0)
            widths wrapable
        in
        let total_r = List.fold_left ( + ) 0 ratios in
        if total_r = 0 then widths
        else
          let max_reduce = List.map (fun _ -> min excess col_diff) widths in
          let reduced = ratio_reduce excess ratios max_reduce widths in
          loop (List.fold_left ( + ) 0 reduced - max_width) reduced
  in
  loop excess widths

let max_word_length str =
  let words = String.split_on_char ' ' str |> List.filter (fun s -> s <> "") in
  if words = [] then 1
  else List.fold_left (fun m w -> max m (String.length w)) 0 words

let cell_measurement (str : string) (column : column) =
  let lines = String.split_on_char '\n' str in
  if lines = [] || str = "" then { min = 0; max = 0 }
  else
    match column.overflow with
    | `Crop | `Ellipsis ->
        (* For crop/ellipsis, measure each line separately *)
        let line_lengths = List.map String.length lines in
        let max_len = List.fold_left max 0 line_lengths in
        { min = max_len; max = max_len }
    | `Fold ->
        (* For fold, consider wrapping of each line *)
        let measure_line line =
          if column.no_wrap then { min = 1; max = String.length line }
          else { min = max_word_length line; max = String.length line }
        in
        let line_measurements = List.map measure_line lines in
        let min_width =
          List.fold_left (fun acc m -> max acc m.min) 0 line_measurements
        in
        let max_width =
          List.fold_left (fun acc m -> max acc m.max) 0 line_measurements
        in
        { min = min_width; max = max_width }

let column_measurement (column : column) col_index show_header show_footer rows
    (pad_width : int) =
  let measurements =
    (if show_header then [ cell_measurement column.header column ] else [])
    @ List.map
        (fun row ->
          match List.nth_opt row col_index with
          | Some s -> cell_measurement s column
          | None -> { min = 0; max = 0 })
        rows
    @
    match column.footer with
    | Some f when show_footer -> [ cell_measurement f column ]
    | _ -> []
  in
  let col_min =
    List.fold_left (fun m meas -> max m meas.min) 0 measurements + pad_width
  in
  let col_max =
    List.fold_left (fun m meas -> max m meas.max) 0 measurements + pad_width
  in
  match column.width with
  | Some w -> { min = w; max = w }
  | None ->
      let col_min =
        match column.min_width with Some v -> max col_min v | None -> col_min
      in
      let col_max =
        match column.max_width with Some v -> min col_max v | None -> col_max
      in
      { min = col_min; max = col_max }

let get_effective_padding top right bottom left is_first_col is_last_col
    is_first_row is_last_row collapse_padding pad_edge =
  let e_top = ref top in
  let e_right = ref right in
  let e_bottom = ref bottom in
  let e_left = ref left in
  (* Simplified collapse_padding: padding "owned" by first/top element *)
  if collapse_padding then (
    if not is_first_col then e_left := 0;
    if not is_first_row then e_top := 0);
  if not pad_edge then (
    if is_first_col then e_left := 0;
    if is_last_col then e_right := 0;
    if is_first_row then e_top := 0;
    if is_last_row then e_bottom := 0);
  (!e_top, !e_right, !e_bottom, !e_left)

let calculate_column_widths columns rows max_width pad_width show_header
    show_footer expand box_style show_edge top_pad right_pad bottom_pad left_pad
    collapse_padding pad_edge =
  let num_cols = List.length columns in
  let extra_width =
    if box_style <> NoBox then (if show_edge then 2 else 0) + (num_cols - 1)
    else 0
  in
  let available = max_width - extra_width in
  (* Compute per-column max effective padding *)
  let get_max_eff_pad col_index =
    let compute_eff_pad is_first_row is_last_row =
      let is_first_col = col_index = 0 in
      let is_last_col = col_index = num_cols - 1 in
      let _, e_right, _, e_left =
        get_effective_padding top_pad right_pad bottom_pad left_pad is_first_col
          is_last_col is_first_row is_last_row collapse_padding pad_edge
      in
      e_left + e_right
    in
    let header_pad = if show_header then compute_eff_pad true false else 0 in
    let footer_pad = if show_footer then compute_eff_pad false true else 0 in
    let data_pads =
      List.mapi
        (fun row_idx _ ->
          let is_first_row = row_idx = 0 && not show_header in
          let is_last_row = row_idx = List.length rows - 1 && not show_footer in
          compute_eff_pad is_first_row is_last_row)
        rows
    in
    List.fold_left max 0 (header_pad :: footer_pad :: data_pads)
  in
  let measurements =
    List.mapi
      (fun i col ->
        let max_eff_pad = get_max_eff_pad i in
        let meas =
          column_measurement col i show_header show_footer rows max_eff_pad
        in
        { min = meas.min; max = meas.max })
      columns
  in
  let widths = List.map (fun m -> m.max) measurements in
  let table_width = List.fold_left ( + ) 0 widths in
  let flexible = List.map (fun col -> col.ratio <> None) columns in
  let widths =
    if table_width < available && expand && List.exists (fun x -> x) flexible
    then
      let ratios =
        List.map
          (fun col -> if col.ratio <> None then Option.get col.ratio else 0)
          columns
      in
      let fixed_widths =
        List.mapi
          (fun i m -> if List.nth flexible i then 0 else m.max)
          measurements
      in
      let flex_mins =
        List.mapi
          (fun i col ->
            if List.nth flexible i then
              Option.value col.width ~default:1 + pad_width
            else 0)
          columns
      in
      let flexible_width = available - List.fold_left ( + ) 0 fixed_widths in
      let flex_widths = ratio_distribute flexible_width ratios flex_mins in
      let j = ref 0 in
      List.mapi
        (fun i _ ->
          if List.nth flexible i then (
            let fw = List.nth flex_widths !j in
            incr j;
            List.nth fixed_widths i + fw)
          else List.nth fixed_widths i)
        measurements
    else widths
  in
  let table_width = List.fold_left ( + ) 0 widths in
  let widths =
    if table_width > available then
      let wrapable =
        List.map (fun col -> col.width = None && not col.no_wrap) columns
      in
      let widths = collapse_widths widths wrapable available in
      let table_width = List.fold_left ( + ) 0 widths in
      if table_width > available then
        ratio_reduce (table_width - available)
          (List.init num_cols (fun _ -> 1))
          widths widths
      else widths
    else widths
  in
  let table_width = List.fold_left ( + ) 0 widths in
  if table_width < available && expand then
    let extra = available - table_width in
    let pad_widths =
      ratio_distribute extra
        (List.map (fun w -> w) widths)
        (List.init num_cols (fun _ -> 0))
    in
    List.map2 ( + ) widths pad_widths
  else widths

let truncate_string str width suffix =
  let suffix_len = String.length suffix in
  if String.length str <= width then str
  else if width <= suffix_len then String.sub str 0 width
  else String.sub str 0 (width - suffix_len) ^ suffix

let word_wrap str width =
  if str = "" then [ "" ]
  else
    let lines = ref [] in
    let current = ref "" in
    String.split_on_char ' ' str
    |> List.iter (fun word ->
           if
             (String.length !current + String.length word
             + if !current = "" then 0 else 1)
             <= width
           then current := !current ^ (if !current = "" then "" else " ") ^ word
           else (
             lines := !current :: !lines;
             current := word));
    if !current <> "" then lines := !current :: !lines;
    List.rev !lines

let chop str width =
  if str = "" then [ "" ]
  else
    let lines = ref [] in
    let s = ref str in
    while !s <> "" do
      let chunk_len = min (String.length !s) width in
      lines := String.sub !s 0 chunk_len :: !lines;
      s := String.sub !s chunk_len (String.length !s - chunk_len)
    done;
    List.rev !lines

let justify_string line width justify =
  let len = String.length line in
  if len >= width then line
  else
    match justify with
    | `Left -> line ^ String.make (width - len) ' '
    | `Right -> String.make (width - len) ' ' ^ line
    | `Center ->
        let left_pad = (width - len) / 2 in
        let right_pad = width - len - left_pad in
        String.make left_pad ' ' ^ line ^ String.make right_pad ' '
    | `Full -> line (* Full justification is handled separately *)

let distribute_spaces line width justify =
  if justify <> `Full then justify_string line width justify
  else
    let words =
      String.split_on_char ' ' line |> List.filter (fun s -> s <> "")
    in
    let nwords = List.length words in
    if nwords <= 1 then justify_string line width `Left
    else
      let base_len =
        List.fold_left (fun acc w -> acc + String.length w) 0 words
        + (nwords - 1)
      in
      let extra = width - base_len in
      let space = 1 + (extra / (nwords - 1)) in
      let remain = extra mod (nwords - 1) in
      let rec join acc ws i =
        match ws with
        | [] -> acc
        | [ last ] -> acc ^ last
        | w :: tl ->
            acc ^ w
            ^ String.make (space + if i < remain then 1 else 0) ' '
            ^ join "" tl (i + 1)
      in
      join "" words 0

let get_lines str column width justify last_line_full =
  (* First split by newlines to handle multi-line input *)
  let input_lines = String.split_on_char '\n' str in
  let process_line line =
    match column.overflow with
    | `Crop -> [ String.sub line 0 (min (String.length line) width) ]
    | `Ellipsis -> [ truncate_string line width "..." ]
    | `Fold -> if column.no_wrap then chop line width else word_wrap line width
  in
  (* Process each input line and flatten the results *)
  let all_lines = List.concat (List.map process_line input_lines) in
  List.mapi
    (fun i line ->
      distribute_spaces line width
        (if last_line_full || i < List.length all_lines - 1 then justify
         else `Left))
    all_lines

let repeat s n = String.concat "" (List.init n (fun _ -> s))

let build_horizontal_line level widths box_chars show_edge border_style =
  let left, right, divider, horizontal =
    match level with
    | "head" ->
        ( box_chars.head_left,
          box_chars.head_right,
          box_chars.head_vertical,
          box_chars.head_horizontal )
    | "foot" ->
        ( box_chars.foot_left,
          box_chars.foot_right,
          box_chars.foot_vertical,
          box_chars.foot_horizontal )
    | "row" ->
        ( box_chars.row_left,
          box_chars.row_right,
          box_chars.row_vertical,
          box_chars.row_horizontal )
    | "mid" ->
        ( box_chars.mid_left,
          box_chars.mid_right,
          box_chars.mid_vertical,
          box_chars.mid_horizontal )
    | _ -> ("", "", "", "")
  in
  let line =
    (if show_edge then left else "")
    ^ String.concat divider (List.map (fun w -> repeat horizontal w) widths)
    ^ if show_edge then right else ""
  in
  E.text ~style:border_style line

let build_vertical_sep height char style =
  if height <= 0 || char = "" then E.text ""
  else E.text ~style (String.concat "\n" (List.init height (fun _ -> char)))

let build_cell_element lines row_height vertical e_top e_bottom e_left e_right
    cell_style cell_width =
  let content_height = List.length lines in
  let extra = row_height - content_height - e_top - e_bottom in
  let top_spacer =
    match vertical with
    | `Top -> e_top
    | `Bottom -> e_top + extra
    | `Middle -> e_top + (extra / 2)
  in
  let bottom_spacer = e_top + e_bottom + extra - top_spacer in
  (* Apply horizontal padding to content lines *)
  let pad_str s = String.make e_left ' ' ^ s ^ String.make e_right ' ' in
  let padded_lines = List.map pad_str lines in
  (* Full-width blank lines *)
  let blank_line = String.make cell_width ' ' in
  let top_blanks = List.init top_spacer (fun _ -> blank_line) in
  let bottom_blanks = List.init bottom_spacer (fun _ -> blank_line) in
  E.vbox ~style:cell_style
    (List.map E.text (top_blanks @ padded_lines @ bottom_blanks))

let rec intersperse sep = function
  | [] -> []
  | [ x ] -> [ x ]
  | x :: xs -> x :: sep :: intersperse sep xs

let build_blank_row widths box_chars border_style show_edge =
  let blanks =
    List.map (fun w -> E.text ~style:Style.empty (String.make w ' ')) widths
  in
  let internal_sep = build_vertical_sep 1 box_chars.vertical border_style in
  let content = intersperse internal_sep blanks in
  let content =
    if show_edge && box_chars.vertical <> "" then
      let left_sep = build_vertical_sep 1 box_chars.vertical border_style in
      let right_sep = build_vertical_sep 1 box_chars.vertical border_style in
      (left_sep :: content) @ [ right_sep ]
    else content
  in
  E.hbox content

let table ?(title = None) ?(caption = None) ?(columns = []) ?(rows = [])
    ?(box_style = HeavyHead) ?(safe_box = false) ?(padding = (0, 1, 0, 1))
    ?(collapse_padding = false) ?(pad_edge = true) ?(expand = false)
    ?(show_header = true) ?(show_footer = false) ?(show_edge = true)
    ?(show_lines = false) ?(leading = 0) ?(style = Style.empty)
    ?(row_styles = []) ?(header_style = Style.(bold ++ fg Ansi.Default))
    ?(footer_style = Style.(empty ++ fg Ansi.Default))
    ?(border_style = Style.(fg Ansi.Default ++ dim))
    ?(title_style = Style.empty) ?(caption_style = Style.empty)
    ?(title_justify = `Center) ?(caption_justify = `Center) ?(width = None)
    ?(min_width = None) () =
  let num_cols = List.length columns in
  if num_cols = 0 then E.vbox []
  else
    let default_max_width = 80 in
    let max_table_width = Option.value width ~default:default_max_width in
    let top_pad, right_pad, bottom_pad, left_pad = padding in
    let pad_width = left_pad + right_pad in
    let col_widths =
      calculate_column_widths columns rows max_table_width pad_width show_header
        show_footer expand box_style show_edge top_pad right_pad bottom_pad
        left_pad collapse_padding pad_edge
    in
    let table_total_width =
      List.fold_left ( + ) 0 col_widths
      +
      if box_style <> NoBox then (if show_edge then 2 else 0) + (num_cols - 1)
      else 0
    in
    let box_chars = get_box_chars box_style safe_box in
    let map_justify_to_align j : E.align_items =
      match j with
      | `Left -> `Start
      | `Center -> `Center
      | `Right -> `End
      | `Full -> `Center
    in
    let title_element =
      match title with
      | None -> None
      | Some t ->
          Some
            (E.vbox ~width:(`Cells table_total_width)
               ~align_items:(map_justify_to_align title_justify)
               [ E.text ~style:title_style t ])
    in
    let caption_element =
      match caption with
      | None -> None
      | Some c ->
          Some
            (E.vbox ~width:(`Cells table_total_width)
               ~align_items:(map_justify_to_align caption_justify)
               [ E.text ~style:caption_style c ])
    in
    let add_column_separators cells height =
      if box_style = NoBox then cells
      else
        intersperse
          (build_vertical_sep height box_chars.vertical border_style)
          cells
    in
    (* Helper function to compute row height including padding *)
    let compute_row_height row row_idx is_header is_footer =
      let is_first_row =
        if is_header then true else row_idx = 0 && not show_header
      in
      let is_last_row =
        if is_footer then true
        else row_idx = List.length rows - 1 && not show_footer
      in
      let e_top_base, _, e_bottom_base, _ =
        get_effective_padding top_pad right_pad bottom_pad left_pad false false
          is_first_row is_last_row collapse_padding pad_edge
      in
      let indices = List.init num_cols (fun i -> i) in
      List.fold_left
        (fun max_h col_idx ->
          let col = List.nth columns col_idx in
          let is_first_col = col_idx = 0 in
          let is_last_col = col_idx = num_cols - 1 in
          let _, e_right, _, e_left =
            get_effective_padding top_pad right_pad bottom_pad left_pad
              is_first_col is_last_col is_first_row is_last_row collapse_padding
              pad_edge
          in
          let content_width = List.nth col_widths col_idx - e_left - e_right in
          let str =
            if is_header then col.header
            else if is_footer then Option.value col.footer ~default:""
            else List.nth row col_idx
          in
          let lines =
            get_lines str col content_width col.justify (not is_footer)
          in
          let content_h = List.length lines in
          max max_h (content_h + e_top_base + e_bottom_base))
        0 indices
    in

    (* Calculate heights for header, footer, and data rows *)
    let header_height =
      if not show_header then 0 else compute_row_height [] 0 true false
    in
    let footer_height =
      if not show_footer then 0 else compute_row_height [] 0 false true
    in
    let row_heights =
      List.mapi
        (fun row_idx row -> compute_row_height row row_idx false false)
        rows
    in

    (* Build the actual table *)
    let all_rows = ref [] in

    (* Add top border if needed *)
    if box_style <> NoBox && show_edge then
      all_rows :=
        !all_rows
        @ [
            build_horizontal_line "head" col_widths box_chars show_edge
              border_style;
          ];

    (* Add title if present - BEFORE top border *)
    let title_added_before = ref false in
    (match title_element with
    | Some t ->
        (* Move title before top border *)
        if box_style <> NoBox && show_edge then (
          all_rows := List.rev (List.tl (List.rev !all_rows));
          all_rows := !all_rows @ [ t ];
          all_rows :=
            !all_rows
            @ [
                build_horizontal_line "head" col_widths box_chars show_edge
                  border_style;
              ];
          title_added_before := true)
        else all_rows := !all_rows @ [ t ]
    | None -> ());

    (* Add header row *)
    if show_header then (
      let header_cells =
        List.mapi
          (fun i col ->
            let e_top, e_right, e_bottom, e_left =
              get_effective_padding top_pad right_pad bottom_pad left_pad (i = 0)
                (i = num_cols - 1)
                true false collapse_padding pad_edge
            in
            let width = List.nth col_widths i in
            let content_width = width - e_left - e_right in
            let lines =
              get_lines col.header col content_width col.justify false
            in
            build_cell_element lines header_height col.vertical e_top e_bottom
              e_left e_right
              (Style.merge header_style col.header_style)
              width)
          columns
      in
      (* Add vertical borders *)
      let row_content =
        if show_edge && box_style <> NoBox then
          let left_sep =
            build_vertical_sep header_height box_chars.vertical border_style
          in
          let right_sep =
            build_vertical_sep header_height box_chars.vertical border_style
          in
          E.hbox
            ((left_sep :: add_column_separators header_cells header_height) @ [ right_sep ])
        else E.hbox (add_column_separators header_cells header_height)
      in
      all_rows := !all_rows @ [ row_content ];

      (* Add header separator - use "row" for thin line *)
      if box_style <> NoBox then
        all_rows :=
          !all_rows
          @ [
              build_horizontal_line "row" col_widths box_chars show_edge
                border_style;
            ]);

    (* Add data rows *)
    List.iteri
      (fun row_idx row ->
        let row_style =
          if row_styles = [] then Style.empty
          else List.nth row_styles (row_idx mod List.length row_styles)
        in
        let row_height = List.nth row_heights row_idx in
        let is_first_row = row_idx = 0 && not show_header in
        let is_last_row = row_idx = List.length rows - 1 && not show_footer in

        let cells =
          List.mapi
            (fun col_idx cell ->
              let col = List.nth columns col_idx in
              let e_top, e_right, e_bottom, e_left =
                get_effective_padding top_pad right_pad bottom_pad left_pad
                  (col_idx = 0)
                  (col_idx = num_cols - 1)
                  is_first_row is_last_row collapse_padding pad_edge
              in
              let width = List.nth col_widths col_idx in
              let content_width = width - e_left - e_right in
              let lines = get_lines cell col content_width col.justify true in
              let cell_style =
                Style.merge (Style.merge style row_style) col.style
              in
              build_cell_element lines row_height col.vertical e_top e_bottom
                e_left e_right cell_style width)
            row
        in

        (* Add vertical borders *)
        let row_content =
          if show_edge && box_style <> NoBox then
            let left_sep =
              build_vertical_sep row_height box_chars.vertical border_style
            in
            let right_sep =
              build_vertical_sep row_height box_chars.vertical border_style
            in
            E.hbox ((left_sep :: add_column_separators cells row_height) @ [ right_sep ])
          else E.hbox (add_column_separators cells row_height)
        in
        all_rows := !all_rows @ [ row_content ];

        (* Add row separator if needed *)
        if show_lines && row_idx < List.length rows - 1 && box_style <> NoBox
        then
          all_rows :=
            !all_rows
            @ [
                build_horizontal_line "row" col_widths box_chars show_edge
                  border_style;
              ];

        (* Add leading (spacing between rows) *)
        if leading > 0 && row_idx < List.length rows - 1 then
          for _ = 1 to leading do
            all_rows :=
              !all_rows
              @ [ build_blank_row col_widths box_chars border_style show_edge ]
          done)
      rows;

    (* Add footer separator if needed *)
    if show_footer && box_style <> NoBox then
      all_rows :=
        !all_rows
        @ [
            build_horizontal_line "row" col_widths box_chars show_edge
              border_style;
          ];

    (* Add footer row *)
    (if show_footer then
       let footer_cells =
         List.mapi
           (fun i col ->
             let e_top, e_right, e_bottom, e_left =
               get_effective_padding top_pad right_pad bottom_pad left_pad
                 (i = 0)
                 (i = num_cols - 1)
                 false true collapse_padding pad_edge
             in
             let width = List.nth col_widths i in
             let content_width = width - e_left - e_right in
             let footer_text = Option.value col.footer ~default:"" in
             let lines =
               get_lines footer_text col content_width col.justify false
             in
             build_cell_element lines footer_height col.vertical e_top e_bottom
               e_left e_right
               (Style.merge footer_style col.footer_style)
               width)
           columns
       in
       (* Add vertical borders *)
       let row_content =
         if show_edge && box_style <> NoBox then
           let left_sep =
             build_vertical_sep footer_height box_chars.vertical border_style
           in
           let right_sep =
             build_vertical_sep footer_height box_chars.vertical border_style
           in
           E.hbox
             ((left_sep :: add_column_separators footer_cells footer_height) @ [ right_sep ])
         else E.hbox (add_column_separators footer_cells footer_height)
       in
       all_rows := !all_rows @ [ row_content ]);

    (* Add bottom border if needed *)
    if box_style <> NoBox && show_edge then
      all_rows :=
        !all_rows
        @ [
            build_horizontal_line "foot" col_widths box_chars show_edge
              border_style;
          ];

    (* Add caption if present *)
    (match caption_element with
    | Some c -> all_rows := !all_rows @ [ c ]
    | None -> ());

    (* Apply min_width constraint *)
    let final_element = E.vbox ~gap:(E.cells 0) !all_rows in
    match min_width with
    | Some min_w when table_total_width < min_w ->
        E.hbox ~width:(`Cells min_w) ~justify_content:`Center [ final_element ]
    | _ -> final_element

let simple_table ~headers ~rows =
  let columns = List.map (fun h -> default_column ~header:h) headers in
  table ~columns ~rows ()

let grid_table ~columns ~rows =
  table ~columns ~rows ~box_style:NoBox ~padding:(0, 1, 0, 0) ~show_header:false
    ~show_footer:false ~show_edge:false ()
