(* Type representing a UI element *)
type t = Toffee.Node_id.t * Renderable.t Toffee.tree
type spacing = Spacing.t

(* Direct type equalities to toffee types *)
type align = Toffee.Style.Align_items.t =
  | Start
  | End
  | Flex_start
  | Flex_end
  | Center
  | Baseline
  | Stretch

type justify = Toffee.Style.Align_content.t =
  | Start
  | End
  | Flex_start
  | Flex_end
  | Center
  | Stretch
  | Space_between
  | Space_evenly
  | Space_around

type wrap = Toffee.Style.Flex_wrap.t = No_wrap | Wrap | Wrap_reverse
type size = Px of int | Percent of float | Auto | Fit_content
type position_type = Toffee.Style.Position.t = Relative | Absolute
type display = Toffee.Style.Display.t = Block | Flex | Grid | None
type direction = Inherit | Ltr | Rtl

type flex_direction = Toffee.Style.Flex_direction.t =
  | Row
  | Column
  | Row_reverse
  | Column_reverse

type overflow = Toffee.Style.Overflow.t = Visible | Clip | Hidden | Scroll

(* Convert our size type to toffee dimension *)
let size_to_dimension = function
  | Px n -> Toffee.Style.Dimension.length (float_of_int n)
  | Percent p -> Toffee.Style.Dimension.percent (p /. 100.0)
  | Auto -> Toffee.Style.Dimension.auto
  | Fit_content ->
      Toffee.Style.Dimension.auto (* toffee doesn't have fit_content *)

(* Convert spacing to toffee rect *)
let spacing_to_rect (spacing : Spacing.t) : Toffee.Style.Length_percentage.t Toffee.Geometry.rect =
  {
    top =
      Toffee.Style.Length_percentage.length (float_of_int (Spacing.top spacing));
    right =
      Toffee.Style.Length_percentage.length
        (float_of_int (Spacing.right spacing));
    bottom =
      Toffee.Style.Length_percentage.length
        (float_of_int (Spacing.bottom spacing));
    left =
      Toffee.Style.Length_percentage.length
        (float_of_int (Spacing.left spacing));
  }

(* Convert spacing to auto rect for margins *)
let spacing_to_auto_rect (spacing : Spacing.t) : Toffee.Style.Length_percentage_auto.t Toffee.Geometry.rect =
  {
    top =
      Toffee.Style.Length_percentage_auto.length
        (float_of_int (Spacing.top spacing));
    right =
      Toffee.Style.Length_percentage_auto.length
        (float_of_int (Spacing.right spacing));
    bottom =
      Toffee.Style.Length_percentage_auto.length
        (float_of_int (Spacing.bottom spacing));
    left =
      Toffee.Style.Length_percentage_auto.length
        (float_of_int (Spacing.left spacing));
  }

(* Convert border to toffee rect *)
let border_to_rect (border : Border.t) : Toffee.Style.Length_percentage.t Toffee.Geometry.rect =
  {
    top =
      Toffee.Style.Length_percentage.length
        (if Border.top border then 1.0 else 0.0);
    right =
      Toffee.Style.Length_percentage.length
        (if Border.right border then 1.0 else 0.0);
    bottom =
      Toffee.Style.Length_percentage.length
        (if Border.bottom border then 1.0 else 0.0);
    left =
      Toffee.Style.Length_percentage.length
        (if Border.left border then 1.0 else 0.0);
  }

(* Global tree instance *)
let tree = Toffee.new_tree ()

let box ?position_type ?display ?direction:_ ?flex_grow ?flex_shrink ?align_self
    ?width ?height ?min_width ?min_height ?max_width ?max_height ?padding
    ?margin ?style ?border ?border_style ?flex_direction ?align_items
    ?justify_content ?flex_wrap ?gap ?row_gap ?col_gap ?overflow children =
  let box_renderable : Renderable.t option =
    let final_border =
      match (border, border_style) with
      | Some b, Some s -> Some (Border.with_style b s)
      | Some b, None -> Some b
      | None, _ -> None
    in
    match (final_border, style) with
    | None, None -> None
    | _ -> Some (Renderable.box ?border:final_border ?background:style ())
  in

  (* Extract child node_ids *)
  let child_ids = List.map fst children in

  (* Create gap size *)
  let gap_size : Toffee.Style.Length_percentage.t Toffee.Geometry.size =
    match (gap, row_gap, col_gap) with
    | Some g, None, None ->
        {
          width =
            Toffee.Style.Length_percentage.length (float_of_int g);
          height = Toffee.Style.Length_percentage.length (float_of_int g);
        }
    | None, Some r, Some c ->
        {
          width =
            Toffee.Style.Length_percentage.length (float_of_int c);
          height = Toffee.Style.Length_percentage.length (float_of_int r);
        }
    | None, Some r, None ->
        {
          width = Toffee.Style.Length_percentage.length 0.0;
          height = Toffee.Style.Length_percentage.length (float_of_int r);
        }
    | None, None, Some c ->
        {
          width =
            Toffee.Style.Length_percentage.length (float_of_int c);
          height = Toffee.Style.Length_percentage.length 0.0;
        }
    | Some g, Some r, _ ->
        {
          width =
            Toffee.Style.Length_percentage.length (float_of_int g);
          height = Toffee.Style.Length_percentage.length (float_of_int r);
        }
    | Some g, _, Some c ->
        {
          width =
            Toffee.Style.Length_percentage.length (float_of_int c);
          height = Toffee.Style.Length_percentage.length (float_of_int g);
        }
    | _ ->
        {
          width = Toffee.Style.Length_percentage.length 0.0;
          height = Toffee.Style.Length_percentage.length 0.0;
        }
  in

  (* Create style *)
  let toffee_style : Toffee.Style.t =
    let overflow_point = 
      match overflow with
      | Some o -> { Toffee.Geometry.Point.x = o; y = o }
      | None -> { Toffee.Geometry.Point.x = Toffee.Style.Overflow.Visible; y = Toffee.Style.Overflow.Visible }
    in
    let size_dim = {
      Toffee.Geometry.Size.width = Option.value (Option.map size_to_dimension width) ~default:Toffee.Style.Dimension.auto;
      height = Option.value (Option.map size_to_dimension height) ~default:Toffee.Style.Dimension.auto;
    } in
    let min_size_dim = {
      Toffee.Geometry.Size.width = Option.value (Option.map size_to_dimension min_width) ~default:Toffee.Style.Dimension.auto;
      height = Option.value (Option.map size_to_dimension min_height) ~default:Toffee.Style.Dimension.auto;
    } in
    let max_size_dim = {
      Toffee.Geometry.Size.width = Option.value (Option.map size_to_dimension max_width) ~default:Toffee.Style.Dimension.auto;
      height = Option.value (Option.map size_to_dimension max_height) ~default:Toffee.Style.Dimension.auto;
    } in
    Toffee.Style.make
      ?display
      ?position:position_type
      ~overflow:overflow_point
      ?align_items
      ?justify_content
      ~gap:gap_size
      ?flex_direction
      ?flex_wrap
      ~flex_grow:(Option.value flex_grow ~default:0.0)
      ~flex_shrink:(Option.value flex_shrink ~default:1.0)
      ~flex_basis:Toffee.Style.Dimension.auto
      ?align_self
      ~size:size_dim
      ~min_size:min_size_dim
      ~max_size:max_size_dim
      ?margin:(Option.map spacing_to_auto_rect margin)
      ?padding:(Option.map spacing_to_rect padding)
      ?border:(Option.map border_to_rect border)
      ()
  in

  (* Create node *)
  let id =
    if List.length child_ids > 0 then
      Toffee.new_with_children tree toffee_style (Array.of_list child_ids) |> Result.get_ok
    else Toffee.new_leaf tree toffee_style |> Result.get_ok
  in

  (* Store renderable in context *)
  (match box_renderable with
  | Some r ->
      let _ = Toffee.set_node_context tree id (Some r) |> Result.get_ok in
      ()
  | None -> ());

  (id, tree)

let vbox ?width ?height ?min_width ?min_height ?max_width ?max_height ?padding
    ?margin ?flex_grow ?flex_shrink ?align_self ?style ?border ?border_style
    ?overflow ?align_items ?justify_content ?gap children =
  box ?width ?height ?min_width ?min_height ?max_width ?max_height ?padding
    ?margin ?flex_grow ?flex_shrink ?align_self ?style ?border ?border_style
    ?overflow ~flex_direction:Column ?align_items ?justify_content ?gap children

let hbox ?width ?height ?min_width ?min_height ?max_width ?max_height ?padding
    ?margin ?flex_grow ?flex_shrink ?align_self ?style ?border ?border_style
    ?overflow ?align_items ?justify_content ?gap children =
  box ?width ?height ?min_width ?min_height ?max_width ?max_height ?padding
    ?margin ?flex_grow ?flex_shrink ?align_self ?style ?border ?border_style
    ?overflow ~flex_direction:Row ?align_items ?justify_content ?gap children

let zbox ?width ?height ?min_width ?min_height ?max_width ?max_height ?padding
    ?margin ?flex_grow ?flex_shrink ?align_self ?style ?border ?border_style
    ?overflow children =
  let box_renderable : Renderable.t option =
    let final_border =
      match (border, border_style) with
      | Some b, Some s -> Some (Border.with_style b s)
      | Some b, None -> Some b
      | None, _ -> None
    in
    match (final_border, style) with
    | None, None -> None
    | _ -> Some (Renderable.box ?border:final_border ?background:style ())
  in

  let stacked_children =
    List.map
      (fun child ->
        let wrapper_style : Toffee.Style.t =
          Toffee.Style.make
            ~position:Toffee.Style.Position.Absolute
            ~inset:{
                top = Toffee.Style.Length_percentage_auto.length 0.0;
                right = Toffee.Style.Length_percentage_auto.length 0.0;
                bottom = Toffee.Style.Length_percentage_auto.length 0.0;
                left = Toffee.Style.Length_percentage_auto.length 0.0;
              }
            ()
        in
        let wrapper_id =
          Toffee.new_with_children tree wrapper_style [| fst child |] |> Result.get_ok
        in
        (wrapper_id, tree))
      children
  in

  let toffee_style : Toffee.Style.t =
    let overflow_point =
      let o = Option.value overflow ~default:Toffee.Style.Overflow.Visible in
      { Toffee.Geometry.Point.x = o; y = o }
    in
    let size_dim = {
      Toffee.Geometry.Size.width =
        Option.value
          (Option.map size_to_dimension width)
          ~default:Toffee.Style.Dimension.auto;
      height =
        Option.value
          (Option.map size_to_dimension height)
          ~default:Toffee.Style.Dimension.auto;
    } in
    let min_size_dim = {
      Toffee.Geometry.Size.width =
        Option.value
          (Option.map size_to_dimension min_width)
          ~default:Toffee.Style.Dimension.auto;
      height =
        Option.value
          (Option.map size_to_dimension min_height)
          ~default:Toffee.Style.Dimension.auto;
    } in
    let max_size_dim = {
      Toffee.Geometry.Size.width =
        Option.value
          (Option.map size_to_dimension max_width)
          ~default:Toffee.Style.Dimension.auto;
      height =
        Option.value
          (Option.map size_to_dimension max_height)
          ~default:Toffee.Style.Dimension.auto;
    } in
    let margin_rect =
      Option.value
        (Option.map spacing_to_auto_rect margin)
        ~default:{
          top = Toffee.Style.Length_percentage_auto.length 0.0;
          right = Toffee.Style.Length_percentage_auto.length 0.0;
          bottom = Toffee.Style.Length_percentage_auto.length 0.0;
          left = Toffee.Style.Length_percentage_auto.length 0.0;
        }
    in
    let padding_rect =
      Option.value
        (Option.map spacing_to_rect padding)
        ~default:{
          top = Toffee.Style.Length_percentage.length 0.0;
          right = Toffee.Style.Length_percentage.length 0.0;
          bottom = Toffee.Style.Length_percentage.length 0.0;
          left = Toffee.Style.Length_percentage.length 0.0;
        }
    in
    let border_rect =
      Option.value
        (Option.map border_to_rect border)
        ~default:{
          top = Toffee.Style.Length_percentage.length 0.0;
          right = Toffee.Style.Length_percentage.length 0.0;
          bottom = Toffee.Style.Length_percentage.length 0.0;
          left = Toffee.Style.Length_percentage.length 0.0;
        }
    in
    Toffee.Style.make
      ~overflow:overflow_point
      ~flex_grow:(Option.value flex_grow ~default:0.0)
      ~flex_shrink:(Option.value flex_shrink ~default:1.0)
      ?align_self
      ~size:size_dim
      ~min_size:min_size_dim
      ~max_size:max_size_dim
      ~margin:margin_rect
      ~padding:padding_rect
      ~border:border_rect
      ()
  in

  let child_ids = List.map fst stacked_children in
  let id = Toffee.new_with_children tree toffee_style (Array.of_list child_ids) |> Result.get_ok in

  (* Store renderable in context *)
  (match box_renderable with
  | Some r ->
      let _ = Toffee.set_node_context tree id (Some r) |> Result.get_ok in
      ()
  | None -> ());

  (id, tree)

let spacer ?(flex_grow = 1.0) ?min_width ?min_height () =
  let toffee_style : Toffee.Style.t =
    let min_size_dim = {
      Toffee.Geometry.Size.width =
        Option.value
          (Option.map size_to_dimension min_width)
          ~default:Toffee.Style.Dimension.auto;
      height =
        Option.value
          (Option.map size_to_dimension min_height)
          ~default:Toffee.Style.Dimension.auto;
    } in
    Toffee.Style.make
      ~flex_grow
      ~min_size:min_size_dim
      ()
  in
  let id = Toffee.new_leaf tree toffee_style |> Result.get_ok in
  (id, tree)

let divider ?(orientation = `Horizontal) ?title:_ ?char ?style ?padding () =
  let (width : size option), (height : size option) =
    match orientation with
    | `Horizontal -> (None, Some (Px 1))
    | `Vertical -> (Some (Px 1), None)
  in
  let renderable =
    match char with
    | None -> Renderable.box ?border:None ?background:style ()
    | Some c ->
        Renderable.text ~style:(Option.value style ~default:Style.empty) c
  in

  let toffee_style : Toffee.Style.t =
    let size_dim = {
      Toffee.Geometry.Size.width =
        Option.value
          (Option.map size_to_dimension width)
          ~default:Toffee.Style.Dimension.auto;
      height =
        Option.value
          (Option.map size_to_dimension height)
          ~default:Toffee.Style.Dimension.auto;
    } in
    let padding_rect =
      Option.value
        (Option.map spacing_to_rect padding)
        ~default:{
          top = Toffee.Style.Length_percentage.length 0.0;
          right = Toffee.Style.Length_percentage.length 0.0;
          bottom = Toffee.Style.Length_percentage.length 0.0;
          left = Toffee.Style.Length_percentage.length 0.0;
        }
    in
    Toffee.Style.make
      ~size:size_dim
      ~padding:padding_rect
      ()
  in

  let id = Toffee.new_leaf tree toffee_style |> Result.get_ok in
  (* Store renderable in context *)
  let _ = Toffee.set_node_context tree id (Some renderable) |> Result.get_ok in
  (id, tree)

let text ?(style = Style.empty) ?(align = `Left) ?(wrap = `Wrap) content =
  let align_renderable =
    match align with `Left -> `Start | `Center -> `Center | `Right -> `End
  in
  let wrap_bool =
    match wrap with `Wrap -> true | `Truncate | `Clip -> false
  in
  let renderable =
    Renderable.text ~style ~align:align_renderable ~wrap:wrap_bool content
  in

  let _measure_fn : Renderable.t Toffee.measure_function =
   fun known_dimensions available_space _node_id _context _style ->
    (* Use the content from the renderable context *)
    let lines = String.split_on_char '\n' content in
    let max_width =
      List.fold_left (fun acc line -> max acc (String.length line)) 0 lines
    in
    let num_lines = List.length lines in

    let computed_width =
      match known_dimensions.width with
      | Some w -> w
      | None -> (
          match available_space.width with
          | Toffee.Available_space.Definite w ->
              min w (float_of_int max_width)
          | _ -> float_of_int max_width)
    in

    let computed_height =
      match known_dimensions.height with
      | Some h -> h
      | None -> (
          match available_space.height with
          | Toffee.Available_space.Definite h ->
              min h (float_of_int num_lines)
          | _ -> float_of_int num_lines)
    in

    { width = computed_width; height = computed_height }
  in

  let id = Toffee.new_leaf tree Toffee.Style.default |> Result.get_ok in
  (* Store renderable in context *)
  let _ = Toffee.set_node_context tree id (Some renderable) |> Result.get_ok in
  (id, tree)

let scroll_view ?width ?height ?min_width ?min_height ?max_width ?max_height
    ?padding ?margin ?flex_grow ?flex_shrink ?align_self ?style ?border
    ?border_style ?(show_scrollbars = true) ~h_offset ~v_offset child =
  let _ = show_scrollbars in
  (* Create the scroll container *)
  let scroll_style : Toffee.Style.t =
    Toffee.Style.make
      ~overflow:{ x = Toffee.Style.Overflow.Hidden; y = Toffee.Style.Overflow.Hidden }
      ()
  in
  let scroll_id = Toffee.new_with_children tree scroll_style [| fst child |] |> Result.get_ok in
  let _ =
    Toffee.set_node_context tree scroll_id
      (Some (Renderable.scroll ~h_offset ~v_offset ()))
  in

  (* Always wrap in a box *)
  box ?width ?height ?min_width ?min_height ?max_width ?max_height ?padding
    ?margin ?flex_grow ?flex_shrink ?align_self ?style ?border ?border_style
    ~overflow:Hidden
    [ (scroll_id, tree) ]

let canvas ?width ?height ?min_width ?min_height ?max_width ?max_height ?padding
    ?margin ?flex_grow ?flex_shrink ?align_self ?style ?border ?border_style
    draw =
  (* Create the canvas node *)
  let canvas_id = Toffee.new_leaf tree Toffee.Style.default |> Result.get_ok in
  let _ = Toffee.set_node_context tree canvas_id (Some (Renderable.canvas draw)) |> Result.get_ok in

  (* Always wrap in a box *)
  box ?width ?height ?min_width ?min_height ?max_width ?max_height ?padding
    ?margin ?flex_grow ?flex_shrink ?align_self ?style ?border ?border_style
    [ (canvas_id, tree) ]

let center child =
  box ~align_items:Center ~justify_content:Center ~width:(Percent 100.)
    ~height:(Percent 100.) [ child ]

let styled style child =
  (* Wrap the child in a box that applies the style *)
  box ~style [ child ]

let flow ?(h_gap = 0) ?(v_gap = 0) children =
  (* Flow layout using flexbox with wrap enabled *)
  box ~display:Flex ~flex_direction:Row ~flex_wrap:Wrap ~gap:h_gap
    ~row_gap:v_gap ~align_items:Start children

let block ?width ?height ?min_width ?min_height ?max_width ?max_height ?padding
    ?margin ?style ?border ?border_style children =
  (* Block layout - each child takes full width, stacked vertically *)
  box ~display:Block ?width ?height ?min_width ?min_height ?max_width
    ?max_height ?padding ?margin ?style ?border ?border_style children

let grid ~columns ~rows ?(col_gap = 0) ?(row_gap = 0) children =
  (* Convert our size type to toffee grid track sizing *)
  let size_to_track_sizing = function
    | Px n ->
        Toffee.Style.Grid.Template_component.length (float_of_int n)
    | Percent p ->
        Toffee.Style.Grid.Template_component.percent (p /. 100.0)
    | Auto ->
        Toffee.Style.Grid.Template_component.auto
    | Fit_content ->
        Toffee.Style.Grid.Template_component.max_content
  in

  let grid_columns = List.map size_to_track_sizing columns in
  let grid_rows = List.map size_to_track_sizing rows in

  (* Extract child node_ids *)
  let child_ids = List.map fst children in

  (* Create grid gap *)
  let gap_size =
    {
      Toffee.Geometry.Size.width =
        Toffee.Style.Length_percentage.length (float_of_int col_gap);
      height = Toffee.Style.Length_percentage.length (float_of_int row_gap);
    }
  in

  (* Create grid style *)
  let grid_style : Toffee.Style.t =
    Toffee.Style.make
      ~display:Toffee.Style.Display.Grid
      ~grid_template_columns:grid_columns
      ~grid_template_rows:grid_rows
      ~gap:gap_size
      ()
  in

  (* Create grid container *)
  let id =
    if List.length child_ids > 0 then
      Toffee.new_with_children tree grid_style (Array.of_list child_ids) |> Result.get_ok
    else Toffee.new_leaf tree grid_style |> Result.get_ok
  in

  (id, tree)

let checkbox ~checked ~label ?(style = Style.empty) () =
  let check_char = Uchar.of_int (if checked then 0x2611 else 0x2610) in
  (* ☑ or ☐ *)
  let check_str =
    let buf = Buffer.create 4 in
    Uutf.Buffer.add_utf_8 buf check_char;
    Buffer.contents buf
  in
  let checkbox_text = check_str ^ " " ^ label in
  text ~style:(Style.merge style (Style.fg Ansi.Default)) checkbox_text

let radio ~checked ~label ?(style = Style.empty) () =
  let radio_char = Uchar.of_int (if checked then 0x25C9 else 0x25CB) in
  (* ◉ or ○ *)
  let radio_str =
    let buf = Buffer.create 4 in
    Uutf.Buffer.add_utf_8 buf radio_char;
    Buffer.contents buf
  in
  let radio_text = radio_str ^ " " ^ label in
  text ~style:(Style.merge style (Style.fg Ansi.Default)) radio_text

let image ~lines ?(align = `Left) () =
  let elements =
    List.map (fun line -> text ~style:(Style.fg Ansi.Default) ~align line) lines
  in
  vbox elements

let list ~items ?(bullet = "") ?(numbering = false) () =
  let default_bullet =
    let buf = Buffer.create 4 in
    Uutf.Buffer.add_utf_8 buf (Uchar.of_int 0x2022);
    (* • *)
    Buffer.contents buf
  in
  let bullet = if bullet = "" then default_bullet else bullet in
  let list_items =
    List.mapi
      (fun i item ->
        let prefix =
          if numbering then Printf.sprintf "%2d. " (i + 1) else bullet ^ "  "
        in
        hbox ~gap:0
          [ text ~style:Style.(merge (fg Ansi.Default) dim) prefix; item ])
      items
  in
  vbox ~gap:0 list_items

let rich_text segments =
  (* Create multiple text elements styled individually and combine them in an hbox *)
  let text_elements =
    List.map (fun (content, style) -> text ~style content) segments
  in
  hbox ~gap:0 text_elements

let measure ?(width : int option) element =
  (* Compute layout with given constraints and return the resulting size *)
  let node_id, tree = element in
  let available_space =
    {
      Toffee.Geometry.Size.width =
        (match width with
        | Some w -> Toffee.Available_space.Definite (float_of_int w)
        | None -> Toffee.Available_space.Max_content);
      height = Toffee.Available_space.Max_content;
    }
  in

  let _ = Toffee.compute_layout tree node_id available_space |> Result.get_ok in

  match Toffee.layout tree node_id with
  | Ok layout ->
      let size = Toffee.Layout.size layout in
      (int_of_float size.width, int_of_float size.height)
  | Error _ -> (0, 0)
