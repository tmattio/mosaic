(* Type representing a UI element *)
type t = Toffee.Node_id.t * Renderable.t Toffee.tree

(* Geometric types *)
type 'a sides = { top : 'a; right : 'a; bottom : 'a; left : 'a }
type 'a span = { start : 'a; end_ : 'a }

(* Dimension types *)
type length_percentage = [ `Cells of int | `Pct of float ]
type length_percentage_auto = [ length_percentage | `Auto ]
type calc_resolver = int -> float -> float
type dimension = [ length_percentage_auto | `Calc of int ]

(* Layout control types - polymorphic variants *)
type display = [ `Block | `Flex | `Grid | `Hidden ]
type position = [ `Relative | `Absolute ]
type overflow = [ `Visible | `Clip | `Hidden | `Scroll ]
type box_sizing = [ `Border_box | `Content_box ]
type text_align = [ `Auto | `Legacy_left | `Legacy_right | `Legacy_center ]

(* Flexbox types *)
type flex_direction = [ `Row | `Column | `Row_reverse | `Column_reverse ]
type flex_wrap = [ `No_wrap | `Wrap | `Wrap_reverse ]

(* Alignment types *)
type align_items =
  [ `Start | `End | `Flex_start | `Flex_end | `Center | `Baseline | `Stretch ]

type align_content =
  [ `Start
  | `End
  | `Flex_start
  | `Flex_end
  | `Center
  | `Stretch
  | `Space_between
  | `Space_evenly
  | `Space_around ]

type align_self = align_items
type justify_items = align_items
type justify_self = align_items
type justify_content = align_content

(* Grid types *)
type grid_auto_flow = [ `Row | `Column | `Row_dense | `Column_dense ]

type grid_placement =
  [ `Auto
  | `Line of int
  | `Span of int
  | `Named_line of string * int
  | `Named_span of string * int ]

type track_sizing_function = Toffee.Style.Grid.Track_sizing_function.t
type grid_repetition = Toffee.Style.Grid.Repetition.t
type repetition_count = [ `Count of int | `Auto_fill | `Auto_fit ]

type grid_template_component =
  | Single of track_sizing_function
  | Repeat of grid_repetition

type grid_template_area = Toffee.Style.Grid.Template_area.t

(* Convert length_percentage to Toffee *)
let length_percentage_to_toffee = function
  | `Cells n -> Toffee.Style.Length_percentage.length (float_of_int n)
  | `Pct f -> Toffee.Style.Length_percentage.percent f

(* Convert length_percentage_auto to Toffee *)
let length_percentage_auto_to_toffee = function
  | #length_percentage as lp ->
      let toffee_lp = length_percentage_to_toffee lp in
      Toffee.Style.Length_percentage_auto.length
        (Toffee.Style.Length_percentage.value toffee_lp)
  | `Auto -> Toffee.Style.Length_percentage_auto.auto

(* Convert dimension to Toffee *)
let dimension_to_toffee = function
  | `Cells n -> Toffee.Style.Dimension.length (float_of_int n)
  | `Pct f -> Toffee.Style.Dimension.percent f
  | `Auto -> Toffee.Style.Dimension.auto
  | `Calc idx -> Toffee.Style.Dimension.calc idx

(* Convert sides with length_percentage to Toffee rect *)
let sides_lp_to_rect (sides : length_percentage sides) :
    Toffee.Style.Length_percentage.t Toffee.Geometry.rect =
  {
    top = length_percentage_to_toffee sides.top;
    right = length_percentage_to_toffee sides.right;
    bottom = length_percentage_to_toffee sides.bottom;
    left = length_percentage_to_toffee sides.left;
  }

(* Convert sides with length_percentage_auto to Toffee rect *)
let sides_lpa_to_rect (sides : length_percentage_auto sides) :
    Toffee.Style.Length_percentage_auto.t Toffee.Geometry.rect =
  {
    top = length_percentage_auto_to_toffee sides.top;
    right = length_percentage_auto_to_toffee sides.right;
    bottom = length_percentage_auto_to_toffee sides.bottom;
    left = length_percentage_auto_to_toffee sides.left;
  }

(* Convert border to toffee rect *)
let border_to_rect (border : Border.t) :
    Toffee.Style.Length_percentage.t Toffee.Geometry.rect =
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

(* Helper function to get border width based on presence *)
let border_width_of_presence (b : Border.t) : length_percentage sides =
  {
    top = `Cells (if Border.top b then 1 else 0);
    right = `Cells (if Border.right b then 1 else 0);
    bottom = `Cells (if Border.bottom b then 1 else 0);
    left = `Cells (if Border.left b then 1 else 0);
  }

(* Type conversions to Toffee types *)
let display_to_toffee = function
  | `Block -> Toffee.Style.Display.Block
  | `Flex -> Toffee.Style.Display.Flex
  | `Grid -> Toffee.Style.Display.Grid
  | `Hidden -> Toffee.Style.Display.None

let position_to_toffee = function
  | `Relative -> Toffee.Style.Position.Relative
  | `Absolute -> Toffee.Style.Position.Absolute

let overflow_to_toffee = function
  | `Visible -> Toffee.Style.Overflow.Visible
  | `Clip -> Toffee.Style.Overflow.Clip
  | `Hidden -> Toffee.Style.Overflow.Hidden
  | `Scroll -> Toffee.Style.Overflow.Scroll

let box_sizing_to_toffee = function
  | `Border_box -> Toffee.Style.Box_sizing.Border_box
  | `Content_box -> Toffee.Style.Box_sizing.Content_box

let text_align_to_toffee = function
  | `Auto -> Toffee.Style.Text_align.Auto
  | `Legacy_left -> Toffee.Style.Text_align.Legacy_left
  | `Legacy_right -> Toffee.Style.Text_align.Legacy_right
  | `Legacy_center -> Toffee.Style.Text_align.Legacy_center

let flex_direction_to_toffee = function
  | `Row -> Toffee.Style.Flex_direction.Row
  | `Column -> Toffee.Style.Flex_direction.Column
  | `Row_reverse -> Toffee.Style.Flex_direction.Row_reverse
  | `Column_reverse -> Toffee.Style.Flex_direction.Column_reverse

let flex_wrap_to_toffee = function
  | `No_wrap -> Toffee.Style.Flex_wrap.No_wrap
  | `Wrap -> Toffee.Style.Flex_wrap.Wrap
  | `Wrap_reverse -> Toffee.Style.Flex_wrap.Wrap_reverse

let align_items_to_toffee = function
  | `Start -> Toffee.Style.Align_items.Start
  | `End -> Toffee.Style.Align_items.End
  | `Flex_start -> Toffee.Style.Align_items.Flex_start
  | `Flex_end -> Toffee.Style.Align_items.Flex_end
  | `Center -> Toffee.Style.Align_items.Center
  | `Baseline -> Toffee.Style.Align_items.Baseline
  | `Stretch -> Toffee.Style.Align_items.Stretch

let align_content_to_toffee = function
  | `Start -> Toffee.Style.Align_content.Start
  | `End -> Toffee.Style.Align_content.End
  | `Flex_start -> Toffee.Style.Align_content.Flex_start
  | `Flex_end -> Toffee.Style.Align_content.Flex_end
  | `Center -> Toffee.Style.Align_content.Center
  | `Stretch -> Toffee.Style.Align_content.Stretch
  | `Space_between -> Toffee.Style.Align_content.Space_between
  | `Space_evenly -> Toffee.Style.Align_content.Space_evenly
  | `Space_around -> Toffee.Style.Align_content.Space_around

let grid_auto_flow_to_toffee = function
  | `Row -> Toffee.Style.Grid.Auto_flow.Row
  | `Column -> Toffee.Style.Grid.Auto_flow.Column
  | `Row_dense -> Toffee.Style.Grid.Auto_flow.Row_dense
  | `Column_dense -> Toffee.Style.Grid.Auto_flow.Column_dense

let grid_placement_to_toffee = function
  | `Auto -> Toffee.Style.Grid.Placement.Auto
  | `Line n -> Toffee.Style.Grid.Placement.Line n
  | `Span n -> Toffee.Style.Grid.Placement.Span n
  | `Named_line (name, n) -> Toffee.Style.Grid.Placement.Named_line (name, n)
  | `Named_span (name, n) -> Toffee.Style.Grid.Placement.Named_span (name, n)

(* Global tree instance *)
let tree = Toffee.new_tree ()

let box ?display ?position ?box_sizing ?text_align ?flex_direction ?flex_wrap
    ?flex_grow ?flex_shrink ?flex_basis ?align_items ?align_self ?align_content
    ?justify_content ?justify_items ?justify_self ?overflow_x ?overflow_y
    ?aspect_ratio ?scrollbar_width ?inset ?width ?height ?min_width ?min_height
    ?max_width ?max_height ?padding ?margin ?border_width ?gap ?row_gap ?col_gap
    ?style ?border ?border_style children =
  (* Compute the final border first *)
  let final_border =
    match (border, border_style) with
    | Some b, Some s -> Some (Border.with_style b s)
    | Some b, None -> Some b
    | None, _ -> None
  in

  let box_renderable =
    match (final_border, style) with
    | None, None -> None
    | _ -> Some (Renderable.box ?border:final_border ?background:style ())
  in

  (* Auto-set border_width when border is provided but border_width is not *)
  let effective_border_width =
    match (border_width, final_border) with
    | Some bw, _ -> Some bw (* Explicit border_width takes precedence *)
    | None, Some b ->
        (* Only auto-set if no border_width is provided at all *)
        let border_presence = border_width_of_presence b in
        Some border_presence
    | None, None -> None
  in

  (* Extract child node_ids *)
  let child_ids = List.map fst children in

  (* Create gap size - row_gap and col_gap override gap *)
  let gap_size =
    let effective_row_gap =
      match row_gap with Some r -> Some r | None -> gap
    in
    let effective_col_gap =
      match col_gap with Some c -> Some c | None -> gap
    in
    match (effective_row_gap, effective_col_gap) with
    | Some r, Some c ->
        Some
          {
            Toffee.Geometry.Size.width = length_percentage_to_toffee c;
            height = length_percentage_to_toffee r;
          }
    | Some r, None ->
        Some
          {
            width = Toffee.Style.Length_percentage.length 0.0;
            height = length_percentage_to_toffee r;
          }
    | None, Some c ->
        Some
          {
            width = length_percentage_to_toffee c;
            height = Toffee.Style.Length_percentage.length 0.0;
          }
    | None, None -> None
  in

  (* Ensure box_sizing is set for consistency *)
  let box_sizing_opt =
    match box_sizing with
    | Some bs -> Some bs
    | None ->
        Some `Border_box (* Default to border-box for principled behavior *)
  in

  (* Create style *)
  let toffee_style =
    let overflow_point =
      match (overflow_x, overflow_y) with
      | Some x, Some y ->
          Some
            {
              Toffee.Geometry.Point.x = overflow_to_toffee x;
              y = overflow_to_toffee y;
            }
      | Some x, None ->
          Some { x = overflow_to_toffee x; y = Toffee.Style.Overflow.Visible }
      | None, Some y ->
          Some { x = Toffee.Style.Overflow.Visible; y = overflow_to_toffee y }
      | None, None -> None
    in
    let dim_to_toffee d =
      match d with
      | `Cells n -> Toffee.Style.Dimension.length (float_of_int n)
      | `Pct f -> Toffee.Style.Dimension.percent f
      | `Auto -> Toffee.Style.Dimension.auto
      | `Calc i -> Toffee.Style.Dimension.calc i
    in
    let size_dim =
      match (width, height) with
      | Some w, Some h ->
          Some
            {
              Toffee.Geometry.Size.width = dim_to_toffee w;
              height = dim_to_toffee h;
            }
      | Some w, None ->
          Some { width = dim_to_toffee w; height = Toffee.Style.Dimension.auto }
      | None, Some h ->
          Some { width = Toffee.Style.Dimension.auto; height = dim_to_toffee h }
      | None, None -> None
    in
    let min_size_dim =
      match (min_width, min_height) with
      | Some w, Some h ->
          Some
            {
              Toffee.Geometry.Size.width = dim_to_toffee w;
              height = dim_to_toffee h;
            }
      | Some w, None ->
          Some { width = dim_to_toffee w; height = Toffee.Style.Dimension.auto }
      | None, Some h ->
          Some { width = Toffee.Style.Dimension.auto; height = dim_to_toffee h }
      | None, None -> None
    in
    let max_size_dim =
      match (max_width, max_height) with
      | Some w, Some h ->
          Some
            {
              Toffee.Geometry.Size.width = dim_to_toffee w;
              height = dim_to_toffee h;
            }
      | Some w, None ->
          Some { width = dim_to_toffee w; height = Toffee.Style.Dimension.auto }
      | None, Some h ->
          Some { width = Toffee.Style.Dimension.auto; height = dim_to_toffee h }
      | None, None -> None
    in
    Toffee.Style.make
      ?display:(Option.map display_to_toffee display)
      ?position:(Option.map position_to_toffee position)
      ?box_sizing:(Option.map box_sizing_to_toffee box_sizing_opt)
      ?text_align:(Option.map text_align_to_toffee text_align)
      ?overflow:overflow_point ?scrollbar_width ?aspect_ratio
      ?align_items:(Option.map align_items_to_toffee align_items)
      ?align_self:(Option.map align_items_to_toffee align_self)
      ?align_content:(Option.map align_content_to_toffee align_content)
      ?justify_items:(Option.map align_items_to_toffee justify_items)
      ?justify_self:(Option.map align_items_to_toffee justify_self)
      ?justify_content:(Option.map align_content_to_toffee justify_content)
      ?gap:gap_size
      ?flex_direction:(Option.map flex_direction_to_toffee flex_direction)
      ?flex_wrap:(Option.map flex_wrap_to_toffee flex_wrap)
      ?flex_grow ?flex_shrink
      ?flex_basis:(Option.map dimension_to_toffee flex_basis)
      ?size:size_dim ?min_size:min_size_dim ?max_size:max_size_dim
      ?inset:(Option.map sides_lpa_to_rect inset)
      ?margin:(Option.map sides_lpa_to_rect margin)
      ?padding:(Option.map sides_lp_to_rect padding)
      ?border:(Option.map sides_lp_to_rect effective_border_width)
      ()
  in

  (* Create node *)
  let id =
    if List.length child_ids > 0 then
      Toffee.new_with_children tree toffee_style (Array.of_list child_ids)
      |> Result.get_ok
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
    ?margin ?border_width ?flex_grow ?flex_shrink ?flex_basis ?align_self
    ?align_items ?justify_content ?gap ?overflow_x ?overflow_y ?style ?border
    ?border_style children =
  box ?width ?height ?min_width ?min_height ?max_width ?max_height ?padding
    ?margin ?border_width ?flex_grow ?flex_shrink ?flex_basis ?align_self ?style
    ?border ?border_style ?overflow_x ?overflow_y ~display:`Flex
    ~flex_direction:`Column ?align_items ?justify_content ?gap children

let hbox ?width ?height ?min_width ?min_height ?max_width ?max_height ?padding
    ?margin ?border_width ?flex_grow ?flex_shrink ?flex_basis ?align_self
    ?align_items ?justify_content ?gap ?overflow_x ?overflow_y ?style ?border
    ?border_style children =
  box ?width ?height ?min_width ?min_height ?max_width ?max_height ?padding
    ?margin ?border_width ?flex_grow ?flex_shrink ?flex_basis ?align_self ?style
    ?border ?border_style ?overflow_x ?overflow_y ~display:`Flex
    ~flex_direction:`Row ?align_items ?justify_content ?gap children

let zbox ?width ?height ?min_width ?min_height ?max_width ?max_height ?padding
    ?margin ?border_width ?flex_grow ?flex_shrink ?align_self ?overflow_x
    ?overflow_y ?style ?border ?border_style children =
  (* Compute the final border first *)
  let final_border =
    match (border, border_style) with
    | Some b, Some s -> Some (Border.with_style b s)
    | Some b, None -> Some b
    | None, _ -> None
  in

  let box_renderable : Renderable.t option =
    match (final_border, style) with
    | None, None -> None
    | _ -> Some (Renderable.box ?border:final_border ?background:style ())
  in

  (* Auto-set border_width when border is provided but border_width is not *)
  let effective_border_width =
    match (border_width, final_border) with
    | Some bw, _ -> Some bw (* Explicit border_width takes precedence *)
    | None, Some b ->
        (* Only auto-set if no border_width is provided at all *)
        let border_presence = border_width_of_presence b in
        Some border_presence
    | None, None -> None
  in

  let stacked_children =
    List.map
      (fun child ->
        let zero = Toffee.Style.Length_percentage_auto.length 0.0 in
        let auto = Toffee.Style.Length_percentage_auto.auto in
        let wrapper_style : Toffee.Style.t =
          Toffee.Style.make ~display:Toffee.Style.Display.Flex
            ~position:Toffee.Style.Position.Absolute
            ~align_items:Toffee.Style.Align_items.Start
            ~justify_content:Toffee.Style.Align_content.Start
            ~inset:
              {
                top = zero;
                (* pin y = 0 *)
                left = zero;
                (* pin x = 0 *)
                right = auto;
                (* let width be content-sized *)
                bottom = auto;
                (* let height be content-sized *)
              }
            ()
        in
        let wrapper_id =
          Toffee.new_with_children tree wrapper_style [| fst child |]
          |> Result.get_ok
        in
        (wrapper_id, tree))
      children
  in

  let toffee_style : Toffee.Style.t =
    let overflow_point =
      match (overflow_x, overflow_y) with
      | Some x, Some y ->
          Some
            {
              Toffee.Geometry.Point.x = overflow_to_toffee x;
              y = overflow_to_toffee y;
            }
      | Some x, None ->
          Some { x = overflow_to_toffee x; y = Toffee.Style.Overflow.Visible }
      | None, Some y ->
          Some { x = Toffee.Style.Overflow.Visible; y = overflow_to_toffee y }
      | None, None -> None
    in
    let size_dim =
      match (width, height) with
      | Some w, Some h ->
          Some
            {
              Toffee.Geometry.Size.width = dimension_to_toffee w;
              height = dimension_to_toffee h;
            }
      | Some w, None ->
          Some
            {
              width = dimension_to_toffee w;
              height = Toffee.Style.Dimension.auto;
            }
      | None, Some h ->
          Some
            {
              width = Toffee.Style.Dimension.auto;
              height = dimension_to_toffee h;
            }
      | None, None -> None
    in
    let min_size_dim =
      match (min_width, min_height) with
      | Some w, Some h ->
          Some
            {
              Toffee.Geometry.Size.width = dimension_to_toffee w;
              height = dimension_to_toffee h;
            }
      | Some w, None ->
          Some
            {
              width = dimension_to_toffee w;
              height = Toffee.Style.Dimension.auto;
            }
      | None, Some h ->
          Some
            {
              width = Toffee.Style.Dimension.auto;
              height = dimension_to_toffee h;
            }
      | None, None -> None
    in
    let max_size_dim =
      match (max_width, max_height) with
      | Some w, Some h ->
          Some
            {
              Toffee.Geometry.Size.width = dimension_to_toffee w;
              height = dimension_to_toffee h;
            }
      | Some w, None ->
          Some
            {
              width = dimension_to_toffee w;
              height = Toffee.Style.Dimension.auto;
            }
      | None, Some h ->
          Some
            {
              width = Toffee.Style.Dimension.auto;
              height = dimension_to_toffee h;
            }
      | None, None -> None
    in
    Toffee.Style.make ?overflow:overflow_point ?flex_grow ?flex_shrink
      ?align_self:(Option.map align_items_to_toffee align_self)
      ?size:size_dim ?min_size:min_size_dim ?max_size:max_size_dim
      ?margin:(Option.map sides_lpa_to_rect margin)
      ?padding:(Option.map sides_lp_to_rect padding)
      ?border:(Option.map sides_lp_to_rect effective_border_width)
      ()
  in

  let child_ids = List.map fst stacked_children in
  let id =
    Toffee.new_with_children tree toffee_style (Array.of_list child_ids)
    |> Result.get_ok
  in

  (* Store renderable in context *)
  (match box_renderable with
  | Some r ->
      let _ = Toffee.set_node_context tree id (Some r) |> Result.get_ok in
      ()
  | None -> ());

  (id, tree)

let spacer ?(flex_grow = 1.0) ?min_width ?min_height () =
  let toffee_style : Toffee.Style.t =
    let min_size_dim =
      match (min_width, min_height) with
      | Some w, Some h ->
          Some
            {
              Toffee.Geometry.Size.width = dimension_to_toffee w;
              height = dimension_to_toffee h;
            }
      | Some w, None ->
          Some
            {
              width = dimension_to_toffee w;
              height = Toffee.Style.Dimension.auto;
            }
      | None, Some h ->
          Some
            {
              width = Toffee.Style.Dimension.auto;
              height = dimension_to_toffee h;
            }
      | None, None -> None
    in
    Toffee.Style.make ~flex_grow ?min_size:min_size_dim ()
  in
  let id = Toffee.new_leaf tree toffee_style |> Result.get_ok in
  (id, tree)

let divider ?(orientation = `Horizontal) ?title:_ ?char ?style ?padding () =
  let (width : dimension option), (height : dimension option) =
    match orientation with
    | `Horizontal -> (Some (`Pct 1.0), Some (`Cells 1))
    | `Vertical -> (Some (`Cells 1), Some (`Pct 1.0))
  in
  let renderable =
    match char with
    | None ->
        (* Use a one-sided border for default divider *)
        let border =
          match orientation with
          | `Horizontal ->
              Border.make ~top:true ~bottom:false ~left:false ~right:false
                ~line_style:Border.Solid ()
          | `Vertical ->
              Border.make ~left:true ~top:false ~bottom:false ~right:false
                ~line_style:Border.Solid ()
        in
        let border =
          match style with
          | Some s -> Border.with_style border s
          | None -> border
        in
        Renderable.box ~border ()
    | Some c ->
        (* For custom char, use text with Stretch alignment to fill the line *)
        Renderable.text
          ~style:(Option.value style ~default:Style.empty)
          ~align:`Stretch c
  in

  let toffee_style : Toffee.Style.t =
    let size_dim =
      {
        Toffee.Geometry.Size.width =
          Option.value
            (Option.map dimension_to_toffee width)
            ~default:Toffee.Style.Dimension.auto;
        height =
          Option.value
            (Option.map dimension_to_toffee height)
            ~default:Toffee.Style.Dimension.auto;
      }
    in
    let padding_rect =
      Option.value
        (Option.map sides_lp_to_rect padding)
        ~default:
          {
            top = Toffee.Style.Length_percentage.length 0.0;
            right = Toffee.Style.Length_percentage.length 0.0;
            bottom = Toffee.Style.Length_percentage.length 0.0;
            left = Toffee.Style.Length_percentage.length 0.0;
          }
    in
    Toffee.Style.make ~size:size_dim ~padding:padding_rect ()
  in

  let id = Toffee.new_leaf tree toffee_style |> Result.get_ok in
  (* Store renderable in context *)
  let _ = Toffee.set_node_context tree id (Some renderable) |> Result.get_ok in
  (id, tree)

let text ?(style = Style.empty) ?(align = `Left) ?(wrap = `Wrap) ?overflow_x
    ?overflow_y content =
  let align_renderable =
    match align with `Left -> `Start | `Center -> `Center | `Right -> `End
  in
  let renderable =
    Renderable.text ~style ~align:align_renderable ~wrap content
  in

  (* Smart defaults for overflow based on wrap behavior *)
  (* When text should wrap, we want it to be able to shrink (overflow: hidden) *)
  (* When text is clipped/truncated, preserve minimum size (overflow: visible) *)
  let default_overflow_x =
    match wrap with
    | `Wrap ->
        Toffee.Style.Overflow.Hidden (* Allow shrinking for proper wrapping *)
    | `Truncate | `Clip ->
        Toffee.Style.Overflow.Visible (* Preserve min-content size *)
  in

  let default_overflow_y = Toffee.Style.Overflow.Visible in
  (* Usually don't need vertical shrinking *)

  let overflow_opt =
    match (overflow_x, overflow_y) with
    | None, None when wrap = `Wrap ->
        (* Smart default: Allow text to shrink when wrapping is enabled *)
        Some
          {
            Toffee.Geometry.Point.x = default_overflow_x;
            y = default_overflow_y;
          }
    | None, None -> None (* Use CSS defaults for non-wrapping text *)
    | Some ox, Some oy ->
        (* Both specified - use them *)
        Some
          {
            Toffee.Geometry.Point.x = overflow_to_toffee ox;
            y = overflow_to_toffee oy;
          }
    | Some ox, None ->
        (* Only X specified - use it and keep Y default *)
        Some
          {
            Toffee.Geometry.Point.x = overflow_to_toffee ox;
            y = default_overflow_y;
          }
    | None, Some oy ->
        (* Only Y specified - use it and keep X default based on wrap *)
        Some
          {
            Toffee.Geometry.Point.x = default_overflow_x;
            y = overflow_to_toffee oy;
          }
  in

  (* Force the node to fill the row when alignment needs spare space *)
  let size_opt =
    match align with
    | `Center | `Right ->
        (* Stretch to fill available width for alignment to work *)
        Some
          {
            Toffee.Geometry.Size.width = Toffee.Style.Dimension.percent 1.0;
            height = Toffee.Style.Dimension.auto;
          }
    | `Left -> None
  in

  (* Build final style *)
  let toffee_style =
    match (size_opt, overflow_opt) with
    | None, None -> Toffee.Style.default
    | Some size, None -> Toffee.Style.make ~size ()
    | None, Some overflow -> Toffee.Style.make ~overflow ()
    | Some size, Some overflow -> Toffee.Style.make ~size ~overflow ()
  in

  (* Create the final node *)
  let id = Toffee.new_leaf tree toffee_style |> Result.get_ok in

  (* Store renderable in context *)
  let _ = Toffee.set_node_context tree id (Some renderable) |> Result.get_ok in
  (* Note: The measure function is used at layout time via compute_layout_with_measure *)
  (id, tree)

let scroll_view ?width ?height ?min_width ?min_height ?max_width ?max_height
    ?padding ?margin ?flex_grow ?flex_shrink ?align_self ?style ?border
    ?border_style ?(show_scrollbars = true) ~h_offset ~v_offset child =
  (* Create the scroll container *)
  (* Use Visible overflow for layout to compute full child size *)
  (* Clipping is handled manually in renderer *)
  let scroll_style : Toffee.Style.t =
    Toffee.Style.make
      ~overflow:
        { x = Toffee.Style.Overflow.Visible; y = Toffee.Style.Overflow.Visible }
      ()
  in
  let scroll_id =
    Toffee.new_with_children tree scroll_style [| fst child |] |> Result.get_ok
  in

  (* Store scroll renderable with scrollbar visibility flag *)
  let renderable =
    if show_scrollbars then
      (* TODO: Extend Renderable.Scroll to include scrollbar rendering *)
      Renderable.scroll ~h_offset ~v_offset ()
    else Renderable.scroll ~h_offset ~v_offset ()
  in
  let _ = Toffee.set_node_context tree scroll_id (Some renderable) in

  (* Always wrap in a box with Hidden overflow for visual clipping *)
  box ?width ?height ?min_width ?min_height ?max_width ?max_height ?padding
    ?margin ?flex_grow ?flex_shrink ?align_self ?style ?border ?border_style
    ~overflow_x:`Hidden ~overflow_y:`Hidden
    [ (scroll_id, tree) ]

let canvas ?width ?height ?min_width ?min_height ?max_width ?max_height ?padding
    ?margin ?flex_grow ?flex_shrink ?align_self ?style ?border ?border_style
    draw =
  (* Create the canvas node with full size to fill its container *)
  let canvas_style =
    Toffee.Style.make
      ~size:
        {
          width = Toffee.Style.Dimension.percent 1.0;
          height = Toffee.Style.Dimension.percent 1.0;
        }
      ()
  in
  let canvas_id = Toffee.new_leaf tree canvas_style |> Result.get_ok in
  let _ =
    Toffee.set_node_context tree canvas_id (Some (Renderable.canvas draw))
    |> Result.get_ok
  in

  (* Always wrap in a box *)
  box ?width ?height ?min_width ?min_height ?max_width ?max_height ?padding
    ?margin ?flex_grow ?flex_shrink ?align_self ?style ?border ?border_style
    [ (canvas_id, tree) ]

let center child =
  box ~display:`Flex ~align_items:`Center ~justify_content:`Center
    ~width:(`Pct 1.0) ~height:(`Pct 1.0) [ child ]

let styled style child =
  (* Wrap the child in a box that applies the style *)
  box ~style [ child ]

let with_key key ((node_id, tree) as element) =
  (* Annotate the existing element with a key, don't create a wrapper *)
  let original =
    match Toffee.get_node_context tree node_id with
    | Some r -> r
    | None -> Renderable.Empty
  in
  let key_str = Attr.key_to_string key in
  let keyed =
    match original with
    | Renderable.Keyed _ -> original (* Already keyed, keep as is *)
    | _ -> Renderable.Keyed { key = key_str; child = original }
  in
  ignore (Toffee.set_node_context tree node_id (Some keyed));
  element

let flow ?h_gap ?v_gap ?overflow_x ?overflow_y children =
  (* Flow layout using flexbox with wrap enabled *)
  (* Default to overflow:hidden to prevent wrapped content from overflowing container *)
  let overflow_y = Option.value overflow_y ~default:`Hidden in
  (* Set align_content to Start to prevent wrapped lines from stretching *)
  (* Use col_gap for horizontal spacing and row_gap for vertical spacing *)
  box ~display:`Flex ~flex_direction:`Row ~flex_wrap:`Wrap ?col_gap:h_gap
    ?row_gap:v_gap ~align_items:`Start ~align_content:`Start ?overflow_x
    ~overflow_y children

let block ?width ?height ?min_width ?min_height ?max_width ?max_height ?padding
    ?margin ?style ?border ?border_style children =
  (* Block layout - create a separate implementation with Display.Block *)
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

  let child_ids = List.map fst children in

  let size_dim =
    match (width, height) with
    | Some w, Some h ->
        Some
          {
            Toffee.Geometry.Size.width = dimension_to_toffee w;
            height = dimension_to_toffee h;
          }
    | Some w, None ->
        Some
          {
            width = dimension_to_toffee w;
            height = Toffee.Style.Dimension.auto;
          }
    | None, Some h ->
        Some
          {
            width = Toffee.Style.Dimension.auto;
            height = dimension_to_toffee h;
          }
    | None, None -> None
  in
  let min_size_dim =
    match (min_width, min_height) with
    | Some w, Some h ->
        Some
          {
            Toffee.Geometry.Size.width = dimension_to_toffee w;
            height = dimension_to_toffee h;
          }
    | Some w, None ->
        Some
          {
            width = dimension_to_toffee w;
            height = Toffee.Style.Dimension.auto;
          }
    | None, Some h ->
        Some
          {
            width = Toffee.Style.Dimension.auto;
            height = dimension_to_toffee h;
          }
    | None, None -> None
  in
  let max_size_dim =
    match (max_width, max_height) with
    | Some w, Some h ->
        Some
          {
            Toffee.Geometry.Size.width = dimension_to_toffee w;
            height = dimension_to_toffee h;
          }
    | Some w, None ->
        Some
          {
            width = dimension_to_toffee w;
            height = Toffee.Style.Dimension.auto;
          }
    | None, Some h ->
        Some
          {
            width = Toffee.Style.Dimension.auto;
            height = dimension_to_toffee h;
          }
    | None, None -> None
  in

  let toffee_style =
    Toffee.Style.make ~display:Toffee.Style.Display.Block ?size:size_dim
      ?min_size:min_size_dim ?max_size:max_size_dim
      ?margin:(Option.map sides_lpa_to_rect margin)
      ?padding:(Option.map sides_lp_to_rect padding)
      ?border:(Option.map border_to_rect border)
      ()
  in

  let id =
    if List.length child_ids > 0 then
      Toffee.new_with_children tree toffee_style (Array.of_list child_ids)
      |> Result.get_ok
    else Toffee.new_leaf tree toffee_style |> Result.get_ok
  in

  (match box_renderable with
  | Some r ->
      let _ = Toffee.set_node_context tree id (Some r) |> Result.get_ok in
      ()
  | None -> ());

  (id, tree)

let grid ?(template_columns = []) ?(template_rows = []) ?(auto_columns = [])
    ?(auto_rows = []) ?(auto_flow = `Row) ?(template_areas = [])
    ?(column_names = []) ?(row_names = []) ?col_gap ?row_gap children =
  (* Convert grid_template_component to toffee *)
  let component_to_toffee = function
    | Single track -> Toffee.Style.Grid.Template_component.single track
    | Repeat rep -> Toffee.Style.Grid.Template_component.repeat rep
  in

  let grid_columns = List.map component_to_toffee template_columns in
  let grid_rows = List.map component_to_toffee template_rows in

  (* Convert auto_flow *)
  let toffee_auto_flow =
    match auto_flow with
    | `Row -> Toffee.Style.Grid.Auto_flow.Row
    | `Column -> Toffee.Style.Grid.Auto_flow.Column
    | `Row_dense -> Toffee.Style.Grid.Auto_flow.Row_dense
    | `Column_dense -> Toffee.Style.Grid.Auto_flow.Column_dense
  in

  (* Extract child node_ids *)
  let child_ids = List.map fst children in

  (* Create grid gap *)
  let gap_size =
    match (col_gap, row_gap) with
    | Some cg, Some rg ->
        Some
          {
            Toffee.Geometry.Size.width = length_percentage_to_toffee cg;
            height = length_percentage_to_toffee rg;
          }
    | Some cg, None ->
        Some
          {
            width = length_percentage_to_toffee cg;
            height = Toffee.Style.Length_percentage.length 0.0;
          }
    | None, Some rg ->
        Some
          {
            width = Toffee.Style.Length_percentage.length 0.0;
            height = length_percentage_to_toffee rg;
          }
    | None, None -> None
  in

  (* Create grid style *)
  let grid_style : Toffee.Style.t =
    Toffee.Style.make ~display:Toffee.Style.Display.Grid
      ~grid_template_columns:grid_columns ~grid_template_rows:grid_rows
      ~grid_auto_columns:auto_columns ~grid_auto_rows:auto_rows
      ~grid_auto_flow:toffee_auto_flow ~grid_template_areas:template_areas
      ~grid_template_column_names:column_names
      ~grid_template_row_names:row_names ?gap:gap_size ()
  in

  (* Create grid container *)
  let id =
    if List.length child_ids > 0 then
      Toffee.new_with_children tree grid_style (Array.of_list child_ids)
      |> Result.get_ok
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

let image ~lines ?align () =
  let align = Option.value align ~default:`Left in
  let elements =
    List.map (fun line -> text ~style:(Style.fg Ansi.Default) ~align line) lines
  in
  match align with
  | `Left -> vbox elements
  | `Center | `Right -> vbox ~width:(`Pct 1.0) elements

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
        hbox ~gap:(`Cells 0)
          [ text ~style:Style.(merge (fg Ansi.Default) dim) prefix; item ])
      items
  in
  vbox ~gap:(`Cells 0) list_items

let rich_text segments =
  (* Create multiple text elements styled individually and combine them in an hbox *)
  let text_elements =
    List.map (fun (content, style) -> text ~style content) segments
  in
  hbox ~gap:(`Cells 0) text_elements

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

  (* Use default measure function that handles text nodes *)
  let default_measure_fn known_dimensions available_space _node_id context
      _style =
    match context with
    | Some (Renderable.Text { content; tab_width; wrap; _ }) ->
        (* Use unified text measurement function *)
        Renderer.measure_text_content ~known_dimensions ~available_space
          ~tab_width ~wrap content
    | _ ->
        (* Non-text nodes: use known dimensions or default to 0 *)
        {
          width =
            Option.value ~default:0.0
              known_dimensions.Toffee.Geometry.Size.width;
          height =
            Option.value ~default:0.0
              known_dimensions.Toffee.Geometry.Size.height;
        }
  in

  let _ =
    Toffee.compute_layout_with_measure tree node_id available_space
      default_measure_fn
    |> Result.get_ok
  in

  match Toffee.layout tree node_id with
  | Ok layout ->
      let size = Toffee.Layout.size layout in
      (int_of_float size.width, int_of_float size.height)
  | Error _ -> (0, 0)

(* Dimension constructors *)
let cells n = `Cells n
let pct f = `Pct f
let auto = `Auto
let calc i = `Calc i

(* Sides constructors *)
let all value =
  let v = `Cells value in
  { top = v; right = v; bottom = v; left = v }

let xy horizontal vertical =
  let h = `Cells horizontal in
  let v = `Cells vertical in
  { top = v; right = h; bottom = v; left = h }

let sides ?(top = 0) ?(bottom = 0) ?(left = 0) ?(right = 0) () =
  {
    top = `Cells top;
    right = `Cells right;
    bottom = `Cells bottom;
    left = `Cells left;
  }

(* Grid track constructors *)
let fr f = Toffee.Style.Grid.Track_sizing_function.fr f

let minmax min max =
  let min_lp = length_percentage_to_toffee min in
  let max_lp = length_percentage_to_toffee max in
  Toffee.Style.Grid.Track_sizing_function.minmax ~min:min_lp ~max:max_lp

let fit_content_track lp =
  let toffee_lp = length_percentage_to_toffee lp in
  Toffee.Style.Grid.Track_sizing_function.fit_content toffee_lp

let min_content = Toffee.Style.Grid.Track_sizing_function.min_content
let max_content = Toffee.Style.Grid.Track_sizing_function.max_content

let track_cells n =
  Toffee.Style.Grid.Track_sizing_function.length (float_of_int n)

let track_pct f = Toffee.Style.Grid.Track_sizing_function.percent f
let track_auto = Toffee.Style.Grid.Track_sizing_function.auto

let repeat ?(line_names = []) count tracks =
  let toffee_count =
    match count with
    | `Count n -> Toffee.Style.Grid.Repetition_count.count n
    | `Auto_fill -> Toffee.Style.Grid.Repetition_count.auto_fill
    | `Auto_fit -> Toffee.Style.Grid.Repetition_count.auto_fit
  in
  Toffee.Style.Grid.Repetition.make ~count:toffee_count ~tracks ~line_names

let grid_area ~name ~row_start ~row_end ~column_start ~column_end =
  Toffee.Style.Grid.Template_area.make ~name ~row_start ~row_end ~column_start
    ~column_end

let grid_item ?row ?column child =
  (* Convert grid placement to toffee *)
  let to_toffee_placement = function
    | `Auto -> Toffee.Style.Grid.Placement.auto
    | `Line i -> Toffee.Style.Grid.Placement.line i
    | `Span i -> Toffee.Style.Grid.Placement.span i
    | `Named_line (name, i) -> Toffee.Style.Grid.Placement.named_line name i
    | `Named_span (name, i) -> Toffee.Style.Grid.Placement.named_span name i
  in

  let toffee_style =
    Toffee.Style.make
      ?grid_row:
        (Option.map
           (fun span ->
             {
               Toffee.Geometry.Line.start = to_toffee_placement span.start;
               end_ = to_toffee_placement span.end_;
             })
           row)
      ?grid_column:
        (Option.map
           (fun span ->
             {
               Toffee.Geometry.Line.start = to_toffee_placement span.start;
               end_ = to_toffee_placement span.end_;
             })
           column)
      ()
  in

  let wrapper_id =
    Toffee.new_with_children tree toffee_style [| fst child |] |> Result.get_ok
  in
  (wrapper_id, tree)
