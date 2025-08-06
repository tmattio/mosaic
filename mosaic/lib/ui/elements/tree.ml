open Element

type guide_style = Normal | ASCII | Bold | Double

type node = {
  label : t;
  expanded : bool;
  children : node list;
  guide_style : Style.t option;
}

let tree ?style ?(guide_style = Style.(fg Ansi.Default ++ dim))
    ?(guides = Normal) ?(hide_root = false) ?expanded root =
  (* Guide characters based on style - matching Rich's implementation *)
  let guides_chars =
    match guides with
    | ASCII -> ("    ", "|   ", "+-- ", "`-- ")
    | Normal -> ("    ", "│   ", "├── ", "└── ")
    | Bold -> ("    ", "┃   ", "┣━━ ", "┗━━ ")
    | Double -> ("    ", "║   ", "╠══ ", "╚══ ")
  in
  let space, vertical, branch, last_branch = guides_chars in

  let rec render_node ~prefix ~is_last ~current_guide_style node =
    (* Determine expanded state - use override if provided *)
    let is_expanded =
      match expanded with Some e -> e | None -> node.expanded
    in

    (* Use node's guide style if specified, otherwise use current *)
    let node_guide_style =
      match node.guide_style with
      | Some s -> Style.(current_guide_style ++ s)
      | None -> current_guide_style
    in

    (* Apply style to label if provided *)
    let label =
      match style with
      | Some s -> Element.styled s node.label
      | None -> node.label
    in

    (* Build the node line *)
    let connector =
      text ~style:node_guide_style (if is_last then last_branch else branch)
    in
    let node_line = hbox ~gap:(cells 0) [ text prefix; connector; label ] in

    (* If collapsed or no children, just return the node line *)
    if (not is_expanded) || node.children = [] then node_line
    else
      (* Expanded with children - render children with updated prefix *)
      let child_prefix = prefix ^ if is_last then space else vertical in
      let children_elements =
        List.mapi
          (fun i child ->
            let is_last_child = i = List.length node.children - 1 in
            render_node ~prefix:child_prefix ~is_last:is_last_child
              ~current_guide_style:node_guide_style child)
          node.children
      in
      vbox ~gap:(cells 0) (node_line :: children_elements)
  in

  (* Handle root node *)
  if hide_root then
    (* Hide root - only show children *)
    if root.children = [] then text ""
    else
      let children_elements =
        List.mapi
          (fun i child ->
            let is_last_child = i = List.length root.children - 1 in
            render_node ~prefix:"" ~is_last:is_last_child
              ~current_guide_style:guide_style child)
          root.children
      in
      vbox ~gap:(cells 0) children_elements
  else
    (* Show root *)
    let is_expanded =
      match expanded with Some e -> e | None -> root.expanded
    in

    let root_label =
      match style with
      | Some s -> Element.styled s root.label
      | None -> root.label
    in

    if (not is_expanded) || root.children = [] then root_label
    else
      let children_elements =
        List.mapi
          (fun i child ->
            let is_last_child = i = List.length root.children - 1 in
            render_node ~prefix:"" ~is_last:is_last_child
              ~current_guide_style:guide_style child)
          root.children
      in
      vbox ~gap:(cells 0) (root_label :: children_elements)
