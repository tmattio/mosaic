(* Properties of Matrix_text.Layout's hit testing, the geometry every text
   consumer asks: paint, selection, editor movement, gutters and diff.

   Two contracts, with different preconditions:
   - An exact round trip, [hit (locate c)] = [c], holds only for a boundary
     that is visible (not inside an omitted suffix) and unambiguous (no other
     boundary of its row shares its column). Zero-width runs and ellipses have
     no unique cell, so elsewhere there is nothing to invert.
   - Everywhere, hit snaps canonically: for a fixed affinity, a caret that hit
     returned is returned again from its own location, on the clicked row, at
     a boundary nearest to the clicked cell's left edge. *)

open Windtrap
module Layout = Matrix_text.Layout

(* ───── Generators ───── *)

(* Fragments that stress geometry: zero-width clusters, a combining mark,
   wide and ZWJ clusters, every line terminator, tabs, invalid UTF-8 and word
   separators. *)
let fragment =
  Gen.of_list
    [
      "a";
      "b";
      "word";
      " ";
      "\t";
      "\n";
      "\r";
      "\r\n";
      "\xe2\x80\x8b" (* U+200B ZERO WIDTH SPACE *);
      "e\xcc\x81" (* e U+0301 *);
      "\xcc\x81" (* a lone U+0301 *);
      "\xe6\x97\xa5" (* 日 *);
      "\xf0\x9f\x91\xa9\xe2\x80\x8d\xf0\x9f\x92\xbb" (* 👩‍💻 *);
      "\xff";
    ]

type case = {
  text : string;
  width : int;
  wrap : Layout.wrap;
  truncate : bool;
  width_method : Matrix_text.width_method;
  tab_width : int;
}

let pp_case ppf c =
  Format.fprintf ppf
    "{ text = %S; width = %d; wrap = %s; truncate = %b; width_method = %s; \
     tab_width = %d }"
    c.text c.width
    (match c.wrap with `None -> "`None" | `Char -> "`Char" | `Word -> "`Word")
    c.truncate
    (match c.width_method with
    | `Unicode -> "`Unicode"
    | `Unicode_wide -> "`Unicode_wide"
    | `Wcwidth -> "`Wcwidth")
    c.tab_width

let gen_case =
  let open Gen in
  let text = map (String.concat "") (list ~size:(int_range 0 12) fragment) in
  let wrap = of_list [ `None; `Char; `Word ] in
  let width_method = of_list [ `Unicode; `Unicode_wide; `Wcwidth ] in
  with_pp pp_case
    (map
       (fun (text, (width, wrap), (truncate, width_method), tab_width) ->
         { text; width; wrap; truncate; width_method; tab_width })
       (quad text
          (pair (int_range (-1) 8) wrap)
          (pair bool width_method) (int_range 0 3)))

let layout c =
  Layout.create ~width_method:c.width_method ~tab_width:c.tab_width
    ~truncate:c.truncate
    (Layout.Source.of_string c.text)
    ~width:c.width ~wrap:c.wrap

(* ───── Row facts, from the public queries ───── *)

let columns c l ~first ~last =
  Layout.Source.columns ~width_method:c.width_method ~tab_width:c.tab_width
    (Layout.source l) ~first ~last

(* The last boundary before a row's ellipsis, or the row's end. *)
let visible_last l row =
  let last = ref (Layout.row_end l row) in
  Layout.iter_cells l ~row ~first_column:0 ~last_column:(Layout.row_width l row)
    (fun ~first ~last:_ ~column:_ ~width:_ ~ellipsis ->
      if ellipsis then last := first);
  !last

let column_of c l row g = columns c l ~first:(Layout.row_start l row) ~last:g

let carets l =
  let n = Layout.Source.length (Layout.source l) in
  List.concat_map
    (fun g ->
      [
        Layout.{ grapheme = g; affinity = Upstream };
        Layout.{ grapheme = g; affinity = Downstream };
      ])
    (List.init (n + 1) Fun.id)

let pp_caret ppf (c : Layout.caret) =
  Format.fprintf ppf "{ grapheme = %d; affinity = %s }" c.grapheme
    (match c.affinity with
    | Upstream -> "Upstream"
    | Downstream -> "Downstream")

let caret = Testable.make ~pp:pp_caret ~equal:( = )
let position = pair int int

(* ───── Exact round trip ───── *)

(* Visible: not past the row's ellipsis. Unambiguous: neither neighbor on the
   row sits at the same column. *)
let round_trips c l (k : Layout.caret) =
  let row, _ = Layout.locate l k in
  let g = k.grapheme in
  let first = Layout.row_start l row and last = visible_last l row in
  g <= last
  && (g = first || columns c l ~first:(g - 1) ~last:g > 0)
  && (g = last || columns c l ~first:g ~last:(g + 1) > 0)

let exact_round_trip c =
  let l = layout c in
  List.iter
    (fun (k : Layout.caret) ->
      if round_trips c l k then begin
        let row, column = Layout.locate l k in
        let back = Layout.hit ~affinity:k.affinity l ~row ~column in
        equal ~msg:"same boundary" int k.grapheme back.grapheme;
        equal ~msg:"same position" position (row, column) (Layout.locate l back)
      end)
    (carets l)

(* ───── Canonical snapping ───── *)

let canonical_snapping c =
  let l = layout c in
  let n = Layout.Source.length (Layout.source l) in
  let rows = Layout.row_count l in
  for row = -1 to rows do
    for column = -1 to Layout.max_width l + 2 do
      List.iter
        (fun affinity ->
          let k = Layout.hit ~affinity l ~row ~column in
          is_true ~msg:"in the source" (k.grapheme >= 0 && k.grapheme <= n);
          if row < 0 then equal ~msg:"above snaps to start" int 0 k.grapheme
          else if row >= rows then
            equal ~msg:"below snaps to end" int n k.grapheme
          else begin
            let row', column' = Layout.locate l k in
            equal ~msg:"a fixed point" caret k
              (Layout.hit ~affinity l ~row:row' ~column:column');
            equal ~msg:"keeps the clicked row" int row row';
            (* Nearest to the cell's left edge, ties forward, among the
               boundaries before any ellipsis. *)
            let last = visible_last l row in
            if last = Layout.row_end l row then begin
              let target = max 0 column in
              for g = Layout.row_start l row to last do
                let col = column_of c l row g in
                let d = abs (col - target) and d' = abs (column' - target) in
                is_true ~msg:"nearest boundary"
                  (d' < d || (d' = d && column' >= col))
              done
            end
          end)
        [ Layout.Upstream; Downstream ]
    done
  done

(* ───── Order ───── *)

let locate_is_monotonic c =
  let l = layout c in
  let rec check = function
    | a :: (b :: _ as rest) ->
        is_true ~msg:"later boundaries locate no earlier"
          (compare (Layout.locate l a) (Layout.locate l b) <= 0);
        check rest
    | _ -> ()
  in
  List.iter
    (fun affinity ->
      check
        (List.filter
           (fun (k : Layout.caret) -> k.affinity = affinity)
           (carets l)))
    [ Layout.Upstream; Downstream ]

(* ───── Regressions the properties found ───── *)

(* ───── Cells ───── *)

(* From any first column, a row's cells are its clusters that take a cell and
   end past that column, in order, at their columns: zero-width clusters and
   line terminators are skipped, whichever the text holds. *)
let cells_skip_zero_width c =
  let l = layout c in
  for row = 0 to Layout.row_count l - 1 do
    let first = Layout.row_start l row and last = visible_last l row in
    let row_width = Layout.row_width l row in
    for first_column = 0 to row_width - 1 do
      let expected =
        List.filter_map
          (fun g ->
            let column = column_of c l row g
            and width = columns c l ~first:g ~last:(g + 1) in
            if width > 0 && column + width > first_column then
              Some (g, column, width)
            else None)
          (List.init (last - first) (fun i -> first + i))
      in
      let visited = ref [] in
      Layout.iter_cells l ~row ~first_column ~last_column:row_width
        (fun ~first ~last:_ ~column ~width ~ellipsis ->
          if not ellipsis then visited := (first, column, width) :: !visited);
      equal
        ~msg:(Printf.sprintf "row %d from column %d" row first_column)
        (list (triple int int int))
        expected (List.rev !visited)
    done
  done

let plain ?(width = 20) ?(truncate = false) text =
  Layout.create ~truncate (Layout.Source.of_string text) ~width ~wrap:`None

let zwsp = "\xe2\x80\x8b"

let past_row_end_snaps_zero_width_run () =
  let l = plain ("ab" ^ zwsp) in
  equal int 2 (Layout.hit ~affinity:Upstream l ~row:0 ~column:5).grapheme;
  equal int 3 (Layout.hit ~affinity:Downstream l ~row:0 ~column:5).grapheme

let nearest_boundary_snaps_zero_width_run () =
  (* The click at column 4 falls inside 日; its following edge is shared with
     the zero-width space. *)
  let l = plain ("aaa\xe6\x97\xa5" ^ zwsp) in
  equal int 4 (Layout.hit ~affinity:Upstream l ~row:0 ~column:4).grapheme;
  equal int 5 (Layout.hit ~affinity:Downstream l ~row:0 ~column:4).grapheme

let ellipsis_edge_snaps_zero_width_run () =
  let l = plain ~width:3 ~truncate:true ("\xe6\x97\xa5" ^ zwsp ^ "aaa") in
  equal int 1 (Layout.hit ~affinity:Upstream l ~row:0 ~column:2).grapheme;
  equal int 2 (Layout.hit ~affinity:Downstream l ~row:0 ~column:2).grapheme;
  (* An ellipsis at column 0: a click left of it is a click on it. *)
  let l = plain ~width:1 ~truncate:true (zwsp ^ "aa") in
  equal int 0 (Layout.hit ~affinity:Upstream l ~row:0 ~column:(-1)).grapheme;
  equal int 0 (Layout.hit ~affinity:Upstream l ~row:0 ~column:0).grapheme;
  equal int 1 (Layout.hit ~affinity:Downstream l ~row:0 ~column:(-1)).grapheme

let omitted_suffix_end_locates_after_ellipsis () =
  let l = plain ~width:3 ~truncate:true "abcdef" in
  List.iter
    (fun affinity ->
      equal position (0, 3) (Layout.locate l Layout.{ grapheme = 6; affinity }))
    [ Layout.Upstream; Downstream ];
  equal position (0, 2)
    (Layout.locate l Layout.{ grapheme = 4; affinity = Upstream })

let () =
  run "matrix.text_layout"
    [
      group "Regressions"
        [
          test "past a row's end snaps a zero-width run"
            past_row_end_snaps_zero_width_run;
          test "nearest boundary snaps a zero-width run"
            nearest_boundary_snaps_zero_width_run;
          test "ellipsis edge snaps a zero-width run"
            ellipsis_edge_snaps_zero_width_run;
          test "omitted suffix end locates after the ellipsis"
            omitted_suffix_end_locates_after_ellipsis;
        ];
      group "Hit and locate"
        [
          prop ~count:1000 "visible unambiguous boundaries round-trip" gen_case
            exact_round_trip;
          prop ~count:1000 "hit snaps canonically" gen_case canonical_snapping;
          prop ~count:1000 "locate is monotonic" gen_case locate_is_monotonic;
        ];
      group "Cells"
        [
          prop ~count:1000 "cells skip zero-width clusters" gen_case
            cells_skip_zero_width;
        ];
    ]
