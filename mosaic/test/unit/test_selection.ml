open Windtrap
open Mosaic_ui

(* ── Helpers ── *)

let pt x y : Selection.point = { x; y }
let point = Testable.make ~pp:Selection.pp_point ~equal:Selection.equal_point

(* Bounded coordinates for property tests — avoids overflow in abs/addition. *)
let coord = Gen.small_int
let gen_point = Gen.pair coord coord

let bounds_t =
  Testable.make ~pp:Selection.pp_bounds ~equal:Selection.equal_bounds

(* ── Creation ── *)

let creation_positions () =
  let sel = Selection.create ~anchor:(pt 2 3) ~focus:(pt 5 7) () in
  equal ~msg:"anchor" point (pt 2 3) (Selection.anchor sel);
  equal ~msg:"focus" point (pt 5 7) (Selection.focus sel)

let creation_flags () =
  let sel = Selection.create ~anchor:(pt 0 0) ~focus:(pt 0 0) () in
  is_true ~msg:"active" (Selection.is_active sel);
  is_true ~msg:"dragging" (Selection.is_dragging sel);
  is_true ~msg:"is_start" (Selection.is_start sel)

(* ── Position ── *)

let set_anchor_updates () =
  let sel = Selection.create ~anchor:(pt 0 0) ~focus:(pt 5 5) () in
  Selection.set_anchor sel (pt 10 20);
  equal ~msg:"anchor" point (pt 10 20) (Selection.anchor sel)

let set_focus_updates () =
  let sel = Selection.create ~anchor:(pt 0 0) ~focus:(pt 5 5) () in
  Selection.set_focus sel (pt 10 20);
  equal ~msg:"focus" point (pt 10 20) (Selection.focus sel)

let set_anchor_replaces_callback () =
  let counter = ref 0 in
  let sel =
    Selection.create
      ~anchor_position:(fun () ->
        incr counter;
        pt 99 99)
      ~anchor:(pt 0 0) ~focus:(pt 5 5) ()
  in
  (* Callback should be used before set_anchor *)
  let _ = Selection.anchor sel in
  let count_before = !counter in
  is_true ~msg:"callback was called" (count_before > 0);
  (* Replace with static *)
  Selection.set_anchor sel (pt 42 42);
  counter := 0;
  let a = Selection.anchor sel in
  equal ~msg:"static anchor" point (pt 42 42) a;
  equal ~msg:"callback not called" int 0 !counter

let anchor_callback_per_access () =
  let counter = ref 0 in
  let sel =
    Selection.create
      ~anchor_position:(fun () ->
        incr counter;
        pt !counter 0)
      ~anchor:(pt 0 0) ~focus:(pt 5 5) ()
  in
  let _ = Selection.anchor sel in
  let _ = Selection.anchor sel in
  equal ~msg:"called twice" int 2 !counter

(* ── Bounds ── *)

let bounds_single_cell () =
  let sel = Selection.create ~anchor:(pt 5 3) ~focus:(pt 5 3) () in
  equal ~msg:"bounds" bounds_t
    { x = 5; y = 3; width = 1; height = 1 }
    (Selection.bounds sel)

let bounds_anchor_before_focus () =
  let sel = Selection.create ~anchor:(pt 2 1) ~focus:(pt 5 3) () in
  equal ~msg:"bounds" bounds_t
    { x = 2; y = 1; width = 4; height = 3 }
    (Selection.bounds sel)

let bounds_focus_before_anchor () =
  let sel = Selection.create ~anchor:(pt 5 3) ~focus:(pt 2 1) () in
  equal ~msg:"bounds" bounds_t
    { x = 2; y = 1; width = 4; height = 3 }
    (Selection.bounds sel)

let bounds_horizontal () =
  let sel = Selection.create ~anchor:(pt 0 5) ~focus:(pt 9 5) () in
  let b = Selection.bounds sel in
  equal ~msg:"width" int 10 b.width;
  equal ~msg:"height" int 1 b.height

let bounds_vertical () =
  let sel = Selection.create ~anchor:(pt 3 0) ~focus:(pt 3 4) () in
  let b = Selection.bounds sel in
  equal ~msg:"width" int 1 b.width;
  equal ~msg:"height" int 5 b.height

let bounds_after_mutation () =
  let sel = Selection.create ~anchor:(pt 0 0) ~focus:(pt 0 0) () in
  Selection.set_focus sel (pt 5 5);
  equal ~msg:"bounds" bounds_t
    { x = 0; y = 0; width = 6; height = 6 }
    (Selection.bounds sel)

(* ── State flags ── *)

let toggle_is_active () =
  let sel = Selection.create ~anchor:(pt 0 0) ~focus:(pt 0 0) () in
  Selection.set_is_active sel false;
  is_false ~msg:"inactive" (Selection.is_active sel);
  Selection.set_is_active sel true;
  is_true ~msg:"active" (Selection.is_active sel)

let toggle_is_dragging () =
  let sel = Selection.create ~anchor:(pt 0 0) ~focus:(pt 0 0) () in
  Selection.set_is_dragging sel false;
  is_false ~msg:"not dragging" (Selection.is_dragging sel);
  Selection.set_is_dragging sel true;
  is_true ~msg:"dragging" (Selection.is_dragging sel)

let toggle_is_start () =
  let sel = Selection.create ~anchor:(pt 0 0) ~focus:(pt 0 0) () in
  Selection.set_is_start sel false;
  is_false ~msg:"not start" (Selection.is_start sel);
  Selection.set_is_start sel true;
  is_true ~msg:"start" (Selection.is_start sel)

(* ── to_local ── *)

let to_local_subtracts_origin () =
  let sel = Selection.create ~anchor:(pt 10 20) ~focus:(pt 15 25) () in
  let lb = Selection.to_local sel ~origin:(pt 5 10) in
  equal ~msg:"anchor" point (pt 5 10) lb.anchor;
  equal ~msg:"focus" point (pt 10 15) lb.focus

let to_local_zero_identity () =
  let sel = Selection.create ~anchor:(pt 3 7) ~focus:(pt 8 2) () in
  let lb = Selection.to_local sel ~origin:(pt 0 0) in
  equal ~msg:"anchor" point (pt 3 7) lb.anchor;
  equal ~msg:"focus" point (pt 8 2) lb.focus

let to_local_negative () =
  let sel = Selection.create ~anchor:(pt 2 3) ~focus:(pt 4 5) () in
  let lb = Selection.to_local sel ~origin:(pt 10 10) in
  equal ~msg:"anchor" point (pt (-8) (-7)) lb.anchor;
  equal ~msg:"focus" point (pt (-6) (-5)) lb.focus

(* ── Equality ── *)

let equal_point_same () =
  is_true ~msg:"same" (Selection.equal_point (pt 1 2) (pt 1 2))

let equal_point_different () =
  is_false ~msg:"diff y" (Selection.equal_point (pt 1 2) (pt 1 3));
  is_false ~msg:"diff x" (Selection.equal_point (pt 1 2) (pt 0 2))

let equal_bounds_same () =
  let b = { Selection.x = 1; y = 2; width = 3; height = 4 } in
  is_true ~msg:"same" (Selection.equal_bounds b b)

let equal_bounds_different () =
  let a = { Selection.x = 1; y = 2; width = 3; height = 4 } in
  let b = { Selection.x = 1; y = 2; width = 3; height = 5 } in
  is_false ~msg:"diff" (Selection.equal_bounds a b)

let equal_local_bounds_same () =
  let lb = { Selection.anchor = pt 1 2; focus = pt 3 4 } in
  is_true ~msg:"same" (Selection.equal_local_bounds lb lb)

let equal_local_bounds_different () =
  let a = { Selection.anchor = pt 1 2; focus = pt 3 4 } in
  let b = { Selection.anchor = pt 1 2; focus = pt 3 5 } in
  is_false ~msg:"diff" (Selection.equal_local_bounds a b)

(* ── Runner ── *)

let () =
  run "mosaic.selection"
    [
      group "Creation"
        [
          test "anchor and focus set" creation_positions;
          test "starts active, dragging, is_start" creation_flags;
        ];
      group "Position"
        [
          test "set_anchor updates" set_anchor_updates;
          test "set_focus updates" set_focus_updates;
          test "set_anchor replaces callback" set_anchor_replaces_callback;
          test "anchor_position callback per access" anchor_callback_per_access;
        ];
      group "Bounds"
        [
          test "single cell" bounds_single_cell;
          test "anchor before focus" bounds_anchor_before_focus;
          test "focus before anchor" bounds_focus_before_anchor;
          test "horizontal line" bounds_horizontal;
          test "vertical line" bounds_vertical;
          test "after mutation" bounds_after_mutation;
          prop "width >= 1" (Gen.pair gen_point gen_point)
            (fun ((ax, ay), (fx, fy)) ->
              let sel =
                Selection.create ~anchor:(pt ax ay) ~focus:(pt fx fy) ()
              in
              greater_equal int ~than:1 (Selection.bounds sel).width);
          prop "height >= 1" (Gen.pair gen_point gen_point)
            (fun ((ax, ay), (fx, fy)) ->
              let sel =
                Selection.create ~anchor:(pt ax ay) ~focus:(pt fx fy) ()
              in
              greater_equal int ~than:1 (Selection.bounds sel).height);
          prop "symmetric in anchor and focus" (Gen.pair gen_point gen_point)
            (fun ((ax, ay), (fx, fy)) ->
              let s1 =
                Selection.create ~anchor:(pt ax ay) ~focus:(pt fx fy) ()
              in
              let s2 =
                Selection.create ~anchor:(pt fx fy) ~focus:(pt ax ay) ()
              in
              equal bounds_t (Selection.bounds s1) (Selection.bounds s2));
          prop "width = abs(x1-x0)+1" (Gen.pair gen_point gen_point)
            (fun ((ax, _ay), (fx, fy)) ->
              let sel =
                Selection.create ~anchor:(pt ax 0) ~focus:(pt fx fy) ()
              in
              equal int (abs (fx - ax) + 1) (Selection.bounds sel).width);
        ];
      group "State flags"
        [
          test "set_is_active" toggle_is_active;
          test "set_is_dragging" toggle_is_dragging;
          test "set_is_start" toggle_is_start;
        ];
      group "to_local"
        [
          test "subtracts origin" to_local_subtracts_origin;
          test "zero origin is identity" to_local_zero_identity;
          test "can produce negative coordinates" to_local_negative;
          prop "subtracts origin componentwise"
            (Gen.triple gen_point gen_point gen_point)
            (fun ((ax, ay), (fx, fy), (ox, oy)) ->
              let sel =
                Selection.create ~anchor:(pt ax ay) ~focus:(pt fx fy) ()
              in
              let lb = Selection.to_local sel ~origin:(pt ox oy) in
              equal ~msg:"anchor" point (pt (ax - ox) (ay - oy)) lb.anchor;
              equal ~msg:"focus" point (pt (fx - ox) (fy - oy)) lb.focus);
        ];
      group "Equality"
        [
          test "equal_point same" equal_point_same;
          test "equal_point different" equal_point_different;
          test "equal_bounds same" equal_bounds_same;
          test "equal_bounds different" equal_bounds_different;
          test "equal_local_bounds same" equal_local_bounds_same;
          test "equal_local_bounds different" equal_local_bounds_different;
        ];
    ]
