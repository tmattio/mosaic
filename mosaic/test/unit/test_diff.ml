open Windtrap
open Mosaic_ui
module Patch = Diff.Patch

(* ── Helpers ── *)

let parse s =
  match Patch.of_unified s with
  | Ok patch -> patch
  | Error message -> failwith ("parse failed: " ^ message)

let base_diff =
  {|--- a/test.js
+++ b/test.js
@@ -1,3 +1,3 @@
 function hello() {
-  console.log("Hello");
+  console.log("Hello, World!");
 }
|}

let grown_diff =
  {|--- a/test.js
+++ b/test.js
@@ -1,3 +1,4 @@
 function hello() {
-  console.log("Hello");
+  console.log("Hello, World!");
+  console.log("Streamed");
 }
|}

let rec collect_nodes node =
  node :: List.concat_map collect_nodes (Renderable.children node)

(* ── Reconciliation ── *)

let unified_update_reuses_children () =
  (* A streaming patch update in the default Unified layout must reconcile the
     existing Line_number and Code children in place rather than destroying
     and recreating them, so highlight jobs and streaming anti-flash state
     survive each chunk. *)
  let renderer = Renderer.create () in
  let d = Diff.create ~parent:(Renderer.root renderer) (parse base_diff) in
  Renderer.render_frame renderer ~width:60 ~height:10 ~delta:0.;
  ignore (Renderer.render ~full:true renderer : string);
  let before = collect_nodes (Diff.node d) in
  Diff.set_patch d (parse grown_diff);
  let after = collect_nodes (Diff.node d) in
  equal ~msg:"same subtree shape" int (List.length before) (List.length after);
  is_true ~msg:"every node is reused" (List.for_all2 ( == ) before after)

let empty_patch_drops_children () =
  let renderer = Renderer.create () in
  let d = Diff.create ~parent:(Renderer.root renderer) (parse base_diff) in
  Diff.set_patch d (Patch.make []);
  equal ~msg:"no children" int 0
    (List.length (Renderable.children (Diff.node d)))

(* ── Spans ── *)

(* A span's bytes map to the surface's graphemes, which index a cluster of no
   width as any other: the span on "b" after a zero-width space colors "b". *)
let span_counts_surface_graphemes () =
  let renderer = Renderer.create () in
  let patch = parse "--- a/t\n+++ b/t\n@@ -0,0 +1,1 @@\n+\u{200B}ab\n" in
  let span =
    {
      Diff.side = New;
      line = 1;
      start_byte = 4;
      end_byte = 5;
      color = Ansi.Color.red;
    }
  in
  ignore
    (Diff.create ~parent:(Renderer.root renderer) ~show_line_numbers:false
       ~line_spans:[ span ] patch
      : Diff.t);
  Renderer.render_frame renderer ~width:20 ~height:3 ~delta:0.;
  let grid = Matrix_screen.next_grid (Renderer.screen renderer) in
  let bg_of text =
    let rec find i =
      if Matrix_grid.get_text grid i = text then Matrix_grid.get_bg grid i
      else find (i + 1)
    in
    find 0
  in
  is_true ~msg:"b is colored" (Ansi.Color.equal Ansi.Color.red (bg_of "b"));
  is_false ~msg:"a is not" (Ansi.Color.equal Ansi.Color.red (bg_of "a"))

(* ── Runner ── *)

let () =
  run "mosaic.diff"
    [
      group "Reconciliation"
        [
          test "unified update reuses children" unified_update_reuses_children;
          test "empty patch drops children" empty_patch_drops_children;
        ];
      group "Spans"
        [ test "span counts surface graphemes" span_counts_surface_graphemes ];
    ]
