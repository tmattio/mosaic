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

(* ── Runner ── *)

let () =
  run "mosaic.diff"
    [
      group "Reconciliation"
        [
          test "unified update reuses children" unified_update_reuses_children;
          test "empty patch drops children" empty_patch_drops_children;
        ];
    ]
