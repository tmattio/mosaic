type 'a t = {
  id : 'a Type.Id.t (* unique witness for the type *);
  default : 'a option (* fallback when no provider  *);
}

let create ?default () = { id = Type.Id.make (); default }

(* A single frame in the stack  ------------------------------------*)
type frame = Frame : 'a t * 'a -> frame

(* One stack per domain --------------------------------------------*)
let dls : frame list Domain.DLS.key = Domain.DLS.new_key (fun () -> [])
let get_stack () = Domain.DLS.get dls
let set_stack nest = Domain.DLS.set dls nest

(*  provide : push value, run thunk, pop                             *)
let provide (type a) (ctx : a t) (value : a) (thunk : unit -> 'b) : 'b =
  let prev = get_stack () in
  set_stack (Frame (ctx, value) :: prev);
  Fun.protect thunk ~finally:(fun () -> set_stack prev)

(*  use : lookup nearest value                                       *)
let use (type a) (ctx : a t) : a =
  let rec find : frame list -> a option = function
    | [] -> None
    | Frame (ctx', v) :: tl -> (
        match Type.Id.provably_equal ctx.id ctx'.id with
        | Some Type.Equal -> Some v
        | None -> find tl)
  in
  match find (get_stack ()) with
  | Some v -> v
  | None -> (
      match ctx.default with
      | Some v -> v
      | None -> failwith "Context.use: no provider and no default")

let unsafe_use = use
