type 'a t = {
  id : 'a Type.Id.t (* unique witness for the type *);
  default : 'a option (* fallback when no provider  *);
  name : string option (* optional name for debugging *);
}

let create ?default ?name () = { id = Type.Id.make (); default; name }

(* A single frame in the stack *)
type frame = Frame : 'a t * 'a -> frame

(* Global stack *)
let stack : frame list ref = ref []
let get_stack () = !stack
let set_stack nest = stack := nest

(*  provide : push value, run thunk, pop *)
let provide (type a) (ctx : a t) (value : a) (thunk : unit -> 'b) : 'b =
  let prev = get_stack () in
  set_stack (Frame (ctx, value) :: prev);
  Fun.protect thunk ~finally:(fun () -> set_stack prev)

(*  use : lookup nearest value *)
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
      | None ->
          let msg =
            match ctx.name with
            | Some n ->
                Printf.sprintf
                  "Context.use: no provider for context '%s' and no default" n
            | None -> "Context.use: no provider and no default"
          in
          failwith msg)
