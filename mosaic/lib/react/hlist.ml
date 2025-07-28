(* Type identifiers.
   This is the core mechanism that allows for runtime type-safe casting. *)
module Tid = struct
  type _ t = ..
end

module type Tid = sig
  type t
  type _ Tid.t += Tid : t Tid.t
end

type 'a tid = (module Tid with type t = 'a)

let tid () (type s) =
  let module M = struct
    type t = s
    type _ Tid.t += Tid : t Tid.t
  end in
  (module M : Tid with type t = s)

type ('a, 'b) teq = Teq : ('a, 'a) teq

let eq : type r s. r tid -> s tid -> (r, s) teq option =
 fun r s ->
  let module R = (val r : Tid with type t = r) in
  let module S = (val s : Tid with type t = s) in
  match R.Tid with S.Tid -> Some Teq | _ -> None

(* Heterogeneous lists *)

module type KEY_INFO = sig
  type 'a t
end

module type S = sig
  type 'a key

  module Key : sig
    type 'a info

    val create : 'a info -> 'a key
    val info : 'a key -> 'a info

    type t

    val hide_type : 'a key -> t
    val equal : t -> t -> bool
    val compare : t -> t -> int
  end

  type t

  val empty : t
  val is_empty : t -> bool
  val cons : 'a key -> 'a -> t -> t

  type binding = B : 'a key * 'a -> binding

  val hd : t -> binding option
  val tl : t -> t
  val find : 'a key -> t -> 'a option
  val get : 'a key -> t -> 'a
  val mem : 'a key -> t -> bool
  val rem : 'a key -> t -> t
  val length : t -> int
  val iter : (binding -> unit) -> t -> unit
  val fold_left : ('a -> binding -> 'a) -> 'a -> t -> 'a
  val for_all : (binding -> bool) -> t -> bool
  val exists : (binding -> bool) -> t -> bool
  val filter : (binding -> bool) -> t -> t
  val rev : t -> t
end

module Make (Key_info : KEY_INFO) : S with type 'a Key.info = 'a Key_info.t =
struct
  (* Keys *)

  module Key = struct
    type 'a info = 'a Key_info.t
    type 'a key = { uid : int; tid : 'a tid; info : 'a Key_info.t }

    let uid =
      let id = ref (-1) in
      fun () ->
        incr id;
        !id

    let create info =
      let uid = uid () in
      let tid = tid () in
      { uid; tid; info }

    let info k = k.info

    type t = V : 'a key -> t

    let hide_type k = V k
    let equal (V k0) (V k1) = Int.equal k0.uid k1.uid
    let compare (V k0) (V k1) = Int.compare k0.uid k1.uid
  end

  type 'a key = 'a Key.key

  (* Lists *)

  type binding = B : 'a key * 'a -> binding
  type t = binding list

  let empty = []
  let is_empty = function [] -> true | _ -> false
  let cons k v l = B (k, v) :: l
  let hd = function [] -> None | h :: _ -> Some h
  let tl = function [] -> invalid_arg "Hlist.tl: empty list" | _ :: t -> t

  let find : type a. a key -> t -> a option =
   fun k l ->
    let rec find_rec (list : t) : a option =
      match list with
      | [] -> None
      | B (k', v) :: rest ->
          if k.Key.uid = k'.Key.uid then
            match eq k.Key.tid k'.Key.tid with
            | Some Teq -> Some v
            | None ->
                find_rec rest (* Logically unreachable if UIDs are unique *)
          else find_rec rest
    in
    find_rec l

  let get k l = match find k l with None -> raise Not_found | Some v -> v

  let mem k l =
    List.exists (fun (B (k', _)) -> Key.equal (Key.V k) (Key.V k')) l

  let rem k l =
    List.filter (fun (B (k', _)) -> not (Key.equal (Key.V k) (Key.V k'))) l

  let length = List.length
  let iter = List.iter
  let fold_left f acc l = List.fold_left f acc l
  let for_all = List.for_all
  let exists = List.exists
  let filter = List.filter
  let rev = List.rev
end

include Make (struct
  type 'a t = unit
end)
