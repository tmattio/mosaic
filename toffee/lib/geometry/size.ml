type 'a t = { width : 'a; height : 'a }

(* Constants *)

let zero = { width = 0.0; height = 0.0 }
let none = { width = None; height = None }

(* Creation *)

let make width height = { width; height }
let square side = { width = side; height = side }

(* Arithmetic operations *)

let add a b = { width = a.width +. b.width; height = a.height +. b.height }
let sub a b = { width = a.width -. b.width; height = a.height -. b.height }
let ( + ) = add
let ( - ) = sub

let mul_scalar size scalar =
  { width = size.width *. scalar; height = size.height *. scalar }

let div_scalar size scalar =
  { width = size.width /. scalar; height = size.height /. scalar }

let ( * ) = mul_scalar
let ( / ) = div_scalar

(* Mapping functions *)

let map f size = { width = f size.width; height = f size.height }
let map_width f size = { width = f size.width; height = size.height }
let map_height f size = { width = size.width; height = f size.height }

let zip_map size other f =
  { width = f size.width other.width; height = f size.height other.height }

let map2 f s1 s2 = zip_map s1 s2 f

(* Axis accessors *)

let get size axis =
  match axis with
  | Abstract_axis.Inline -> size.width
  | Abstract_axis.Block -> size.height

let set size axis value =
  match axis with
  | Abstract_axis.Inline -> { size with width = value }
  | Abstract_axis.Block -> { size with height = value }

let get_absolute size axis =
  match axis with
  | Absolute_axis.Horizontal -> size.width
  | Absolute_axis.Vertical -> size.height

(* Float-specific functions *)

let max a b = { width = max a.width b.width; height = max a.height b.height }
let min a b = { width = min a.width b.width; height = min a.height b.height }
let has_non_zero_area size = size.width > 0.0 && size.height > 0.0
let equal a b = a.width = b.width && a.height = b.height

(* Option-specific functions *)

let unwrap_or size alt =
  {
    width = (match size.width with Some w -> w | None -> alt.width);
    height = (match size.height with Some h -> h | None -> alt.height);
  }

let choose_first size alt =
  {
    width = (match size.width with Some _ as w -> w | None -> alt.width);
    height = (match size.height with Some _ as h -> h | None -> alt.height);
  }

let both_axis_defined size =
  match (size.width, size.height) with Some _, Some _ -> true | _ -> false

(* Utility operations *)

let apply_aspect_ratio size aspect_ratio =
  match aspect_ratio with
  | None -> size
  | Some ratio -> (
      match (size.width, size.height) with
      | Some width, None ->
          { width = Some width; height = Some (width /. ratio) }
      | None, Some height ->
          { width = Some (height *. ratio); height = Some height }
      | _ -> size)

(* Comparison and string functions *)

let compare cmp a b =
  let cmp_width = cmp a.width b.width in
  if cmp_width <> 0 then cmp_width else cmp a.height b.height

let equal_with eq a b = eq a.width b.width && eq a.height b.height

let to_string f size =
  Printf.sprintf "{ width: %s; height: %s }" (f size.width) (f size.height)

let pp f fmt size =
  Format.fprintf fmt "{ width: %a; height: %a }" f size.width f size.height

(* For compatibility with existing code that might use equal_option *)
let equal_option eq a b = equal_with (Option.equal eq) a b

(* Operations with optional values - implementing Taffy's MaybeMath patterns *)

(* Option → Option operations *)

let min_option a b =
  {
    width =
      (match (a.width, b.width) with
      | Some l, Some r -> Some (Float.min l r)
      | Some _, None -> a.width
      | None, Some _ -> None
      | None, None -> None);
    height =
      (match (a.height, b.height) with
      | Some l, Some r -> Some (Float.min l r)
      | Some _, None -> a.height
      | None, Some _ -> None
      | None, None -> None);
  }

let max_option a b =
  {
    width =
      (match (a.width, b.width) with
      | Some l, Some r -> Some (Float.max l r)
      | Some _, None -> a.width
      | None, Some _ -> None
      | None, None -> None);
    height =
      (match (a.height, b.height) with
      | Some l, Some r -> Some (Float.max l r)
      | Some _, None -> a.height
      | None, Some _ -> None
      | None, None -> None);
  }

let add_option a b =
  {
    width =
      (match (a.width, b.width) with
      | Some l, Some r -> Some (l +. r)
      | Some _, None -> a.width
      | None, Some _ -> None
      | None, None -> None);
    height =
      (match (a.height, b.height) with
      | Some l, Some r -> Some (l +. r)
      | Some _, None -> a.height
      | None, Some _ -> None
      | None, None -> None);
  }

let sub_option a b =
  {
    width =
      (match (a.width, b.width) with
      | Some l, Some r -> Some (l -. r)
      | Some _, None -> a.width
      | None, Some _ -> None
      | None, None -> None);
    height =
      (match (a.height, b.height) with
      | Some l, Some r -> Some (l -. r)
      | Some _, None -> a.height
      | None, Some _ -> None
      | None, None -> None);
  }

let clamp_option self min max =
  {
    width =
      (match (self.width, min.width, max.width) with
      | Some base, Some min, Some max ->
          Some (Float.min max (Float.max min base))
      | Some base, None, Some max -> Some (Float.min max base)
      | Some base, Some min, None -> Some (Float.max min base)
      | Some _, None, None -> self.width
      | None, _, _ -> None);
    height =
      (match (self.height, min.height, max.height) with
      | Some base, Some min, Some max ->
          Some (Float.min max (Float.max min base))
      | Some base, None, Some max -> Some (Float.min max base)
      | Some base, Some min, None -> Some (Float.max min base)
      | Some _, None, None -> self.height
      | None, _, _ -> None);
  }

(* Concrete + Option → Option operations *)

let min_with_option concrete optional =
  {
    width =
      (match optional.width with
      | Some opt_val -> Some (Float.min concrete.width opt_val)
      | None -> Some concrete.width);
    height =
      (match optional.height with
      | Some opt_val -> Some (Float.min concrete.height opt_val)
      | None -> Some concrete.height);
  }

let max_with_option concrete optional =
  {
    width =
      (match optional.width with
      | Some opt_val -> Some (Float.max concrete.width opt_val)
      | None -> Some concrete.width);
    height =
      (match optional.height with
      | Some opt_val -> Some (Float.max concrete.height opt_val)
      | None -> Some concrete.height);
  }

let add_with_option concrete optional =
  {
    width =
      (match optional.width with
      | Some opt_val -> Some (concrete.width +. opt_val)
      | None -> Some concrete.width);
    height =
      (match optional.height with
      | Some opt_val -> Some (concrete.height +. opt_val)
      | None -> Some concrete.height);
  }

let sub_with_option concrete optional =
  {
    width =
      (match optional.width with
      | Some opt_val -> Some (concrete.width -. opt_val)
      | None -> Some concrete.width);
    height =
      (match optional.height with
      | Some opt_val -> Some (concrete.height -. opt_val)
      | None -> Some concrete.height);
  }

(* Concrete operations with optional parameters *)

let clamp concrete min max =
  {
    width =
      (match (min.width, max.width) with
      | Some min_val, Some max_val ->
          Float.min max_val (Float.max min_val concrete.width)
      | None, Some max_val -> Float.min max_val concrete.width
      | Some min_val, None -> Float.max min_val concrete.width
      | None, None -> concrete.width);
    height =
      (match (min.height, max.height) with
      | Some min_val, Some max_val ->
          Float.min max_val (Float.max min_val concrete.height)
      | None, Some max_val -> Float.min max_val concrete.height
      | Some min_val, None -> Float.max min_val concrete.height
      | None, None -> concrete.height);
  }

let min_or_self concrete optional =
  {
    width =
      (match optional.width with
      | Some val_ -> Float.min concrete.width val_
      | None -> concrete.width);
    height =
      (match optional.height with
      | Some val_ -> Float.min concrete.height val_
      | None -> concrete.height);
  }

let max_or_self concrete optional =
  {
    width =
      (match optional.width with
      | Some val_ -> Float.max concrete.width val_
      | None -> concrete.width);
    height =
      (match optional.height with
      | Some val_ -> Float.max concrete.height val_
      | None -> concrete.height);
  }

let add_or_zero concrete optional =
  {
    width =
      (match optional.width with
      | Some val_ -> concrete.width +. val_
      | None -> concrete.width);
    height =
      (match optional.height with
      | Some val_ -> concrete.height +. val_
      | None -> concrete.height);
  }

let sub_or_zero concrete optional =
  {
    width =
      (match optional.width with
      | Some val_ -> concrete.width -. val_
      | None -> concrete.width);
    height =
      (match optional.height with
      | Some val_ -> concrete.height -. val_
      | None -> concrete.height);
  }

let max_concrete a b =
  { width = Float.max a.width b.width; height = Float.max a.height b.height }
