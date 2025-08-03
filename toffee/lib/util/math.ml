module OptionOption = struct
  let maybe_min self rhs =
    match (self, rhs) with
    | Some l, Some r -> Some (min l r)
    | Some _l, None -> self
    | None, Some _r -> None
    | None, None -> None

  let maybe_max self rhs =
    match (self, rhs) with
    | Some l, Some r -> Some (max l r)
    | Some _l, None -> self
    | None, Some _r -> None
    | None, None -> None

  let maybe_clamp self min max =
    match (self, min, max) with
    | Some base, Some min, Some max -> Some (Float.min base max |> Float.max min)
    | Some base, None, Some max -> Some (Float.min base max)
    | Some base, Some min, None -> Some (Float.max base min)
    | Some _, None, None -> self
    | None, _, _ -> None

  let maybe_add self rhs =
    match (self, rhs) with
    | Some l, Some r -> Some (l +. r)
    | Some _l, None -> self
    | None, Some _r -> None
    | None, None -> None

  let maybe_sub self rhs =
    match (self, rhs) with
    | Some l, Some r -> Some (l -. r)
    | Some _l, None -> self
    | None, Some _r -> None
    | None, None -> None
end

module OptionFloat = struct
  let maybe_min self rhs = Option.map (fun value -> Float.min value rhs) self
  let maybe_max self rhs = Option.map (fun value -> Float.max value rhs) self

  let maybe_clamp self min max =
    Option.map (fun value -> Float.min value max |> Float.max min) self

  let maybe_add self rhs = Option.map (fun value -> value +. rhs) self
  let maybe_sub self rhs = Option.map (fun value -> value -. rhs) self
end

module FloatOption = struct
  let maybe_min self rhs =
    match rhs with Some value -> Float.min self value | None -> self

  let maybe_max self rhs =
    match rhs with Some value -> Float.max self value | None -> self

  let maybe_clamp self min max =
    match (min, max) with
    | Some min, Some max -> Float.min self max |> Float.max min
    | None, Some max -> Float.min self max
    | Some min, None -> Float.max self min
    | None, None -> self

  let maybe_add self rhs =
    match rhs with Some value -> self +. value | None -> self

  let maybe_sub self rhs =
    match rhs with Some value -> self -. value | None -> self
end

module AvailableSpaceFloat = struct
  open Style

  let maybe_min self rhs =
    match self with
    | Available_space.Definite value ->
        Available_space.Definite (Float.min value rhs)
    | Available_space.Min_content -> Available_space.Definite rhs
    | Available_space.Max_content -> Available_space.Definite rhs

  let maybe_max self rhs =
    match self with
    | Available_space.Definite value ->
        Available_space.Definite (Float.max value rhs)
    | Available_space.Min_content -> Available_space.Min_content
    | Available_space.Max_content -> Available_space.Max_content

  let maybe_clamp self min max =
    match self with
    | Available_space.Definite value ->
        Available_space.Definite (Float.min value max |> Float.max min)
    | Available_space.Min_content -> Available_space.Min_content
    | Available_space.Max_content -> Available_space.Max_content

  let maybe_add self rhs =
    match self with
    | Available_space.Definite value -> Available_space.Definite (value +. rhs)
    | Available_space.Min_content -> Available_space.Min_content
    | Available_space.Max_content -> Available_space.Max_content

  let maybe_sub self rhs =
    match self with
    | Available_space.Definite value -> Available_space.Definite (value -. rhs)
    | Available_space.Min_content -> Available_space.Min_content
    | Available_space.Max_content -> Available_space.Max_content
end

module AvailableSpaceOption = struct
  open Style

  let maybe_min self rhs =
    match (self, rhs) with
    | Available_space.Definite value, Some rhs ->
        Available_space.Definite (Float.min value rhs)
    | Available_space.Definite value, None -> Available_space.Definite value
    | Available_space.Min_content, Some rhs -> Available_space.Definite rhs
    | Available_space.Min_content, None -> Available_space.Min_content
    | Available_space.Max_content, Some rhs -> Available_space.Definite rhs
    | Available_space.Max_content, None -> Available_space.Max_content

  let maybe_max self rhs =
    match (self, rhs) with
    | Available_space.Definite value, Some rhs ->
        Available_space.Definite (Float.max value rhs)
    | Available_space.Definite value, None -> Available_space.Definite value
    | Available_space.Min_content, _ -> Available_space.Min_content
    | Available_space.Max_content, _ -> Available_space.Max_content

  let maybe_clamp self min max =
    match (self, min, max) with
    | Available_space.Definite value, Some min, Some max ->
        Available_space.Definite (Float.min value max |> Float.max min)
    | Available_space.Definite value, None, Some max ->
        Available_space.Definite (Float.min value max)
    | Available_space.Definite value, Some min, None ->
        Available_space.Definite (Float.max value min)
    | Available_space.Definite value, None, None ->
        Available_space.Definite value
    | Available_space.Min_content, _, _ -> Available_space.Min_content
    | Available_space.Max_content, _, _ -> Available_space.Max_content

  let maybe_add self rhs =
    match (self, rhs) with
    | Available_space.Definite value, Some rhs ->
        Available_space.Definite (value +. rhs)
    | Available_space.Definite value, None -> Available_space.Definite value
    | Available_space.Min_content, _ -> Available_space.Min_content
    | Available_space.Max_content, _ -> Available_space.Max_content

  let maybe_sub self rhs =
    match (self, rhs) with
    | Available_space.Definite value, Some rhs ->
        Available_space.Definite (value -. rhs)
    | Available_space.Definite value, None -> Available_space.Definite value
    | Available_space.Min_content, _ -> Available_space.Min_content
    | Available_space.Max_content, _ -> Available_space.Max_content
end

(* Size operations for different math modules *)
let size_maybe_min_option_option self rhs =
  let open Geometry in
  {
    width = OptionOption.maybe_min self.width rhs.width;
    height = OptionOption.maybe_min self.height rhs.height;
  }

let size_maybe_max_option_option self rhs =
  let open Geometry in
  {
    width = OptionOption.maybe_max self.width rhs.width;
    height = OptionOption.maybe_max self.height rhs.height;
  }

let size_maybe_clamp_option_option self min max =
  let open Geometry in
  {
    width = OptionOption.maybe_clamp self.width min.width max.width;
    height = OptionOption.maybe_clamp self.height min.height max.height;
  }

let size_maybe_add_option_option self rhs =
  let open Geometry in
  {
    width = OptionOption.maybe_add self.width rhs.width;
    height = OptionOption.maybe_add self.height rhs.height;
  }

let size_maybe_sub_option_option self rhs =
  let open Geometry in
  {
    width = OptionOption.maybe_sub self.width rhs.width;
    height = OptionOption.maybe_sub self.height rhs.height;
  }

let size_maybe_min_option_float self rhs =
  let open Geometry in
  {
    width = OptionFloat.maybe_min self.width rhs.width;
    height = OptionFloat.maybe_min self.height rhs.height;
  }

let size_maybe_max_option_float self rhs =
  let open Geometry in
  {
    width = OptionFloat.maybe_max self.width rhs.width;
    height = OptionFloat.maybe_max self.height rhs.height;
  }

let size_maybe_clamp_option_float self min max =
  let open Geometry in
  {
    width = OptionFloat.maybe_clamp self.width min.width max.width;
    height = OptionFloat.maybe_clamp self.height min.height max.height;
  }

let size_maybe_add_option_float self rhs =
  let open Geometry in
  {
    width = OptionFloat.maybe_add self.width rhs.width;
    height = OptionFloat.maybe_add self.height rhs.height;
  }

let size_maybe_sub_option_float self rhs =
  let open Geometry in
  {
    width = OptionFloat.maybe_sub self.width rhs.width;
    height = OptionFloat.maybe_sub self.height rhs.height;
  }

let size_maybe_min_float_option self rhs =
  let open Geometry in
  {
    width = FloatOption.maybe_min self.width rhs.width;
    height = FloatOption.maybe_min self.height rhs.height;
  }

let size_maybe_max_float_option self rhs =
  let open Geometry in
  {
    width = FloatOption.maybe_max self.width rhs.width;
    height = FloatOption.maybe_max self.height rhs.height;
  }

let size_maybe_clamp_float_option self min max =
  let open Geometry in
  {
    width = FloatOption.maybe_clamp self.width min.width max.width;
    height = FloatOption.maybe_clamp self.height min.height max.height;
  }

let size_maybe_add_float_option self rhs =
  let open Geometry in
  {
    width = FloatOption.maybe_add self.width rhs.width;
    height = FloatOption.maybe_add self.height rhs.height;
  }

let size_maybe_sub_float_option self rhs =
  let open Geometry in
  {
    width = FloatOption.maybe_sub self.width rhs.width;
    height = FloatOption.maybe_sub self.height rhs.height;
  }

let size_maybe_min_available_space_float self rhs =
  let open Geometry in
  {
    width = AvailableSpaceFloat.maybe_min self.width rhs.width;
    height = AvailableSpaceFloat.maybe_min self.height rhs.height;
  }

let size_maybe_max_available_space_float self rhs =
  let open Geometry in
  {
    width = AvailableSpaceFloat.maybe_max self.width rhs.width;
    height = AvailableSpaceFloat.maybe_max self.height rhs.height;
  }

let size_maybe_clamp_available_space_float self min max =
  let open Geometry in
  {
    width = AvailableSpaceFloat.maybe_clamp self.width min.width max.width;
    height = AvailableSpaceFloat.maybe_clamp self.height min.height max.height;
  }

let size_maybe_add_available_space_float self rhs =
  let open Geometry in
  {
    width = AvailableSpaceFloat.maybe_add self.width rhs.width;
    height = AvailableSpaceFloat.maybe_add self.height rhs.height;
  }

let size_maybe_sub_available_space_float self rhs =
  let open Geometry in
  {
    width = AvailableSpaceFloat.maybe_sub self.width rhs.width;
    height = AvailableSpaceFloat.maybe_sub self.height rhs.height;
  }

let size_maybe_min_available_space_option self rhs =
  let open Geometry in
  {
    width = AvailableSpaceOption.maybe_min self.width rhs.width;
    height = AvailableSpaceOption.maybe_min self.height rhs.height;
  }

let size_maybe_max_available_space_option self rhs =
  let open Geometry in
  {
    width = AvailableSpaceOption.maybe_max self.width rhs.width;
    height = AvailableSpaceOption.maybe_max self.height rhs.height;
  }

let size_maybe_clamp_available_space_option self min max =
  let open Geometry in
  {
    width = AvailableSpaceOption.maybe_clamp self.width min.width max.width;
    height = AvailableSpaceOption.maybe_clamp self.height min.height max.height;
  }

let size_maybe_add_available_space_option self rhs =
  let open Geometry in
  {
    width = AvailableSpaceOption.maybe_add self.width rhs.width;
    height = AvailableSpaceOption.maybe_add self.height rhs.height;
  }

let size_maybe_sub_available_space_option self rhs =
  let open Geometry in
  {
    width = AvailableSpaceOption.maybe_sub self.width rhs.width;
    height = AvailableSpaceOption.maybe_sub self.height rhs.height;
  }
