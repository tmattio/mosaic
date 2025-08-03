type key =
  | Backspace
  | Delete
  | Down
  | Enter
  | Escape
  | Insert
  | Left
  | PageDown
  | PageUp
  | Right
  | Space
  | Tab
  | Up
  | Home
  | End

type setting =
  | Shell
  | FontFamily
  | FontSize
  | Framerate
  | Height
  | LetterSpacing
  | LineHeight
  | LoopOffset
  | Padding
  | PlaybackSpeed
  | Theme
  | TypingSpeed
  | Width
  | WindowBar
  | WindowBarSize
  | BorderRadius
  | Margin
  | MarginFill
  | WaitTimeout
  | WaitPattern
  | CursorBlink

type value =
  | String of string
  | Int of int
  | Float of float
  | Json of string
  | Bool of bool

type command =
  | Output of string
  | Set of setting * value
  | Sleep of float
  | Type of { text : string; speed : float option }
  | KeyPress of { key : key; count : int; speed : float option }
  | Ctrl of string list
  | Alt of string
  | Shift of string
  | Hide
  | Show
  | Require of string
  | Screenshot of string
  | Copy of string
  | Paste
  | Env of string * string
  | Source of string
  | Wait of { target : string; timeout : float option; pattern : string option }

type tape = command list

(* Pretty printers *)
let pp_key fmt = function
  | Backspace -> Format.pp_print_string fmt "Backspace"
  | Delete -> Format.pp_print_string fmt "Delete"
  | Down -> Format.pp_print_string fmt "Down"
  | Enter -> Format.pp_print_string fmt "Enter"
  | Escape -> Format.pp_print_string fmt "Escape"
  | Insert -> Format.pp_print_string fmt "Insert"
  | Left -> Format.pp_print_string fmt "Left"
  | PageDown -> Format.pp_print_string fmt "PageDown"
  | PageUp -> Format.pp_print_string fmt "PageUp"
  | Right -> Format.pp_print_string fmt "Right"
  | Space -> Format.pp_print_string fmt "Space"
  | Tab -> Format.pp_print_string fmt "Tab"
  | Up -> Format.pp_print_string fmt "Up"
  | Home -> Format.pp_print_string fmt "Home"
  | End -> Format.pp_print_string fmt "End"

let pp_setting fmt = function
  | Shell -> Format.pp_print_string fmt "Shell"
  | FontFamily -> Format.pp_print_string fmt "FontFamily"
  | FontSize -> Format.pp_print_string fmt "FontSize"
  | Framerate -> Format.pp_print_string fmt "Framerate"
  | Height -> Format.pp_print_string fmt "Height"
  | LetterSpacing -> Format.pp_print_string fmt "LetterSpacing"
  | LineHeight -> Format.pp_print_string fmt "LineHeight"
  | LoopOffset -> Format.pp_print_string fmt "LoopOffset"
  | Padding -> Format.pp_print_string fmt "Padding"
  | PlaybackSpeed -> Format.pp_print_string fmt "PlaybackSpeed"
  | Theme -> Format.pp_print_string fmt "Theme"
  | TypingSpeed -> Format.pp_print_string fmt "TypingSpeed"
  | Width -> Format.pp_print_string fmt "Width"
  | WindowBar -> Format.pp_print_string fmt "WindowBar"
  | WindowBarSize -> Format.pp_print_string fmt "WindowBarSize"
  | BorderRadius -> Format.pp_print_string fmt "BorderRadius"
  | Margin -> Format.pp_print_string fmt "Margin"
  | MarginFill -> Format.pp_print_string fmt "MarginFill"
  | WaitTimeout -> Format.pp_print_string fmt "WaitTimeout"
  | WaitPattern -> Format.pp_print_string fmt "WaitPattern"
  | CursorBlink -> Format.pp_print_string fmt "CursorBlink"

let pp_value fmt = function
  | String s -> Format.fprintf fmt "String(%S)" s
  | Int i -> Format.fprintf fmt "Int(%d)" i
  | Float f -> Format.fprintf fmt "Float(%g)" f
  | Json j -> Format.fprintf fmt "Json(%s)" j
  | Bool b -> Format.fprintf fmt "Bool(%b)" b

let pp_command fmt = function
  | Output s -> Format.fprintf fmt "Output(%S)" s
  | Set (setting, value) ->
      Format.fprintf fmt "Set(%a, %a)" pp_setting setting pp_value value
  | Sleep f -> Format.fprintf fmt "Sleep(%g)" f
  | Type { text; speed } ->
      Format.fprintf fmt "Type{text=%S; speed=%a}" text
        (Format.pp_print_option Format.pp_print_float)
        speed
  | KeyPress { key; count; speed } ->
      Format.fprintf fmt "KeyPress{key=%a; count=%d; speed=%a}" pp_key key count
        (Format.pp_print_option Format.pp_print_float)
        speed
  | Ctrl keys -> Format.fprintf fmt "Ctrl[%s]" (String.concat "; " keys)
  | Alt s -> Format.fprintf fmt "Alt(%S)" s
  | Shift s -> Format.fprintf fmt "Shift(%S)" s
  | Hide -> Format.pp_print_string fmt "Hide"
  | Show -> Format.pp_print_string fmt "Show"
  | Require s -> Format.fprintf fmt "Require(%S)" s
  | Screenshot s -> Format.fprintf fmt "Screenshot(%S)" s
  | Copy s -> Format.fprintf fmt "Copy(%S)" s
  | Paste -> Format.pp_print_string fmt "Paste"
  | Env (k, v) -> Format.fprintf fmt "Env(%S, %S)" k v
  | Source s -> Format.fprintf fmt "Source(%S)" s
  | Wait { target; timeout; pattern } ->
      Format.fprintf fmt "Wait{target=%S; timeout=%a; pattern=%a}" target
        (Format.pp_print_option Format.pp_print_float)
        timeout
        (Format.pp_print_option Format.pp_print_string)
        pattern

let pp_tape = Format.pp_print_list ~pp_sep:Format.pp_print_newline pp_command

let setting_to_string = function
  | Shell -> "Shell"
  | FontFamily -> "FontFamily"
  | FontSize -> "FontSize"
  | Framerate -> "Framerate"
  | Height -> "Height"
  | LetterSpacing -> "LetterSpacing"
  | LineHeight -> "LineHeight"
  | LoopOffset -> "LoopOffset"
  | Padding -> "Padding"
  | PlaybackSpeed -> "PlaybackSpeed"
  | Theme -> "Theme"
  | TypingSpeed -> "TypingSpeed"
  | Width -> "Width"
  | WindowBar -> "WindowBar"
  | WindowBarSize -> "WindowBarSize"
  | BorderRadius -> "BorderRadius"
  | Margin -> "Margin"
  | MarginFill -> "MarginFill"
  | WaitTimeout -> "WaitTimeout"
  | WaitPattern -> "WaitPattern"
  | CursorBlink -> "CursorBlink"
