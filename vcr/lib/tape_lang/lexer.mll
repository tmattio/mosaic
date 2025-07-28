{
  open Parser
  exception SyntaxError of string

  let unquote s =
    let len = String.length s in
    if len < 2 then s
    else String.sub s 1 (len - 2)
}

let digit = ['0'-'9']
let number = digit+ ('.' digit*)? | '.' digit+
let time_unit = "ms" | "s" | "m"
let identifier = ['a'-'z' 'A'-'Z' '.'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-' '.' '/']*

rule token = parse
  | [' ' '\t']+     { token lexbuf }
  | '\n'+           { Lexing.new_line lexbuf; token lexbuf }
  | eof             { EOF }

  (* Comments *)
  | '#' [^ '\n']*   { token lexbuf }

  (* Keywords *)
  | "Output"        { OUTPUT }
  | "Set"           { SET }
  | "Sleep"         { SLEEP }
  | "Type"          { TYPE }
  | "Require"       { REQUIRE }
  | "Hide"          { HIDE }
  | "Show"          { SHOW }
  | "Source"        { SOURCE }
  | "Screenshot"    { SCREENSHOT }
  | "Copy"          { COPY }
  | "Paste"         { PASTE }
  | "Env"           { ENV }
  | "Wait"          { WAIT }

  (* Settings *)
  | "Shell"         { SHELL }
  | "FontFamily"    { FONT_FAMILY }
  | "FontSize"      { FONT_SIZE }
  | "Framerate"     { FRAMERATE }
  | "Height"        { HEIGHT }
  | "LetterSpacing" { LETTER_SPACING }
  | "LineHeight"    { LINE_HEIGHT }
  | "LoopOffset"    { LOOP_OFFSET }
  | "Padding"       { PADDING }
  | "PlaybackSpeed" { PLAYBACK_SPEED }
  | "Theme"         { THEME }
  | "TypingSpeed"   { TYPING_SPEED }
  | "Width"         { WIDTH }
  | "WindowBar"     { WINDOW_BAR }
  | "WindowBarSize" { WINDOW_BAR_SIZE }
  | "BorderRadius"  { BORDER_RADIUS }
  | "Margin"        { MARGIN }
  | "MarginFill"    { MARGIN_FILL }
  | "WaitTimeout"   { WAIT_TIMEOUT }
  | "WaitPattern"   { WAIT_PATTERN }
  | "CursorBlink"   { CURSOR_BLINK }

  (* Keys *)
  | "Backspace"     { BACKSPACE }
  | "Delete"        { DELETE }
  | "Down"          { DOWN }
  | "Enter"         { ENTER }
  | "Escape"        { ESCAPE }
  | "Insert"        { INSERT }
  | "Left"          { LEFT }
  | "PageDown"      { PAGEDOWN }
  | "PageUp"        { PAGEUP }
  | "Right"         { RIGHT }
  | "Space"         { SPACE }
  | "Tab"           { TAB }
  | "Up"            { UP }
  | "Home"          { HOME }
  | "End"           { END }

  (* Control characters *)
  | "Ctrl"          { CTRL }
  | "Alt"           { ALT } (* Not used in VHS, but good to have *)
  | "Shift"         { SHIFT } (* Not used in VHS, but good to have *)
  | "true"          { BOOL true }
  | "false"         { BOOL false }

  (* Literals *)
  | '"' ([^ '"'] | "\\\"")* '"' as lxm { STRING (unquote lxm) }
  | '\'' ([^ '\''] | "\\\'")* '\'' as lxm { STRING (unquote lxm) }
  | '`' ([^ '`'] | "\\`")* '`' as lxm { STRING (unquote lxm) }
  | '{' [^ '}']* '}' as lxm { JSON lxm }
  | '/' [^ '/']* '/' as lxm { REGEX (unquote lxm) }
  | number as lxm   { NUMBER (float_of_string lxm) }

  (* Symbols *)
  | '@'             { AT }
  | '+'             { PLUS }
  | '%'             { PERCENT }

  (* Time Units *)
  | "ms"            { MS }
  | "s"             { S }
  | "m"             { M }

  (* General identifiers (for program names, etc.) *)
  | identifier as s { IDENT s }

  (* Fallback for unrecognized characters *)
  | _ as char       { raise (SyntaxError (Printf.sprintf "Unexpected character: %c" char)) }