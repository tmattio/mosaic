%{
  open Ast

  let parse_time value unit =
    match unit with
    | "ms" -> value /. 1000.0
    | "s"  -> value
    | "m"  -> value *. 60.0
    | _    -> value
%}

%token <string> STRING JSON REGEX IDENT
%token <float> NUMBER
%token <bool> BOOL

%token OUTPUT SET SLEEP TYPE REQUIRE HIDE SHOW SOURCE SCREENSHOT COPY PASTE ENV WAIT
%token SHELL FONT_FAMILY FONT_SIZE FRAMERATE HEIGHT LETTER_SPACING LINE_HEIGHT
%token LOOP_OFFSET PADDING PLAYBACK_SPEED THEME TYPING_SPEED WIDTH WINDOW_BAR WINDOW_BAR_SIZE
%token BORDER_RADIUS MARGIN MARGIN_FILL WAIT_TIMEOUT WAIT_PATTERN CURSOR_BLINK
%token BACKSPACE DELETE DOWN ENTER ESCAPE INSERT LEFT PAGEDOWN PAGEUP RIGHT SPACE TAB UP HOME END
%token CTRL ALT SHIFT
%token AT PLUS PERCENT
%token MS S M
%token EOF

%start <Ast.tape> tape
%type <Ast.command> command

%%

tape:
  | commands = list(command) EOF { commands }
;

command:
  | c = output_command       { c }
  | c = set_command          { c }
  | c = sleep_command        { c }
  | c = type_command         { c }
  | c = key_press_command    { c }
  | c = ctrl_command         { c }
  | c = alt_command          { c }
  | c = shift_command        { c }
  | HIDE                     { Hide }
  | SHOW                     { Show }
  | c = require_command      { c }
  | c = screenshot_command   { c }
  | c = copy_command         { c }
  | PASTE                    { Paste }
  | c = env_command          { c }
  | c = source_command       { c }
  | c = wait_command         { c }
;

output_command:
  | OUTPUT path=any_string { Output path }
;

set_command:
  (* Special case for LoopOffset with optional % *)
  (* CORRECTION: Removed the `_=` binding. Just match the optional token directly. *)
  | SET LOOP_OFFSET v=NUMBER optional(PERCENT) { Set (LoopOffset, Float v) }
  (* Generic rule for all other settings *)
  | SET k=setting v=value { Set (k, v) }
;

sleep_command:
  | SLEEP t=time { Sleep t }
;

type_command:
  | TYPE spd=optional(speed) txt=STRING { Type { text=txt; speed=spd } }
;

key_press_command:
  | k=key spd=optional(speed) cnt=optional(int_val) {
      let count_val = match cnt with Some c -> c | None -> 1 in
      KeyPress { key=k; count=count_val; speed=spd }
    }
;

ctrl_command:
  | CTRL keys=ctrl_keys { Ctrl keys }
;

ctrl_keys:
  | PLUS k=ctrl_arg rest=ctrl_keys { k :: rest }
  | PLUS k=ctrl_arg { [k] }
  | k=ctrl_arg { [k] }
;

alt_command:
  | ALT PLUS k=any_string { Alt k }
;

shift_command:
  | SHIFT PLUS k=any_string { Shift k }
;

require_command:
  | REQUIRE p=any_string { Require p }
;

screenshot_command:
  | SCREENSHOT p=any_string { Screenshot p }
;

copy_command:
  | COPY s=STRING { Copy s }
;

env_command:
  | ENV k=any_string v=STRING { Env (k, v) }
;

source_command:
  | SOURCE p=any_string { Source p }
;

wait_command:
  | WAIT
    tgt=optional(wait_target)
    t=optional(speed)
    pat=optional(wait_pattern)
    {
      let target_str = match tgt with Some s -> s | None -> "Line" in
      Wait { target=target_str; timeout=t; pattern=pat }
    }
;

wait_target:
 | PLUS s=IDENT { s }
;
wait_pattern:
 | r=REGEX { r }
;

%public key:
  | BACKSPACE { Backspace } | DELETE { Delete } | DOWN { Down } | ENTER { Enter }
  | ESCAPE { Escape } | INSERT { Insert } | LEFT { Left } | PAGEDOWN { PageDown }
  | PAGEUP { PageUp } | RIGHT { Right } | SPACE { Space } | TAB { Tab }
  | UP { Up } | HOME { Home } | END { End }
;

%public setting:
  | SHELL { Shell } | FONT_FAMILY { FontFamily } | FONT_SIZE { FontSize }
  | FRAMERATE { Framerate } | HEIGHT { Height } | LETTER_SPACING { LetterSpacing }
  | LINE_HEIGHT { LineHeight } | PADDING { Padding } | PLAYBACK_SPEED { PlaybackSpeed }
  | THEME { Theme } | TYPING_SPEED { TypingSpeed } | WIDTH { Width }
  | WINDOW_BAR { WindowBar } | WINDOW_BAR_SIZE { WindowBarSize } | BORDER_RADIUS { BorderRadius } | MARGIN { Margin }
  | MARGIN_FILL { MarginFill } | WAIT_TIMEOUT { WaitTimeout } | WAIT_PATTERN { WaitPattern }
  | CURSOR_BLINK { CursorBlink }
;

%public value:
  | s=any_string { String s }
  | n=NUMBER { Float n }
  | j=JSON { Json j }
  | b=BOOL { Bool b }
;

int_val:
  | n=NUMBER { int_of_float n }
;

optional(X):
  | (* empty *) { None }
  | x=X         { Some x }
;

time:
  | v=NUMBER u=optional(time_unit) {
      match u with
      | Some unit -> parse_time v unit
      | None -> v
    }
;

time_unit:
  | MS { "ms" } | S { "s" } | M { "m" }
;

speed:
  | AT t=time { t }
;

any_string:
 | s=STRING { s }
 | i=IDENT { i }
;

ctrl_arg:
 | s=any_string { s }
 | ALT { "Alt" }
 | SHIFT { "Shift" }
