(* Spinners are from cli-spinners:
   MIT License
   Copyright (c) Sindre Sorhus <sindresorhus@gmail.com> (sindresorhus.com)
   
   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights to
   use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
   the Software, and to permit persons to whom the Software is furnished to do so,
   subject to the following conditions:
   
   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.
   
   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
   INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
   PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
   FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
   ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
   IN THE SOFTWARE. *)

type spinner_def = { frames : string list; interval : int (* milliseconds *) }

type spinner_kind =
  (* Braille patterns *)
  | Braille_dots  (** Classic rotating braille dots: ⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏ *)
  | Braille_dots2  (** Alternative braille dots: ⣾⣽⣻⢿⡿⣟⣯⣷ *)
  | Braille_dots3  (** Third braille variation: ⠋⠙⠚⠞⠖⠦⠴⠲⠳⠓ *)
  | Braille_dots4  (** Bouncing braille dots *)
  | Braille_dots5  (** Complex braille cycle *)
  | Braille_dots6  (** Extended braille animation *)
  | Braille_dots7  (** Another braille pattern *)
  | Braille_dots8  (** Long braille sequence *)
  | Braille_dots9  (** Vertical braille: ⢹⢺⢼⣸⣇⡧⡗⡏ *)
  | Braille_dots10  (** Small braille set: ⢄⢂⢁⡁⡈⡐⡠ *)
  | Braille_dots11  (** Simple braille cycle *)
  | Braille_dots12  (** Two-column braille animation *)
  | Braille_dots13  (** Dense braille: ⣼⣹⢻⠿⡟⣏⣧⣶ *)
  | Braille_dots14  (** Two-character braille patterns *)
  | Braille_8bit  (** Full 8-bit braille character set *)
  | Braille_circle  (** Circular braille motion *)
  | Braille_sand  (** Sand falling effect with braille *)
  (* Line and ASCII *)
  | Line_spin  (** Simple line rotation: - \ | / *)
  | Line_pulse  (** Pulsing line: ⠂-–—–- *)
  | Pipe_spin  (** Box drawing rotation: ┤┘┴└├┌┬┐ *)
  | Ascii_dots  (** Simple ASCII dots: . .. ... *)
  | Ascii_dots_scroll  (** Scrolling ASCII dots *)
  | Ascii_star  (** ASCII star rotation: + x * *)
  | Ascii_flip  (** Flipping characters: _ - ` ' ´ *)
  | Ascii_hamburger  (** Hamburger menu: ☱☲☴ *)
  | Ascii_binary  (** Binary numbers animation *)
  | Ascii_dqpb  (** Letters d q p b rotation *)
  (* Bars and blocks *)
  | Bar_vertical_grow  (** Growing vertical bar: ▁▃▄▅▆▇ *)
  | Bar_horizontal_grow  (** Growing horizontal bar: ▏▎▍▌▋▊▉ *)
  | Bar_bounce  (** Bouncing bar: [=   ] [==  ] *)
  | Block_bounce  (** Bouncing block corners: ▖▘▝▗ *)
  | Block_wave  (** Wave with blocks: ▌▀▐▄ *)
  | Block_square  (** Square toggle: □■ *)
  | Block_squish  (** Squishing blocks: ╫╪ *)
  (* Geometric shapes *)
  | Triangle_spin  (** Rotating triangle: ◢◣◤◥ *)
  | Square_corners  (** Rotating square corners: ◰◳◲◱ *)
  | Circle_quarters  (** Circle quarters: ◴◷◶◵ *)
  | Circle_halves  (** Circle halves: ◐◓◑◒ *)
  | Circle_simple  (** Simple circle: ◡⊙◠ *)
  | Arc_spin  (** Arc rotation: ◜◠◝◞◡◟ *)
  (* Progress indicators *)
  | Progress_bar  (** Material design progress bar *)
  | Progress_balloon  (** Balloon expansion: . o O @ * *)
  | Progress_balloon2  (** Alternative balloon: . o O ° *)
  | Progress_layer  (** Layered progress: - = ≡ *)
  | Progress_point  (** Moving point: ∙∙∙ ●∙∙ ∙●∙ *)
  | Progress_beta_wave  (** Beta wave: ρββββββ *)
  (* Animations *)
  | Anim_pong  (** Pong game animation *)
  | Anim_shark  (** Swimming shark *)
  | Anim_grenade  (** Exploding grenade *)
  | Anim_ball_bounce  (** Bouncing ball in parentheses *)
  | Anim_aesthetic  (** Aesthetic wave: ▰▱▱▱▱▱▱ *)
  | Anim_dwarf_fortress  (** Dwarf Fortress style animation *)
  (* Noise and effects *)
  | Noise_fade  (** Fading noise: ▓▒░ *)
  | Effect_dots_bounce  (** Simple bouncing dots: ⠁⠂⠄⠂ *)
  (* Toggle animations *)
  | Toggle_box  (** Box toggle: ▫▪ *)
  | Toggle_box2  (** Alternative box: ⊶⊷ *)
  | Toggle_square  (** Square toggle: □■ *)
  | Toggle_square2  (** Multi-state square: ■□▪▫ *)
  | Toggle_square3  (** Bold square: ▮▯ *)
  | Toggle_circle  (** Circle toggle: ဝ၀ *)
  | Toggle_circle2  (** Filled circles: ⦾⦿ *)
  | Toggle_circle3  (** Dotted circles: ◍◌ *)
  | Toggle_circle4  (** Bold circles: ◉◎ *)
  | Toggle_circle5  (** Numbered circles: ㊂㊀㊁ *)
  | Toggle_diamond  (** Diamond toggle: ⧇⧆ *)
  | Toggle_shogi  (** Shogi pieces: ☗☖ *)
  | Toggle_equals  (** Equals toggle: = * - *)
  (* Arrows *)
  | Arrow_rotate  (** Rotating arrows: ←↖↑↗→↘↓↙ *)
  | Arrow_rotate2  (** Emoji arrows: ⬆️↗️➡️↘️⬇️↙️⬅️↖️ *)
  | Arrow_progress  (** Arrow progress: ▹▹▹▹▹ ▸▹▹▹▹ *)
  (* Unicode and emoji *)
  | Unicode_star_pulse  (** Star pulse: ✶✸✹✺✹✷ *)
  | Unicode_moon_phases  (** Moon phases: 🌑🌒🌓🌔🌕🌖🌗🌘 *)
  | Unicode_earth_rotate  (** Earth rotation: 🌍🌎🌏 *)
  | Unicode_clock  (** Clock faces: 🕛🕐🕑... *)
  | Unicode_weather  (** Weather cycle: ☀️🌤⛅️🌥☁️🌧 *)
  (* Emoji animations *)
  | Emoji_hearts  (** Colored hearts: 💛💙💜💚❤️ *)
  | Emoji_monkey  (** See/hear/speak no evil: 🙈🙉🙊 *)
  | Emoji_faces  (** Face expressions: 😄😝 *)
  | Emoji_runner  (** Running figure: 🚶🏃 *)
  | Emoji_christmas  (** Christmas trees: 🌲🎄 *)
  | Emoji_finger_dance  (** Hand gestures: 🤘🤟🖖✋🤚👆 *)
  | Emoji_fist_bump  (** Fist bump animation *)
  | Emoji_soccer  (** Soccer header animation *)
  | Emoji_mindblown  (** Mind blown sequence *)
  | Emoji_speaker  (** Speaker volume: 🔈🔉🔊 *)
  (* Pulse animations *)
  | Pulse_orange  (** Orange pulse: 🔸🔶🟠 *)
  | Pulse_blue  (** Blue pulse: 🔹🔷🔵 *)
  | Pulse_orange_blue  (** Combined orange/blue pulse *)
  (* Special *)
  | Time_travel  (** Clock time travel effect *)
  (* Custom *)
  | Custom of { frames : string list; interval : int }
      (** Custom animation frames *)

let spinners = function
  | Braille_dots ->
      {
        frames = [ "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" ];
        interval = 80;
      }
  | Braille_dots2 ->
      { frames = [ "⣾"; "⣽"; "⣻"; "⢿"; "⡿"; "⣟"; "⣯"; "⣷" ]; interval = 80 }
  | Braille_dots3 ->
      {
        frames = [ "⠋"; "⠙"; "⠚"; "⠞"; "⠖"; "⠦"; "⠴"; "⠲"; "⠳"; "⠓" ];
        interval = 80;
      }
  | Braille_dots4 ->
      {
        frames =
          [
            "⠄"; "⠆"; "⠇"; "⠋"; "⠙"; "⠸"; "⠰"; "⠠"; "⠰"; "⠸"; "⠙"; "⠋"; "⠇"; "⠆";
          ];
        interval = 80;
      }
  | Braille_dots5 ->
      {
        frames =
          [
            "⠋";
            "⠙";
            "⠚";
            "⠒";
            "⠂";
            "⠂";
            "⠒";
            "⠲";
            "⠴";
            "⠦";
            "⠖";
            "⠒";
            "⠐";
            "⠐";
            "⠒";
            "⠓";
            "⠋";
          ];
        interval = 80;
      }
  | Braille_dots6 ->
      {
        frames =
          [
            "⠁";
            "⠉";
            "⠙";
            "⠚";
            "⠒";
            "⠂";
            "⠂";
            "⠒";
            "⠲";
            "⠴";
            "⠤";
            "⠄";
            "⠄";
            "⠤";
            "⠴";
            "⠲";
            "⠒";
            "⠂";
            "⠂";
            "⠒";
            "⠚";
            "⠙";
            "⠉";
            "⠁";
          ];
        interval = 80;
      }
  | Braille_dots7 ->
      {
        frames =
          [
            "⠈";
            "⠉";
            "⠋";
            "⠓";
            "⠒";
            "⠐";
            "⠐";
            "⠒";
            "⠖";
            "⠦";
            "⠤";
            "⠠";
            "⠠";
            "⠤";
            "⠦";
            "⠖";
            "⠒";
            "⠐";
            "⠐";
            "⠒";
            "⠓";
            "⠋";
            "⠉";
            "⠈";
          ];
        interval = 80;
      }
  | Braille_dots8 ->
      {
        frames =
          [
            "⠁";
            "⠁";
            "⠉";
            "⠙";
            "⠚";
            "⠒";
            "⠂";
            "⠂";
            "⠒";
            "⠲";
            "⠴";
            "⠤";
            "⠄";
            "⠄";
            "⠤";
            "⠠";
            "⠠";
            "⠤";
            "⠦";
            "⠖";
            "⠒";
            "⠐";
            "⠐";
            "⠒";
            "⠓";
            "⠋";
            "⠉";
            "⠈";
            "⠈";
          ];
        interval = 80;
      }
  | Braille_dots9 ->
      { frames = [ "⢹"; "⢺"; "⢼"; "⣸"; "⣇"; "⡧"; "⡗"; "⡏" ]; interval = 80 }
  | Braille_dots10 ->
      { frames = [ "⢄"; "⢂"; "⢁"; "⡁"; "⡈"; "⡐"; "⡠" ]; interval = 80 }
  | Braille_dots11 ->
      { frames = [ "⠁"; "⠂"; "⠄"; "⡀"; "⢀"; "⠠"; "⠐"; "⠈" ]; interval = 100 }
  | Braille_dots12 ->
      {
        frames =
          [
            "⢀⠀";
            "⡀⠀";
            "⠄⠀";
            "⢂⠀";
            "⡂⠀";
            "⠅⠀";
            "⢃⠀";
            "⡃⠀";
            "⠍⠀";
            "⢋⠀";
            "⡋⠀";
            "⠍⠁";
            "⢋⠁";
            "⡋⠁";
            "⠍⠉";
            "⠋⠉";
            "⠋⠉";
            "⠉⠙";
            "⠉⠙";
            "⠉⠩";
            "⠈⢙";
            "⠈⡙";
            "⢈⠩";
            "⡀⢙";
            "⠄⡙";
            "⢂⠩";
            "⡂⢘";
            "⠅⡘";
            "⢃⠨";
            "⡃⢐";
            "⠍⡐";
            "⢋⠠";
            "⡋⢀";
            "⠍⡁";
            "⢋⠁";
            "⡋⠁";
            "⠍⠉";
            "⠋⠉";
            "⠋⠉";
            "⠉⠙";
            "⠉⠙";
            "⠉⠩";
            "⠈⢙";
            "⠈⡙";
            "⠈⠩";
            "⠀⢙";
            "⠀⡙";
            "⠀⠩";
            "⠀⢘";
            "⠀⡘";
            "⠀⠨";
            "⠀⢐";
            "⠀⡐";
            "⠀⠠";
            "⠀⢀";
            "⠀⡀";
          ];
        interval = 80;
      }
  | Braille_dots13 ->
      { frames = [ "⣼"; "⣹"; "⢻"; "⠿"; "⡟"; "⣏"; "⣧"; "⣶" ]; interval = 80 }
  | Braille_dots14 ->
      {
        frames =
          [
            "⠉⠉";
            "⠈⠙";
            "⠀⠹";
            "⠀⢸";
            "⠀⣰";
            "⢀⣠";
            "⣀⣀";
            "⣄⡀";
            "⣆⠀";
            "⡇⠀";
            "⠏⠀";
            "⠋⠁";
          ];
        interval = 80;
      }
  | Braille_8bit ->
      {
        frames =
          String.split_on_char ' '
            "⠀ ⠁ ⠂ ⠃ ⠄ ⠅ ⠆ ⠇ ⡀ ⡁ ⡂ ⡃ ⡄ ⡅ ⡆ ⡇ ⠈ ⠉ ⠊ ⠋ ⠌ ⠍ ⠎ ⠏ ⡈ ⡉ ⡊ ⡋ ⡌ ⡍ ⡎ ⡏ ⠐ \
             ⠑ ⠒ ⠓ ⠔ ⠕ ⠖ ⠗ ⡐ ⡑ ⡒ ⡓ ⡔ ⡕ ⡖ ⡗ ⠘ ⠙ ⠚ ⠛ ⠜ ⠝ ⠞ ⠟ ⡘ ⡙ ⡚ ⡛ ⡜ ⡝ ⡞ ⡟ ⠠ ⠡ \
             ⠢ ⠣ ⠤ ⠥ ⠦ ⠧ ⡠ ⡡ ⡢ ⡣ ⡤ ⡥ ⡦ ⡧ ⠨ ⠩ ⠪ ⠫ ⠬ ⠭ ⠮ ⠯ ⡨ ⡩ ⡪ ⡫ ⡬ ⡭ ⡮ ⡯ ⠰ ⠱ ⠲ \
             ⠳ ⠴ ⠵ ⠶ ⠷ ⡰ ⡱ ⡲ ⡳ ⡴ ⡵ ⡶ ⡷ ⠸ ⠹ ⠺ ⠻ ⠼ ⠽ ⠾ ⠿ ⡸ ⡹ ⡺ ⡻ ⡼ ⡽ ⡾ ⡿ ⢀ ⢁ ⢂ ⢃ \
             ⢄ ⢅ ⢆ ⢇ ⣀ ⣁ ⣂ ⣃ ⣄ ⣅ ⣆ ⣇ ⢈ ⢉ ⢊ ⢋ ⢌ ⢍ ⢎ ⢏ ⣈ ⣉ ⣊ ⣋ ⣌ ⣍ ⣎ ⣏ ⢐ ⢑ ⢒ ⢓ ⢔ \
             ⢕ ⢖ ⢗ ⣐ ⣑ ⣒ ⣓ ⣔ ⣕ ⣖ ⣗ ⢘ ⢙ ⢚ ⢛ ⢜ ⢝ ⢞ ⢟ ⣘ ⣙ ⣚ ⣛ ⣜ ⣝ ⣞ ⣟ ⢠ ⢡ ⢢ ⢣ ⢤ ⢥ \
             ⢦ ⢧ ⣠ ⣡ ⣢ ⣣ ⣤ ⣥ ⣦ ⣧ ⢨ ⢩ ⢪ ⢫ ⢬ ⢭ ⢮ ⢯ ⣨ ⣩ ⣪ ⣫ ⣬ ⣭ ⣮ ⣯ ⢰ ⢱ ⢲ ⢳ ⢴ ⢵ ⢶ \
             ⢷ ⣰ ⣱ ⣲ ⣳ ⣴ ⣵ ⣶ ⣷ ⢸ ⢹ ⢺ ⢻ ⢼ ⢽ ⢾ ⢿ ⣸ ⣹ ⣺ ⣻ ⣼ ⣽ ⣾ ⣿";
        interval = 80;
      }
  | Braille_circle ->
      {
        frames = [ "⢎ "; "⠎⠁"; "⠊⠑"; "⠈⠱"; " ⡱"; "⢀⡰"; "⢄⡠"; "⢆⡀" ];
        interval = 80;
      }
  | Braille_sand ->
      {
        frames =
          [
            "⠁";
            "⠂";
            "⠄";
            "⡀";
            "⡈";
            "⡐";
            "⡠";
            "⣀";
            "⣁";
            "⣂";
            "⣄";
            "⣌";
            "⣔";
            "⣤";
            "⣥";
            "⣦";
            "⣮";
            "⣶";
            "⣷";
            "⣿";
            "⡿";
            "⠿";
            "⢟";
            "⠟";
            "⡛";
            "⠛";
            "⠫";
            "⢋";
            "⠋";
            "⠍";
            "⡉";
            "⠉";
            "⠑";
            "⠡";
            "⢁";
          ];
        interval = 80;
      }
  | Line_spin -> { frames = [ "-"; "\\"; "|"; "/" ]; interval = 130 }
  | Line_pulse -> { frames = [ "⠂"; "-"; "–"; "—"; "–"; "-" ]; interval = 100 }
  | Pipe_spin ->
      { frames = [ "┤"; "┘"; "┴"; "└"; "├"; "┌"; "┬"; "┐" ]; interval = 100 }
  | Ascii_dots -> { frames = [ ".  "; ".. "; "..."; "   " ]; interval = 400 }
  | Ascii_dots_scroll ->
      { frames = [ ".  "; ".. "; "..."; " .."; "  ."; "   " ]; interval = 200 }
  | Unicode_star_pulse ->
      { frames = [ "✶"; "✸"; "✹"; "✺"; "✹"; "✷" ]; interval = 70 }
  | Ascii_star -> { frames = [ "+"; "x"; "*" ]; interval = 80 }
  | Ascii_flip ->
      {
        frames = [ "_"; "_"; "_"; "-"; "`"; "`"; "'"; "´"; "-"; "_"; "_"; "_" ];
        interval = 70;
      }
  | Ascii_hamburger -> { frames = [ "☱"; "☲"; "☴" ]; interval = 100 }
  | Bar_vertical_grow ->
      {
        frames = [ "▁"; "▃"; "▄"; "▅"; "▆"; "▇"; "▆"; "▅"; "▄"; "▃" ];
        interval = 120;
      }
  | Bar_horizontal_grow ->
      {
        frames = [ "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉"; "▊"; "▋"; "▌"; "▍"; "▎" ];
        interval = 120;
      }
  | Progress_balloon ->
      { frames = [ " "; "."; "o"; "O"; "@"; "*"; " " ]; interval = 140 }
  | Progress_balloon2 ->
      { frames = [ "."; "o"; "O"; "°"; "O"; "o"; "." ]; interval = 120 }
  | Noise_fade -> { frames = [ "▓"; "▒"; "░" ]; interval = 100 }
  | Effect_dots_bounce -> { frames = [ "⠁"; "⠂"; "⠄"; "⠂" ]; interval = 120 }
  | Block_bounce -> { frames = [ "▖"; "▘"; "▝"; "▗" ]; interval = 120 }
  | Block_wave -> { frames = [ "▌"; "▀"; "▐"; "▄" ]; interval = 100 }
  | Triangle_spin -> { frames = [ "◢"; "◣"; "◤"; "◥" ]; interval = 50 }
  | Ascii_binary ->
      {
        frames =
          [
            "010010";
            "001100";
            "100101";
            "111010";
            "111101";
            "010111";
            "101011";
            "111000";
            "110011";
            "110101";
          ];
        interval = 80;
      }
  | Arc_spin -> { frames = [ "◜"; "◠"; "◝"; "◞"; "◡"; "◟" ]; interval = 100 }
  | Circle_simple -> { frames = [ "◡"; "⊙"; "◠" ]; interval = 120 }
  | Square_corners -> { frames = [ "◰"; "◳"; "◲"; "◱" ]; interval = 180 }
  | Circle_quarters -> { frames = [ "◴"; "◷"; "◶"; "◵" ]; interval = 120 }
  | Circle_halves -> { frames = [ "◐"; "◓"; "◑"; "◒" ]; interval = 50 }
  | Block_squish -> { frames = [ "╫"; "╪" ]; interval = 100 }
  | Block_square -> { frames = [ "□"; "■" ]; interval = 120 }
  | Toggle_box2 -> { frames = [ "⊶"; "⊷" ]; interval = 250 }
  | Toggle_box -> { frames = [ "▫"; "▪" ]; interval = 80 }
  | Toggle_square -> { frames = [ "□"; "■" ]; interval = 120 }
  | Toggle_square2 -> { frames = [ "■"; "□"; "▪"; "▫" ]; interval = 100 }
  | Toggle_square3 -> { frames = [ "▮"; "▯" ]; interval = 100 }
  | Toggle_circle -> { frames = [ "ဝ"; "၀" ]; interval = 300 }
  | Toggle_circle2 -> { frames = [ "⦾"; "⦿" ]; interval = 80 }
  | Toggle_circle3 -> { frames = [ "◍"; "◌" ]; interval = 100 }
  | Toggle_circle4 -> { frames = [ "◉"; "◎" ]; interval = 100 }
  | Toggle_circle5 -> { frames = [ "㊂"; "㊀"; "㊁" ]; interval = 100 }
  | Toggle_diamond -> { frames = [ "⧇"; "⧆" ]; interval = 50 }
  | Toggle_shogi -> { frames = [ "☗"; "☖" ]; interval = 120 }
  | Toggle_equals -> { frames = [ "="; "*"; "-" ]; interval = 80 }
  | Arrow_rotate ->
      { frames = [ "←"; "↖"; "↑"; "↗"; "→"; "↘"; "↓"; "↙" ]; interval = 100 }
  | Arrow_rotate2 ->
      {
        frames = [ "⬆️ "; "↗️ "; "➡️ "; "↘️ "; "⬇️ "; "↙️ "; "⬅️ "; "↖️ " ];
        interval = 80;
      }
  | Arrow_progress ->
      {
        frames = [ "▹▹▹▹▹"; "▸▹▹▹▹"; "▹▸▹▹▹"; "▹▹▸▹▹"; "▹▹▹▸▹"; "▹▹▹▹▸" ];
        interval = 120;
      }
  | Bar_bounce ->
      {
        frames =
          [
            "[    ]";
            "[=   ]";
            "[==  ]";
            "[=== ]";
            "[====]";
            "[ ===]";
            "[  ==]";
            "[   =]";
            "[    ]";
            "[   =]";
            "[  ==]";
            "[ ===]";
            "[====]";
            "[=== ]";
            "[==  ]";
            "[=   ]";
          ];
        interval = 80;
      }
  | Anim_ball_bounce ->
      {
        frames =
          [
            "( ●    )";
            "(  ●   )";
            "(   ●  )";
            "(    ● )";
            "(     ●)";
            "(    ● )";
            "(   ●  )";
            "(  ●   )";
            "( ●    )";
            "(●     )";
          ];
        interval = 80;
      }
  | Emoji_faces -> { frames = [ "😄 "; "😝 " ]; interval = 200 }
  | Emoji_monkey -> { frames = [ "🙈 "; "🙈 "; "🙉 "; "🙊 " ]; interval = 300 }
  | Emoji_hearts ->
      { frames = [ "💛 "; "💙 "; "💜 "; "💚 "; "❤️ " ]; interval = 100 }
  | Unicode_clock ->
      {
        frames =
          [
            "🕛 ";
            "🕐 ";
            "🕑 ";
            "🕒 ";
            "🕓 ";
            "🕔 ";
            "🕕 ";
            "🕖 ";
            "🕗 ";
            "🕘 ";
            "🕙 ";
            "🕚 ";
          ];
        interval = 100;
      }
  | Unicode_earth_rotate -> { frames = [ "🌍 "; "🌎 "; "🌏 " ]; interval = 180 }
  | Progress_bar ->
      {
        frames =
          [
            "█▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁";
            "██▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁";
            "███▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁";
            "████▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁";
            "██████▁▁▁▁▁▁▁▁▁▁▁▁▁▁";
            "██████▁▁▁▁▁▁▁▁▁▁▁▁▁▁";
            "███████▁▁▁▁▁▁▁▁▁▁▁▁▁";
            "████████▁▁▁▁▁▁▁▁▁▁▁▁";
            "█████████▁▁▁▁▁▁▁▁▁▁▁";
            "█████████▁▁▁▁▁▁▁▁▁▁▁";
            "██████████▁▁▁▁▁▁▁▁▁▁";
            "███████████▁▁▁▁▁▁▁▁▁";
            "█████████████▁▁▁▁▁▁▁";
            "██████████████▁▁▁▁▁▁";
            "██████████████▁▁▁▁▁▁";
            "▁██████████████▁▁▁▁▁";
            "▁██████████████▁▁▁▁▁";
            "▁██████████████▁▁▁▁▁";
            "▁▁██████████████▁▁▁▁";
            "▁▁▁██████████████▁▁▁";
            "▁▁▁▁█████████████▁▁▁";
            "▁▁▁▁██████████████▁▁";
            "▁▁▁▁██████████████▁▁";
            "▁▁▁▁▁██████████████▁";
            "▁▁▁▁▁██████████████▁";
            "▁▁▁▁▁██████████████▁";
            "▁▁▁▁▁▁██████████████";
            "▁▁▁▁▁▁██████████████";
            "▁▁▁▁▁▁▁█████████████";
            "▁▁▁▁▁▁▁█████████████";
            "▁▁▁▁▁▁▁▁████████████";
            "▁▁▁▁▁▁▁▁████████████";
            "▁▁▁▁▁▁▁▁▁███████████";
            "▁▁▁▁▁▁▁▁▁███████████";
            "▁▁▁▁▁▁▁▁▁▁██████████";
            "▁▁▁▁▁▁▁▁▁▁██████████";
            "▁▁▁▁▁▁▁▁▁▁▁▁████████";
            "▁▁▁▁▁▁▁▁▁▁▁▁▁███████";
            "▁▁▁▁▁▁▁▁▁▁▁▁▁▁██████";
            "▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁█████";
            "▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁█████";
            "█▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁████";
            "██▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁███";
            "██▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁███";
            "███▁▁▁▁▁▁▁▁▁▁▁▁▁▁███";
            "████▁▁▁▁▁▁▁▁▁▁▁▁▁▁██";
            "█████▁▁▁▁▁▁▁▁▁▁▁▁▁▁█";
            "█████▁▁▁▁▁▁▁▁▁▁▁▁▁▁█";
            "██████▁▁▁▁▁▁▁▁▁▁▁▁▁█";
            "████████▁▁▁▁▁▁▁▁▁▁▁▁";
            "█████████▁▁▁▁▁▁▁▁▁▁▁";
            "█████████▁▁▁▁▁▁▁▁▁▁▁";
            "█████████▁▁▁▁▁▁▁▁▁▁▁";
            "█████████▁▁▁▁▁▁▁▁▁▁▁";
            "███████████▁▁▁▁▁▁▁▁▁";
            "████████████▁▁▁▁▁▁▁▁";
            "████████████▁▁▁▁▁▁▁▁";
            "██████████████▁▁▁▁▁▁";
            "██████████████▁▁▁▁▁▁";
            "▁██████████████▁▁▁▁▁";
            "▁██████████████▁▁▁▁▁";
            "▁▁▁█████████████▁▁▁▁";
            "▁▁▁▁▁████████████▁▁▁";
            "▁▁▁▁▁████████████▁▁▁";
            "▁▁▁▁▁▁███████████▁▁▁";
            "▁▁▁▁▁▁▁▁█████████▁▁▁";
            "▁▁▁▁▁▁▁▁█████████▁▁▁";
            "▁▁▁▁▁▁▁▁▁█████████▁▁";
            "▁▁▁▁▁▁▁▁▁█████████▁▁";
            "▁▁▁▁▁▁▁▁▁▁█████████▁";
            "▁▁▁▁▁▁▁▁▁▁▁████████▁";
            "▁▁▁▁▁▁▁▁▁▁▁████████▁";
            "▁▁▁▁▁▁▁▁▁▁▁▁███████▁";
            "▁▁▁▁▁▁▁▁▁▁▁▁███████▁";
            "▁▁▁▁▁▁▁▁▁▁▁▁▁███████";
            "▁▁▁▁▁▁▁▁▁▁▁▁▁███████";
            "▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁█████";
            "▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁████";
            "▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁████";
            "▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁████";
            "▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁███";
            "▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁███";
            "▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁██";
            "▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁██";
            "▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁██";
            "▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁█";
            "▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁█";
            "▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁█";
            "▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁";
            "▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁";
            "▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁";
            "▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁";
          ];
        interval = 17;
      }
  | Unicode_moon_phases ->
      {
        frames = [ "🌑 "; "🌒 "; "🌓 "; "🌔 "; "🌕 "; "🌖 "; "🌗 "; "🌘 " ];
        interval = 80;
      }
  | Emoji_runner -> { frames = [ "🚶 "; "🏃 " ]; interval = 140 }
  | Anim_pong ->
      {
        frames =
          [
            "▐⠂       ▌";
            "▐⠈       ▌";
            "▐ ⠂      ▌";
            "▐ ⠠      ▌";
            "▐  ⡀     ▌";
            "▐  ⠠     ▌";
            "▐   ⠂    ▌";
            "▐   ⠈    ▌";
            "▐    ⠂   ▌";
            "▐    ⠠   ▌";
            "▐     ⡀  ▌";
            "▐     ⠠  ▌";
            "▐      ⠂ ▌";
            "▐      ⠈ ▌";
            "▐       ⠂▌";
            "▐       ⠠▌";
            "▐       ⡀▌";
            "▐      ⠠ ▌";
            "▐      ⠂ ▌";
            "▐     ⠈  ▌";
            "▐     ⠂  ▌";
            "▐    ⠠   ▌";
            "▐    ⡀   ▌";
            "▐   ⠠    ▌";
            "▐   ⠂    ▌";
            "▐  ⠈     ▌";
            "▐  ⠂     ▌";
            "▐ ⠠      ▌";
            "▐ ⡀      ▌";
            "▐⠠       ▌";
          ];
        interval = 80;
      }
  | Anim_shark ->
      {
        frames =
          [
            "▐|\\____________▌";
            "▐_|\\___________▌";
            "▐__|\\__________▌";
            "▐___|\\_________▌";
            "▐____|\\________▌";
            "▐_____|\\_______▌";
            "▐______|\\______▌";
            "▐_______|\\_____▌";
            "▐________|\\____▌";
            "▐_________|\\___▌";
            "▐__________|\\__▌";
            "▐___________|\\_▌";
            "▐____________|\\▌";
            "▐____________/|▌";
            "▐___________/|_▌";
            "▐__________/|__▌";
            "▐_________/|___▌";
            "▐________/|____▌";
            "▐_______/|_____▌";
            "▐______/|______▌";
            "▐_____/|_______▌";
            "▐____/|________▌";
            "▐___/|_________▌";
            "▐__/|__________▌";
            "▐_/|___________▌";
            "▐/|____________▌";
          ];
        interval = 120;
      }
  | Ascii_dqpb -> { frames = [ "d"; "q"; "p"; "b" ]; interval = 100 }
  | Unicode_weather ->
      {
        frames =
          [
            "☀️ ";
            "☀️ ";
            "☀️ ";
            "🌤 ";
            "⛅️ ";
            "🌥 ";
            "☁️ ";
            "🌧 ";
            "🌨 ";
            "🌧 ";
            "🌨 ";
            "🌧 ";
            "🌨 ";
            "⛈ ";
            "🌨 ";
            "🌧 ";
            "🌨 ";
            "☁️ ";
            "🌥 ";
            "⛅️ ";
            "🌤 ";
            "☀️ ";
            "☀️ ";
          ];
        interval = 100;
      }
  | Emoji_christmas -> { frames = [ "🌲"; "🎄" ]; interval = 400 }
  | Anim_grenade ->
      {
        frames =
          [
            "،  ";
            "′  ";
            " ´ ";
            " ‾ ";
            "  ⸌";
            "  ⸊";
            "  |";
            "  ⁎";
            "  ⁕";
            " ෴ ";
            "  ⁓";
            "   ";
            "   ";
            "   ";
          ];
        interval = 80;
      }
  | Progress_point ->
      { frames = [ "∙∙∙"; "●∙∙"; "∙●∙"; "∙∙●"; "∙∙∙" ]; interval = 125 }
  | Progress_layer -> { frames = [ "-"; "="; "≡" ]; interval = 150 }
  | Progress_beta_wave ->
      {
        frames =
          [
            "ρββββββ";
            "βρβββββ";
            "ββρββββ";
            "βββρβββ";
            "ββββρββ";
            "βββββρβ";
            "ββββββρ";
          ];
        interval = 80;
      }
  | Emoji_finger_dance ->
      { frames = [ "🤘 "; "🤟 "; "🖖 "; "✋ "; "🤚 "; "👆 " ]; interval = 160 }
  | Emoji_fist_bump ->
      {
        frames =
          [
            "🤜　　　　🤛 ";
            "🤜　　　　🤛 ";
            "🤜　　　　🤛 ";
            "　🤜　　🤛　 ";
            "　　🤜🤛　　 ";
            "　🤜✨🤛　　 ";
            "🤜　✨　🤛　 ";
          ];
        interval = 80;
      }
  | Emoji_soccer ->
      {
        frames =
          [
            " 🧑⚽️       🧑 ";
            "🧑  ⚽️      🧑 ";
            "🧑   ⚽️     🧑 ";
            "🧑    ⚽️    🧑 ";
            "🧑     ⚽️   🧑 ";
            "🧑      ⚽️  🧑 ";
            "🧑       ⚽️🧑  ";
            "🧑      ⚽️  🧑 ";
            "🧑     ⚽️   🧑 ";
            "🧑    ⚽️    🧑 ";
            "🧑   ⚽️     🧑 ";
            "🧑  ⚽️      🧑 ";
          ];
        interval = 80;
      }
  | Emoji_mindblown ->
      {
        frames =
          [
            "😐 ";
            "😐 ";
            "😮 ";
            "😮 ";
            "😦 ";
            "😦 ";
            "😧 ";
            "😧 ";
            "🤯 ";
            "💥 ";
            "✨ ";
            "　 ";
            "　 ";
            "　 ";
          ];
        interval = 160;
      }
  | Emoji_speaker -> { frames = [ "🔈 "; "🔉 "; "🔊 "; "🔉 " ]; interval = 160 }
  | Pulse_orange ->
      { frames = [ "🔸 "; "🔶 "; "🟠 "; "🟠 "; "🔶 " ]; interval = 100 }
  | Pulse_blue -> { frames = [ "🔹 "; "🔷 "; "🔵 "; "🔵 "; "🔷 " ]; interval = 100 }
  | Pulse_orange_blue ->
      {
        frames = [ "🔸 "; "🔶 "; "🟠 "; "🟠 "; "🔶 "; "🔹 "; "🔷 "; "🔵 "; "🔵 "; "🔷 " ];
        interval = 100;
      }
  | Time_travel ->
      {
        frames =
          [
            "🕛 ";
            "🕚 ";
            "🕙 ";
            "🕘 ";
            "🕗 ";
            "🕖 ";
            "🕕 ";
            "🕔 ";
            "🕓 ";
            "🕒 ";
            "🕑 ";
            "🕐 ";
          ];
        interval = 100;
      }
  | Anim_aesthetic ->
      {
        frames =
          [
            "▰▱▱▱▱▱▱";
            "▰▰▱▱▱▱▱";
            "▰▰▰▱▱▱▱";
            "▰▰▰▰▱▱▱";
            "▰▰▰▰▰▱▱";
            "▰▰▰▰▰▰▱";
            "▰▰▰▰▰▰▰";
            "▰▱▱▱▱▱▱";
          ];
        interval = 80;
      }
  | Anim_dwarf_fortress ->
      {
        frames =
          [
            " ██████£££  ";
            "☺██████£££  ";
            "☺██████£££  ";
            "☺▓█████£££  ";
            "☺▓█████£££  ";
            "☺▒█████£££  ";
            "☺▒█████£££  ";
            "☺░█████£££  ";
            "☺░█████£££  ";
            "☺ █████£££  ";
            " ☺█████£££  ";
            " ☺█████£££  ";
            " ☺▓████£££  ";
            " ☺▓████£££  ";
            " ☺▒████£££  ";
            " ☺▒████£££  ";
            " ☺░████£££  ";
            " ☺░████£££  ";
            " ☺ ████£££  ";
            "  ☺████£££  ";
            "  ☺████£££  ";
            "  ☺▓███£££  ";
            "  ☺▓███£££  ";
            "  ☺▒███£££  ";
            "  ☺▒███£££  ";
            "  ☺░███£££  ";
            "  ☺░███£££  ";
            "  ☺ ███£££  ";
            "   ☺███£££  ";
            "   ☺███£££  ";
            "   ☺▓██£££  ";
            "   ☺▓██£££  ";
            "   ☺▒██£££  ";
            "   ☺▒██£££  ";
            "   ☺░██£££  ";
            "   ☺░██£££  ";
            "   ☺ ██£££  ";
            "    ☺██£££  ";
            "    ☺██£££  ";
            "    ☺▓█£££  ";
            "    ☺▓█£££  ";
            "    ☺▒█£££  ";
            "    ☺▒█£££  ";
            "    ☺░█£££  ";
            "    ☺░█£££  ";
            "    ☺ █£££  ";
            "     ☺█£££  ";
            "     ☺█£££  ";
            "     ☺▓£££  ";
            "     ☺▓£££  ";
            "     ☺▒£££  ";
            "     ☺▒£££  ";
            "     ☺░£££  ";
            "     ☺░£££  ";
            "     ☺ £££  ";
            "      ☺£££  ";
            "      ☺£££  ";
            "      ☺▓££  ";
            "      ☺▓££  ";
            "      ☺▒££  ";
            "      ☺▒££  ";
            "      ☺░££  ";
            "      ☺░££  ";
            "      ☺ ££  ";
            "       ☺££  ";
            "       ☺££  ";
            "       ☺▓£  ";
            "       ☺▓£  ";
            "       ☺▒£  ";
            "       ☺▒£  ";
            "       ☺░£  ";
            "       ☺░£  ";
            "       ☺ £  ";
            "        ☺£  ";
            "        ☺£  ";
            "        ☺▓  ";
            "        ☺▓  ";
            "        ☺▒  ";
            "        ☺▒  ";
            "        ☺░  ";
            "        ☺░  ";
            "        ☺   ";
            "        ☺  &";
            "        ☺ ☼&";
            "       ☺ ☼ &";
            "       ☺☼  &";
            "      ☺☼  & ";
            "      ‼   & ";
            "     ☺   &  ";
            "    ‼    &  ";
            "   ☺    &   ";
            "  ‼     &   ";
            " ☺     &    ";
            "‼      &    ";
            "      &     ";
            "      &     ";
            "     &   ░  ";
            "     &   ▒  ";
            "    &    ▓  ";
            "    &    £  ";
            "   &    ░£  ";
            "   &    ▒£  ";
            "  &     ▓£  ";
            "  &     ££  ";
            " &     ░££  ";
            " &     ▒££  ";
            "&      ▓££  ";
            "&      £££  ";
            "      ░£££  ";
            "      ▒£££  ";
            "      ▓£££  ";
            "      █£££  ";
            "     ░█£££  ";
            "     ▒█£££  ";
            "     ▓█£££  ";
            "     ██£££  ";
            "    ░██£££  ";
            "    ▒██£££  ";
            "    ▓██£££  ";
            "    ███£££  ";
            "   ░███£££  ";
            "   ▒███£££  ";
            "   ▓███£££  ";
            "   ████£££  ";
            "  ░████£££  ";
            "  ▒████£££  ";
            "  ▓████£££  ";
            "  █████£££  ";
            " ░█████£££  ";
            " ▒█████£££  ";
            " ▓█████£££  ";
            " ██████£££  ";
            " ██████£££  ";
          ];
        interval = 80;
      }
  | Custom { frames; interval } -> { frames; interval }

let get_frame spinner_name frame_index =
  let def = spinners spinner_name in
  let frame_count = List.length def.frames in
  if frame_count = 0 then ""
  else
    let idx = frame_index mod frame_count in
    List.nth def.frames idx

(* Helper to calculate frame from time *)
let frame_from_time ~name ~time ~speed =
  let def = spinners name in
  let interval_seconds = float_of_int def.interval /. 1000.0 in
  let frame_no = time *. speed /. interval_seconds in
  int_of_float frame_no

let spinner ?(speed = 1.0) ?(time = 0.0) kind =
  let frame_idx = frame_from_time ~name:kind ~time ~speed in
  let frame_text = get_frame kind frame_idx in
  Element.text frame_text
