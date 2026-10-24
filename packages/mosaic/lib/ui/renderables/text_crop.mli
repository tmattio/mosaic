(** Grapheme-aware cropping and ellipsis truncation shared by widgets.

    Byte-based truncation splits UTF-8 sequences and confuses bytes with display
    columns. These helpers iterate grapheme clusters and measure with the
    caller-supplied width method so cropping agrees with how the target grid
    lays cells down. *)

val crop_to_width :
  width_method:Matrix.Text.width_method -> string -> int -> string
(** [crop_to_width ~width_method text width] is the longest grapheme-aligned
    prefix of [text] whose display width does not exceed [width]. *)

val truncate_with_ellipsis :
  width_method:Matrix.Text.width_method ->
  ?ellipsis:string ->
  string ->
  int ->
  string
(** [truncate_with_ellipsis ~width_method text width] is [text] when it fits in
    [width] columns, and otherwise a grapheme-aligned prefix of [text] followed
    by [ellipsis]. When [width] does not exceed the width of [ellipsis] itself,
    the text is cropped without an ellipsis. [ellipsis] defaults to ["..."]. *)
