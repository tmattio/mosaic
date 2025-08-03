(** Streaming ANSI / VT escape‑sequence tokenizer.

    The module turns an arbitrary byte stream (which may arrive in chunks of any
    size) into a sequence of high‑level [token] values, while preserving UTF‑8
    integrity and handling common control‑ and OSC‑ sequences (notably OSC 8
    hyperlinks). *)

type control =
  | CUU of int
  | CUD of int
  | CUF of int
  | CUB of int
  | CNL of int
  | CPL of int
  | CHA of int
  | VPA of int
  | CUP of int * int
  | ED of int  (** Erase‑in‑Display *)
  | EL of int  (** Erase‑in‑Line *)
  | IL of int  (** Insert Line *)
  | DL of int  (** Delete Line *)
  | OSC of int * string  (** Generic OSC *)
  | Hyperlink of ((string * string) list * string) option
      (** OSC 8 open/close hyperlink.
          [None]  → “close current link” ( ESC ] 8 ; ; ST )
          [Some (kvs,uri)] → open a hyperlink with optional parameters. *)
  | Reset  (** *RIS* – ESC c *)
  | Unknown of string  (** Fallback for anything else *)
  | DECSC  (** ESC 7 - Save cursor *)
  | DECRC  (** ESC 8 - Restore cursor *)

type token =
  | Text of string  (** Plain UTF‑8 text *)
  | SGR of Internal.attr list  (** Complete Select‑Graphic‑Rendition command *)
  | Control of control  (** Other recognised controls *)

type t
(** Mutable stream parser state. *)

val create : unit -> t
(** Fresh parser in the default state. *)

val reset : t -> unit
(** Clear internal buffers and return to the default state. *)

val pending : t -> bytes
(** Bytes currently buffered that have not yet formed a complete token (useful
    for diagnostics or debugging). *)

val feed : t -> bytes -> int -> int -> token list
(** [feed p buf off len] consumes [len] bytes from [buf] starting at [off],
    returning **any complete tokens** recognised so far. Partial
    escape‑sequences or UTF‑8 characters are retained internally until more data
    arrives. *)

val parse : string -> token list
(** Convenience wrapper that feeds the whole string at once and flushes the
    parser afterwards. Not suitable for streaming use. *)
