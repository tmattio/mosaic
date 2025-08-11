(** Eio-based wrapper for Matrix TTY module.
    
    This module wraps the Matrix TTY functionality with Eio's non-blocking
    IO primitives to prevent domain suspension during terminal operations. *)

open Eio

type t
(** Terminal handle with Eio flow-based IO *)

val create : 
  sw:Switch.t -> 
  env:< stdin : _ Flow.source; stdout : _ Flow.sink; .. > ->
  ?tty:bool -> 
  unit ->
  t
(** [create ~sw ~env ?tty] creates an Eio-based terminal handle.
    Uses env's stdin/stdout as Eio flows for non-blocking IO.
    @param sw Switch for resource management
    @param env Eio environment with stdin/stdout
    @param tty Force TTY status (default: autodetect) *)

val with_terminal :
  sw:Switch.t ->
  env:< stdin : _ Flow.source; stdout : _ Flow.sink; .. > ->
  ?tty:bool ->
  (t -> 'a) ->
  'a
(** [with_terminal ~sw ~env ?tty f] creates a terminal and ensures cleanup. *)

(** {1 Delegated Functions} 
    These functions delegate to the underlying Matrix TTY implementation *)

val release : t -> unit
val save_state : t -> unit
val restore_state : t -> unit
val set_mode : t -> Tty.mode -> unit
val size : t -> int * int
val input_fd : t -> Unix.file_descr
val output_fd : t -> Unix.file_descr

(** {1 Non-blocking IO Operations} *)

val write : t -> bytes -> int -> int -> unit
(** [write t buf ofs len] writes bytes using Eio flows (non-blocking) *)

val write_string : t -> string -> unit
(** [write_string t s] writes a string using Eio flows (non-blocking) *)

val read : t -> bytes -> int -> int -> int
(** [read t buf ofs len] reads bytes using Eio flows (non-blocking) *)

val flush : t -> unit
(** [flush t] ensures all buffered output is written *)

(** {1 Screen Management} *)

val enable_alternate_screen : t -> unit
val disable_alternate_screen : t -> unit
val clear_screen : t -> unit
val set_title : t -> string -> unit
val bell : t -> unit

(** {1 Cursor Control} *)

val show_cursor : t -> unit
val hide_cursor : t -> unit
val move_cursor : t -> int -> int -> unit

(** {1 Mouse Support} *)

val set_mouse_mode : t -> Tty.mouse_mode -> unit
val enable_mouse_sgr : t -> unit
val disable_mouse_sgr : t -> unit

(** {1 Advanced Features} *)

val enable_focus_reporting : t -> unit
val disable_focus_reporting : t -> unit
val enable_bracketed_paste : t -> unit
val disable_bracketed_paste : t -> unit
val enable_kitty_keyboard : t -> unit
val disable_kitty_keyboard : t -> unit

(** {1 Window Management} *)

val set_resize_handler : t -> (int * int -> unit) -> unit
val remove_resize_handlers : t -> unit

(** {1 Color and Feature Detection} *)

val set_dark_background : t -> dark:bool -> unit
val has_dark_background : t -> bool
val has_truecolor_support : t -> bool
val supports_feature : t -> Tty.feature -> bool

(** {1 Testing Support} *)

val create_from_strings : 
  sw:Switch.t -> 
  string -> 
  t * (unit -> string) * (unit -> unit)
(** [create_from_strings ~sw input] creates a mock terminal for testing.
    
    Creates a terminal that reads from the provided string and writes to an
    internal buffer. Control sequences have no effect.
    
    @param sw Switch for resource management
    @param input String to use as input data
    @return
      [(term, get_output, close)] where:
      - [term] is the mock terminal handle  
      - [get_output ()] returns all output written so far
      - [close ()] closes the mock file descriptors *)

(** {1 Internal Access} *)

val get_matrix_tty : t -> Tty.t
(** [get_matrix_tty t] returns the underlying Matrix TTY handle.
    Use with caution - direct writes may block. *)