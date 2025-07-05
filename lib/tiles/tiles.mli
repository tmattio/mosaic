(** Mosaic Tiles - A collection of reusable UI components

    This module provides easy access to all available tiles in the Mosaic
    framework. Each tile is a self-contained component that follows The Elm
    Architecture pattern.

    {[
      open Mosaic_tiles.Tiles

      (* Use any tile *)
      let input = Input.init ~title:"Name" ()
      let select = Select.init ~options:[ ("a", "Option A") ] ()
    ]} *)

(** {2 Basic Components} *)

module Spinner = Spinner
(** Animated loading spinner *)

(** {2 Form Components} *)

module Input = Input
(** Advanced single-line text input with validation and suggestions *)

module Text = Text
(** Multi-line text area with scrolling support *)

module Confirm = Confirm
(** Yes/no confirmation dialog *)

module Select = Select
(** Single-choice selection from a list *)

module Multi_select = Multi_select
(** Multiple-choice selection from a list *)

(** {2 Display Components} *)

module Note = Note
(** Informational messages and alerts *)

module Table = Table
(** Table for displaying structured, tabular data *)

module Progress = Progress
(** Progress bar for long-running operations *)

(** {2 File System Components} *)

module File_picker = File_picker
(** File browser and selector *)

(** {2 Helper Components} *)

module Viewport = Viewport
(** Generic scrollable viewport for displaying large content *)

module Paginator = Paginator
(** Pagination logic and display component *)

module Help = Help
(** Help display for keyboard shortcuts and commands *)
