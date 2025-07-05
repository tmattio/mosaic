# Mosaic Tiles

A collection of reusable UI components for the Mosaic TUI framework, following the [bubbles](https://github.com/charmbracelet/bubbles) philosophy of self-contained, generic components.

## Overview

Mosaic Tiles provides a set of self-contained, composable UI components that follow The Elm Architecture pattern. Each tile manages its own state and rendering, while composition and layout (like adding labels or descriptions) are handled by the consumer of the library.

## Available Tiles

### Form Components

- **Input** - Advanced single-line text input with validation, suggestions, and password mode
- **Text** - Multi-line text area with scrolling and word wrapping
- **Confirm** - Yes/no confirmation dialog with customizable labels
- **Select** - Single-choice selection from a list with filtering support
- **MultiSelect** - Multiple-choice selection with limits and batch operations

### Display Components

- **Note** - Informational messages and alerts with different severity levels
- **Spinner** - Animated loading indicator

### File System Components

- **FilePicker** - File browser with navigation and filtering capabilities

## Usage

```ocaml
open Mosaic
open Mosaic_tiles

(* Initialize a component *)
let input_model, input_cmd = 
  Input.init 
    ~placeholder:"user@example.com"
    ~validate:(fun s -> 
      if String.contains s '@' then Ok ()
      else Error "Invalid email address")
    ()

(* In your update function *)
| InputMsg msg ->
    let new_input, cmd = Input.update msg model.input in
    ({ model with input = new_input }, 
     Cmd.map (fun m -> InputMsg m) cmd)

(* In your view - compose with labels as needed *)
let open Ui in
vbox ~gap:1 [
  text ~style:Style.bold "Enter your email";
  Input.view model.input;
  text ~style:Style.(fg (Index 8)) "We'll never share your email with anyone else.";
]

(* Access the value *)
let email = Input.value model.input
```

## Common Patterns

### Composition with Labels and Descriptions

Tiles are designed to be composed with your own layout elements:

```ocaml
(* Creating a form field with label and help text *)
let form_field ~label ~help tile_view =
  let open Ui in
  vbox ~gap:1 [
    text ~style:Style.bold label;
    tile_view;
    (match help with
     | Some h -> text ~style:Style.(fg (Index 8)) h
     | None -> space 0);
  ]

(* Using it with different tiles *)
let email_field = 
  form_field 
    ~label:"Email Address"
    ~help:(Some "Your primary contact email")
    (Input.view model.email_input)

let skills_field =
  form_field
    ~label:"Select your skills"
    ~help:(Some "Choose up to 3 skills")
    (Multi_select.view model.skills_select)
```

### Navigation Between Tiles

Use Tab/Shift+Tab or custom key bindings to navigate between tiles:

```ocaml
let subscriptions model =
  Sub.batch [
    Sub.on_key Tab NextTile;
    Sub.on_key ~shift:true Tab PrevTile;
  ]
```

### Validation

Most form components support validation:

```ocaml
let validate_age s =
  try
    let age = int_of_string s in
    if age >= 18 && age <= 120 then Ok ()
    else Error "Age must be between 18 and 120"
  with _ -> Error "Please enter a valid number"
```

### Theming

All tiles support custom theming:

```ocaml
let custom_theme = {
  Input.default_theme with
  focused_style = Style.(fg Green ++ bold);
  error_style = Style.(fg Red ++ italic);
}

let input = Input.with_theme custom_theme input
```

## Examples

See `examples/tiles_demo.ml` for a comprehensive demonstration of all available tiles.

## Architecture

Each tile follows a consistent architecture:

1. **Model** - The component's state
2. **Messages** - Internal events the component handles
3. **Init** - Creates a new instance with configuration
4. **Update** - Handles messages and updates state
5. **View** - Renders the component
6. **Subscriptions** - Listens for keyboard/mouse events when focused

This design ensures tiles are:
- Self-contained and reusable
- Easy to compose into larger applications
- Type-safe with clear interfaces
- Consistent in behavior and appearance