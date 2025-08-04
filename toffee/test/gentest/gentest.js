#!/usr/bin/env node

import { chromium } from 'playwright';
import fs from 'fs/promises';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const rootDir = path.resolve(__dirname, '../..');
const testDir = path.resolve(__dirname, '..');

// Convert style object to OCaml code
function styleToOCaml(style, boxSizing = null) {
  const fields = [];
  
  // Always set display - default to Block if not specified (HTML default)
  const displayMap = {
    'grid': 'Grid',
    'flex': 'Flex', 
    'block': 'Block',
    'none': 'None'
  };
  const displayValue = style.display || 'block';  // Default to block if not set
  fields.push(`display = Toffee.Style.${displayMap[displayValue] || 'Block'}`);
  
  if (style.position) {
    const positionMap = {
      'relative': 'Relative',
      'absolute': 'Absolute'
    };
    fields.push(`position = Toffee.Style.${positionMap[style.position] || 'Relative'}`);
  }
  
  // Grid properties
  if (style.gridTemplateColumns?.length > 0) {
    const tracks = style.gridTemplateColumns.map(trackToOCaml).join('; ');
    fields.push(`grid_template_columns = [${tracks}]`);
  }
  
  if (style.gridTemplateRows?.length > 0) {
    const tracks = style.gridTemplateRows.map(trackToOCaml).join('; ');
    fields.push(`grid_template_rows = [${tracks}]`);
  }
  
  if (style.gridAutoRows?.length > 0) {
    const tracks = style.gridAutoRows.map(t => nonRepeatedTrackToOCaml(t)).join('; ');
    fields.push(`grid_auto_rows = [${tracks}]`);
  }
  
  if (style.gridAutoColumns?.length > 0) {
    const tracks = style.gridAutoColumns.map(t => nonRepeatedTrackToOCaml(t)).join('; ');
    fields.push(`grid_auto_columns = [${tracks}]`);
  }
  
  if (style.gridAutoFlow) {
    const flowMap = {
      'row': 'Row',
      'column': 'Column',
      'row dense': 'Row_dense',
      'column dense': 'Column_dense'
    };
    // Handle both object and string formats
    let flowValue;
    if (typeof style.gridAutoFlow === 'string') {
      flowValue = style.gridAutoFlow;
    } else {
      const direction = style.gridAutoFlow.direction || 'row';
      const isDense = style.gridAutoFlow.algorithm === 'dense';
      flowValue = isDense ? `${direction} dense` : direction;
    }
    fields.push(`grid_auto_flow = Toffee.Style.Grid.${flowMap[flowValue] || 'Row'}`);
  }
  
  // Handle grid placement - can be specified as gridColumn/gridRow or individual properties
  if (style.gridColumn || style.gridColumnStart || style.gridColumnEnd) {
    const start = style.gridColumn?.start || style.gridColumnStart || { kind: 'auto' };
    const end_ = style.gridColumn?.end || style.gridColumnEnd || { kind: 'auto' };
    const startOCaml = gridLineToOCaml(start);
    const endOCaml = gridLineToOCaml(end_);
    fields.push(`grid_column = { start = ${startOCaml}; end_ = ${endOCaml} }`);
  }
  
  if (style.gridRow || style.gridRowStart || style.gridRowEnd) {
    const start = style.gridRow?.start || style.gridRowStart || { kind: 'auto' };
    const end_ = style.gridRow?.end || style.gridRowEnd || { kind: 'auto' };
    const startOCaml = gridLineToOCaml(start);
    const endOCaml = gridLineToOCaml(end_);
    fields.push(`grid_row = { start = ${startOCaml}; end_ = ${endOCaml} }`);
  }
  
  // Flexbox properties
  if (style.flexDirection) {
    const dirMap = {
      'row': 'Row',
      'row-reverse': 'Row_reverse',
      'column': 'Column',
      'column-reverse': 'Column_reverse'
    };
    fields.push(`flex_direction = Toffee.Style.Flex.${dirMap[style.flexDirection] || 'Row'}`);
  }
  
  if (style.flexWrap) {
    const wrapMap = {
      'nowrap': 'No_wrap',
      'wrap': 'Wrap',
      'wrap-reverse': 'Wrap_reverse'
    };
    fields.push(`flex_wrap = Toffee.Style.Flex.${wrapMap[style.flexWrap] || 'No_wrap'}`);
  }
  
  if (style.alignItems) {
    const alignMap = {
      'stretch': 'Stretch',
      'flex-start': 'Flex_start',
      'flex-end': 'Flex_end',
      'center': 'Center',
      'baseline': 'Baseline',
      'start': 'Start',
      'end': 'End'
    };
    const value = alignMap[style.alignItems];
    if (value) {
      fields.push(`align_items = Some (Toffee.Style.Alignment.${value})`);
    }
  }
  
  if (style.alignSelf && style.alignSelf !== 'auto') {
    const alignMap = {
      'stretch': 'Stretch',
      'flex-start': 'Flex_start',
      'flex-end': 'Flex_end',
      'center': 'Center',
      'baseline': 'Baseline',
      'start': 'Start',
      'end': 'End'
    };
    const value = alignMap[style.alignSelf];
    if (value) {
      fields.push(`align_self = Some (Toffee.Style.Alignment.${value})`);
    }
  }
  
  if (style.justifyContent) {
    const justifyMap = {
      'flex-start': 'Flex_start',
      'flex-end': 'Flex_end',
      'center': 'Center',
      'space-between': 'Space_between',
      'space-around': 'Space_around',
      'space-evenly': 'Space_evenly',
      'start': 'Start',
      'end': 'End'
    };
    fields.push(`justify_content = Some (Toffee.Style.Alignment.${justifyMap[style.justifyContent] || 'Flex_start'})`);
  }
  
  if (style.alignContent) {
    const alignMap = {
      'stretch': 'Stretch',
      'flex-start': 'Flex_start',
      'flex-end': 'Flex_end',
      'center': 'Center',
      'space-between': 'Space_between',
      'space-around': 'Space_around',
      'space-evenly': 'Space_evenly',
      'start': 'Start',
      'end': 'End'
    };
    fields.push(`align_content = Some (Toffee.Style.Alignment.${alignMap[style.alignContent] || 'Stretch'})`);
  }
  
  if (style.justifyItems) {
    const justifyMap = {
      'stretch': 'Stretch',
      'flex-start': 'Flex_start',
      'flex-end': 'Flex_end',
      'center': 'Center',
      'baseline': 'Baseline',
      'start': 'Start',
      'end': 'End'
    };
    fields.push(`justify_items = Some (Toffee.Style.Alignment.${justifyMap[style.justifyItems] || 'Stretch'})`);
  }
  
  if (style.justifySelf && style.justifySelf !== 'auto') {
    const justifyMap = {
      'stretch': 'Stretch',
      'flex-start': 'Flex_start',
      'flex-end': 'Flex_end',
      'center': 'Center',
      'baseline': 'Baseline',
      'start': 'Start',
      'end': 'End'
    };
    const value = justifyMap[style.justifySelf];
    if (value) {
      fields.push(`justify_self = Some (Toffee.Style.Alignment.${value})`);
    }
  }
  
  if (style.flexGrow !== undefined) {
    // Ensure flex_grow is a float
    const floatValue = String(style.flexGrow).includes('.') ? style.flexGrow : `${style.flexGrow}.0`;
    fields.push(`flex_grow = ${floatValue}`);
  }
  
  if (style.flexShrink !== undefined) {
    // Ensure flex_shrink is a float
    const floatValue = String(style.flexShrink).includes('.') ? style.flexShrink : `${style.flexShrink}.0`;
    fields.push(`flex_shrink = ${floatValue}`);
  }
  
  if (style.flexBasis) {
    fields.push(`flex_basis = ${dimensionToOCaml(style.flexBasis)}`);
  }
  
  // Size properties
  if (style.size) {
    const size = sizeToOCaml(style.size);
    if (size) fields.push(`size = ${size}`);
  }
  
  if (style.minSize) {
    const minSize = sizeToOCaml(style.minSize);
    if (minSize) fields.push(`min_size = ${minSize}`);
  }
  
  if (style.maxSize) {
    const maxSize = sizeToOCaml(style.maxSize);
    if (maxSize) fields.push(`max_size = ${maxSize}`);
  }
  
  if (style.aspectRatio !== undefined) {
    // Ensure aspect ratio is a float
    const floatValue = String(style.aspectRatio).includes('.') ? style.aspectRatio : `${style.aspectRatio}.0`;
    fields.push(`aspect_ratio = Some ${floatValue}`);
  }
  
  // Gap
  if (style.gap) {
    const gap = gapToOCaml(style.gap);
    if (gap) fields.push(`gap = ${gap}`);
  }
  
  // Margin/Padding/Border
  if (style.margin) {
    const margin = rectToOCaml(style.margin, true);  // margin uses Length_percentage_auto
    if (margin) fields.push(`margin = ${margin}`);
  }
  
  if (style.padding) {
    const padding = rectToOCaml(style.padding, false);  // padding uses Length_percentage
    if (padding) fields.push(`padding = ${padding}`);
  }
  
  if (style.border) {
    const border = rectToOCaml(style.border, false);  // border uses Length_percentage
    if (border) fields.push(`border = ${border}`);
  }
  
  // Positioning (inset)
  if (style.inset) {
    const inset = rectToOCaml(style.inset, true);  // inset uses Length_percentage_auto
    if (inset) fields.push(`inset = ${inset}`);
  }
  
  // Box sizing - only set if explicitly requested and different from default
  if (boxSizing === 'border_box') {
    // For border-box test, only set if style is explicitly content-box (not default)
    if (style.boxSizing === 'content-box') {
      // Don't set box_sizing for border-box test - it defaults to BorderBox
    }
  } else if (boxSizing === 'content_box') {
    // For content-box test, always set to ContentBox
    fields.push(`box_sizing = Toffee.Style.Content_box`);
  }
  
  return fields.length > 0 
    ? `{ Toffee.Style.default with ${fields.join('; ')} }`
    : 'Toffee.Style.default';
}

function nonRepeatedTrackToOCaml(track) {
  // For grid_auto_rows/columns, we return just the record without Single constructor
  // Handle different track types from test_helper.js
  if (track.kind === 'scalar') {
    switch (track.unit) {
      case 'fraction':
      case 'fr': 
        // Fr can only be max, min is auto
        const frVal = String(track.value).includes('.') ? track.value : `${track.value}.0`;
        return `{ min = Toffee.Style.Grid.Auto; max = Toffee.Style.Grid.Fr (${frVal}) }`;
      case 'px': 
      case 'points': 
        // Length for both min and max - ensure float value
        const floatVal = String(track.value).includes('.') ? track.value : `${track.value}.0`;
        return `{ min = Toffee.Style.Grid.Length (${floatVal}); max = Toffee.Style.Grid.Length (${floatVal}) }`;
      case 'percent': 
        // Percent for both min and max - ensure float value
        const percentVal = String(track.value).includes('.') ? track.value : `${track.value}.0`;
        return `{ min = Toffee.Style.Grid.Percent (${percentVal}); max = Toffee.Style.Grid.Percent (${percentVal}) }`;
      case 'auto': 
        return `{ min = Toffee.Style.Grid.Auto; max = Toffee.Style.Grid.Auto }`;
      case 'min-content': 
        return `{ min = Toffee.Style.Grid.Min_content; max = Toffee.Style.Grid.Min_content }`;
      case 'max-content': 
        return `{ min = Toffee.Style.Grid.Max_content; max = Toffee.Style.Grid.Max_content }`;
      default: 
        return `{ min = Toffee.Style.Grid.Auto; max = Toffee.Style.Grid.Auto }`;
    }
  } else if (track.kind === 'function') {
    // Handle minmax() and other track sizing functions
    if (track.name === 'minmax' && track.arguments?.length === 2) {
      const min = trackMinToOCaml(track.arguments[0]);
      const max = trackMaxToOCaml(track.arguments[1]);
      return `{ min = ${min}; max = ${max} }`;
    }
  }
  return `{ min = Toffee.Style.Grid.Auto; max = Toffee.Style.Grid.Auto }`;
}

function trackToOCaml(track) {
  // For grid_template_*, we need to wrap with Single constructor
  const nonRepeated = nonRepeatedTrackToOCaml(track);
  return `Toffee.Style.Grid.Single ${nonRepeated}`;
}

function trackMinToOCaml(track) {
  // Helper to ensure value is a float
  const floatValue = (val) => {
    const strVal = String(val);
    return strVal.includes('.') ? strVal : `${strVal}.0`;
  };
  
  if (track.unit === 'px' || track.unit === 'points') return `Toffee.Style.Grid.Length (${floatValue(track.value)})`;
  if (track.unit === 'percent') {
    const percentVal = String(track.value).includes('.') ? track.value : `${track.value}.0`;
    return `Toffee.Style.Grid.Percent (${percentVal})`;
  }
  if (track.unit === 'auto') return 'Toffee.Style.Grid.Auto';
  if (track.unit === 'min-content') return 'Toffee.Style.Grid.Min_content';
  if (track.unit === 'max-content') return 'Toffee.Style.Grid.Max_content';
  return 'Toffee.Style.Grid.Auto';
}

function trackMaxToOCaml(track) {
  // Helper to ensure value is a float
  const floatValue = (val) => {
    const strVal = String(val);
    return strVal.includes('.') ? strVal : `${strVal}.0`;
  };
  
  if (track.unit === 'fr' || track.unit === 'fraction') {
    const frVal = String(track.value).includes('.') ? track.value : `${track.value}.0`;
    return `Toffee.Style.Grid.Fr (${frVal})`;
  }
  if (track.unit === 'px' || track.unit === 'points') return `Toffee.Style.Grid.Length (${floatValue(track.value)})`;
  if (track.unit === 'percent') {
    const percentVal = String(track.value).includes('.') ? track.value : `${track.value}.0`;
    return `Toffee.Style.Grid.Percent (${percentVal})`;
  }
  if (track.unit === 'auto') return 'Toffee.Style.Grid.Auto';
  if (track.unit === 'min-content') return 'Toffee.Style.Grid.Min_content';
  if (track.unit === 'max-content') return 'Toffee.Style.Grid.Max_content';
  return 'Toffee.Style.Grid.Auto';
}

function gridLineToOCaml(line) {
  if (!line) return 'Toffee.Style.Grid.Auto';
  switch (line.tag || line.kind) {
    case 'auto': return 'Toffee.Style.Grid.Auto';
    case 'line': return `Toffee.Style.Grid.Line ${line.value}`;
    case 'span': return `Toffee.Style.Grid.Span ${line.value}`;
    default: return 'Toffee.Style.Grid.Auto';
  }
}

function dimensionToOCaml(dim) {
  if (!dim) return 'Toffee.Style.Dimension.auto';
  
  // Helper to ensure value is a float
  const floatValue = (val) => {
    // Check if the value is already a float (has decimal point)
    const strVal = String(val);
    return strVal.includes('.') ? strVal : `${strVal}.0`;
  };
  
  switch (dim.unit) {
    case 'px': return `Toffee.Style.Dimension.length ${floatValue(dim.value)}`;
    case 'points': return `Toffee.Style.Dimension.length ${floatValue(dim.value)}`;
    case 'percent': 
      // Ensure percent values are floats
      const percentVal = String(dim.value).includes('.') ? dim.value : `${dim.value}.0`;
      return `Toffee.Style.Dimension.percent ${percentVal}`;
    case 'auto': return 'Toffee.Style.Dimension.auto';
    default: return 'Toffee.Style.Dimension.auto';
  }
}

function sizeToOCaml(size) {
  if (!size) return null;
  const width = size.width ? dimensionToOCaml(size.width) : 'Toffee.Style.Dimension.auto';
  const height = size.height ? dimensionToOCaml(size.height) : 'Toffee.Style.Dimension.auto';
  return `{ width = ${width}; height = ${height} }`;
}

function gapToOCaml(gap) {
  if (!gap) return null;
  
  const convertGapValue = (value) => {
    if (!value) return 'Toffee.Style.Length_percentage.Length (0.0)';
    
    // Helper to ensure value is a float
    const floatValue = (val) => {
      const strVal = String(val);
      return strVal.includes('.') ? strVal : `${strVal}.0`;
    };
    
    switch (value.unit) {
      case 'px':
      case 'points':
        return `Toffee.Style.Length_percentage.Length (${floatValue(value.value)})`;
      case 'percent':
        // Ensure percent values are floats
        const percentVal = String(value.value).includes('.') ? value.value : `${value.value}.0`;
        return `Toffee.Style.Length_percentage.Percent (${percentVal})`;
      default:
        return 'Toffee.Style.Length_percentage.Length (0.0)';
    }
  };
  
  const width = gap.width ? convertGapValue(gap.width) : 'Toffee.Style.Length_percentage.Length (0.0)';
  const height = gap.height ? convertGapValue(gap.height) : 'Toffee.Style.Length_percentage.Length (0.0)';
  return `{ width = ${width}; height = ${height} }`;
}

function rectToOCaml(rect, isMarginOrInset = false) {
  if (!rect) return null;
  
  // Helper to convert a single edge value
  const convertEdge = (edge) => {
    if (!edge) {
      // For margin/inset, missing values should be Auto, not 0
      return isMarginOrInset 
        ? 'Toffee.Style.Length_percentage_auto.Auto'
        : 'Toffee.Style.Length_percentage.Length (0.0)';
    }
    
    const prefix = isMarginOrInset 
      ? 'Toffee.Style.Length_percentage_auto' 
      : 'Toffee.Style.Length_percentage';
    
    // Helper to ensure value is a float
    const floatValue = (val) => {
      const strVal = String(val);
      return strVal.includes('.') ? strVal : `${strVal}.0`;
    };
    
    switch (edge.unit) {
      case 'px':
      case 'points':
        const lengthVal = floatValue(edge.value);
        // Wrap negative values in parentheses for OCaml syntax
        return `${prefix}.Length (${lengthVal})`;
      case 'percent':
        // Ensure percent values are floats
        const percentVal = String(edge.value).includes('.') ? edge.value : `${edge.value}.0`;
        // Wrap in parentheses for OCaml syntax
        return `${prefix}.Percent (${percentVal})`;
      case 'auto':
        return isMarginOrInset ? `${prefix}.Auto` : `${prefix}.Length (0.0)`;
      default:
        return isMarginOrInset 
          ? `${prefix}.Length (0.0)`
          : `${prefix}.Length (0.0)`;
    }
  };
  
  const left = convertEdge(rect.left);
  const right = convertEdge(rect.right);
  const top = convertEdge(rect.top);
  const bottom = convertEdge(rect.bottom);
  
  return `{ left = ${left}; right = ${right}; top = ${top}; bottom = ${bottom} }`;
}

function availableSpaceToOCaml(space) {
  if (!space) return 'Toffee.Style.Available_space.Max_content';
  
  // Helper to ensure value is a float
  const floatValue = (val) => {
    const strVal = String(val);
    return strVal.includes('.') ? strVal : `${strVal}.0`;
  };
  
  switch (space.unit) {
    case 'points': return `Toffee.Style.Available_space.Definite ${floatValue(space.value)}`;
    case 'min-content': return 'Toffee.Style.Available_space.Min_content';
    case 'max-content': return 'Toffee.Style.Available_space.Max_content';
    default: return 'Toffee.Style.Available_space.Max_content';
  }
}

function generateNode(nodeVar, node, parentVar, boxSizing) {
  const style = styleToOCaml(node.style, boxSizing);
  const textContent = node.textContent;
  
  let nodeCreation = `let ${nodeVar} = Toffee.new_leaf tree (${style}) in`;
  
  if (textContent) {
    // Set context for text nodes - pass the actual text content
    const escapedText = textContent.replace(/\\/g, '\\\\').replace(/"/g, '\\"');
    nodeCreation += `\n  let _ = Toffee.set_node_context tree ${nodeVar} (MeasureFunction.Text "${escapedText}") |> Result.get_ok in`;
  }
  
  return nodeCreation;
}

function generateAssertions(nodeVar, node, useRounding) {
  const layout = useRounding ? node.smartRoundedLayout : node.unroundedLayout;
  const width = layout.width;
  const height = layout.height;
  const x = layout.x;
  const y = layout.y;
  
  // Handle negative values by wrapping in parentheses
  const formatFloat = (val) => {
    const strVal = String(val);
    const floatVal = strVal.includes('.') ? strVal : `${strVal}.0`;
    // If it's negative and doesn't have parentheses, add them
    return floatVal.startsWith('-') ? `(${floatVal})` : floatVal;
  };
  
  let assertions = `
  let layout = Toffee.layout tree ${nodeVar} in
  let layout = layout |> Result.get_ok in
  assert_eq ~msg:"width of ${nodeVar}" ${formatFloat(width)} layout.size.width;
  assert_eq ~msg:"height of ${nodeVar}" ${formatFloat(height)} layout.size.height;
  assert_eq ~msg:"x of ${nodeVar}" ${formatFloat(x)} layout.location.x;
  assert_eq ~msg:"y of ${nodeVar}" ${formatFloat(y)} layout.location.y;`;
  
  return assertions;
}

function generateTest(name, testData, boxSizing, category = '') {
  const data = testData;
  const useRounding = data.useRounding !== false;
  
  // Check if we have any text nodes
  let hasTextNodes = false;
  function checkForTextNodes(node) {
    if (node.textContent) {
      hasTextNodes = true;
    }
    if (node.children) {
      node.children.forEach(checkForTextNodes);
    }
  }
  checkForTextNodes(data);
  
  // Generate node creation and tree building
  const nodes = [];
  const assertions = [];
  let nodeCounter = 0;
  
  function processNode(node, parentVar) {
    const nodeVar = parentVar ? `node${nodeCounter++}` : 'node';
    nodes.push(generateNode(nodeVar, node, parentVar, boxSizing));
    
    if (parentVar) {
      nodes.push(`let _ = Toffee.add_child tree ${parentVar} ${nodeVar} |> Result.get_ok in`);
    }
    
    // Generate assertions for this node
    assertions.push(generateAssertions(nodeVar, node, useRounding));
    
    // Process children
    if (node.children) {
      node.children.forEach(child => processNode(child, nodeVar));
    }
  }
  
  processNode(data, null);
  
  // Get available space from viewport
  const viewport = data.viewport || { width: { unit: 'max-content' }, height: { unit: 'max-content' } };
  const availableSpace = `{
    width = ${availableSpaceToOCaml(viewport.width)};
    height = ${availableSpaceToOCaml(viewport.height)};
  }`;
  
  // Generate test function - note we need measure_function parameter if hasTextNodes
  const measureParam = hasTextNodes ? ' measure_function' : '';
  const functionPrefix = category ? `${category}_` : '';
  
  return `
let test_${functionPrefix}${name}_${boxSizing}${measureParam} () =
  (* Setup test helpers *)
  let assert_eq ~msg expected actual =
    let open Alcotest in
    check (float 0.001) msg expected actual
  in
  
  let tree = Toffee.create () in
  ${useRounding ? '' : 'let _ = Toffee.set_rounding_enabled tree false in'}
  
  (* Create nodes *)
  ${nodes.join('\n  ')}
  
  (* Compute layout *)
  ${hasTextNodes 
    ? `let _ = Toffee.compute_layout_with_measure tree node ${availableSpace} measure_function |> Result.get_ok in`
    : `let _ = Toffee.compute_layout tree node ${availableSpace} |> Result.get_ok in`
  }
  
  (* Print tree for debugging *)
  Printf.printf "\\nComputed tree:\\n";
  Toffee.print_tree tree node;
  Printf.printf "\\n";
  
  (* Verify layout *)${assertions.join('')}
  ()
`;
}

function generateMeasureFunction() {
  return `
(* Test measure function *)
let measure_function ~known_dimensions ~available_space _node_id node_context _style =
  match node_context with
  | Some (MeasureFunction.Fixed size) -> size
  | Some (MeasureFunction.Text text) ->
      (* Ahem font simulation: each character is 10x10 *)
      let h_width = 10.0 in
      let h_height = 10.0 in
      let zws = "\\u{200b}" in
      let lines = String.split_on_char (String.get zws 0) text in
      let min_line_length = List.fold_left max 0 (List.map String.length lines) in
      let max_line_length = List.fold_left (+) 0 (List.map String.length lines) in
      
      let inline_size = 
        match known_dimensions.Toffee.Geometry.width with
        | Some w -> w
        | None -> (
            match available_space.Toffee.Geometry.width with
            | Toffee.Style.Available_space.Min_content -> float_of_int min_line_length *. h_width
            | Toffee.Style.Available_space.Max_content -> float_of_int max_line_length *. h_width
            | Toffee.Style.Available_space.Definite inline_size -> 
                Float.min inline_size (float_of_int max_line_length *. h_width)
        ) |> Float.max (float_of_int min_line_length *. h_width)
      in
      
      let block_size =
        match known_dimensions.Toffee.Geometry.height with
        | Some h -> h
        | None ->
            let inline_line_length = int_of_float (Float.floor (inline_size /. h_width)) in
            let rec count_lines current_line_length line_count = function
              | [] -> line_count
              | line :: rest ->
                  let line_len = String.length line in
                  if current_line_length + line_len > inline_line_length then
                    if current_line_length > 0 then
                      count_lines line_len (line_count + 1) rest
                    else
                      count_lines line_len line_count rest
                  else
                    count_lines (current_line_length + line_len) line_count rest
            in
            float_of_int (count_lines 0 1 lines) *. h_height
      in
      { width = inline_size; height = block_size }
  | None -> { width = 0.0; height = 0.0 }
`;
}

async function processFixture(fixturePath) {
  const name = path.basename(fixturePath, '.html')
    .replace(/-/g, '_')
    .replace(/\./g, '_');
  
  console.log(`Processing ${name}...`);
  
  const browser = await chromium.launch({
    args: [
      '--force-device-scale-factor=1',
      '--hide-scrollbars'
    ]
  });
  const page = await browser.newPage();
  
  // Set consistent viewport and device scale
  await page.setViewportSize({ width: 1024, height: 768 });
  
  // Force device scale factor
  await page.evaluate(() => {
    window.devicePixelRatio = 1;
  });
  
  // Add a route to intercept CSS requests and serve from correct location
  await page.route('**/scripts/gentest/test_base_style.css', route => {
    const cssPath = path.join(__dirname, 'test_base_style.css');
    route.fulfill({ path: cssPath });
  });
  
  // Add a route to intercept test_helper.js requests  
  await page.route('**/scripts/gentest/test_helper.js', route => {
    const jsPath = path.join(__dirname, 'test_helper.js');
    route.fulfill({ path: jsPath });
  });
  
  // Load the fixture
  await page.goto(`file://${fixturePath}`);
  
  // Reset any CSS zoom or transforms
  await page.addStyleTag({
    content: `
      * {
        zoom: 1 !important;
        transform: none !important;
      }
      body {
        zoom: 1 !important;
        transform: none !important;
      }
    `
  });
  
  // Verify scrollbar settings
  const scrollbarWidth = await page.evaluate(() => {
    const el = document.createElement("div");
    el.style.cssText = "overflow:scroll; visibility:hidden; position:absolute;";
    document.body.appendChild(el);
    const width = el.offsetWidth - el.clientWidth;
    el.remove();
    return width;
  });
  
  if (scrollbarWidth > 0) {
    console.warn(`Warning: Scrollbars are taking up ${scrollbarWidth}px. Tests may be inaccurate.`);
    console.warn('On macOS, set "Show scrollbars" to "When scrolling" in System Preferences > Appearance');
  }
  
  // Inject test helper
  const testHelperPath = path.join(__dirname, 'test_helper.js');
  const testHelper = await fs.readFile(testHelperPath, 'utf-8');
  await page.addScriptTag({ content: testHelper });
  
  // Extract test data for both box models
  const testData = await page.evaluate(() => getTestData());
  const data = JSON.parse(testData);
  
  await browser.close();
  
  // If the data doesn't have separate box model data, use the same data for both
  if (data.borderBoxData && data.contentBoxData) {
    return { name, borderBoxData: data.borderBoxData, contentBoxData: data.contentBoxData };
  } else {
    // Fallback for simpler test format
    return { name, borderBoxData: data, contentBoxData: data };
  }
}

async function main() {
  // Check if a specific fixture path was provided
  const args = process.argv.slice(2);
  if (args.length > 0) {
    // Process single fixture
    const fixturePath = path.resolve(args[0]);
    try {
      const { name, borderBoxData, contentBoxData } = await processFixture(fixturePath);
      
      // Validate the test data
      if (!borderBoxData || !contentBoxData) {
        throw new Error(`Invalid test data: missing borderBoxData or contentBoxData for ${fixturePath}`);
      }
      
      // Generate both border-box and content-box tests
      const borderBoxTest = generateTest(name, borderBoxData, 'border_box');
      const contentBoxTest = generateTest(name, contentBoxData, 'content_box');
      
      if (!borderBoxTest || !contentBoxTest) {
        throw new Error(`Failed to generate test functions for ${fixturePath}`);
      }
    
    // Check if we have any text nodes
    let hasTextNodes = false;
    function checkForTextNodes(node) {
      if (node.textContent) hasTextNodes = true;
      if (node.children) node.children.forEach(checkForTextNodes);
    }
    checkForTextNodes(borderBoxData);
    checkForTextNodes(contentBoxData);
    
    const testModule = `
(* Generated test for ${name} *)
(* Do not edit this file directly. It is generated by gentest.js *)

open Toffee

${hasTextNodes ? `(* Test context for nodes *)
module MeasureFunction = struct
  type t = 
    | Fixed of float Toffee.Geometry.size
    | Text of string
  [@@warning "-37"]
end

${generateMeasureFunction()}
` : ''}

${borderBoxTest}

${contentBoxTest}

(* Export tests for aggregation *)
let tests =
  let open Alcotest in
  [
    test_case "${name} (border-box)" \`Quick ${hasTextNodes ? '(fun () -> test_' + name.replace(/-/g, '_') + '_border_box measure_function ())' : 'test_' + name.replace(/-/g, '_') + '_border_box'};
    test_case "${name} (content-box)" \`Quick ${hasTextNodes ? '(fun () -> test_' + name.replace(/-/g, '_') + '_content_box measure_function ())' : 'test_' + name.replace(/-/g, '_') + '_content_box'};
  ]
`;
    
    const outputName = `test_${name.replace(/-/g, '_')}.ml`;
    const outputPath = path.join(testDir, 'generated', outputName);
    await fs.mkdir(path.dirname(outputPath), { recursive: true });
    await fs.writeFile(outputPath, testModule);
    
      console.log(`Generated ${outputPath}`);
    } catch (err) {
      console.error(`ERROR processing ${fixturePath}:`, err.message);
      console.error(err.stack);
      process.exit(1);
    }
    return;
  }
  
  // Original behavior: process all fixtures
  const fixturesDir = path.join(testDir, 'fixtures');
  const categories = ['grid', 'flex', 'block'];
  
  // Create generated directory
  const generatedDir = path.join(testDir, 'generated');
  await fs.mkdir(generatedDir, { recursive: true });
  
  for (const category of categories) {
    const categoryDir = path.join(fixturesDir, category);
    
    try {
      const files = await fs.readdir(categoryDir);
      const htmlFiles = files.filter(f => f.endsWith('.html') && !f.startsWith('x'));
      
      if (htmlFiles.length === 0) continue;
      
      console.log(`\nProcessing ${category} category with ${htmlFiles.length} fixtures...`);
      
      // Track test cases for this category
      const categoryTestCases = [];
      
      // Process each fixture individually
      for (const file of htmlFiles) {
        const fixturePath = path.join(categoryDir, file);
        console.log(`  Processing ${file}...`);
        
        try {
          const { name, borderBoxData, contentBoxData } = await processFixture(fixturePath);
          
          // Validate the test data
          if (!borderBoxData || !contentBoxData) {
            throw new Error(`Invalid test data: missing borderBoxData or contentBoxData for ${file}`);
          }
          
          // Remove category prefix from name if it's already there
          const cleanName = name.startsWith(`${category}_`) ? name.substring(category.length + 1) : name;
          
          // Generate both border-box and content-box tests
          const borderBoxTest = generateTest(cleanName, borderBoxData, 'border_box', category);
          const contentBoxTest = generateTest(cleanName, contentBoxData, 'content_box', category);
          
          if (!borderBoxTest || !contentBoxTest) {
            throw new Error(`Failed to generate test functions for ${file}`);
          }
          
          // Check if we have any text nodes to determine if we need measure functions
          let hasTextNodes = false;
          function checkForTextNodes(node) {
            if (node.textContent) hasTextNodes = true;
            if (node.children) node.children.forEach(checkForTextNodes);
          }
          checkForTextNodes(borderBoxData);
          checkForTextNodes(contentBoxData);
          
          // Write individual test file without test runner
          const testModuleName = `test_${category}_${cleanName.replace(/-/g, '_')}`;
          const testName = cleanName.replace(/-/g, '_').replace(/\./g, '_');
          const testModule = `
(* Generated test for ${name} in ${category} layout *)
(* Do not edit this file directly. It is generated by gentest.js *)

open Toffee

${hasTextNodes ? `(* Test context for nodes *)
module MeasureFunction = struct
  type t = 
    | Fixed of float Toffee.Geometry.size
    | Text of string
  [@@warning "-37"]
end

${generateMeasureFunction()}
` : ''}

${borderBoxTest}

${contentBoxTest}

(* Export tests for aggregation *)
let tests =
  let open Alcotest in
  [
    test_case "${testName} (border-box)" \`Quick ${hasTextNodes ? '(fun () -> test_' + category + '_' + testName + '_border_box measure_function ())' : 'test_' + category + '_' + testName + '_border_box'};
    test_case "${testName} (content-box)" \`Quick ${hasTextNodes ? '(fun () -> test_' + category + '_' + testName + '_content_box measure_function ())' : 'test_' + category + '_' + testName + '_content_box'};
  ]
`;
          
          const outputPath = path.join(generatedDir, `${testModuleName}.ml`);
          await fs.writeFile(outputPath, testModule);
          console.log(`    Generated ${testModuleName}.ml`);
          
          // Track for summary
          categoryTestCases.push({
            name: testName,
            file: `${testModuleName}.ml`
          });
          
        } catch (err) {
          console.error(`    ERROR processing ${file}:`, err.message);
          console.error(err.stack);
          // Exit with error code to fail the build
          process.exit(1);
        }
      }
      
      // Print summary for this category
      if (categoryTestCases.length > 0) {
        console.log(`  Generated ${categoryTestCases.length} test files for ${category}`);
      }
      
    } catch (err) {
      if (err.code !== 'ENOENT') {
        console.error(`Error processing ${category}:`, err);
      }
    }
  }
  
  // Generate test runner and dune file
  console.log('\nGenerating test runner and dune file...');
  
  try {
    const generatedFiles = await fs.readdir(generatedDir);
    const testFiles = generatedFiles
      .filter(f => f.endsWith('.ml') && f.startsWith('test_') && f !== 'test_runner.ml')
      .map(f => f.replace('.ml', ''))
      .sort();
    
    if (testFiles.length > 0) {
      // Group test files by category
      const gridTests = testFiles.filter(f => f.startsWith('test_grid_'));
      const flexTests = testFiles.filter(f => f.startsWith('test_flex_'));
      const blockTests = testFiles.filter(f => f.startsWith('test_block_'));
      
      // Generate test_runner.ml that aggregates all tests grouped by category
      const testRunnerContent = `(* Generated test runner that aggregates all tests *)
(* Do not edit this file directly. It is generated by gentest.js *)

open Alcotest

(* Grid tests *)
let grid_tests =
  List.concat [
${gridTests.map(f => {
  const moduleName = f.charAt(0).toUpperCase() + f.slice(1);
  return `    ${moduleName}.tests;`;
}).join('\n')}
  ]

(* Flex tests *)
let flex_tests =
  List.concat [
${flexTests.map(f => {
  const moduleName = f.charAt(0).toUpperCase() + f.slice(1);
  return `    ${moduleName}.tests;`;
}).join('\n')}
  ]

(* Block tests *)
let block_tests =
  List.concat [
${blockTests.map(f => {
  const moduleName = f.charAt(0).toUpperCase() + f.slice(1);
  return `    ${moduleName}.tests;`;
}).join('\n')}
  ]

(* Run all tests grouped by category *)
let () =
  run "Toffee Layout Tests" [
    "grid", grid_tests;
    "flex", flex_tests;
    "block", block_tests;
  ]
`;
      
      await fs.writeFile(path.join(generatedDir, 'test_runner.ml'), testRunnerContent);
      console.log('Generated test_runner.ml');
      
      // Generate dune file
      const duneContent = `(test
 (name test_runner)
 (libraries toffee alcotest)
 (modules ${testFiles.join(' ')} test_runner))
`;
      
      await fs.writeFile(path.join(generatedDir, 'dune'), duneContent);
      console.log(`Generated dune file with ${testFiles.length} test modules`);
    }
  } catch (err) {
    console.error('Error generating test runner and dune file:', err);
  }
  
  console.log('\nTest generation complete!');
}

main().catch(console.error);