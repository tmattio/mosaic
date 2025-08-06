#!/usr/bin/env node

import { chromium } from 'playwright';
import fs from 'fs/promises';
import path from 'path';
import { fileURLToPath } from 'url';
import os from 'os';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const testDir = path.resolve(__dirname, '..');

// Convert style object to OCaml code
function styleToOCaml(style, boxSizing) {
  const params = [];

  // Display property - only set if explicitly specified since Toffee now defaults to Flex
  let displayMode = style.display || 'flex';  // Track display mode for later checks
  if (style.display) {
    const displayMap = {
      'grid': 'Grid',
      'flex': 'Flex',
      'block': 'Block',
      'none': 'None'
    };
    params.push(`~display:(Style.Display.${displayMap[style.display] || 'Flex'})`);
  };

  // Direction property
  if (style.direction) {
    const directionMap = {
      'rtl': 'Rtl',
      'ltr': 'Ltr'
    };
    if (directionMap[style.direction]) {
      params.push(`~direction:(Style.${directionMap[style.direction]})`);
    }
  }

  if (style.position) {
    const positionMap = {
      'relative': 'Relative',
      'absolute': 'Absolute'
    };
    params.push(`~position:(Style.Position.${positionMap[style.position] || 'Relative'})`);
  }

  // Grid properties
  if (style.gridTemplateColumns?.length > 0) {
    const tracks = style.gridTemplateColumns.map(trackToOCaml).join('; ');
    params.push(`~grid_template_columns:([${tracks}])`);
  }

  if (style.gridTemplateRows?.length > 0) {
    const tracks = style.gridTemplateRows.map(trackToOCaml).join('; ');
    params.push(`~grid_template_rows:([${tracks}])`);
  }

  if (style.gridAutoRows?.length > 0) {
    const tracks = style.gridAutoRows.map(t => nonRepeatedTrackToOCaml(t)).join('; ');
    params.push(`~grid_auto_rows:([${tracks}])`);
  }

  if (style.gridAutoColumns?.length > 0) {
    const tracks = style.gridAutoColumns.map(t => nonRepeatedTrackToOCaml(t)).join('; ');
    params.push(`~grid_auto_columns:([${tracks}])`);
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
    params.push(`~grid_auto_flow:(Style.Grid.Auto_flow.${flowMap[flowValue] || 'Row'})`);
  }

  // Handle grid placement - can be specified as gridColumn/gridRow or individual properties
  if (style.gridColumn || style.gridColumnStart || style.gridColumnEnd) {
    const start = style.gridColumn?.start || style.gridColumnStart || { kind: 'auto' };
    const end_ = style.gridColumn?.end || style.gridColumnEnd || { kind: 'auto' };
    const startOCaml = gridLineToOCaml(start);
    const endOCaml = gridLineToOCaml(end_);
    params.push(`~grid_column:({ start = ${startOCaml}; end_ = ${endOCaml} })`);
  }

  if (style.gridRow || style.gridRowStart || style.gridRowEnd) {
    const start = style.gridRow?.start || style.gridRowStart || { kind: 'auto' };
    const end_ = style.gridRow?.end || style.gridRowEnd || { kind: 'auto' };
    const startOCaml = gridLineToOCaml(start);
    const endOCaml = gridLineToOCaml(end_);
    params.push(`~grid_row:({ start = ${startOCaml}; end_ = ${endOCaml} })`);
  }

  // Flexbox properties
  if (style.flexDirection) {
    const dirMap = {
      'row': 'Row',
      'row-reverse': 'Row_reverse',
      'column': 'Column',
      'column-reverse': 'Column_reverse'
    };
    params.push(`~flex_direction:(Style.Flex_direction.${dirMap[style.flexDirection] || 'Row'})`);
  }

  if (style.flexWrap) {
    const wrapMap = {
      'nowrap': 'No_wrap',
      'wrap': 'Wrap',
      'wrap-reverse': 'Wrap_reverse'
    };
    params.push(`~flex_wrap:(Style.Flex_wrap.${wrapMap[style.flexWrap] || 'No_wrap'})`);
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
      params.push(`~align_items:(${value})`);
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
      params.push(`~align_self:(${value})`);
    }
  }

  // Only add justify_content for flex/grid layouts
  if (style.justifyContent && displayMode !== 'block') {
    const justifyMap = {
      'flex-start': 'Flex_start',
      'flex-end': 'Flex_end',
      'center': 'Center',
      'space-between': 'Space_between',
      'space-around': 'Space_around',
      'space-evenly': 'Space_evenly',
      'start': 'Start',
      'end': 'End',
      'stretch': 'Stretch'  // Add stretch support to match Rust implementation
    };
    const value = justifyMap[style.justifyContent];
    if (value) {
      params.push(`~justify_content:(${value})`);
    }
  }

  // Only add align_content for flex/grid layouts
  if (style.alignContent && displayMode !== 'block') {
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
    params.push(`~align_content:(${alignMap[style.alignContent] || 'Stretch'})`);
  }

  // Only add justify_items for grid layouts  
  if (style.justifyItems && displayMode === 'grid') {
    const justifyMap = {
      'stretch': 'Stretch',
      'flex-start': 'Flex_start',
      'flex-end': 'Flex_end',
      'center': 'Center',
      'baseline': 'Baseline',
      'start': 'Start',
      'end': 'End'
    };
    params.push(`~justify_items:(${justifyMap[style.justifyItems] || 'Stretch'})`);
  }

  // Only add justify_self for grid layouts
  if (style.justifySelf && style.justifySelf !== 'auto' && displayMode === 'grid') {
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
      params.push(`~justify_self:(${value})`);
    }
  }

  if (style.flexGrow !== undefined) {
    // Ensure flex_grow is a float
    const floatStr = String(style.flexGrow).includes('.') ? String(style.flexGrow) : `${style.flexGrow}.0`;
    const floatValue = floatStr.startsWith('-') ? `(${floatStr})` : floatStr;
    params.push(`~flex_grow:(${floatValue})`);
  }

  if (style.flexShrink !== undefined) {
    // Ensure flex_shrink is a float
    const floatStr = String(style.flexShrink).includes('.') ? String(style.flexShrink) : `${style.flexShrink}.0`;
    const floatValue = floatStr.startsWith('-') ? `(${floatStr})` : floatStr;
    params.push(`~flex_shrink:(${floatValue})`);
  }

  if (style.flexBasis) {
    params.push(`~flex_basis:(${dimensionToOCaml(style.flexBasis)})`);
  }

  // Size properties
  if (style.size) {
    const size = sizeToOCaml(style.size);
    if (size) params.push(`~size:(${size})`);
  }

  if (style.minSize) {
    const minSize = sizeToOCaml(style.minSize);
    if (minSize) params.push(`~min_size:(${minSize})`);
  }

  if (style.maxSize) {
    const maxSize = sizeToOCaml(style.maxSize);
    if (maxSize) params.push(`~max_size:(${maxSize})`);
  }

  if (style.aspectRatio !== undefined) {
    // Ensure aspect ratio is a float
    const floatStr = String(style.aspectRatio).includes('.') ? String(style.aspectRatio) : `${style.aspectRatio}.0`;
    const floatValue = floatStr.startsWith('-') ? `(${floatStr})` : floatStr;
    params.push(`~aspect_ratio:(${floatValue})`);
  }

  // Gap
  if (style.gap) {
    const gap = gapToOCaml(style.gap);
    if (gap) params.push(`~gap:(${gap})`);
  }

  // Margin/Padding/Border
  if (style.margin) {
    const margin = rectToOCaml(style.margin, 'margin');  // margin uses Length_percentage_auto
    if (margin) params.push(`~margin:(${margin})`);
  }

  if (style.padding) {
    const padding = rectToOCaml(style.padding, 'padding');  // padding uses Length_percentage
    if (padding) params.push(`~padding:(${padding})`);
  }

  if (style.border) {
    const border = rectToOCaml(style.border, 'border');  // border uses Length_percentage
    if (border) params.push(`~border:(${border})`);
  }

  // Inset properties (top, left, right, bottom)
  if (style.inset || style.top !== undefined || style.left !== undefined || 
      style.right !== undefined || style.bottom !== undefined) {
    const inset = {
      top: style.inset?.top ?? style.top,
      left: style.inset?.left ?? style.left,
      right: style.inset?.right ?? style.right,
      bottom: style.inset?.bottom ?? style.bottom
    };
    const insetOCaml = rectToOCaml(inset, 'inset');  // inset uses Length_percentage_auto
    if (insetOCaml) params.push(`~inset:(${insetOCaml})`);
  }

  // Overflow properties
  let hasNonVisibleOverflow = false;
  if (style.overflowX || style.overflowY) {
    const overflowMap = {
      'visible': 'Visible',
      'hidden': 'Hidden',
      'scroll': 'Scroll',
      'auto': 'Auto'  // 'auto' should map to Auto, not Scroll
    };

    const x = style.overflowX ? (overflowMap[style.overflowX] || 'Visible') : 'Visible';
    const y = style.overflowY ? (overflowMap[style.overflowY] || 'Visible') : 'Visible';

    if (x !== 'Visible' || y !== 'Visible') {
      hasNonVisibleOverflow = true;
      params.push(`~overflow:({ x = Style.Overflow.${x}; y = Style.Overflow.${y} })`);
    }
  }

  // Scrollbar width - only emit if at least one overflow axis is not visible (matches Rust behavior)
  if (hasNonVisibleOverflow && style.scrollbar_width !== undefined) {
    const floatStr = String(style.scrollbar_width).includes('.') ? String(style.scrollbar_width) : `${style.scrollbar_width}.0`;
    const floatValue = floatStr.startsWith('-') ? `(${floatStr})` : floatStr;
    params.push(`~scrollbar_width:(${floatValue})`);
  }

  // Text align
  if (style.textAlign) {
    const textAlignMap = {
      '-webkit-left': 'Legacy_left',
      '-webkit-right': 'Legacy_right',
      '-webkit-center': 'Legacy_center'
    };
    if (textAlignMap[style.textAlign]) {
      params.push(`~text_align:(Style.Text_align.${textAlignMap[style.textAlign]})`);
    }
  }

  // Box sizing - add for all nodes when test is content-box mode
  // In the Rust tests, when a test is run with content-box, ALL nodes get ContentBox
  if (boxSizing === 'content_box') {
    params.push('~box_sizing:Style.Box_sizing.Content_box');
  }

  return params.length > 0
    ? `Style.make ${params.join(' ')} ()`
    : 'Style.default';
}

function getTrackSizeValue(track) {
  // Helper to ensure value is a float
  const floatValue = (val) => {
    const strVal = String(val);
    const floatStr = strVal.includes('.') ? strVal : `${strVal}.0`;
    // Wrap negative values in parentheses for OCaml syntax
    return floatStr.startsWith('-') ? `(${floatStr})` : floatStr;
  };

  switch (track.unit) {
    case 'px':
    case 'points':
      return `(Style.Length_percentage.length ${floatValue(track.value)})`;
    case 'percent':
      return `(Style.Length_percentage.percent ${floatValue(track.value)})`;
    case 'fr':
    case 'fraction':
      return `(Style.Length_percentage.length 0.0)`; // Fr not valid for min/max in Length_percentage
    case 'auto':
      return `(Style.Length_percentage.length 0.0)`; // Auto maps to 0 for Length_percentage
    case 'min-content':
      return `(Style.Length_percentage.length 0.0)`; // Min-content maps to 0 for Length_percentage
    case 'max-content':
      return `(Style.Length_percentage.length 0.0)`; // Max-content maps to 0 for Length_percentage
    default:
      return `(Style.Length_percentage.length 0.0)`;
  }
}

function nonRepeatedTrackToOCaml(track) {
  // For grid_auto_rows/columns, we return just the record without Single constructor
  // Handle different track types from test_helper.js
  if (track.kind === 'scalar') {
    switch (track.unit) {
      case 'fraction':
      case 'fr':
        // For scalar fr tracks, min must be Auto (Fr only valid for max in Toffee)
        const frVal = String(track.value).includes('.') ? track.value : `${track.value}.0`;
        return `(Style.Grid.Track_sizing_function.fr ${frVal})`;
      case 'px':
      case 'points':
        // Length for both min and max - ensure float value
        const floatVal = String(track.value).includes('.') ? track.value : `${track.value}.0`;
        return `(Style.Grid.Track_sizing_function.length ${floatVal})`;
      case 'percent':
        // Percent for both min and max - ensure float value
        const percentVal = String(track.value).includes('.') ? track.value : `${track.value}.0`;
        return `(Style.Grid.Track_sizing_function.percent ${percentVal})`;
      case 'auto':
        return `Style.Grid.Track_sizing_function.auto`;
      case 'min-content':
        return `Style.Grid.Track_sizing_function.min_content`;
      case 'max-content':
        return `Style.Grid.Track_sizing_function.max_content`;
      default:
        return `Style.Grid.Track_sizing_function.auto`;
    }
  } else if (track.kind === 'function') {
    // Handle minmax() and fit-content() track sizing functions
    if (track.name === 'minmax' && track.arguments?.length === 2) {
      const min = track.arguments[0];
      const max = track.arguments[1];
      const minVal = getTrackSizeValue(min);
      const maxVal = getTrackSizeValue(max);
      return `(Style.Grid.Track_sizing_function.minmax ~min:${minVal} ~max:${maxVal})`;
    } else if (track.name === 'fit-content' && track.arguments?.length === 1) {
      // fit-content(limit) expects a Length_percentage.t, not a Grid type
      const arg = track.arguments[0];
      let limitValue;

      if (arg.unit === 'px' || arg.unit === 'points') {
        const floatVal = String(arg.value).includes('.') ? arg.value : `${arg.value}.0`;
        limitValue = `(Style.Length_percentage.length ${floatVal})`;
      } else if (arg.unit === 'percent') {
        const percentVal = String(arg.value).includes('.') ? arg.value : `${arg.value}.0`;
        limitValue = `(Style.Length_percentage.percent ${percentVal})`;
      } else {
        limitValue = `(Style.Length_percentage.length 0.0)`;
      }

      return `(Style.Grid.Track_sizing_function.fit_content ${limitValue})`;
    }
  }
  return `Style.Grid.Track_sizing_function.auto`;
}

function trackToOCaml(track) {
  // Handle repeat() function for grid templates
  if (track.kind === 'function' && track.name === 'repeat' && track.arguments?.length >= 2) {
    const repetitionArg = track.arguments[0];
    let repetition;

    if (repetitionArg.kind === 'keyword') {
      switch (repetitionArg.value) {
        case 'auto-fill':
          repetition = 'Style.Grid.Repetition_count.auto_fill';
          break;
        case 'auto-fit':
          repetition = 'Style.Grid.Repetition_count.auto_fit';
          break;
        default:
          repetition = 'Style.Grid.Repetition_count.count 1';
      }
    } else if (repetitionArg.kind === 'integer') {
      repetition = `Style.Grid.Repetition_count.count ${repetitionArg.value}`;
    } else {
      repetition = 'Style.Grid.Repetition_count.count 1';
    }

    // Process the track list (all arguments after the repetition)
    const tracks = track.arguments.slice(1).map(t => nonRepeatedTrackToOCaml(t)).join('; ');
    return `Style.Grid.Template_component.repeat (Style.Grid.Repetition.make ~count:(${repetition}) ~tracks:[${tracks}] ~line_names:[])`;
  }

  // For non-repeat tracks, wrap with Single constructor
  const nonRepeated = nonRepeatedTrackToOCaml(track);
  return `Style.Grid.Template_component.single ${nonRepeated}`;
}

// trackMinToOCaml is no longer needed with the new API

// trackMaxToOCaml is no longer needed with the new API

function gridLineToOCaml(line) {
  if (!line) return 'Style.Grid.Placement.auto';
  switch (line.tag || line.kind) {
    case 'auto': return 'Style.Grid.Placement.auto';
    case 'line': return `(Style.Grid.Placement.line (${line.value}))`;
    case 'span': return `(Style.Grid.Placement.span (${line.value}))`;
    default: return 'Style.Grid.Placement.auto';
  }
}

function dimensionToOCaml(dim) {
  if (!dim) return 'Style.Dimension.auto';

  // Helper to ensure value is a float
  const floatValue = (val) => {
    // Check if the value is already a float (has decimal point)
    const strVal = String(val);
    const floatStr = strVal.includes('.') ? strVal : `${strVal}.0`;
    // Wrap negative values in parentheses for OCaml syntax
    return floatStr.startsWith('-') ? `(${floatStr})` : floatStr;
  };

  switch (dim.unit) {
    case 'px': return `Style.Dimension.length ${floatValue(dim.value)}`;
    case 'points': return `Style.Dimension.length ${floatValue(dim.value)}`;
    case 'percent':
      // Ensure percent values are floats
      const percentStr = String(dim.value).includes('.') ? String(dim.value) : `${dim.value}.0`;
      // Wrap negative values in parentheses for OCaml syntax
      const percentVal = percentStr.startsWith('-') ? `(${percentStr})` : percentStr;
      return `Style.Dimension.percent ${percentVal}`;
    case 'auto': return 'Style.Dimension.auto';
    default: return 'Style.Dimension.auto';
  }
}

function sizeToOCaml(size) {
  if (!size) return null;
  const width = size.width ? dimensionToOCaml(size.width) : 'Style.Dimension.auto';
  const height = size.height ? dimensionToOCaml(size.height) : 'Style.Dimension.auto';
  return `{ width = ${width}; height = ${height} }`;
}

function gapToOCaml(gap) {
  if (!gap) return null;

  const convertGapValue = (value) => {
    if (!value) return 'Style.Length_percentage.length 0.0';

    // Helper to ensure value is a float
    const floatValue = (val) => {
      const strVal = String(val);
      const floatStr = strVal.includes('.') ? strVal : `${strVal}.0`;
      // Wrap negative values in parentheses for OCaml syntax
      return floatStr.startsWith('-') ? `(${floatStr})` : floatStr;
    };

    switch (value.unit) {
      case 'px':
      case 'points':
        return `Style.Length_percentage.length ${floatValue(value.value)}`;
      case 'percent':
        // Ensure percent values are floats
        const percentStr = String(value.value).includes('.') ? String(value.value) : `${value.value}.0`;
        // Wrap negative values in parentheses for OCaml syntax
        const percentVal = percentStr.startsWith('-') ? `(${percentStr})` : percentStr;
        return `Style.Length_percentage.percent ${percentVal}`;
      default:
        return 'Style.Length_percentage.length 0.0';
    }
  };

  // Use column/row keys as in the Rust version, but output as width/height
  const width = gap.column ? convertGapValue(gap.column) : 'Style.Length_percentage.length 0.0';
  const height = gap.row ? convertGapValue(gap.row) : 'Style.Length_percentage.length 0.0';
  return `{ width = ${width}; height = ${height} }`;
}

function rectToOCaml(rect, fieldType = 'default') {
  if (!rect) return null;

  // Helper to convert a single edge value
  const convertEdge = (edge) => {
    if (!edge) {
      // Default behavior based on field type:
      // - margin: missing values should be zero() = Length(0.0)
      // - inset: missing values should be auto() = Auto
      // - padding/border: missing values should be Length(0.0)
      if (fieldType === 'margin') {
        return 'Style.Length_percentage_auto.length 0.0';
      } else if (fieldType === 'inset') {
        return 'Style.Length_percentage_auto.auto';
      } else {
        return 'Style.Length_percentage.length 0.0';
      }
    }

    const prefix = (fieldType === 'margin' || fieldType === 'inset')
      ? 'Style.Length_percentage_auto'
      : 'Style.Length_percentage';

    // Helper to ensure value is a float
    const floatValue = (val) => {
      const strVal = String(val);
      const floatStr = strVal.includes('.') ? strVal : `${strVal}.0`;
      // Wrap negative values in parentheses for OCaml syntax
      return floatStr.startsWith('-') ? `(${floatStr})` : floatStr;
    };

    switch (edge.unit) {
      case 'px':
      case 'points':
        const lengthVal = floatValue(edge.value);
        // Wrap negative values in parentheses for OCaml syntax
        return `${prefix}.length ${lengthVal}`;
      case 'percent':
        // Ensure percent values are floats
        const percentStr = String(edge.value).includes('.') ? String(edge.value) : `${edge.value}.0`;
        // Wrap negative values in parentheses for OCaml syntax
        const percentVal = percentStr.startsWith('-') ? `(${percentStr})` : percentStr;
        return `${prefix}.percent ${percentVal}`;
      case 'auto':
        return (fieldType === 'margin' || fieldType === 'inset') ? `${prefix}.auto` : `${prefix}.length 0.0`;
      default:
        return `${prefix}.Length (0.0)`;
    }
  };

  const left = convertEdge(rect.left);
  const right = convertEdge(rect.right);
  const top = convertEdge(rect.top);
  const bottom = convertEdge(rect.bottom);

  return `{ left = ${left}; right = ${right}; top = ${top}; bottom = ${bottom} }`;
}

function availableSpaceToOCaml(space) {
  if (!space) return 'Available_space.Max_content';

  // Helper to ensure value is a float
  const floatValue = (val) => {
    const strVal = String(val);
    const floatStr = strVal.includes('.') ? strVal : `${strVal}.0`;
    // Wrap negative values in parentheses for OCaml syntax
    return floatStr.startsWith('-') ? `(${floatStr})` : floatStr;
  };

  switch (space.unit) {
    case 'points': return `Available_space.Definite ${floatValue(space.value)}`;
    // Match Rust behavior: min-content maps to MaxContent (likely a Taffy test suite simplification)
    case 'min-content': return 'Available_space.Max_content';
    case 'max-content': return 'Available_space.Max_content';
    default: return 'Available_space.Max_content';
  }
}

function generateNode(nodeVar, node, _parentVar, boxSizing) {
  const style = styleToOCaml(node.style, boxSizing);
  const textContent = node.textContent;
  const writingMode = node.style?.writingMode;
  // Note: aspect_ratio is read in Rust but not used in TestNodeContext

  let nodeCreation = `let ${nodeVar} = new_leaf tree (${style}) |> Result.get_ok in`;

  // Only attach text context on true leaf nodes (nodes without children) - matches Rust behavior
  const isLeafNode = !node.children || node.children.length === 0;
  if (textContent && isLeafNode) {
    // Set context for text nodes - pass the actual text content and writing mode
    // Note: The Rust version reads aspect_ratio but doesn't use it in the context
    // We match this behavior for test parity
    const escapedText = textContent.replace(/\\/g, '\\\\').replace(/"/g, '\\"');
    // Determine if vertical writing mode
    const isVertical = writingMode === 'vertical-rl' || writingMode === 'vertical-lr';
    const contextType = isVertical ? 'Text_vertical' : 'Text';
    nodeCreation += `\n  let _ = set_node_context tree ${nodeVar} (Some (MeasureFunction.${contextType} "${escapedText}")) |> Result.get_ok in`;
  }

  return nodeCreation;
}

function generateAssertions(nodeVar, node, useRounding) {
  const layout = useRounding ? node.smartRoundedLayout : node.unroundedLayout;
  const width = layout.width;
  const height = layout.height;
  const x = layout.x;
  const y = layout.y;

  // Check if this is a scroll container
  const isScrollContainer =
    (node.style?.overflowX && node.style.overflowX !== 'visible') ||
    (node.style?.overflowY && node.style.overflowY !== 'visible');

  // Handle negative values by wrapping in parentheses
  const formatFloat = (val) => {
    const strVal = String(val);
    const floatVal = strVal.includes('.') ? strVal : `${strVal}.0`;
    // If it's negative and doesn't have parentheses, add them
    return floatVal.startsWith('-') ? `(${floatVal})` : floatVal;
  };

  let assertions = `
  let layout_result = layout tree ${nodeVar} |> Result.get_ok in
  assert_eq ~msg:"width of ${nodeVar}" ${formatFloat(width)} (Layout.size layout_result).width;
  assert_eq ~msg:"height of ${nodeVar}" ${formatFloat(height)} (Layout.size layout_result).height;
  assert_eq ~msg:"x of ${nodeVar}" ${formatFloat(x)} (Layout.location layout_result).x;
  assert_eq ~msg:"y of ${nodeVar}" ${formatFloat(y)} (Layout.location layout_result).y;`;

  // Add scroll content size assertions for scroll containers
  if (isScrollContainer && layout.scrollWidth !== undefined && layout.scrollHeight !== undefined) {
    // Calculate scroll dimensions like the Rust version does
    // Use naivelyRoundedLayout for client dimensions to match Rust implementation
    const naiveLayout = node.naivelyRoundedLayout || layout;
    const scrollWidth = Math.max(0, (layout.scrollWidth || 0) - (naiveLayout.clientWidth || naiveLayout.width));
    const scrollHeight = Math.max(0, (layout.scrollHeight || 0) - (naiveLayout.clientHeight || naiveLayout.height));

    assertions += `
  (* Content size assertions for scroll container *)
  (* Note: In Toffee, scroll_width and scroll_height are functions, not fields *)
  assert_eq ~msg:"scroll_width of ${nodeVar}" ${formatFloat(scrollWidth)} (Layout.scroll_width layout_result);
  assert_eq ~msg:"scroll_height of ${nodeVar}" ${formatFloat(scrollHeight)} (Layout.scroll_height layout_result);`;
  }

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
  const nodeMap = new Map(); // Map to store node variable names

  // First pass: assign variable names to all nodes
  function assignNodeVars(node) {
    const nodeVar = `node${nodeCounter++}`;
    nodeMap.set(node, nodeVar);
    
    if (node.children) {
      node.children.forEach(child => assignNodeVars(child));
    }
  }

  // Second pass: generate code
  function generateNodeCode(node, parentVar) {
    const nodeVar = nodeMap.get(node);
    const hasChildren = node.children && node.children.length > 0;
    
    if (hasChildren) {
      // Generate children first
      node.children.forEach(child => {
        generateNodeCode(child, nodeVar);
      });
      
      // Now create the parent node with children
      const style = styleToOCaml(node.style, boxSizing);
      const childVars = node.children.map(child => nodeMap.get(child));
      const childrenArray = `[|${childVars.join('; ')}|]`;
      nodes.push(`let ${nodeVar} = new_with_children tree (${style}) ${childrenArray} |> Result.get_ok in`);
    } else {
      // Leaf node - use new_leaf
      nodes.push(generateNode(nodeVar, node, parentVar, boxSizing));
    }

    // Generate assertions for this node
    assertions.push(generateAssertions(nodeVar, node, useRounding));
  }

  assignNodeVars(data);
  generateNodeCode(data, null);
  
  // Get the root node variable
  const rootNodeVar = nodeMap.get(data);

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
    check (float ${useRounding ? `0.001` : `0.1`}) msg expected actual
  in
  
  let tree = new_tree () in
  ${useRounding ? '' : 'let tree = disable_rounding tree in'}
  
  (* Create nodes *)
  ${nodes.join('\n  ')}
  
  (* Compute layout *)
  ${hasTextNodes
      ? `let _ = compute_layout_with_measure tree ${rootNodeVar} ${availableSpace} measure_function |> Result.get_ok in`
      : `let _ = compute_layout tree ${rootNodeVar} ${availableSpace} |> Result.get_ok in`
    }
  
  (* Print tree for debugging *)
  Printf.printf "\\nComputed tree:\\n";
  print_tree tree ${rootNodeVar};
  Printf.printf "\\n";
  
  (* Verify layout *)${assertions.join('')}
  ()
`;
}

function generateMeasureFunction() {
  return `
(* Test measure function *)
let measure_function known_dimensions available_space _node_id node_context _style =
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
        match known_dimensions.Geometry.Size.width with
        | Some w -> w
        | None -> (
            match available_space.Geometry.Size.width with
            | Available_space.Min_content -> float_of_int min_line_length *. h_width
            | Available_space.Max_content -> float_of_int max_line_length *. h_width
            | Available_space.Definite inline_size -> 
                Float.min inline_size (float_of_int max_line_length *. h_width)
        ) |> Float.max (float_of_int min_line_length *. h_width)
      in
      
      let block_size =
        match known_dimensions.Geometry.Size.height with
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
  | Some (MeasureFunction.Text_vertical text) ->
      (* Vertical text: height is based on text length, width is based on available space *)
      let h_width = 10.0 in
      let h_height = 10.0 in
      let text_length = String.length text in
      
      let block_size = float_of_int text_length *. h_height in
      let inline_size = 
        match known_dimensions.Geometry.Size.width with
        | Some w -> w
        | None -> (
            match available_space.Geometry.Size.width with
            | Available_space.Min_content -> h_width
            | Available_space.Max_content -> h_width
            | Available_space.Definite w -> w
        )
      in
      { width = inline_size; height = block_size }
  | None -> { width = 0.0; height = 0.0 }
`;
}

// Global browser instance
let globalBrowser = null;

async function getBrowser() {
  if (!globalBrowser) {
    globalBrowser = await chromium.launch({
      args: [
        '--force-device-scale-factor=1',
        '--hide-scrollbars'
      ]
    });
  }
  return globalBrowser;
}

async function processFixture(fixturePath, page = null) {
  const name = path.basename(fixturePath, '.html')
    .replace(/-/g, '_')
    .replace(/\./g, '_');

  // Create page if not provided
  const shouldClosePage = !page;
  if (!page) {
    const browser = await getBrowser();
    page = await browser.newPage();
  }

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

  // Don't reset CSS zoom or transforms - match Rust version behavior

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
    console.error(`ERROR: Scrollbars are taking up ${scrollbarWidth}px. Tests will be inaccurate.`);
    console.error('On macOS, set "Show scrollbars" to "When scrolling" in System Preferences > Appearance');
    console.error('On Linux, configure your window manager to use overlay scrollbars');
    console.error('On Windows, this may require system-level configuration');
    if (shouldClosePage) await page.close();
    process.exit(1);
  }

  // Inject test helper
  const testHelperPath = path.join(__dirname, 'test_helper.js');
  const testHelper = await fs.readFile(testHelperPath, 'utf-8');
  await page.addScriptTag({ content: testHelper });

  // Extract test data for both box models
  const testData = await page.evaluate(() => {
    // getTestData is defined in the injected test_helper.js
    // @ts-ignore - getTestData is injected from test_helper.js
    if (typeof window.getTestData === 'function') {
      // @ts-ignore
      return window.getTestData();
    }
    throw new Error('getTestData function not found. Make sure test_helper.js is loaded correctly.');
  });
  const data = JSON.parse(testData);

  if (shouldClosePage) await page.close();

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
    | Fixed of float Geometry.size
    | Text of string
    | Text_vertical of string
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

      // Determine batch size based on CPU cores
      const cpuCount = os.cpus().length;
      const batchSize = Math.max(2, Math.min(cpuCount, 8)); // Between 2 and 8
      
      console.log(`  Processing ${htmlFiles.length} files in batches of ${batchSize}...`);

      // Create pages upfront for parallel processing
      const browser = await getBrowser();
      const pages = await Promise.all(
        Array(batchSize).fill(null).map(() => browser.newPage())
      );

      // Process fixtures in batches
      for (let i = 0; i < htmlFiles.length; i += batchSize) {
        const batch = htmlFiles.slice(i, i + batchSize);
        
        // Process batch in parallel, reusing pages
        const batchPromises = batch.map(async (file, index) => {
          const fixturePath = path.join(categoryDir, file);
          const page = pages[index];
          
          try {
            const { name, borderBoxData, contentBoxData } = await processFixture(fixturePath, page);

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
    | Fixed of float Geometry.size
    | Text of string
    | Text_vertical of string
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
          
          return {
            outputPath,
            testModule,
            testModuleName,
            testName,
            file,
            success: true
          };

        } catch (err) {
          console.error(`    ERROR processing ${file}:`, err.message);
          return {
            file,
            success: false,
            error: err
          };
        }
      });
      
      // Wait for batch to complete
      const batchResults = await Promise.all(batchPromises);
      
      // Write successful results to files
      for (const result of batchResults) {
        if (result.success) {
          await fs.writeFile(result.outputPath, result.testModule);
          console.log(`    Generated ${result.testModuleName}.ml`);
          
          // Track for summary
          categoryTestCases.push({
            name: result.testName,
            file: `${result.testModuleName}.ml`
          });
        } else {
          console.error(`    Failed: ${result.file}`);
          // Exit with error code to fail the build
          process.exit(1);
        }
      }
    }

      // Close pages after processing category
      await Promise.all(pages.map(page => page.close()));
      
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
  
  // Close the global browser if it was created
  if (globalBrowser) {
    await globalBrowser.close();
  }
}

main().catch(console.error).finally(async () => {
  // Ensure browser is closed on exit
  if (globalBrowser) {
    await globalBrowser.close();
  }
});