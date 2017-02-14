import React from 'react';

import { AxisType } from '../../Axis';
import { MEASURE_ID, TOTAL_ID } from '../../constants';
import ResizeHandle from '../ResizeHandle';

function getLeafSubheaders(header, result) {
  if (header.subheaders && header.subheaders.length) {
    header.subheaders.forEach(subheader =>
      getLeafSubheaders(subheader, result));
    return result;
  }
  result.push(header);
  return result;
}

const Header = (
  {
    axis,
    getLastChildSize,
    header,
    positionStyle,
    previewSizes,
    previewOffsets,
    span,
    startIndex,
    scrollLeft,
    scrollTop,
  },
) => {
  const { left, top, width, height } = positionStyle;
  const { x, y } = header;
  // Handle affix
  let style;
  if (span > 1 && x <= startIndex) {
    let offset;
    const lastChildSize = getLastChildSize(axis, header);
    if (axis === AxisType.COLUMNS) {
      offset = Math.min(scrollLeft - left, width - (lastChildSize || 0));
      style = { position: 'relative', left: offset };
    } else {
      offset = Math.min(scrollTop - top, height - (lastChildSize || 0));
      style = { position: 'relative', top: offset };
    }
  }
  const innerHeader = (
    <InnerHeader key={`${axis}-${x}-${y}`} cell={header} style={style} />
  );
  const leafHeaderId = header.key;
  let dimensionId;
  if (!header.dim) {
    // Measure header
    dimensionId = MEASURE_ID;
  } else if (header.dim.field) {
    // Normal header
    dimensionId = header.dim.field.id;
  } else {
    // Total header
    dimensionId = TOTAL_ID;
  }
  const leafSubheaders = header.subheaders ? getLeafSubheaders(header, []) : [];
  return (
    <div
      key={`fixed-${axis}-${x}-${y}`}
      className="orb-cell orb-header orb-column-header"
      style={{
        boxSizing: 'border-box',
        overflow: 'hidden',
        // border: 'solid lightgrey thin',
        // backgroundColor: '#eef8fb',
        zIndex: 1,
        display: 'flex',
        ...positionStyle,
      }}
    >
      {innerHeader}
      <ResizeHandle
        position="right"
        size={height}
        id={axis === AxisType.COLUMNS ? leafHeaderId : dimensionId}
        leafSubheaders={leafSubheaders}
        axis={axis}
        previewSize={previewSizes.height}
        previewOffset={previewOffsets.right}
      />
      <ResizeHandle
        position="bottom"
        size={width}
        id={axis === AxisType.ROWS ? leafHeaderId : dimensionId}
        leafSubheaders={leafSubheaders}
        axis={axis}
        previewSize={previewSizes.width}
        previewOffset={previewOffsets.bottom}
      />
    </div>
  );
};

const InnerHeader = ({ cell, style }) => {
  let value;
  switch (cell.template) {
    case 'cell-template-row-header':
    case 'cell-template-column-header':
      value = cell.value;
      break;
    case 'cell-template-dataheader':
    case 'cell-template-dimensionheader':
      value = cell.value.caption;
      break;
    default:
      break;
  }

  const computedStyle = {
    whiteSpace: 'nowrap',
    overflow: 'hidden',
    ...style,
  };

  return (
    <span className="orb-header-inner" style={computedStyle}>
      {value}
    </span>
  );
};

export default Header;
