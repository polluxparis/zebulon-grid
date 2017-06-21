import React from "react";

import { AxisType } from "../../Axis";
import { MEASURE_ID, TOTAL_ID } from "../../constants";
import ResizeHandle from "../ResizeHandle";

function getLeafSubheaders(header, result) {
  if (header.subheaders && header.subheaders.length) {
    header.subheaders.forEach(subheader =>
      getLeafSubheaders(subheader, result)
    );
    return result;
  }
  result.push(header);
  return result;
}

const Header = ({
  axis,
  getLastChildSize,
  gridId,
  header,
  positionStyle,
  previewSizes,
  span,
  startIndex,
  scrollLeft,
  scrollTop
}) => {
  const { left, top, width, height } = positionStyle;
  const { x, y } = header;
  let style = { height, width };
  // Handle affix
  if (span > 1 && x <= startIndex) {
    let offset;
    const lastChildSize = getLastChildSize(header);
    if (axis === AxisType.COLUMNS) {
      offset = Math.min(scrollLeft - left, width - (lastChildSize || 0));
      style = { position: "relative", left: offset };
    } else {
      offset = Math.min(scrollTop - top, height - (lastChildSize || 0));
      style = { position: "relative", top: offset };
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
  } else if (header.dim.dimension) {
    // Normal header
    dimensionId = header.dim.dimension.id;
  } else {
    // Total header
    dimensionId = TOTAL_ID;
  }
  const leafSubheaders = header.subheaders ? getLeafSubheaders(header, []) : [];
  return (
    <div
      key={`fixed-${axis}-${x}-${y}`}
      className="pivotgrid-cell pivotgrid-header pivotgrid-column-header"
      style={{
        boxSizing: "border-box",
        overflow: "hidden",
        // border: 'solid lightgrey thin',
        // backgroundColor: '#eef8fb',
        zIndex: 1,
        display: "flex",
        ...positionStyle
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
        gridId={gridId}
      />
      <ResizeHandle
        position="bottom"
        size={width}
        id={axis === AxisType.ROWS ? leafHeaderId : dimensionId}
        gridId={gridId}
        leafSubheaders={leafSubheaders}
        axis={axis}
        previewSize={previewSizes.width}
      />
    </div>
  );
};

const InnerHeader = ({ cell, style }) => {
  let value;
  switch (cell.template) {
    case "cell-template-row-header":
    case "cell-template-column-header":
      value = cell.format(cell.caption);
      break;
    case "cell-template-dataheader":
    case "cell-template-dimensionheader":
      value = cell.value.caption;
      break;
    default:
      break;
  }

  const computedStyle = {
    whiteSpace: "nowrap",
    overflow: "hidden",
    ...style
  };

  return (
    <div className="pivotgrid-header-inner" style={computedStyle}>
      {value}
    </div>
  );
};

export default Header;
