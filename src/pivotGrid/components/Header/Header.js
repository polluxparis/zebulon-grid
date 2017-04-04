import React, { PureComponent } from 'react';
import { AxisType } from '../../Axis';
import { MEASURE_ID, TOTAL_ID } from '../../constants';
import ResizeHandle from '../ResizeHandle';
import { isNumber } from '../../utils/generic';

function getLeafSubheaders(header, result) {
  if (header.subheaders && header.subheaders.length) {
    header.subheaders.forEach(subheader =>
      getLeafSubheaders(subheader, result));
    return result;
  }
  result.push(header);
  return result;
}

class Header extends PureComponent {
  constructor() {
    super();
    this.handleResizeCell = this.handleResizeCell.bind(this);
  }

  handleResizeCell() {
    const { resizeCell, header } = this.props;
    resizeCell(header);
  }

  render() {
    const {
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
    } = this.props;

    const { left, top, width, height } = positionStyle;
    const { x, y } = header;

    // Handle affix
    let affixStyle;
    // When measuring cell, width is set to 'auto'
    // No need for affix then
    if (span > 1 && x <= startIndex && isNumber(width)) {
      let offset;
      const lastChildSize = getLastChildSize(header);
      if (axis === AxisType.COLUMNS) {
        let offset = 0;
        offset = Math.min(scrollLeft - left, width - (lastChildSize || 0));
        affixStyle = { position: 'relative', left: offset };
      } else {
        offset = Math.min(scrollTop - top, height - (lastChildSize || 0));
        affixStyle = { position: 'relative', top: offset };
      }
    }
    const innerHeader = (
      <InnerHeader key={`${axis}-${x}-${y}`} cell={header} style={affixStyle} />
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
    const leafSubheaders = header.subheaders
      ? getLeafSubheaders(header, [])
      : [];

    return (
      <div
        key={`fixed-${axis}-${x}-${y}`}
        className="pivotgrid-cell pivotgrid-header pivotgrid-column-header"
        style={{
          boxSizing: 'border-box',
          overflow: 'hidden',
          zIndex: 1,
          display: 'flex',
          ...positionStyle
        }}
      >
        {innerHeader}
        <div
          style={{ height: 'inherit' }}
          onDoubleClick={this.handleResizeCell}
        >
          <ResizeHandle
            position="right"
            size={height}
            id={axis === AxisType.COLUMNS ? leafHeaderId : dimensionId}
            leafSubheaders={leafSubheaders}
            axis={axis}
            previewSize={previewSizes.height}
            gridId={gridId}
          />
        </div>
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
  }
}

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
    ...style
  };

  return (
    <span className="pivotgrid-header-inner" style={computedStyle}>
      {value}
    </span>
  );
};

export default Header;
