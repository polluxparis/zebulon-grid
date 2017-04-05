import React, { Component } from 'react';

import { AxisType } from '../../Axis';
import ResizeHandle from '../ResizeHandle';

class DimensionHeader extends Component {
  constructor() {
    super();
    this.handleResizeCell = this.handleResizeCell.bind(this);
  }
  handleResizeCell() {
    const {
      columnIndex,
      field,
      crossFieldId,
      resizeCell,
      mainDirection
    } = this.props;
    // If the main direction is down, get the cross field id
    // It's the field whose id must be changed in sizes object.
    const mockField = mainDirection === 'down' ? field : { id: crossFieldId };
    const mockHeader = {
      y: columnIndex,
      axisType: AxisType.ROWS,
      dim: { field: mockField }
    };
    resizeCell(mockHeader);
  }
  render() {
    const {
      field,
      style,
      crossFieldId,
      mainDirection,
      previewSizes,
      gridId
    } = this.props;
    const ids = {};
    if (mainDirection === 'down') {
      ids.right = field.id;
      ids.bottom = crossFieldId;
    } else {
      ids.bottom = field.id;
      ids.right = crossFieldId;
    }
    return (
      <div
        key={`fixed-dim-${field.id}`}
        className="pivotgrid-cell pivotgrid-dimension-header"
        style={{
          zIndex: 3,
          boxSizing: 'border-box',
          display: 'flex',
          ...style
        }}
      >
        <span
          className="pivotgrid-dimension-header-inner"
          style={{ whiteSpace: 'nowrap' }}
        >
          {field.caption}
        </span>
        <div
          style={{ height: 'inherit' }}
          onDoubleClick={this.handleResizeCell}
        >
          <ResizeHandle
            position="right"
            id={ids.right}
            axis={AxisType.ROWS}
            gridId={gridId}
            previewSize={previewSizes.height}
          />
        </div>
        <ResizeHandle
          position="bottom"
          gridId={gridId}
          id={ids.bottom}
          axis={AxisType.COLUMNS}
          previewSize={previewSizes.width}
        />
      </div>
    );
  }
}

export default DimensionHeader;
