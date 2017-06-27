import React from 'react';

import { AxisType } from '../../Axis';
import ResizeHandle from '../ResizeHandle';

const DimensionHeader = ({
  dimensionId,
  caption,
  left,
  top,
  height,
  width,
  crossDimensionId,
  mainDirection,
  previewSizes,
  gridId
}) => {
  const ids = {};
  if (mainDirection === 'down') {
    ids.right = dimensionId;
    ids.bottom = crossDimensionId;
  } else {
    ids.bottom = dimensionId;
    ids.right = crossDimensionId;
  }
  return (
    <div
      key={`fixed-dim-${dimensionId}`}
      className="pivotgrid-cell pivotgrid-dimension-header"
      style={{
        position: 'absolute',
        left,
        top,
        width,
        height,
        zIndex: 3,
        boxSizing: 'border-box',
        display: 'flex'
      }}
    >
      <span className="pivotgrid-dimension-header-inner">
        {caption}
      </span>
      <ResizeHandle
        position="right"
        size={height}
        id={ids.right}
        axis={AxisType.ROWS}
        gridId={gridId}
        previewSize={previewSizes.height}
      />
      <ResizeHandle
        position="bottom"
        size={width}
        gridId={gridId}
        id={ids.bottom}
        axis={AxisType.COLUMNS}
        previewSize={previewSizes.width}
      />
    </div>
  );
};

export default DimensionHeader;
