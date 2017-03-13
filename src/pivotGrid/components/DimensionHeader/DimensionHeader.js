import React from 'react';

import { AxisType } from '../../Axis';
import ResizeHandle from '../ResizeHandle';

const DimensionHeader = (
  {
    field,
    left,
    top,
    height,
    width,
    crossFieldId,
    mainDirection,
    previewSizes,
    gridId
  }
) => {
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
        {field.caption}
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
