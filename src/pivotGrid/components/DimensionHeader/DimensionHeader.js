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
      className="orb-cell orb-dimension-header"
      style={{
        position: 'absolute',
        left,
        top,
        width,
        height,
        zIndex: 3,
        // border: 'lightgrey 0.1em solid',
        boxSizing: 'border-box',
        // textAlign: 'left',
        display: 'flex'
        // backgroundColor: '#fafad2',
      }}
    >
      <span className="orb-dimension-header-inner">
        {field.caption}
      </span>
      <ResizeHandle
        position="right"
        size={height}
        id={ids.right}
        isOnDimensionHeader
        axis={AxisType.ROWS}
        gridId={gridId}
        previewSize={previewSizes.height}
      />
      <ResizeHandle
        position="bottom"
        size={width}
        gridId={gridId}
        id={ids.bottom}
        isOnDimensionHeader
        axis={AxisType.COLUMNS}
        previewSize={previewSizes.width}
      />
    </div>
  );
};

export default DimensionHeader;
