import React from 'react';

import { AxisType } from '../../Axis';
import ResizeHandle from '../ResizeHandle';

const DimensionHeader = ({
  field,
  left,
  right,
  top,
  bottom,
  height,
  width,
  crossFieldCode,
  mainDirection,
  previewSizes,
 }) => {
  const ids = {};
  if (mainDirection === 'down') {
    ids.right = field.code;
    ids.bottom = crossFieldCode;
  } else {
    ids.bottom = field.code;
    ids.right = crossFieldCode;
  }
  return (
    <div
      key={`fixed-dim-${field.code}`}
      className={'OrbGrid-cell'}
      style={{
        position: 'absolute',
        left: left && left,
        right: right && right,
        bottom: bottom && bottom,
        top: top && top,
        width,
        height,
        zIndex: 3,
        border: 'lightgrey 0.1em solid',
        boxSizing: 'border-box',
        textAlign: 'left',
        display: 'flex',
        backgroundColor: '#fafad2',
      }}
    >
      <span>{ field.caption }</span>
      <ResizeHandle
        position="right"
        size={height}
        id={ids.right}
        isOnDimensionHeader
        axis={AxisType.ROWS}
        previewSize={previewSizes.height}
        previewOffset={top}
      />
      <ResizeHandle
        position="bottom"
        size={width}
        id={ids.bottom}
        isOnDimensionHeader
        axis={AxisType.COLUMNS}
        previewSize={previewSizes.width}
        previewOffset={left}
      />
    </div>
  );
};

export default DimensionHeader;
