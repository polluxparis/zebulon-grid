import { AxisType, toAxis } from '../Axis';
import { UPDATE_CELL_SIZE } from '../constants';

function getNewCellSize(size, offset) {
  return Math.max(size + offset, 10);
}

// const mapAxisTypeToLabel = axisType => {
//   switch (axisType) {
//     case AxisType.COLUMNS:
//       return 'columns';
//     case AxisType.ROWS:
//       return 'rows';
//     default:
//       throw new Error(`Axis type ${axisType} not supported`);
//   }
// };

/* eslint-disable import/prefer-default-export */
export const updateCellSize = ({
  handle,
  offset,
  initialOffset,
  defaultCellSizes,
  sizes
}) => {
  let size;
  let direction;
  if (handle.axis === AxisType.COLUMNS && handle.position === 'right') {
    direction = 'widths';
    size = getNewCellSize(
      sizes.widths[handle.id] || defaultCellSizes.width,
      offset.x - initialOffset.x
    );
  } else if (handle.axis === AxisType.COLUMNS && handle.position === 'bottom') {
    direction = 'heights';
    size = getNewCellSize(
      sizes.heights[handle.id] || defaultCellSizes.height,
      offset.y - initialOffset.y
    );
  } else if (handle.axis === AxisType.ROWS && handle.position === 'right') {
    direction = 'widths';
    size = getNewCellSize(
      sizes.widths[handle.id] || defaultCellSizes.width,
      offset.x - initialOffset.x
    );
  } else if (handle.axis === AxisType.ROWS && handle.position === 'bottom') {
    direction = 'heights';
    size = getNewCellSize(
      sizes.heights[handle.id] || defaultCellSizes.height,
      offset.y - initialOffset.y
    );
  }
  return {
    type: UPDATE_CELL_SIZE,
    id: handle.id,
    size,
    axis: toAxis(handle.axis),
    direction
  };
};
/* eslint-enable */
