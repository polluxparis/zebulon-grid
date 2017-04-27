import { AxisType } from '../Axis';
import { UPDATE_CELL_SIZE } from '../constants';

function getNewCellSize(size, offset) {
  return Math.max(size + offset, 10);
}

const mapAxisTypeToLabel = axisType => {
  switch (axisType) {
    case AxisType.COLUMNS:
      return 'columns';
    case AxisType.ROWS:
      return 'rows';
    default:
      throw new Error(`Axis type ${axisType} not supported`);
  }
};

/* eslint-disable import/prefer-default-export */
export const updateCellSize = (
  {
    handle,
    offset,
    initialOffset,
    defaultCellSizes,
    sizes
  }
) => {
  let size;
  let direction;
  if (handle.axis === AxisType.COLUMNS && handle.position === 'right') {
    direction = 'leafs';
    size = getNewCellSize(
      sizes.leafs.columns[handle.id] || defaultCellSizes.width,
      offset.x - initialOffset.x
    );
  } else if (handle.axis === AxisType.ROWS && handle.position === 'bottom') {
    direction = 'leafs';
    size = getNewCellSize(
      sizes.leafs.rows[handle.id] || defaultCellSizes.height,
      offset.y - initialOffset.y
    );
  } else if (handle.axis === AxisType.COLUMNS && handle.position === 'bottom') {
    direction = 'dimensions';
    size = getNewCellSize(
      sizes.dimensions.columns[handle.id] || defaultCellSizes.height,
      offset.y - initialOffset.y
    );
  } else if (handle.axis === AxisType.ROWS && handle.position === 'right') {
    direction = 'dimensions';
    size = getNewCellSize(
      sizes.dimensions.rows[handle.id] || defaultCellSizes.width,
      offset.x - initialOffset.x
    );
  }
  return {
    type: UPDATE_CELL_SIZE,
    id: handle.id,
    size,
    axis: mapAxisTypeToLabel(handle.axis),
    direction
  };
};
/* eslint-enable */
