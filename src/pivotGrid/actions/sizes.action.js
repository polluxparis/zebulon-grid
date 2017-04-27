import { AxisType, mapAxisTypeToLabel } from '../Axis';
import { UPDATE_CELL_SIZE, MINIMUM_CELL_SIZE } from '../constants';

function getNewCellSize(size, offset) {
  return Math.max(size + offset, MINIMUM_CELL_SIZE);
}

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

export const setCellSize = ({ cell, size, direction }) => ({
  type: UPDATE_CELL_SIZE,
  id: cell.key,
  size,
  axis: mapAxisTypeToLabel(cell.axisType),
  direction
});
