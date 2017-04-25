import { AxisType, mapAxisTypeToLabel } from '../Axis';
import { UPDATE_CELL_SIZE, MINIMUM_CELL_SIZE } from '../constants';

function getNewCellSize(size, offset, zoom) {
  // We divide the size by zoom so that cell sizes set when zoom is not 1 keep the same behaviour when zooming as cell using default size
  return Math.max((size + offset) / zoom, MINIMUM_CELL_SIZE);
}

export const updateCellSize = (
  {
    handle,
    offset,
    initialOffset,
    defaultCellSizes,
    sizes,
    zoom
  }
) => {
  let size;
  let direction;
  if (handle.axis === AxisType.COLUMNS && handle.position === 'right') {
    direction = 'leafs';
    size = getNewCellSize(
      sizes.leafs.columns[handle.id] || defaultCellSizes.width,
      offset.x - initialOffset.x,
      zoom
    );
  } else if (handle.axis === AxisType.ROWS && handle.position === 'bottom') {
    direction = 'leafs';
    size = getNewCellSize(
      sizes.leafs.rows[handle.id] || defaultCellSizes.height,
      offset.y - initialOffset.y,
      zoom
    );
  } else if (handle.axis === AxisType.COLUMNS && handle.position === 'bottom') {
    direction = 'dimensions';
    size = getNewCellSize(
      sizes.dimensions.columns[handle.id] || defaultCellSizes.height,
      offset.y - initialOffset.y,
      zoom
    );
  } else if (handle.axis === AxisType.ROWS && handle.position === 'right') {
    direction = 'dimensions';
    size = getNewCellSize(
      sizes.dimensions.rows[handle.id] || defaultCellSizes.width,
      offset.x - initialOffset.x,
      zoom
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

export const setCellSize = ({ cell, size, direction, zoom }) => ({
  type: UPDATE_CELL_SIZE,
  id: cell.key,
  size: getNewCellSize(size, 0, zoom),
  axis: mapAxisTypeToLabel(cell.axisType),
  direction
});
