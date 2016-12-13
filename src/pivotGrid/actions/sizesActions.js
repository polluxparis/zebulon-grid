import { AxisType } from '../Axis';
import { UPDATE_CELL_SIZE } from '../constants';

function getNewCellSize(size, offset) { return Math.max(size + offset, 10); }

export const updateCellSize = ({
  handle,
  offset,
  initialOffset,
  defaultCellSizes,
  sizes,
 }) => {
  let size;
  let axis;
  let direction;
  if (handle.isOnDimensionHeader) {
    direction = 'dimensions';
    if (handle.axis === AxisType.COLUMNS) {
      size = getNewCellSize(
        sizes.columns.dimensions[handle.id] || defaultCellSizes.height,
        offset.y - initialOffset.y);
      axis = 'columns';
    } else {
      size = getNewCellSize(sizes.rows.dimensions[handle.id] || defaultCellSizes.width,
      offset.x - initialOffset.x);
      axis = 'rows';
    }
  } else if (handle.axis === AxisType.COLUMNS && handle.position === 'right') {
    direction = 'leafs';
    axis = 'columns';
    size = getNewCellSize(
        sizes.columns.leafs[handle.id] || defaultCellSizes.width,
        offset.x - initialOffset.x);
  } else if (handle.axis === AxisType.ROWS && handle.position === 'bottom') {
    direction = 'leafs';
    axis = 'rows';
    size = getNewCellSize(
        sizes.rows.leafs[handle.id] || defaultCellSizes.height,
        offset.y - initialOffset.y);
  } else if (handle.axis === AxisType.COLUMNS && handle.position === 'bottom') {
    axis = 'columns';
    direction = 'dimensions';
    size = getNewCellSize(
        sizes.columns.dimensions[handle.id] || defaultCellSizes.height,
        offset.y - initialOffset.y);
  } else if (handle.axis === AxisType.ROWS && handle.position === 'right') {
    axis = 'rows';
    direction = 'dimensions';
    size = getNewCellSize(
        sizes.rows.dimensions[handle.id] || defaultCellSizes.width,
        offset.x - initialOffset.x);
  }
  return ({
    type: UPDATE_CELL_SIZE,
    id: handle.id,
    size,
    axis,
    direction,
  });
};
