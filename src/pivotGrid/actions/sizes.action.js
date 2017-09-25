import { UPDATE_CELL_SIZE, SET_SIZES } from "../constants";
import { isNullOrUndefined } from "../utils/generic";
import { getLeaves } from "../utils/headers";

function calculateSize(size, ratio) {
  return Math.max(size * ratio, 10);
}
const getInitialSize = (sizes, defaultSize, leaves) =>
  leaves.reduce((size, leaf) => size + (sizes[leaf.key] || defaultSize), 0);
export const updateCellSize = ({
  handle,
  offset,
  initialOffset,
  defaultCellSizes,
  sizes
}) => {
  let direction, ratio, initialSize, defaultSize, leaves, axisSizes;
  if (!isNullOrUndefined(handle.header)) {
    leaves = getLeaves(handle.header);
  }
  if (handle.position === "right") {
    direction = "widths";
    axisSizes = sizes.widths;
    defaultSize = defaultCellSizes.width;
    initialSize = isNullOrUndefined(leaves)
      ? axisSizes[handle.id] || defaultSize
      : getInitialSize(sizes.widths, defaultSize, leaves);
    ratio = (initialSize + offset.x - initialOffset.x) / initialSize;
  } else if (handle.position === "bottom") {
    direction = "heights";
    axisSizes = sizes.heights;
    defaultSize = defaultCellSizes.height;
    initialSize = isNullOrUndefined(leaves)
      ? axisSizes[handle.id] || defaultSize
      : getInitialSize(sizes.heights, defaultSize, leaves);
    ratio = (initialSize + offset.y - initialOffset.y) / initialSize;
  }
  let newSizes = {};
  if (isNullOrUndefined(handle.header)) {
    // Dimension size
    newSizes = { [handle.id]: calculateSize(initialSize, ratio) };
  } else {
    // Header size
    newSizes = leaves.reduce(
      (acc, hdr) => ({
        ...acc,
        [hdr.key]: calculateSize(axisSizes[hdr.key] || defaultSize, ratio)
      }),
      {}
    );
  }
  return {
    type: UPDATE_CELL_SIZE,
    sizes: newSizes,
    direction
  };
};
export const setSizes = sizes => {
  return {
    type: SET_SIZES,
    sizes
  };
};
