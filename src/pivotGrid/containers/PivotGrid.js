import { connect } from "react-redux";
import { DragDropContext } from "react-dnd";
import HTML5Backend from "react-dnd-html5-backend";

import PivotGrid from "../components/PivotGrid/PivotGrid";
import {
  columnLeavesSelector,
  defaultCellSizesSelector,
  // layoutSelector,
  rowLeavesSelector,
  selectedRangeSelector,
  copySelector
} from "../selectors";
import {
  updateCellSize,
  setConfigProperty,
  selectRange,
  selectCell,
  zoom
} from "../actions";

const mapStateToProps = state => {
  const rowLeaves = rowLeavesSelector(state);
  const columnLeaves = columnLeavesSelector(state);
  return {
    status: state.status,
    width: state.config.width,
    // layout: layoutSelector(state),
    defaultCellSizes: defaultCellSizesSelector(state),
    sizes: state.sizes,
    columnLeaves,
    rowLeaves,
    selectedRange: selectedRangeSelector(state),
    zoomValue: state.config.zoom,
    copy: copySelector(state)
  };
};

const mapDispatchToProps = dispatch => ({
  updateCellSize: ({
    handle,
    offset,
    initialOffset,
    sizes,
    defaultCellSizes
  }) => {
    dispatch(
      updateCellSize({
        handle,
        offset,
        initialOffset,
        sizes,
        defaultCellSizes
      })
    );
  },
  setSizes: ({ height, width }) => {
    if (height) dispatch(setConfigProperty({ height, width }, "height"));
    if (width) dispatch(setConfigProperty({ height, width }, "width"));
  },
  selectRange: selectedRange => {
    dispatch(selectRange(selectedRange));
  },
  selectCell: cell => {
    dispatch(selectCell(cell));
  },
  zoom: type => {
    dispatch(zoom(type));
  }
});

const mergeProps = (
  {
    status,
    width,
    // layout,
    headerSizes,
    sizes,
    defaultCellSizes,
    selectedRange,
    copy,
    zoomValue
  },
  { updateCellSize, setSizes, selectRange, selectCell, zoom },
  ownProps
) => ({
  status,
  width,
  // layout,
  updateCellSize: ({ handle, offset, initialOffset }) =>
    updateCellSize({ handle, offset, initialOffset, sizes, defaultCellSizes }),
  setSizes,
  selectRange,
  selectCell,
  selectedRange,
  zoom,
  copy,
  zoomValue,
  ...ownProps
});

export const GridWithoutStoreAndDndContext = connect(
  mapStateToProps,
  mapDispatchToProps,
  mergeProps
)(PivotGrid);

export default DragDropContext(HTML5Backend)(GridWithoutStoreAndDndContext);
