import { connect } from "react-redux";
import { DragDropContext } from "react-dnd";
import HTML5Backend from "react-dnd-html5-backend";

import PivotGrid from "../components/PivotGrid/PivotGrid";
import {
  rowHeadersSelector,
  columnHeadersSelector,
  selectedRangeSelector,
  copySelector,
  defaultCellSizesSelector
} from "../selectors";
import {
  updateCellSize,
  setConfigProperty,
  selectRange,
  selectCell,
  scrollToIndex,
  zoom
} from "../actions";

const mapStateToProps = state => {
  return {
    status: state.status,
    width: state.config.width,
    height: state.config.height,
    rows: rowHeadersSelector(state),
    columns: columnHeadersSelector(state),
    sizes: state.sizes,
    selectedRange: selectedRangeSelector(state),
    zoomValue: state.config.zoom,
    copy: copySelector(state),
    defaultCellSizes: defaultCellSizesSelector(state)
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
  selectRange: selectedRange => dispatch(selectRange(selectedRange)),
  selectCell: cell => dispatch(selectCell(cell)),
  zoom: type => dispatch(zoom(type)),
  scrollToRow: scroll => dispatch(scrollToIndex(scroll, null)),
  scrollToColumn: scroll => dispatch(scrollToIndex(null, scroll))
});

const mergeProps = (
  { sizes, defaultCellSizes, ...restStateProps },
  { updateCellSize, ...restDispatchProps },
  ownProps
) => ({
  updateCellSize: ({ handle, offset, initialOffset }) =>
    updateCellSize({ handle, offset, initialOffset, sizes, defaultCellSizes }),
  ...restStateProps,
  ...restDispatchProps,
  ...ownProps
});

export const GridWithoutStoreAndDndContext = connect(
  mapStateToProps,
  mapDispatchToProps,
  mergeProps
)(PivotGrid);

export default DragDropContext(HTML5Backend)(GridWithoutStoreAndDndContext);
