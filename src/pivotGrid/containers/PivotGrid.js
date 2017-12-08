import { connect } from "react-redux";
import { DragDropContext } from "react-dnd";
import HTML5Backend from "react-dnd-html5-backend";

import PivotGrid from "../components/PivotGrid/PivotGrid";
import {
  rowAndColumnHeadersSelector,
  selectedRangeSelector,
  copySelector,
  exportSelector,
  pasteSelector,
  defaultCellSizesSelector
} from "../selectors";
import {
  updateCellSize,
  setConfigurationProperty,
  selectRange,
  selectCell,
  scrollToIndex,
  zoom,
  applyPushedData
} from "../actions";

const mapStateToProps = state => {
  return {
    status: state.status,
    width: state.configuration.width,
    height: state.configuration.height,
    headers: rowAndColumnHeadersSelector(state),
    sizes: state.sizes,
    selectedRange: selectedRangeSelector(state),
    zoomValue: state.configuration.zoom,
    copy: copySelector(state),
    paste: pasteSelector(state),
    export: exportSelector(state),
    defaultCellSizes: defaultCellSizesSelector(state),
    pushedData: state.data.pushedData,
    onEdit: state.configuration.callbacks.onEdit,
    editable:
      state.configuration.edition.activated &&
      state.configuration.edition.editable
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
    if (height) dispatch(setConfigurationProperty({ height, width }, "height"));
    if (width) dispatch(setConfigurationProperty({ height, width }, "width"));
  },
  selectRange: selectedRange => dispatch(selectRange(selectedRange)),
  selectCell: cell => dispatch(selectCell(cell)),
  zoom: type => dispatch(zoom(type)),
  scrollToRow: scroll => dispatch(scrollToIndex(scroll, null)),
  scrollToColumn: scroll => dispatch(scrollToIndex(null, scroll)),
  editData: data => dispatch(applyPushedData(data)),
  scrollToCell: () => {}
});

const mergeProps = (
  { sizes, defaultCellSizes, paste, onEdit, ...restStateProps },
  { updateCellSize, editData, ...restDispatchProps },
  ownProps
) => {
  return {
    updateCellSize: ({ handle, offset, initialOffset }) =>
      updateCellSize({
        handle,
        offset,
        initialOffset,
        sizes,
        defaultCellSizes
      }),
    paste: (clipboard, cell) => {
      const data = paste(clipboard, cell);
      editData(data);
      if (onEdit) {
        data.map(row => onEdit(row));
      }
    },
    ...restStateProps,
    ...restDispatchProps,
    ...ownProps
  };
};

export const GridWithoutStoreAndDndContext = connect(
  mapStateToProps,
  mapDispatchToProps,
  mergeProps
)(PivotGrid);

export default DragDropContext(HTML5Backend)(GridWithoutStoreAndDndContext);
