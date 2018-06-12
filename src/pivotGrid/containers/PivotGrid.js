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
// const rowAndColumnHeadersSel = () => rowAndColumnHeadersSelector;
const mapStateToProps = () => (state, ownProps) => {
  // const rowAndColumnHeaders = rowAndColumnHeadersSel();
  const props = {
    gridId: ownProps.id || "pivotgrid",
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
  // console.log("map", props);
  return props;
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
  zoom: value => dispatch(zoom(value)),
  // scrollToCell: cell => dispatch(scrollToIndex(cell.rows, cell.columns)),
  scrollToRow: scroll => dispatch(scrollToIndex(scroll, null)),
  scrollToColumn: scroll => dispatch(scrollToIndex(null, scroll)),
  editData: data => dispatch(applyPushedData(data))
});

const mergeProps = (
  { sizes, defaultCellSizes, paste, onEdit, ...restStateProps },
  { updateCellSize, editData, ...restDispatchProps },
  ownProps
) => {
  // console.log("merge", restStateProps);

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
