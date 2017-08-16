import { connect } from 'react-redux';
import { DragDropContext } from 'react-dnd';
import HTML5Backend from 'react-dnd-html5-backend';

import PivotGrid from '../components/PivotGrid/PivotGrid';
import {
  activatedMeasuresSelector,
  columnDimensionsSelector,
  columnLeavesSelector,
  defaultCellSizesSelector,
  getCellDimensionInfosSelector,
  getCellValueSelector,
  getLayoutSelector,
  rowDimensionsSelector,
  rowLeavesSelector,
  selectedRangeSelector
} from '../selectors';
import {
  updateCellSize,
  setConfigProperty,
  selectRange,
  selectCell,
  zoom
} from '../actions';
import copy from '../services/copyService';

const mapStateToProps = state => {
  const rowLeaves = rowLeavesSelector(state);
  const columnLeaves = columnLeavesSelector(state);
  const rowDimensions = rowDimensionsSelector(state);
  const columnDimensions = columnDimensionsSelector(state);
  const measures = activatedMeasuresSelector(state);
  const getCellValue = getCellValueSelector(state);
  return {
    status: state.config.status,
    width: state.config.width,
    layout: getLayoutSelector(state),
    defaultCellSizes: defaultCellSizesSelector(state),
    sizes: state.sizes,
    columnLeaves,
    rowLeaves,
    selectedRange: selectedRangeSelector(state),
    copy: selectedRange =>
      copy({
        selectedRange,
        columnLeaves,
        rowLeaves,
        rowDimensions,
        columnDimensions,
        measures,
        getCellValue,
        getCellDimensionInfos: getCellDimensionInfosSelector(state)
      })
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
    if (height) dispatch(setConfigProperty({ height, width }, 'height'));
    if (width) dispatch(setConfigProperty({ height, width }, 'width'));
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
    layout,
    headerSizes,
    sizes,
    defaultCellSizes,
    selectedRange,
    copy
  },
  { updateCellSize, setSizes, selectRange, selectCell, zoom },
  ownProps
) => ({
  status,
  width,
  layout,
  updateCellSize: ({ handle, offset, initialOffset }) =>
    updateCellSize({ handle, offset, initialOffset, sizes, defaultCellSizes }),
  setSizes,
  selectRange,
  selectCell,
  selectedRange,
  zoom,
  copy,
  ...ownProps
});

export const PivotGridWithoutDndContext = connect(
  mapStateToProps,
  mapDispatchToProps,
  mergeProps
)(PivotGrid);

export default DragDropContext(HTML5Backend)(PivotGridWithoutDndContext);
