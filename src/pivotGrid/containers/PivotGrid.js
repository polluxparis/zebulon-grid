import { connect } from 'react-redux';
import { DragDropContext } from 'react-dnd';
import HTML5Backend from 'react-dnd-html5-backend';

import PivotGrid from '../components/PivotGrid/PivotGrid';
import {
  activatedMeasuresSelector,
  columnDimensionsSelector,
  // columnHeadersSelector,
  columnLeavesSelector,
  defaultCellSizesSelector,
  getCellDimensionInfosSelector,
  getCellSizes,
  getCellValueSelector,
  getLayoutSelector,
  rowDimensionsSelector,
  // rowHeadersSelector,
  rowLeavesSelector,
  selectedRangeSelector,
  availableDimensionsSelector
} from '../selectors';
import {
  updateCellSize,
  setConfigProperty,
  selectRange,
  selectCell
} from '../actions';
import copy from '../services/copyService';
import { AxisType } from '../Axis';
import { isNullOrUndefined } from '../utils/generic';

const mapStateToProps = state => {
  const rowLeaves = rowLeavesSelector(state);
  const columnLeaves = columnLeavesSelector(state);
  const rowDimensions = rowDimensionsSelector(state);
  const columnDimensions = columnDimensionsSelector(state);
  const measures = activatedMeasuresSelector(state);
  const getCellValue = getCellValueSelector(state);
  return {
    width: state.config.width,
    layout: getLayoutSelector(state),
    defaultCellSizes: defaultCellSizesSelector(state),
    sizes: state.sizes,
    columnLeaves,
    rowLeaves,
    rowDimensions: rowDimensionsSelector(state),
    columnDimensions: columnDimensionsSelector(state),
    availableDimensions: availableDimensionsSelector(state),
    dataDimensionsCount: activatedMeasuresSelector(state).length,
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

// const mapDispatchToProps = dispatch => ({
//   updateCellSize: ({
//     handle,
//     offset,
//     initialOffset,
//     sizes,
//     defaultCellSizes
//   }) => {
//     if (
//       handle.leafSubheaders &&
//       handle.leafSubheaders.length &&
//       ((handle.axis === AxisType.COLUMNS && handle.position === 'right') ||
//         (handle.axis === AxisType.ROWS && handle.position === 'bottom'))
//     ) {
//       const fractionalOffset = {
//         x: (offset.x - initialOffset.x) / handle.leafSubheaders.length,
//         y: (offset.y - initialOffset.y) / handle.leafSubheaders.length
//       };
//       handle.leafSubheaders.forEach(subheader => {
//         dispatch(
//           updateCellSize({
//             handle: { ...handle, leafSubheaders: [], id: subheader.key },
//             offset: fractionalOffset,
//             initialOffset: { x: 0, y: 0 },
//             defaultCellSizes,
//             sizes
//           })
//         );
//       });
//     } else {
//       dispatch(
//         updateCellSize({
//           handle,
//           offset,
//           initialOffset,
//           sizes,
//           defaultCellSizes
//         })
//       );
//     }
//   },
//   setSizes: ({ height, width }) => {
//     if (height) dispatch(setConfigProperty({ height, width }, 'height'));
//     if (width) dispatch(setConfigProperty({ height, width }, 'width'));
//   }
// });
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
  }
});
const mergeProps = (
  {
    width,
    layout,
    headerSizes,
    sizes,
    defaultCellSizes,
    columnLeaves,
    rowLeaves,
    rowDimensions,
    columnDimensions,
    availableDimensions,
    dataDimensionsCount,
    selectedRange,
    copy
  },
  { updateCellSize, setSizes, selectRange, selectCell },
  ownProps
) => ({
  rowDimensions,
  columnDimensions,
  availableDimensions,
  columnLeaves,
  rowLeaves,
  width,
  layout,
  dataDimensionsCount,
  updateCellSize: ({ handle, offset, initialOffset }) =>
    updateCellSize({ handle, offset, initialOffset, sizes, defaultCellSizes }),
  setSizes,
  selectRange,
  selectCell,
  selectedRange,
  copy,
  ...ownProps
});

export const PivotGridWithoutDndContext = connect(
  mapStateToProps,
  mapDispatchToProps,
  mergeProps
)(PivotGrid);

export default DragDropContext(HTML5Backend)(PivotGridWithoutDndContext);
