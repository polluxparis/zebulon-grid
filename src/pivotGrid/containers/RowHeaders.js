import { connect } from 'react-redux';

import { AxisType } from '../Axis';
import {
  getLastChildHeight,
  getLayoutSelector,
  getPreviewSizes,
  getRowHeadersVisibleHeight,
  getCellWidthByKeySelector,
  getCellHeightByKeySelector,
  rowHeadersSelector,
  rowLeavesSelector,
  getAxisActivatedMeasuresSelector,
  filteredDataSelector,
  rowDimensionsSelector,
  rowsVisibleHeightSelector,
  rowHeadersWidthSelector,
  getSelectedRowRangeSelector,
  crossPositionsSelector
} from '../selectors';
import Headers from '../components/Headers';
import {
  toggleCollapse,
  selectRange,
  selectCell,
  moveDimension
} from '../actions';

const mapStateToProps = (state, ownProps) => {
  const rowDimensions = rowDimensionsSelector(state);
  const leaves = rowLeavesSelector(state);

  return {
    axisType: AxisType.ROWS,
    data: filteredDataSelector(state),
    dimensions: rowDimensionsSelector(state),
    measures: getAxisActivatedMeasuresSelector(AxisType.ROWS)(state),
    columnCount: getLayoutSelector(state).rowHorizontalCount,
    getColumnWidth: ({ index }) =>
      getCellWidthByKeySelector(state)(rowDimensions[index].id),
    getRowHeight: ({ index }) =>
      getCellHeightByKeySelector(state)(leaves[index].key),
    getSizeByKey: getCellHeightByKeySelector(state),
    crossPositions: crossPositionsSelector(state)[AxisType.ROWS],
    height: rowsVisibleHeightSelector(state),
    width: rowHeadersWidthSelector(state),
    previewSizes: getPreviewSizes(state),
    rowCount: getLayoutSelector(state).rowVerticalCount,
    headers: rowHeadersSelector(state),
    getLastChildSize: header => getLastChildHeight(state)(header),
    leaves,
    // getIsCollapsed: ({ index }) =>
    //   getIsCollapsedRowByKeySelector(state)(leaves[index].key),
    sizes: state.sizes,
    zoom: state.config.zoom,
    gridId: ownProps.gridId,
    getSelectedRowRange: getSelectedRowRangeSelector(state)
  };
};

const mapDispatchToProps = dispatch => ({
  toggleCollapse: key => {
    dispatch(toggleCollapse({ axisType: AxisType.ROWS, key }));
  },
  selectAxis: getSelectedRowRange => header => {
    const selectedRange = getSelectedRowRange(header);
    dispatch(selectRange(selectedRange));
  },
  moveDimension: (dimensionId, oldAxis, newAxis, position) =>
    dispatch(moveDimension(dimensionId, oldAxis, newAxis, position))
});

const mergeProps = (
  { getSelectedRowRange, ...restStateProps },
  { selectAxis, ...restDispatchProps },
  ownProps
) => ({
  selectAxis: selectAxis(getSelectedRowRange),
  ...restStateProps,
  ...restDispatchProps,
  ...ownProps
});

export default connect(mapStateToProps, mapDispatchToProps, mergeProps)(
  Headers
);
