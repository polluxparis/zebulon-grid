import { connect } from 'react-redux';

import { AxisType } from '../constants';
import {
  getLastChildHeightSelector,
  getLayoutSelector,
  previewSizesSelector,
  getCellHeightByKeySelector,
  rowLeavesSelector,
  getAxisActivatedMeasuresSelector,
  availableMeasuresSelector,
  filteredDataSelector,
  rowDimensionsSelector,
  rowsVisibleHeightSelector,
  rowHeadersWidthSelector,
  getSelectedRowRangeSelector,
  crossPositionsSelector,
  getRowDimensionWidthSelector,
  getRowHeightSelector
} from '../selectors';
import Headers from '../components/Headers/Headers';
import {
  toggleCollapse,
  selectRange,
  moveDimension,
  toggleMeasure
} from '../actions';

const mapStateToProps = (state, ownProps) => {
  const leaves = rowLeavesSelector(state);

  return {
    axisType: AxisType.ROWS,
    data: filteredDataSelector(state),
    dimensions: rowDimensionsSelector(state),
    measures: getAxisActivatedMeasuresSelector(AxisType.ROWS)(state),
    availableMeasures: availableMeasuresSelector(state),
    columnCount: getLayoutSelector(state).rowHorizontalCount,
    getColumnWidth: getRowDimensionWidthSelector(state),
    getRowHeight: getRowHeightSelector(state),
    getSizeByKey: getCellHeightByKeySelector(state),
    crossPositions: crossPositionsSelector(state)[AxisType.ROWS],
    height: rowsVisibleHeightSelector(state),
    width: rowHeadersWidthSelector(state),
    previewSizes: previewSizesSelector(state),
    rowCount: getLayoutSelector(state).rowVerticalCount,
    getLastChildSize: getLastChildHeightSelector(state),
    leaves,
    sizes: state.sizes,
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
    dispatch(moveDimension(dimensionId, oldAxis, newAxis, position)),
  toggleMeasure: measureId => dispatch(toggleMeasure(measureId))
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
