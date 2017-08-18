import { connect } from 'react-redux';

import { AxisType } from '../constants';
import {
  getCellWidthByKeySelector,
  getCellHeightByKeySelector,
  columnDimensionsSelector,
  columnHeadersWidthSelector,
  getLastChildWidthSelector,
  getLayoutSelector,
  columnsVisibleWidthSelector,
  previewSizesSelector,
  columnLeavesSelector,
  getAxisActivatedMeasuresSelector,
  filteredDataSelector,
  getSelectedColumnRangeSelector,
  crossPositionsSelector,
  availableMeasuresSelector
} from '../selectors';
import {
  toggleCollapse,
  selectRange,
  // selectCell,
  moveDimension,
  toggleMeasure
} from '../actions';
import Headers from '../components/Headers/Headers';

const mapStateToProps = (state, ownProps) => {
  const columnDimensions = columnDimensionsSelector(state);
  const leaves = columnLeavesSelector(state);
  return {
    axisType: AxisType.COLUMNS,
    data: filteredDataSelector(state),
    dimensions: columnDimensionsSelector(state),
    measures: getAxisActivatedMeasuresSelector(AxisType.COLUMNS)(state),
    availableMeasures: availableMeasuresSelector(state),
    columnCount: getLayoutSelector(state).columnHorizontalCount,
    getColumnWidth: ({ index }) =>
      getCellWidthByKeySelector(state)(leaves[index].key),
    getRowHeight: ({ index }) =>
      getCellHeightByKeySelector(state)(columnDimensions[index].id),
    getSizeByKey: getCellWidthByKeySelector(state),
    crossPositions: crossPositionsSelector(state)[AxisType.COLUMNS],
    height: columnHeadersWidthSelector(state),
    width: columnsVisibleWidthSelector(state),
    previewSizes: previewSizesSelector(state),
    rowCount: getLayoutSelector(state).columnVerticalCount,
    getLastChildSize: header => getLastChildWidthSelector(state)(header),
    leaves,
    sizes: state.sizes,
    gridId: ownProps.gridId,
    getSelectedColumnRange: getSelectedColumnRangeSelector(state)
  };
};

const mapDispatchToProps = dispatch => ({
  toggleCollapse: key => {
    dispatch(toggleCollapse({ axisType: AxisType.COLUMNS, key }));
  },
  moveDimension: (dimensionId, oldAxis, newAxis, position) => {
    // dispatch(selectCell(null));
    dispatch(moveDimension(dimensionId, oldAxis, newAxis, position));
  },
  toggleMeasure: measureId => dispatch(toggleMeasure(measureId)),
  selectAxis: getSelectedColumnRange => header => {
    const selectedRange = getSelectedColumnRange(header);
    dispatch(selectRange(selectedRange));
  }
});

const mergeProps = (
  { getSelectedColumnRange, ...restStateProps },
  { selectAxis, ...restDispatchProps },
  ownProps
) => ({
  selectAxis: selectAxis(getSelectedColumnRange),
  ...restStateProps,
  ...restDispatchProps,
  ...ownProps
});
export default connect(mapStateToProps, mapDispatchToProps, mergeProps)(
  Headers
);
