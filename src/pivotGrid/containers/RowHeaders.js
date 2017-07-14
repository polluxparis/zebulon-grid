import { connect } from 'react-redux';

import { AxisType } from '../constants';
import {
  getLastChildHeightSelector,
  getLayoutSelector,
  previewSizesSelector,
  getCellWidthByKeySelector,
  getCellHeightByKeySelector,
  rowLeavesSelector,
  getAxisActivatedMeasuresSelector,
  availableMeasuresSelector,
  filteredDataSelector,
  rowDimensionsSelector,
  rowsVisibleHeightSelector,
  rowHeadersWidthSelector,
  getSelectedRowRangeSelector,
  crossPositionsSelector
} from '../selectors';
import Headers from '../components/Headers/Headers';
import {
  toggleCollapse,
  selectRange,
  moveDimension,
  toggleMeasure
} from '../actions';

const mapStateToProps = (state, ownProps) => {
  const rowDimensions = rowDimensionsSelector(state);
  const leaves = rowLeavesSelector(state);

  return {
    axisType: AxisType.ROWS,
    data: filteredDataSelector(state),
    dimensions: rowDimensionsSelector(state),
    measures: getAxisActivatedMeasuresSelector(AxisType.ROWS)(state),
    availableMeasures: availableMeasuresSelector(state),
    columnCount: getLayoutSelector(state).rowHorizontalCount,
    getColumnWidth: ({ index }) =>
      getCellWidthByKeySelector(state)(rowDimensions[index].id),
    getRowHeight: ({ index }) =>
      getCellHeightByKeySelector(state)(leaves[index].key),
    getSizeByKey: getCellHeightByKeySelector(state),
    crossPositions: crossPositionsSelector(state)[AxisType.ROWS],
    height: rowsVisibleHeightSelector(state),
    width: rowHeadersWidthSelector(state),
    previewSizes: previewSizesSelector(state),
    rowCount: getLayoutSelector(state).rowVerticalCount,
    getLastChildSize: header => getLastChildHeightSelector(state)(header),
    leaves,
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
