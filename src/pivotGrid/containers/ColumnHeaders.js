import { connect } from 'react-redux';

import { AxisType } from '../Axis';
import {
  columnHeadersSelector,
  getCellWidthByKeySelector,
  getCellHeightByKeySelector,
  getColumnHeight,
  columnDimensionsSelector,
  columnHeadersWidthSelector,
  getLastChildWidth,
  // crossPositionsSelector,
  getLayout,
  columnsVisibleWidthSelector,
  getPreviewSizes,
  columnLeavesSelector,
  getAxisActivatedMeasuresSelector,
  filteredDataSelector,
  getSelectedColumnRangeSelector
} from '../selectors';
import { toggleCollapse, selectRange } from '../actions';
import Headers from '../components/Headers';

const mapStateToProps = (state, ownProps) => {
  const columnDimensions = columnDimensionsSelector(state);
  const leaves = columnLeavesSelector(state);
  return {
    axisType: AxisType.COLUMNS,
    data: filteredDataSelector(state),
    dimensions: columnDimensionsSelector(state),
    measures: getAxisActivatedMeasuresSelector(AxisType.COLUMNS)(state),
    columnCount: getLayout(state).columnHorizontalCount,
    // crossPositions: crossPositionsSelector(state),
    getColumnWidth: ({ index }) =>
      getCellWidthByKeySelector(state)(leaves[index].key),
    // getCrossSize: getCrossSize(state),
    getRowHeight: ({ index }) =>
      getCellHeightByKeySelector(state)(columnDimensions[index].id),
    height: columnHeadersWidthSelector(state),
    width: columnsVisibleWidthSelector(state),
    previewSizes: getPreviewSizes(state),
    rowCount: getLayout(state).columnVerticalCount,
    headers: columnHeadersSelector(state),
    getLastChildSize: header => getLastChildWidth(state)(header),
    leaves,

    sizes: state.sizes,
    zoom: state.config.zoom,

    gridId: ownProps.gridId,
    getSelectedColumnRange: getSelectedColumnRangeSelector(state)
  };
};

const mapDispatchToProps = dispatch => ({
  toggleCollapse: key => {
    dispatch(toggleCollapse({ axisType: AxisType.COLUMNS, key }));
  },
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
// export default connect(mapStateToProps, mapDispatchToProps, mergeProps)(
export default connect(mapStateToProps, mapDispatchToProps, mergeProps)(
  Headers
);
