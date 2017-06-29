import { connect } from 'react-redux';

import { AxisType } from '../Axis';
import {
  // crossPositionsSelector,
  // getLastChildSizeOnRows,
  getLayout,
  getPreviewSizes,
  getRowHeadersVisibleHeight,
  getCellWidthByKeySelector,
  getCellHeightByKeySelector,
  rowHeadersSelector,
  rowLeavesSelector,
  getAxisActivatedMeasures,
  filteredDataSelector,
  rowDimensionsSelector,
  rowsVisibleHeightSelector,
  rowHeadersWidthSelector
} from '../selectors';
import Headers from '../components/Headers';
import { toggleCollapse } from '../actions';

const mapStateToProps = (state, ownProps) => {
  const rowDimensions = rowDimensionsSelector(state);
  const leaves = rowLeavesSelector(state);
  return {
    axisType: AxisType.ROWS,
    data: filteredDataSelector(state),
    dimensions: rowDimensionsSelector(state),
    measures: getAxisActivatedMeasures(AxisType.ROWS)(state),
    columnCount: getLayout(state).rowHorizontalCount,
    // crossPositions: crossPositionsSelector(state),
    getColumnWidth: ({ index }) =>
      getCellWidthByKeySelector(state)(rowDimensions[index].id),
    getRowHeight: ({ index }) =>
      getCellHeightByKeySelector(state)(leaves[index].key),
    height: rowsVisibleHeightSelector(state),
    width: rowHeadersWidthSelector(state),
    previewSizes: getPreviewSizes(state),
    rowCount: getLayout(state).rowVerticalCount,
    headers: rowHeadersSelector(state),
    getLastChildSize: header =>
      getCellHeightByKeySelector(state)(leaves[index].key),
    leaves,
    // getIsCollapsed: ({ index }) =>
    //   getIsCollapsedRowByKeySelector(state)(leaves[index].key),
    sizes: state.sizes,
    zoom: state.config.zoom,
    gridId: ownProps.gridId
  };
};

const mapDispatchToProps = dispatch => ({
  toggleCollapse: key => {
    dispatch(toggleCollapse({ axisType: AxisType.ROWS, key }));
  }
});
export default connect(mapStateToProps, mapDispatchToProps)(Headers);
