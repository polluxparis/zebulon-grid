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
  filteredDataSelector
} from '../selectors';
import { toggleCollapse } from '../actions';
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
    headers: columnHeadersSelector(state),
    leaves,
    // crossPositions: crossPositionsSelector(state),
    getColumnWidth: ({ index }) =>
      getCellWidthByKeySelector(state)(leaves[index].key),
    // getCrossSize: getCrossSize(state),
    getLastChildSize: header => getLastChildWidth(state)(header),
    getRowHeight: ({ index }) =>
      getCellHeightByKeySelector(state)(columnDimensions[index].id),
    height: columnHeadersWidthSelector(state),
    width: columnsVisibleWidthSelector(state),
    previewSizes: getPreviewSizes(state),
    rowCount: getLayout(state).columnVerticalCount,
    zoom: state.config.zoom,
    sizes: state.sizes,
    gridId: ownProps.gridId
  };
};

const mapDispatchToProps = dispatch => ({
  toggleCollapse: key => {
    dispatch(toggleCollapse({ axisType: AxisType.COLUMNS, key }));
  }
});
export default connect(mapStateToProps, mapDispatchToProps)(Headers);
