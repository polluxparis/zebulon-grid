import { connect } from 'react-redux';

import { AxisType } from '../Axis';
import {
  crossPositionsSelector,
  getLastChildSizeOnRows,
  getLayout,
  getPreviewSizes,
  getRowHeadersVisibleHeight,
  getCellWidthByKeySelector,
  getCellHeightByKeySelector,
  getRowHeaders,
  getRowLeaves,
  getAxisActivatedMeasures,
  getFilteredData,
  rowDimensionsSelector,
  columnDimensionsSelector,
  rowsVisibleHeightSelector,
  rowHeadersWidthSelector
} from '../selectors';
import Headers from '../components/Headers';

const mapStateToProps = (state, ownProps) => {
  const columnDimensions = columnDimensionsSelector(state);
  const leaves = getRowLeaves(state);
  return {
    axisType: AxisType.ROWS,
    data: getFilteredData(state),
    dimensions: rowDimensionsSelector(state),
    measures: getAxisActivatedMeasures(AxisType.ROWS)(state),
    columnCount: getLayout(state).rowHorizontalCount,
    crossPositions: crossPositionsSelector(state),
    getColumnWidth: ({ index }) =>
      getCellWidthByKeySelector(state)(columnDimensions[index].id),
    getRowHeight: ({ index }) =>
      getCellHeightByKeySelector(state)(leaves[index].key),
    height: rowsVisibleHeightSelector(state),
    width: rowHeadersWidthSelector(state),
    previewSizes: getPreviewSizes(state),
    rowCount: getLayout(state).rowVerticalCount,
    headers: getRowHeaders(state),
    leaves,

    sizes: state.sizes.height,
    zoom: state.config.zoom,
    gridId: ownProps.gridId
  };
};

export default connect(mapStateToProps)(Headers);
