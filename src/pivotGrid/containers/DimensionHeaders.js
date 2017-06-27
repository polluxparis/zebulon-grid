import { connect } from 'react-redux';

import {
  getColumnDimensionHeaders,
  getRowDimensionHeaders,
  getCellWidthByKeySelector,
  getCellHeightByKeySelector,
  columnDimensionsSelector,
  rowDimensionsSelector,
  columnHeadersWidthSelector,
  rowHeadersWidthSelector,
  // getCrossSize,
  crossPositionsSelector,
  getPreviewSizes
} from '../selectors';
import DimensionHeaders from '../components/DimensionHeaders';

const mapStateToProps = (state, ownProps) => {
  const columnDimensions = columnDimensionsSelector(state);
  const rowDimensions = rowDimensionsSelector(state);
  return {
    columnDimensions,
    rowDimensions,
    crossPositions: crossPositionsSelector(state),
    // getCrossSize: getCrossSize(state),
    // getColumnWidth: ({ index }) =>
    //   getCellWidthByKeySelector(state)(columnDimensions[index].id),
    // getRowHeight: ({ index }) =>
    //   getCellHeightByKeySelector(state)(rowDimensions[index].id),
    height: columnHeadersWidthSelector(state),
    previewSizes: getPreviewSizes(state),
    rowDimensions: rowDimensionsSelector(state),
    width: rowHeadersWidthSelector(state),
    zoom: state.config.zoom,
    gridId: ownProps.gridId
  };
};

export default connect(mapStateToProps)(DimensionHeaders);
