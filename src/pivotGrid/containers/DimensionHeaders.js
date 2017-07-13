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
  getPreviewSizes,
  availableDimensionsSelector
} from '../selectors';
import DimensionHeaders from '../components/DimensionHeaders';
import {
  toggleCollapseDimension,
  toggleSortOrder,
  moveDimension,
  selectCell
} from '../actions';
import { AxisType } from '../Axis';
const mapStateToProps = (state, ownProps) => {
  const columnDimensions = columnDimensionsSelector(state);
  const rowDimensions = rowDimensionsSelector(state);
  return {
    columnDimensions,
    rowDimensions,
    availableDimensions: availableDimensionsSelector(state),
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
    gridId: ownProps.gridId,
    zoom: state.config.zoom,
    sizes: state.sizes
  };
};
const mapDispatchToProps = dispatch => ({
  toggleCollapseDimension: key => dispatch(toggleCollapseDimension({ key })),
  toggleSortOrder: key => dispatch(toggleSortOrder(key)),
  moveDimension: (dimensionId, oldAxis, newAxis, position) => {
    dispatch(selectCell(null));
    dispatch(moveDimension(dimensionId, oldAxis, newAxis, position));
  }
});
export default connect(mapStateToProps, mapDispatchToProps)(DimensionHeaders);
