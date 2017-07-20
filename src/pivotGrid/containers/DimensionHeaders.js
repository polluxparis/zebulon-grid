import { connect } from 'react-redux';

import {
  columnDimensionsSelector,
  rowDimensionsSelector,
  columnHeadersWidthSelector,
  rowHeadersWidthSelector,
  crossPositionsSelector,
  previewSizesSelector,
  availableDimensionsSelector,
  dimensionValuesSelector
} from '../selectors';
import DimensionHeaders from '../components/DimensionHeaders/DimensionHeaders';
import {
  toggleCollapseDimension,
  toggleSortOrder,
  moveDimension
} from '../actions';

const mapStateToProps = (state, ownProps) => {
  const columnDimensions = columnDimensionsSelector(state);
  const rowDimensions = rowDimensionsSelector(state);
  return {
    columnDimensions,
    rowDimensions,
    dimensionValues: dimensionValuesSelector(state),
    filters: state.filters,
    dimensionFilter: dimensionValuesSelector(state),
    availableDimensions: availableDimensionsSelector(state),
    crossPositions: crossPositionsSelector(state),
    height: columnHeadersWidthSelector(state),
    previewSizes: previewSizesSelector(state),
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
    dispatch(moveDimension(dimensionId, oldAxis, newAxis, position));
  }
});
export default connect(mapStateToProps, mapDispatchToProps)(DimensionHeaders);
