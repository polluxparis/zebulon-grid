import { connect } from 'react-redux';

import {
  columnDimensionsSelector,
  rowDimensionsSelector,
  columnHeadersWidthSelector,
  rowHeadersWidthSelector,
  crossPositionsSelector,
  previewSizesSelector,
  availableDimensionsSelector,
  getDimensionKeysSelector
} from '../selectors';
import DimensionHeaders from '../components/DimensionHeaders/DimensionHeaders';
import {
  toggleCollapseDimension,
  toggleSortOrder,
  moveDimension,
  expandCollapseAll
} from '../actions';

const mapStateToProps = (state, ownProps) => {
  const columnDimensions = columnDimensionsSelector(state);
  const rowDimensions = rowDimensionsSelector(state);
  return {
    columnDimensions,
    rowDimensions,
    getDimensionKeys: getDimensionKeysSelector(state),
    filters: state.filters,
    availableDimensions: availableDimensionsSelector(state),
    crossPositions: crossPositionsSelector(state),
    height: columnHeadersWidthSelector(state),
    previewSizes: previewSizesSelector(state),
    width: rowHeadersWidthSelector(state),
    gridId: ownProps.gridId,
    sizes: state.sizes
  };
};
const mapDispatchToProps = dispatch => ({
  toggleCollapseDimension: key => dispatch(toggleCollapseDimension({ key })),
  toggleSortOrder: key => dispatch(toggleSortOrder(key)),
  expandCollapseAll: (axisType, keys) =>
    dispatch(expandCollapseAll({ axisType, keys })),
  moveDimension: (dimensionId, oldAxis, newAxis, position) => {
    dispatch(moveDimension(dimensionId, oldAxis, newAxis, position));
  }
});
export default connect(mapStateToProps, mapDispatchToProps)(DimensionHeaders);
