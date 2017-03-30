import { connect } from 'react-redux';

import {
  getColumnUiAxis,
  getRowUiAxis,
  getColumnAxis,
  getRowAxis,
  getColumnHeadersHeight,
  getRowHeadersWidth,
  getDimensionSize,
  getDimensionPositions,
  getPreviewSizes
} from '../selectors';
import DimensionHeaders from '../components/DimensionHeaders';

const mapStateToProps = (state, ownProps) => ({
  columnDimensionHeaders: getColumnUiAxis(state).dimensionHeaders,
  columnFields: getColumnAxis(state).fields,
  dataHeadersLocation: state.config.dataHeadersLocation,
  dimensionPositions: getDimensionPositions(state),
  getDimensionSize: getDimensionSize(state),
  height: getColumnHeadersHeight(state),
  previewSizes: getPreviewSizes(state),
  rowDimensionHeaders: getRowUiAxis(state).dimensionHeaders,
  rowFields: getRowAxis(state).fields,
  width: getRowHeadersWidth(state),
  zoom: state.config.zoom,
  gridId: ownProps.gridId
});

export default connect(mapStateToProps)(DimensionHeaders);
