import { connect } from 'react-redux';

import {
  getColumnUiAxis,
  getRowUiAxis,
  getColumnAxis,
  getRowAxis,
  getHeaderSizes,
  getDimensionSize,
  getDimensionPositions,
  getPreviewSizes,
} from '../selectors';
import DimensionHeaders from '../components/DimensionHeaders';

const mapStateToProps = state => ({
  columnDimensionHeaders: getColumnUiAxis(state).dimensionHeaders,
  columns: getColumnAxis(state),
  dataHeadersLocation: state.config.dataHeadersLocation,
  dimensionPositions: getDimensionPositions(state),
  getDimensionSize: getDimensionSize(state),
  height: getHeaderSizes(state).columnHeadersHeight,
  previewSizes: getPreviewSizes(state),
  rowDimensionHeaders: getRowUiAxis(state).dimensionHeaders,
  rows: getRowAxis(state),
  width: getHeaderSizes(state).rowHeadersWidth,
  zoom: state.config.zoom,
});

export default connect(mapStateToProps)(DimensionHeaders);
