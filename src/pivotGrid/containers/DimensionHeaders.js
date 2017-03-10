import { connect } from 'react-redux';

import {
  getColumnUiAxis,
  getRowUiAxis,
  getColumnAxis,
  getRowAxis,
  getHeaderSizes,
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
  height: getHeaderSizes(state).columnHeadersHeight,
  previewSizes: getPreviewSizes(state),
  rowDimensionHeaders: getRowUiAxis(state).dimensionHeaders,
  rowFields: getRowAxis(state).fields,
  width: getHeaderSizes(state).rowHeadersWidth,
  zoom: state.config.zoom,
  gridId: ownProps.gridId
});

export default connect(mapStateToProps)(DimensionHeaders);
