import { connect } from "react-redux";

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
} from "../selectors";
import DimensionHeaders from "../components/DimensionHeaders";

const mapStateToProps = (state, ownProps) => ({
  columnDimensionHeaders: getColumnUiAxis(state).dimensionHeaders,
  columnDimensions: getColumnAxis(state).dimensions,
  dataHeadersLocation: state.config.dataHeadersLocation,
  dimensionPositions: getDimensionPositions(state),
  getDimensionSize: getDimensionSize(state),
  height: getColumnHeadersHeight(state),
  previewSizes: getPreviewSizes(state),
  rowDimensionHeaders: getRowUiAxis(state).dimensionHeaders,
  rowDimensions: getRowAxis(state).dimensions,
  width: getRowHeadersWidth(state),
  zoom: state.config.zoom,
  gridId: ownProps.gridId
});

export default connect(mapStateToProps)(DimensionHeaders);
