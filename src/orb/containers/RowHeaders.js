import { connect } from 'react-redux';

import {
  getRowUiAxis,
  getColumnWidth,
  getRowHeight,
  getHeaderSizes,
  getLastChildSize,
  getDimensionSizeSelector,
  getDimensionPositions,
} from '../selectors';
import RowHeaders from '../components/RowHeaders';

const mapStateToProps = state => ({
  zoom: state.config.zoom,
  rowHeaders: getRowUiAxis(state).headers,
  sizes: getHeaderSizes(state),
  getColumnWidth: getColumnWidth(state),
  getDimensionSize: getDimensionSizeSelector(state),
  dimensionPositions: getDimensionPositions(state).rows,
  getRowHeight: getRowHeight(state),
  getLastChildSize: getLastChildSize(state),
  dataHeadersLocation: state => state.config.dataHeadersLocation,
});

export default connect(mapStateToProps)(RowHeaders);
