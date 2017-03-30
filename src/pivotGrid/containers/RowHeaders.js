import { connect } from 'react-redux';

import { AxisType } from '../Axis';
import {
  getDimensionPositions,
  getDimensionSize,
  getRowHeadersWidth,
  getLastChildSizeOnRows,
  getLayout,
  getPreviewSizes,
  getRowHeadersVisibleHeight,
  getRowHeight,
  getRowUiAxis
} from '../selectors';
import RowHeaders from '../components/RowHeaders';

const mapStateToProps = (state, ownProps) => ({
  columnCount: getLayout(state).rowHorizontalCount,
  dimensionPositions: getDimensionPositions(state),
  getColumnWidth: ({ index }) =>
    getDimensionSize(state)(AxisType.ROWS, state.axis.rows[index]),
  getDimensionSize: getDimensionSize(state),
  getLastChildSize: getLastChildSizeOnRows(state),
  getRowHeight: getRowHeight(state),
  height: getRowHeadersVisibleHeight(state),
  previewSizes: getPreviewSizes(state),
  rowCount: getLayout(state).rowVerticalCount,
  rowHeaders: getRowUiAxis(state).headers,
  width: getRowHeadersWidth(state),
  sizesRowsLeafs: state.sizes.leafs.rows,
  zoom: state.config.zoom,
  gridId: ownProps.gridId
});

export default connect(mapStateToProps)(RowHeaders);
