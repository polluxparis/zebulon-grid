import { connect } from 'react-redux';

import { AxisType } from '../Axis';
import {
  getColumnUiAxis,
  getColumnWidth,
  getHeaderSizes,
  getLastChildSize,
  getDimensionSize,
  getDimensionPositions,
  getLayout,
  getColumnHeadersVisibleWidth,
  getPreviewSizes
} from '../selectors';
import ColumnHeaders from '../components/ColumnHeaders';

const mapStateToProps = (state, ownProps) => ({
  columnCount: getLayout(state).columnHorizontalCount,
  columnHeaders: getColumnUiAxis(state).headers,
  dimensionPositions: getDimensionPositions(state),
  getColumnWidth: getColumnWidth(state),
  getDimensionSize: getDimensionSize(state),
  getLastChildSize: getLastChildSize(state),
  getRowHeight: ({ index }) =>
    getDimensionSize(state)(AxisType.COLUMNS, state.axis.columns[index]),
  height: getHeaderSizes(state).columnHeadersHeight,
  previewSizes: getPreviewSizes(state),
  rowCount: getLayout(state).columnVerticalCount,
  width: getColumnHeadersVisibleWidth(state),
  zoom: state.config.zoom,
  sizesColumnsLeafs: state.sizes.columns.leafs,
  gridId: ownProps.gridId
});

export default connect(mapStateToProps)(ColumnHeaders);
