import { connect } from 'react-redux';

import { AxisType } from '../Axis';
import {
  getColumnUiAxis,
  getColumnWidth,
  getColumnHeadersHeight,
  getLastChildSizeOnColumns,
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
  getLastChildSize: getLastChildSizeOnColumns(state),
  getRowHeight: ({ index }) =>
    getDimensionSize(state)(AxisType.COLUMNS, state.axis.columns[index]),
  height: getColumnHeadersHeight(state),
  previewSizes: getPreviewSizes(state),
  rowCount: getLayout(state).columnVerticalCount,
  width: getColumnHeadersVisibleWidth(state),
  zoom: state.config.zoom,
  sizesColumnsLeafs: state.sizes.leafs.columns,
  gridId: ownProps.gridId
});

export default connect(mapStateToProps)(ColumnHeaders);
