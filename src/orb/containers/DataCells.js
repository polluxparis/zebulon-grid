import { connect } from 'react-redux';

import {
  getDataCellsWidth,
  getDataCellsHeight,
  getRowUiAxis,
  getLayout,
  getColumnUiAxis,
  getColumnWidth,
  getRowHeight,
  getCellValue,
 } from '../selectors';
import DataCells from '../components/DataCells';

const mapStateToProps = state => ({
  zoom: state.config.zoom,
  columnHeaders: getColumnUiAxis(state).headers,
  rowHeaders: getRowUiAxis(state).headers,
  getColumnWidth: getColumnWidth(state),
  getRowHeight: getRowHeight(state),
  getCellValue: getCellValue(state),
  dataHeadersLocation: state => state.config.dataHeadersLocation,
  columnCount: getLayout(state).columnHorizontalCount,
  rowCount: getLayout(state).rowVerticalCount,
  height: getDataCellsHeight(state),
  width: getDataCellsWidth(state),
});

export default connect(mapStateToProps)(DataCells);
