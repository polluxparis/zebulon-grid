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
  getCellInfos
} from '../selectors';
import DataCells from '../components/DataCells';
import copy from '../services/copyService';

const mapStateToProps = (state, ownProps) => {
  const { customFunctions } = ownProps;
  const rowUiAxis = getRowUiAxis(state);
  const columnUiAxis = getColumnUiAxis(state);
  const rowDimensionHeaders = rowUiAxis.dimensionHeaders;
  const columnDimensionHeaders = columnUiAxis.dimensionHeaders;
  const rowHeaders = rowUiAxis.headers;
  const columnHeaders = columnUiAxis.headers;
  const dataHeadersLocation = state.config.dataHeadersLocation;
  return {
    columnCount: getLayout(state).columnHorizontalCount,
    columnHeaders,
    copy: ({ selectedCellStart, selectedCellEnd }) =>
      copy({
        columnDimensionHeaders,
        columnHeaders,
        dataHeadersLocation,
        getCellValue: getCellValue(state),
        rowDimensionHeaders,
        rowHeaders,
        selectedCellEnd,
        selectedCellStart,
        customFunctions
      }),
    dataHeadersLocation,
    getCellValue: getCellValue(state),
    getCellInfos: getCellInfos(state),
    getColumnWidth: getColumnWidth(state),
    getRowHeight: getRowHeight(state),
    height: getDataCellsHeight(state),
    rowCount: getLayout(state).rowVerticalCount,
    rowHeaders,
    sizes: state.sizes,
    width: getDataCellsWidth(state),
    zoom: state.config.zoom
  };
};

export default connect(mapStateToProps)(DataCells);
