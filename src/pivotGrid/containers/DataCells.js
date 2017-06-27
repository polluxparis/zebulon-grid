import { connect } from 'react-redux';

import {
  dataCellsWidthSelector,
  dataCellsHeightSelector,
  getRowLeaves,
  getLayout,
  getColumnLeaves,
  getCellWidthByKeySelector,
  getCellHeightByKeySelector,
  getCellValue,
  getCellInfos,
  activatedMeasuresSelector,
  rowDimensionsSelector,
  columnDimensionsSelector,
  getCellDimensionInfos
} from '../selectors';
import DataCells from '../components/DataCells';
import copy from '../services/copyService';
// import getCellDimensionInfos from '../selectors/cell.selector';

const mapStateToProps = (state, ownProps) => {
  const { customFunctions, focusCellIndexes } = ownProps;
  const rowLeaves = getRowLeaves(state);
  const columnLeaves = getColumnLeaves(state);
  const rowDimensions = rowDimensionsSelector(state);
  const columnDimensions = columnDimensionsSelector(state);
  const measures = activatedMeasuresSelector(state);
  // const rowDimensionHeaders = rowHeaders.dimensionHeaders;
  // const columnDimensionHeaders = columnHeaders.dimensionHeaders;
  return {
    columnCount: getLayout(state).columnHorizontalCount,
    columnLeaves,
    copy: ({ selectedCellStart, selectedCellEnd }) =>
      copy({
        selectedCellStart,
        selectedCellEnd,
        columnLeaves,
        rowLeaves,
        rowDimensions,
        columnDimensions,
        measures,
        getCellValue: getCellValue(state),
        getCellDimensionInfos: getCellDimensionInfos(state)
      }),
    focusCellIndexes,
    getCellValue: getCellValue(state),
    getCellInfos: getCellInfos(state),
    getColumnWidth: ({ index }) =>
      getCellWidthByKeySelector(state)(columnLeaves[index].key),
    getRowHeight: ({ index }) =>
      getCellHeightByKeySelector(state)(rowLeaves[index].key),
    height: dataCellsHeightSelector(state),
    rowCount: getLayout(state).rowVerticalCount,
    measures,
    rowLeaves,
    sizes: state.sizes,
    width: dataCellsWidthSelector(state),
    zoom: state.config.zoom
  };
};

export default connect(mapStateToProps)(DataCells);
