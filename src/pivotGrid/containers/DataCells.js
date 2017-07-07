import { connect } from 'react-redux';

import {
  dataCellsWidthSelector,
  dataCellsHeightSelector,
  rowLeavesSelector,
  getLayout,
  columnLeavesSelector,
  getCellWidthByKeySelector,
  getCellHeightByKeySelector,
  getCellValueSelector,
  getCellInfosSelector,
  activatedMeasuresSelector,
  rowDimensionsSelector,
  columnDimensionsSelector,
  getCellDimensionInfosSelector,
  selectedRangeSelector
} from '../selectors';
import DataCells from '../components/DataCells';
import copy from '../services/copyService';
import { selectRange, selectCell } from '../actions';
// import getCellDimensionInfosSelector from '../selectors/cell.selector';

const mapStateToProps = (state, ownProps) => {
  const { customFunctions, focusCellIndexes, handleMouseDown } = ownProps;
  const rowLeaves = rowLeavesSelector(state);
  const columnLeaves = columnLeavesSelector(state);
  const rowDimensions = rowDimensionsSelector(state);
  const columnDimensions = columnDimensionsSelector(state);
  const measures = activatedMeasuresSelector(state);
  const getCellValue = getCellValueSelector(state);
  const selectedRange = selectedRangeSelector(state);
  // const rowDimensionHeaders = rowHeaders.dimensionHeaders;
  // const columnDimensionHeaders = columnHeaders.dimensionHeaders;
  return {
    columnCount: getLayout(state).columnHorizontalCount,
    columnLeaves,
    copy: selectedRange =>
      copy({
        selectedRange,
        columnLeaves,
        rowLeaves,
        rowDimensions,
        columnDimensions,
        measures,
        getCellValue,
        getCellDimensionInfosSelector: getCellDimensionInfosSelector(state)
      }),
    selectedRange,
    getCellValue,
    getCellInfosSelector: getCellInfosSelector(state),
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

const mapDispatchToProps = dispatch => ({
  selectRange: selectedRange => {
    dispatch(selectRange(selectedRange));
  },
  selectCell: cell => {
    dispatch(selectCell(cell));
  }
});
export default connect(mapStateToProps, mapDispatchToProps)(DataCells);
