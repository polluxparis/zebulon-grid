import { connect } from 'react-redux';

import {
  dataCellsWidthSelector,
  dataCellsHeightSelector,
  rowLeavesSelector,
  getLayoutSelector,
  columnLeavesSelector,
  getCellWidthByKeySelector,
  getCellHeightByKeySelector,
  getCellValueSelector,
  getCellInfosSelector,
  activatedMeasuresSelector,
  selectedRangeSelector,
  dimensionsSelector
} from '../selectors';
import DataCells from '../components/DataCells/DataCells';
import { selectRange, selectCell } from '../actions';

const mapStateToProps = (state, ownProps) => {
  const rowLeaves = rowLeavesSelector(state);
  const columnLeaves = columnLeavesSelector(state);
  const measures = activatedMeasuresSelector(state);
  const getCellValue = getCellValueSelector(state);
  const selectedRange = selectedRangeSelector(state);
  return {
    columnCount: getLayoutSelector(state).columnHorizontalCount,
    columnLeaves,
    selectedRange,
    getCellValue,
    dimensions: dimensionsSelector(state),
    getCellInfosSelector: getCellInfosSelector(state),
    getColumnWidth: ({ index }) =>
      getCellWidthByKeySelector(state)(columnLeaves[index].key),
    getRowHeight: ({ index }) =>
      getCellHeightByKeySelector(state)(rowLeaves[index].key),
    height: dataCellsHeightSelector(state),
    rowCount: getLayoutSelector(state).rowVerticalCount,
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
