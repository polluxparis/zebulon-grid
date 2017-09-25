import { connect } from 'react-redux';

import {
  dataCellsWidthSelector,
  dataCellsHeightSelector,
  rowLeavesSelector,
  layoutSelector,
  columnLeavesSelector,
  getColumnWidthSelector,
  getRowHeightSelector,
  getCellValueSelector,
  getCellInfosSelector,
  getRangeInfosSelector,
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
    columnCount: layoutSelector(state).columnHorizontalCount,
    columnLeaves,
    filters: state.filters,
    selectedRange,
    getCellValue,
    dimensions: dimensionsSelector(state),
    getCellInfos: getCellInfosSelector(state),
    getRangeInfos: getRangeInfosSelector(state),
    getColumnWidth: getColumnWidthSelector(state),
    getRowHeight: getRowHeightSelector(state),
    height: dataCellsHeightSelector(state),
    rowCount: layoutSelector(state).rowVerticalCount,
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
