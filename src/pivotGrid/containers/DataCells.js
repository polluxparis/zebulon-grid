import { connect } from "react-redux";

import {
  dataCellsWidthSelector,
  dataCellsHeightSelector,
  getCellValueSelector,
  getCellInfosSelector,
  getRangeInfosSelector,
  activatedMeasuresSelector,
  dimensionsSelector,
  selectedRangeSelector,
  columnHeadersPositionsAndSizesSelector,
  rowHeadersPositionsAndSizesSelector,
  scrollbarSizesSelector
} from "../selectors";
import { DataCells } from "../components/DataCells/DataCells";
import { selectRange, selectCell } from "../actions";

const mapStateToProps = (state, ownProps) => {
  const measures = activatedMeasuresSelector(state);
  const getCellValue = getCellValueSelector(state);
  const selectedRange = selectedRangeSelector(state);
  const columnHeaders = columnHeadersPositionsAndSizesSelector(state);
  const rowHeaders = rowHeadersPositionsAndSizesSelector(state);
  const props = {
    measures,
    filters: state.filters,
    dimensions: dimensionsSelector(state),

    selectedRange,
    getCellValue,
    columnHeaders: columnHeaders.headers,
    rowHeaders: rowHeaders.headers,
    getCellInfos: getCellInfosSelector(state),
    getRangeInfos: getRangeInfosSelector(state),
    height: dataCellsHeightSelector(state),
    width: dataCellsWidthSelector(state),
    rowsSize: rowHeaders.size,
    columnsSize: columnHeaders.size,
    scrollbarSizes: scrollbarSizesSelector(state)
  };
  if (ownProps.scroll) {
    props.scrollToRow = ownProps.scroll.row;
    props.scrollToColumn = ownProps.scroll.column;
  }
  return props;
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
