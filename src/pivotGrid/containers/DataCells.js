import { connect } from "react-redux";

import {
  getCellValueSelector,
  getCellInfosSelector,
  getRangeInfosSelector,
  activatedMeasuresSelector,
  dimensionsSelector,
  selectedRangeSelector
} from "../selectors";
import { DataCells } from "../components/DataCells/DataCells";
import { selectRange, selectCell } from "../actions";

const mapStateToProps = (state, ownProps) => {
  const measures = activatedMeasuresSelector(state);
  const dimensions = dimensionsSelector(state);
  const getCellValue = getCellValueSelector(state);
  const selectedRange = selectedRangeSelector(state);
  const props = {
    measures,
    filters: state.filters,
    dimensions,
    selectedRange,
    getCellValue,
    getCellInfos: getCellInfosSelector(state),
    getRangeInfos: getRangeInfosSelector(state),
    height: state.config.height,
    width: state.config.width,
    ...ownProps
  };
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
