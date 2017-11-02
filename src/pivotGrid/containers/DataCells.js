import { connect } from "react-redux";

import {
  getCellValueSelector,
  getCellInfosSelector,
  getRangeInfosSelector,
  activatedMeasuresSelector,
  dimensionsSelector,
  selectedRangeSelector,
  getElementsSelector
} from "../selectors";
import { DataCells } from "../components/DataCells/DataCells";
import { selectRange, selectCell, setConfigurationProperty } from "../actions";

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
    getGridInfos: () => getElementsSelector(state)(),
    // height: state.configuration.height,
    // width: state.configuration.width,
    configuration: state.configuration,
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
  },
  setConfigProperty: (prop, value) =>
    dispatch(setConfigurationProperty({ [prop]: value }, prop))
});
export default connect(mapStateToProps, mapDispatchToProps)(DataCells);
