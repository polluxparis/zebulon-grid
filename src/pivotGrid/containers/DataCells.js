import { connect } from "react-redux";

import {
  getCellValueSelector,
  getCellInfosSelector,
  getRangeInfosSelector,
  activatedMeasuresSelector,
  dimensionsSelector,
  selectedRangeSelector,
  getElementsSelector,
  buildDataSelector
} from "../selectors";
import { DataCells } from "../components/DataCells/DataCells";
import {
  selectRange,
  selectCell,
  setConfigurationProperty,
  applyPushedData
} from "../actions";

const mapStateToProps = () => (state, ownProps) => {
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
    buildData: (rowLeaf, columnLeaf, oldValue, newValue, comment) =>
      buildDataSelector(state)(
        rowLeaf,
        columnLeaf,
        oldValue,
        newValue,
        comment
      ),
    // height: state.configuration.height,
    // width: state.configuration.width,
    configuration: state.configuration,
    status: state.status,
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
    dispatch(setConfigurationProperty({ [prop]: value }, prop)),
  editData: data => dispatch(applyPushedData(data))
});
export default connect(mapStateToProps, mapDispatchToProps)(DataCells);
