import { connect } from "react-redux";

import { AxisType } from "../constants";
import {
  previewSizesSelector,
  dataCellsHeightSelector,
  rowHeadersWidthSelector,
  getSelectedRowRangeSelector,
  rowHeadersPositionsAndSizesSelector,
  getAxisActivatedMeasuresSelector,
  availableMeasuresSelector
} from "../selectors";
import Headers from "../components/Headers/Headers";
import {
  toggleCollapse,
  selectRange,
  moveDimension,
  moveMeasure,
  toggleMeasure
} from "../actions";

const mapStateToProps = (state, ownProps) => {
  const headers = rowHeadersPositionsAndSizesSelector(state);
  return {
    axisType: AxisType.ROWS,
    height: dataCellsHeightSelector(state),
    width: rowHeadersWidthSelector(state),
    previewSizes: previewSizesSelector(state),
    rowCount: headers.headers.length,
    columnCount: headers.depth,
    rowsSize: headers.size,
    columnsSize: headers.crossSize,
    gridId: ownProps.gridId,
    getSelectedRowRange: getSelectedRowRangeSelector(state),
    headers: headers.headers,
    measures: getAxisActivatedMeasuresSelector(AxisType.ROWS)(state),
    availableMeasures: availableMeasuresSelector(state)
  };
};

const mapDispatchToProps = dispatch => ({
  toggleCollapse: key => {
    dispatch(toggleCollapse({ axisType: AxisType.ROWS, key }));
  },
  selectAxis: getSelectedRowRange => header => {
    const selectedRange = getSelectedRowRange(header);
    dispatch(selectRange(selectedRange));
  },
  moveDimension: (dimensionId, oldAxis, newAxis, position) =>
    dispatch(moveDimension(dimensionId, oldAxis, newAxis, position)),
  moveMeasure: measures => (measureId, measureToId) => {
    dispatch(
      moveMeasure(measureId, Object.keys(measures).indexOf(measureToId))
    );
  },
  toggleMeasure: measureId => dispatch(toggleMeasure(measureId))
});

const mergeProps = (
  { getSelectedRowRange, measures, ...restStateProps },
  { selectAxis, moveMeasure, ...restDispatchProps },
  ownProps
) => ({
  selectAxis: selectAxis(getSelectedRowRange),
  moveMeasure: moveMeasure(measures),
  measures,
  ...restStateProps,
  ...restDispatchProps,
  ...ownProps
});

export default connect(mapStateToProps, mapDispatchToProps, mergeProps)(
  Headers
);
