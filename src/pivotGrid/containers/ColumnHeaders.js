import { connect } from "react-redux";

import { AxisType } from "../constants";
import {
  dataCellsWidthSelector,
  columnHeadersHeightSelector,
  previewSizesSelector,
  getSelectedColumnRangeSelector,
  columnHeadersPositionsAndSizesSelector
} from "../selectors";
import {
  toggleCollapse,
  selectRange,
  moveDimension,
  moveMeasure,
  toggleMeasure
} from "../actions";
import Headers from "../components/Headers/Headers";

const mapStateToProps = (state, ownProps) => {
  const headers = columnHeadersPositionsAndSizesSelector(state);
  return {
    axisType: AxisType.COLUMNS,
    height: columnHeadersHeightSelector(state),
    width: dataCellsWidthSelector(state),
    previewSizes: previewSizesSelector(state),
    gridId: ownProps.gridId,
    getSelectedColumnRange: getSelectedColumnRangeSelector(state),
    headers: headers.headers,
    rowCount: headers.depth,
    columnCount: headers.headers.length,
    rowsSize: headers.crossSize,
    columnsSize: headers.size
  };
};

const mapDispatchToProps = dispatch => ({
  toggleCollapse: key => {
    dispatch(toggleCollapse({ axisType: AxisType.COLUMNS, key }));
  },
  moveDimension: (dimensionId, oldAxis, newAxis, position) => {
    dispatch(moveDimension(dimensionId, oldAxis, newAxis, position));
  },
  moveMeasure: measures => (measureId, measureToId) => {
    dispatch(
      moveMeasure(measureId, Object.keys(measures).indexOf(measureToId))
    );
  },
  toggleMeasure: measureId => dispatch(toggleMeasure(measureId)),
  selectAxis: getSelectedColumnRange => header => {
    const selectedRange = getSelectedColumnRange(header);
    dispatch(selectRange(selectedRange));
  }
});

const mergeProps = (
  { getSelectedColumnRange, measures, ...restStateProps },
  { selectAxis, moveMeasure, ...restDispatchProps },
  ownProps
) => ({
  selectAxis: selectAxis(getSelectedColumnRange),
  moveMeasure: moveMeasure(measures),
  measures,
  ...restStateProps,
  ...restDispatchProps,
  ...ownProps
});
export default connect(mapStateToProps, mapDispatchToProps, mergeProps)(
  Headers
);
