import { connect } from "react-redux";

import { AxisType } from "../constants";
import {
  previewSizesSelector,
  getSelectedRowRangeSelector,
  getAxisActivatedMeasuresSelector,
  availableMeasuresSelector
} from "../selectors";
import Headers from "../components/Headers/Headers";
import {
  toggleCollapse,
  selectRange,
  setConfigurationProperty,
  moveDimension,
  moveMeasure,
  toggleMeasure
} from "../actions";

const mapStateToProps = (state, ownProps) => {
  return {
    axisType: AxisType.ROWS,
    height: state.configuration.height,
    width: state.configuration.width,
    previewSizes: previewSizesSelector(state),
    gridId: ownProps.gridId,
    getSelectedRowRange: getSelectedRowRangeSelector(state),
    measures: getAxisActivatedMeasuresSelector(AxisType.ROWS)(state),
    availableMeasures: availableMeasuresSelector(state)
  };
};

const mapDispatchToProps = dispatch => ({
  toggleCollapse: (key, n) => {
    dispatch(toggleCollapse({ axisType: AxisType.ROWS, key, n }));
  },
  selectAxis: getSelectedRowRange => header => {
    const selectedRange = getSelectedRowRange(header);
    dispatch(selectRange(selectedRange));
  },
  moveDimension: (dimensionId, oldAxis, newAxis, position) => {
    dispatch(moveDimension(dimensionId, oldAxis, newAxis, position));
  },
  toggleMeasuresAxis: axis =>
    dispatch(
      setConfigurationProperty(
        { measureHeadersAxis: axis },
        "measureHeadersAxis"
      )
    ),
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
