import { connect } from "react-redux";

import { AxisType } from "../constants";
import {
  previewSizesSelector,
  getSelectedColumnRangeSelector,
  getAxisActivatedMeasuresSelector,
  availableMeasuresSelector
} from "../selectors";
import {
  toggleCollapse,
  selectRange,
  moveDimension,
  moveMeasure,
  toggleMeasure,
  setConfigurationProperty
} from "../actions";
import Headers from "../components/Headers/Headers";

const mapStateToProps = () => (state, ownProps) => {
  return {
    axisType: AxisType.COLUMNS,
    gridId: ownProps.gridId,
    getSelectedColumnRange: getSelectedColumnRangeSelector(state),
    measures: getAxisActivatedMeasuresSelector(AxisType.COLUMNS)(state),
    availableMeasures: availableMeasuresSelector(state),
    previewSizes: previewSizesSelector(state),
    features: state.configuration.features
  };
};

const mapDispatchToProps = dispatch => ({
  toggleCollapse: (key, n) => {
    dispatch(toggleCollapse({ axisType: AxisType.COLUMNS, key, n }));
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
  toggleMeasuresAxis: axis =>
    dispatch(
      setConfigurationProperty(
        { measureHeadersAxis: axis },
        "measureHeadersAxis"
      )
    ),
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
