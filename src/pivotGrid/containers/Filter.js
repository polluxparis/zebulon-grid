import { connect } from "react-redux";
// import VirtualizedCheckbox from "react-virtualized-checkbox";

import { getDimensionValuesSelector } from "../selectors";
import { addFilter, deleteFilter } from "../actions";
import { Filter } from "zebulon-controls";
const mapStateToProps = () => (state, ownProps) => {
  return {
    items: getDimensionValuesSelector(state)(ownProps.dimensionId),
    filter: (state.filters[ownProps.dimensionId] || {}).values || {}
    // filterLeaves: filter =>
    //   filterLeavesSelector(state)(ownProps.dimensionId, filter)
  };
};
const mapDispatchToProps = (dispatch, ownProps) => ({
  onOk: filter => {
    if (!filter) {
      dispatch(deleteFilter(ownProps.dimensionId));
    } else {
      dispatch(addFilter(ownProps.dimensionId, "in", null, false, filter));
    }
  }
});

export default connect(mapStateToProps, mapDispatchToProps)(Filter);
