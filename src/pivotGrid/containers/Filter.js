import { connect } from "react-redux";
import { getDimensionValuesSelector } from "../selectors";
import { addFilter, deleteFilter } from "../actions";
import { Filter } from "zebulon-controls";
const mapStateToProps = () => (state, ownProps) => {
  return {
    items: getDimensionValuesSelector(state)(ownProps.dimensionId),
    filter: (state.filters[ownProps.dimensionId] || {}).values || {},
    ...ownProps
  };
};
const mapDispatchToProps = (dispatch, ownProps) => ({
  onOk: filter => {
    if (!filter) {
      dispatch(deleteFilter(ownProps.dimensionId));
    } else {
      dispatch(addFilter(ownProps.dimensionId, "in", null, false, filter));
    }
  },
  ...ownProps
});

export default connect(mapStateToProps, mapDispatchToProps)(Filter);
