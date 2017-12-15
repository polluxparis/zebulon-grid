import { connect } from "react-redux";
// import VirtualizedCheckbox from "react-virtualized-checkbox";

// import { getDimensionValuesSelector } from "../selectors";
// import { addFilter, deleteFilter } from "../actions";
import { Table } from "../components/controls/Table";
const mapStateToProps = (state, ownProps) => {
	return {
		data: state.data.data[0].map,
		meta: state.data.meta,
		...ownProps
	};
};
// const mapDispatchToProps = (dispatch, ownProps) => ({
//   onOk: filter => {
//     if (!filter) {
//       dispatch(deleteFilter(ownProps.dimensionId));
//     } else {
//       dispatch(addFilter(ownProps.dimensionId, "in", null, false, filter));
//     }
//   }
// });

// export default connect(mapStateToProps, mapDispatchToProps)(Filter);
export default connect(mapStateToProps)(Table);
