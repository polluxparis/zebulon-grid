import { connect } from "react-redux";
import Configuration from "../components/PivotGrid/Configuration";
import { indexedRowsSelector, metaSelector } from "../selectors";
// import {
//   updateMeta
// } from "../actions";

const mapStateToProps = (state, ownProps) => {
	return {
		status: state.status,
		width: state.configuration.width,
		height: state.configuration.height,
		data: indexedRowsSelector(state),
		meta: metaSelector(state),
		// measures: Object.values(state.measures),
		// dimensions: dimensionsWithAxisSelector(state),
		...ownProps
	};
};

export default connect(mapStateToProps)(Configuration);
