import { connect } from "react-redux";
import Configuration from "../components/PivotGrid/Configuration";
import { indexedRowsSelector, metaSelector } from "../selectors";
// import {
//   updateMeta
// } from "../actions";

const mapStateToProps = state => {
    return {
        status: state.status,
        width: state.configuration.width,
        height: state.configuration.height,
        data: indexedRowsSelector(state),
        meta: metaSelector(state)
    };
};

export default connect(mapStateToProps)(Configuration);
