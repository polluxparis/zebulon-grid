import { connect } from "react-redux";

import Chart from "../components/PivotGrid/Chart";
// import {

const mapStateToProps = state => {
  return {
    status: state.status,
    width: state.configuration.width,
    height: state.configuration.height
  };
};

export default connect(mapStateToProps)(Chart);
