import { combineReducers } from "redux";

import data from "./data.reducer";
import status from "./status.reducer";
import configuration from "./configuration.reducer";
import dimensions from "./dimensions.reducer";
import measures from "./measures.reducer";
import axis from "./axis.reducer";
import sizes from "./sizes.reducer";
import filters from "./filters.reducer";
import collapses from "./collapses.reducer";
import selectedRange from "./selection.reducer";
import subtotals from "./subtotals.reducer";
export default combineReducers({
	status,
	data,
	configuration,
	dimensions,
	measures,
	axis,
	sizes,
	filters,
	collapses,
	selectedRange,
	subtotals
});
