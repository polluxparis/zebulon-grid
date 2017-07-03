import { combineReducers } from 'redux';

import data from './data.reducer';
import config from './config.reducer';
import dimensions from './dimensions.reducer';
import measures from './measures.reducer';
import axis from './axis.reducer';
import sizes from './sizes.reducer';
import filters from './filters.reducer';
import collapses from './collapses.reducer';
import selectedRange from './selection.reducer';
export default combineReducers({
	data,
	config,
	dimensions,
	measures,
	axis,
	sizes,
	filters,
	collapses,
	selectedRange
});
