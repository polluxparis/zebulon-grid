import { createSelector } from 'reselect';

import { filteredDataSelector } from './data.selector';
import { EMPTY_ID } from '../constants';

export const rowDimensionsSelector = createSelector(
	[state => state.axis.rows, state => state.dimensions],
	(rowAxis, dimensions) => rowAxis.map(id => dimensions[id])
);

export const columnDimensionsSelector = createSelector(
	[state => state.axis.columns, state => state.dimensions],
	(columnAxis, dimensions) => columnAxis.map(id => dimensions[id])
);

export const availableDimensionsSelector = createSelector(
	[state => state.axis.dimensions, state => state.dimensions],
	(dimensionAxis, dimensions) => dimensionAxis.map(id => dimensions[id])
);

const measuresSelector = state => state.measures;

export const activatedMeasuresSelector = createSelector(
	[measuresSelector],
	measures =>
		Object.keys(measures)
			.map(id => measures[id])
			.filter(measure => measure.activated)
			.reduce((acc, mea) => ({ ...acc, [mea.id]: mea }), {})
);
