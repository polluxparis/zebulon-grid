import { createSelector } from "reselect";

import { getFilteredData } from "./data.selector";

export const getRowDimensions = createSelector(
	[state => state.axis.rows, state => state.dimensions],
	(rowAxis, dimensions) => rowAxis.map(id => dimensions[id])
);

export const getColumnDimensions = createSelector(
	[state => state.axis.columns, state => state.dimensions],
	(columnAxis, dimensions) => columnAxis.map(id => dimensions[id])
);

export const getAvailableDimensions = createSelector(
	[state => state.axis.dimensions, state => state.dimensions],
	(dimensionAxis, dimensions) => dimensionAxis.map(id => dimensions[id])
);

const getMeasures = state => state.measures;

export const getActivatedMeasures = createSelector([getMeasures], measures =>
	Object.keys(measures)
		.map(id => measures[id])
		.filter(dimension => dimension.activated)
);

