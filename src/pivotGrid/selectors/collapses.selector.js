import { createSelector } from 'reselect';

export const getIsCollapsedColumnByKeySelector = createSelector(
	[state => state.collapses.columns],
	areCollapsed => key => {
		return areCollapsed[key] || false;
	}
);
export const getIsCollapsedRowByKeySelector = createSelector(
	[state => state.collapses.rows],
	areCollapsed => key => {
		return areCollapsed[key] || false;
	}
);
