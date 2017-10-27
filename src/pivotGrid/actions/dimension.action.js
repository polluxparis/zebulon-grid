import { CHANGE_SORT_ORDER, toAxis } from "../constants";
// import { toggleSortOrderSelector } from "../selectors";
export const toggleSortOrder = (axis, depth) => {
	// toggleSortOrderSelector(axis, depth);
	return {
		type: CHANGE_SORT_ORDER,
		axis: toAxis(axis)
	};
};
