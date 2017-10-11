import {
	toAxis,
	EXPAND_COLLAPSE,
	EXPAND_COLLAPSE_ALL,
	SET_COLLAPSES
} from "../constants";

export const toggleCollapse = ({ axisType, key, n }) => {
	return {
		type: EXPAND_COLLAPSE,
		key,
		axis: toAxis(axisType),
		n
	};
};
export const expandCollapseAll = ({ axisType, keys, n }) => {
	return {
		type: EXPAND_COLLAPSE_ALL,
		keys,
		axis: toAxis(axisType),
		n
	};
};
export const toggleCollapseDimension = ({ key }) => {
	return {
		type: EXPAND_COLLAPSE,
		key,
		axis: "dimensions"
	};
};
export const setCollapses = collapses => {
	return {
		type: SET_COLLAPSES,
		collapses
	};
};
