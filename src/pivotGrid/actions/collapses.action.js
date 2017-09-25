import {
	toAxis,
	EXPAND_COLLAPSE,
	EXPAND_COLLAPSE_ALL,
	SET_COLLAPSES
} from "../constants";

export const toggleCollapse = ({ axisType, key }) => {
	return {
		type: EXPAND_COLLAPSE,
		key,
		axis: toAxis(axisType)
	};
};
export const expandCollapseAll = ({ axisType, keys }) => {
	return {
		type: EXPAND_COLLAPSE_ALL,
		keys,
		axis: toAxis(axisType)
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
