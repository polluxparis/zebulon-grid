import { toAxis, EXPAND_COLLAPSE } from '../constants';

export const toggleCollapse = ({ axisType, key }) => {
	return {
		type: EXPAND_COLLAPSE,
		key,
		axis: toAxis(axisType)
	};
};
export const toggleCollapseDimension = ({ key }) => {
	return {
		type: EXPAND_COLLAPSE,
		key,
		axis: 'dimensions'
	};
};
