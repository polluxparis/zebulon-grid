import { AxisType, toAxis } from '../Axis';
import { EXPAND_COLLAPSE, EXPAND_COLLAPSE_ATTRIBUTE } from '../constants';

/* eslint-disable import/prefer-default-export */
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
/* eslint-enable */
