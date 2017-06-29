import { AxisType, toAxis } from '../Axis';
import { EXPAND_COLLAPSE } from '../constants';

/* eslint-disable import/prefer-default-export */
export const toggleCollapse = ({ axisType, key }) => {
	return {
		type: EXPAND_COLLAPSE,
		key,
		axis: toAxis(axisType)
	};
};
/* eslint-enable */
