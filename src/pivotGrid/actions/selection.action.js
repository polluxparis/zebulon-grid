import { AxisType, toAxis } from '../Axis';
import { SELECT_RANGE } from '../constants';

/* eslint-disable import/prefer-default-export */
export const selectRange = selectedRange => {
	return {
		type: SELECT_RANGE,
		selectedRange
	};
};

export const selectCell = cell => {
	return {
		type: SELECT_RANGE,
		selectedRange: { selectedCellStart: cell, selectedCellEnd: cell }
	};
};

