import { SELECT_RANGE } from "../constants";

export const selectRange = selectedRange => {
	return {
		type: SELECT_RANGE,
		selectedRange
	};
};

export const selectCell = cell => {
	return {
		type: SELECT_RANGE,
		selectedRange: {
			selectedCellStart: cell,
			selectedCellEnd: cell
		}
	};
};
