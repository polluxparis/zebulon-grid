import { CHANGE_SORT_ORDER } from "../constants";

/* eslint-disable import/prefer-default-export */
export const changeSortOrder = dimensionId => ({
	type: CHANGE_SORT_ORDER,
	dimensionId
});
/* eslint-enable */
