import { CHANGE_SORT_ORDER } from '../constants';

/* eslint-disable import/prefer-default-export */
export const toggleSortOrder = key => ({
	type: CHANGE_SORT_ORDER,
	key
});
/* eslint-enable */
