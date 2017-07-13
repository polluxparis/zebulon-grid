import { CHANGE_SORT_ORDER } from '../constants';

export const toggleSortOrder = key => ({
	type: CHANGE_SORT_ORDER,
	key
});
