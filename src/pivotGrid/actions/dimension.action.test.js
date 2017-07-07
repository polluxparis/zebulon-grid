import { CHANGE_SORT_ORDER } from '../constants';

import { toggleSortOrder } from './dimension.action';

describe('toggleSortOrder creates correct action', () => {
	test('in normal case', () => {
		expect(toggleSortOrder('toto')).toEqual({
			type: CHANGE_SORT_ORDER,
			dimensionId: 'toto'
		});
	});
});
