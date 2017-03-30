import { CHANGE_SORT_ORDER } from '../constants';

import { changeSortOrder } from './field.action';

describe('changeSortOrder creates correct action', () => {
  test('in normal case', () => {
    expect(changeSortOrder('toto')).toEqual({
      type: CHANGE_SORT_ORDER,
      fieldId: 'toto'
    });
  });
});
