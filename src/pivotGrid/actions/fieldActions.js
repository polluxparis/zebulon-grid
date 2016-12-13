import { CHANGE_SORT_ORDER } from '../constants';

export const changeSortOrder = fieldId => ({
  type: CHANGE_SORT_ORDER,
  field: fieldId,
});
