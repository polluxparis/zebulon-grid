import { CHANGE_SORT_ORDER } from '../constants';

/* eslint-disable import/prefer-default-export */
export const changeSortOrder = fieldId => ({
  type: CHANGE_SORT_ORDER,
  fieldId
});
/* eslint-enable */
