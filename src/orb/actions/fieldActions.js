export const CHANGE_SORT_ORDER = 'CHANGE_SORT_ORDER';
export const changeSortOrder = fieldId => ({
  type: CHANGE_SORT_ORDER,
  field: fieldId,
});
