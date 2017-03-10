import reducer from './filters.reducer';

describe('filters reducer', () => {
  test('with ADD_FILTER action', () => {
    const state = {};
    expect(
      reducer(state, {
        type: 'ADD_FILTER',
        field: 'titi',
        filter: { inFilter: true }
      })
    ).toEqual({ titi: { inFilter: true } });
  });
  test('with DELETE_FILTER action', () => {
    const state = { titi: { inFilter: true }, tutu: { inFilter: true } };
    expect(
      reducer(state, {
        type: 'DELETE_FILTER',
        field: 'titi'
      })
    ).toEqual({ tutu: { inFilter: true } });
  });
  test('with __FOO__ action', () => {
    const bogusState = { foo: 'bar' };
    expect(reducer(bogusState, { type: '__FOO__' })).toEqual(bogusState);
  });
});
