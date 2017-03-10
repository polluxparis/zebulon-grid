import reducer from './fields.reducer';

describe('fields reducer', () => {
  test('with setFields action', () => {
    const state = reducer(
      {},
      {
        type: 'SET_FIELDS',
        fields: [
          {
            id: 'toto',
            name: 'toto_lb',
            caption: 'Toto',
            sort: { order: 'asc' },
            subTotal: {}
          },
          {
            id: 'titi',
            name: 'titi',
            caption: 'Titi',
            sort: { order: null },
            subTotal: {}
          },
          {
            id: 'tutu',
            name: 'tutu',
            caption: 'Tutu',
            sort: { order: null },
            subTotal: {}
          }
        ]
      }
    );
    expect(state).toEqual({
      toto: {
        id: 'toto',
        name: 'toto_lb',
        caption: 'Toto',
        sort: { order: 'asc' },
        subTotal: {}
      },
      titi: {
        id: 'titi',
        name: 'titi',
        caption: 'Titi',
        sort: { order: null },
        subTotal: {}
      },
      tutu: {
        id: 'tutu',
        name: 'tutu',
        caption: 'Tutu',
        sort: { order: null },
        subTotal: {}
      }
    });
  });
  describe('with changeSortOrderAction', () => {
    test('with ascending sorted field', () => {
      const state = {
        toto: {
          id: 'toto',
          name: 'toto_lb',
          caption: 'Toto',
          sort: { order: 'asc' },
          subTotal: {}
        }
      };
      expect(
        reducer(state, {
          type: 'CHANGE_SORT_ORDER',
          fieldId: 'toto'
        })
      ).toEqual({
        toto: {
          id: 'toto',
          name: 'toto_lb',
          caption: 'Toto',
          sort: { order: 'desc' },
          subTotal: {}
        }
      });
    });
    test('with descending sorted field', () => {
      const state = {
        toto: {
          id: 'toto',
          name: 'toto_lb',
          caption: 'Toto',
          sort: { order: 'desc' },
          subTotal: {}
        }
      };
      expect(
        reducer(state, {
          type: 'CHANGE_SORT_ORDER',
          fieldId: 'toto'
        })
      ).toEqual({
        toto: {
          id: 'toto',
          name: 'toto_lb',
          caption: 'Toto',
          sort: { order: 'asc' },
          subTotal: {}
        }
      });
    });
    test('with unsorted field', () => {
      const state = {
        toto: {
          id: 'toto',
          name: 'toto_lb',
          caption: 'Toto',
          sort: { order: null },
          subTotal: {}
        }
      };
      expect(
        reducer(state, {
          type: 'CHANGE_SORT_ORDER',
          fieldId: 'toto'
        })
      ).toEqual({
        toto: {
          id: 'toto',
          name: 'toto_lb',
          caption: 'Toto',
          sort: { order: 'asc' },
          subTotal: {}
        }
      });
    });
  });
  test('with __FOO__ action', () => {
    const bogusState = { foo: 'bar' };
    expect(reducer(bogusState, { type: '__FOO__' })).toEqual(bogusState);
  });
});
