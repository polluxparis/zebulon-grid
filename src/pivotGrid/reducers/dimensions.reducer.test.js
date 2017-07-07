import reducer from './dimensions.reducer';
import { CHANGE_SORT_ORDER, SET_DIMENSIONS } from '../constants';

describe('dimensions reducer', () => {
  test('with setDimensions action', () => {
    const state = reducer(
      {},
      {
        type: SET_DIMENSIONS,
        dimensions: [
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
  describe('with toggleSortOrderAction', () => {
    test('with ascending sorted dimension', () => {
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
          type: CHANGE_SORT_ORDER,
          dimensionId: 'toto'
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
    test('with descending sorted dimension', () => {
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
          type: CHANGE_SORT_ORDER,
          dimensionId: 'toto'
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
    test('with unsorted dimension', () => {
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
          type: CHANGE_SORT_ORDER,
          dimensionId: 'toto'
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
