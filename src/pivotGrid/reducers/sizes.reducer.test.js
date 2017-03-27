import reducer from './sizes.reducer';
import { UPDATE_CELL_SIZE } from '../constants';

describe('sizes reducer', () => {
  describe('with updateCellSizes action', () => {
    test('for dimension on row axis', () => {
      const state = {
        rows: { leafs: {}, dimensions: {} },
        columns: { leafs: {}, dimensions: {} }
      };
      expect(
        reducer(state, {
          type: UPDATE_CELL_SIZE,
          id: 'tutu',
          size: 225,
          axis: 'rows',
          direction: 'dimensions'
        })
      ).toEqual({
        rows: { leafs: {}, dimensions: { tutu: 225 } },
        columns: { leafs: {}, dimensions: {} }
      });
    });
    test('for dimension on column axis', () => {
      const state = {
        rows: { leafs: {}, dimensions: {} },
        columns: { leafs: {}, dimensions: {} }
      };
      expect(
        reducer(state, {
          type: UPDATE_CELL_SIZE,
          id: 'tutu',
          size: 225,
          axis: 'columns',
          direction: 'dimensions'
        })
      ).toEqual({
        columns: { leafs: {}, dimensions: { tutu: 225 } },
        rows: { leafs: {}, dimensions: {} }
      });
    });
    test('for leaf header on row axis', () => {
      const state = {
        rows: { leafs: {}, dimensions: {} },
        columns: { leafs: {}, dimensions: {} }
      };
      expect(
        reducer(state, {
          type: UPDATE_CELL_SIZE,
          id: 'tutu',
          size: 225,
          axis: 'rows',
          direction: 'leafs'
        })
      ).toEqual({
        rows: { dimensions: {}, leafs: { tutu: 225 } },
        columns: { leafs: {}, dimensions: {} }
      });
    });
    test('for leaf header on column axis', () => {
      const state = {
        rows: { leafs: {}, dimensions: {} },
        columns: { leafs: {}, dimensions: {} }
      };
      expect(
        reducer(state, {
          type: UPDATE_CELL_SIZE,
          id: 'tutu',
          size: 225,
          axis: 'columns',
          direction: 'leafs'
        })
      ).toEqual({
        columns: { dimensions: {}, leafs: { tutu: 225 } },
        rows: { leafs: {}, dimensions: {} }
      });
    });
  });
  test('with __FOO__ action', () => {
    const bogusState = { foo: 'bar' };
    expect(reducer(bogusState, { type: '__FOO__' })).toEqual(bogusState);
  });
});
