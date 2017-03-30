import reducer from './sizes.reducer';
import { UPDATE_CELL_SIZE } from '../constants';

describe('sizes reducer', () => {
  describe('with updateCellSizes action', () => {
    test('for dimension on row axis', () => {
      const state = {
        dimensions: { rows: {}, columns: {} },
        leafs: { rows: {}, columns: {} }
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
        dimensions: { columns: {}, rows: { tutu: 225 } },
        leafs: { columns: {}, rows: {} }
      });
    });
    test('for dimension on column axis', () => {
      const state = {
        dimensions: { rows: {}, columns: {} },
        leafs: { rows: {}, columns: {} }
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
        leafs: { columns: {}, rows: {} },
        dimensions: { rows: {}, columns: { tutu: 225 } }
      });
    });
    test('for leaf header on row axis', () => {
      const state = {
        dimensions: { rows: {}, columns: {} },
        leafs: { rows: {}, columns: {} }
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
        leafs: { columns: {}, rows: { tutu: 225 } },
        dimensions: { columns: {}, rows: {} }
      });
    });
    test('for leaf header on column axis', () => {
      const state = {
        dimensions: { rows: {}, columns: {} },
        leafs: { rows: {}, columns: {} }
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
        dimensions: { columns: {}, rows: {} },
        leafs: { rows: {}, columns: { tutu: 225 } }
      });
    });
  });
  test('with __FOO__ action', () => {
    const bogusState = { foo: 'bar' };
    expect(reducer(bogusState, { type: '__FOO__' })).toEqual(bogusState);
  });
});
