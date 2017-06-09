import reducer from './datafields.reducer';
import { SET_DATAFIELDS, TOGGLE_DATAFIELD } from '../constants';

describe('datafield reducer', () => {
  test('with setDatafield action', () => {
    const state = reducer(
      {},
      {
        type: SET_DATAFIELDS,
        datafields: [
          {
            id: 'qty',
            name: 'qty',
            caption: 'Quantity',
            aggregationName: 'sum'
          },
          {
            id: 'amt',
            name: 'amt',
            caption: 'Amount',
            aggregationName: 'whatever'
          }
        ]
      }
    );
    expect(state).toEqual({
      qty: {
        id: 'qty',
        name: 'qty',
        caption: 'Quantity',
        aggregationName: 'sum'
      },
      amt: {
        id: 'amt',
        name: 'amt',
        caption: 'Amount',
        aggregationName: 'whatever'
      }
    });
  });
  test('with toggleDatafield action', () => {
    const state = reducer(
      {
        qty: {
          id: 'qty',
          name: 'qty',
          caption: 'Quantity',
          aggregationName: 'sum'
        },
        amt: {
          id: 'amt',
          name: 'amt',
          caption: 'Amount',
          aggregationName: 'whatever'
        }
      },
      { type: TOGGLE_DATAFIELD, id: 'qty' }
    );
    expect(state).toEqual({
      qty: {
        id: 'qty',
        name: 'qty',
        caption: 'Quantity',
        aggregationName: 'sum',
        activated: true
      },
      amt: {
        id: 'amt',
        name: 'amt',
        caption: 'Amount',
        aggregationName: 'whatever'
      }
    });
  });
  test('with __FOO__ action', () => {
    const bogusState = { foo: 'bar' };
    expect(reducer(bogusState, { type: '__FOO__' })).toEqual(bogusState);
  });
});
