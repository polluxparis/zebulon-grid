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
            aggregateFuncName: 'sum'
          },
          {
            id: 'amt',
            name: 'amt',
            caption: 'Amount',
            aggregateFuncName: 'whatever'
          }
        ]
      }
    );
    expect(state).toEqual({
      qty: {
        id: 'qty',
        name: 'qty',
        caption: 'Quantity',
        aggregateFuncName: 'sum'
      },
      amt: {
        id: 'amt',
        name: 'amt',
        caption: 'Amount',
        aggregateFuncName: 'whatever'
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
          aggregateFuncName: 'sum'
        },
        amt: {
          id: 'amt',
          name: 'amt',
          caption: 'Amount',
          aggregateFuncName: 'whatever'
        }
      },
      { type: TOGGLE_DATAFIELD, id: 'qty' }
    );
    expect(state).toEqual({
      qty: {
        id: 'qty',
        name: 'qty',
        caption: 'Quantity',
        aggregateFuncName: 'sum',
        activated: true
      },
      amt: {
        id: 'amt',
        name: 'amt',
        caption: 'Amount',
        aggregateFuncName: 'whatever'
      }
    });
  });
  test('with __FOO__ action', () => {
    const bogusState = { foo: 'bar' };
    expect(reducer(bogusState, { type: '__FOO__' })).toEqual(bogusState);
  });
});
