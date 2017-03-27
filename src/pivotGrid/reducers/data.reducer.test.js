import reducer from './data.reducer';
import { PUSH_DATA, SET_DATA } from '../constants';

describe('data reducer', () => {
  test('with setData action', () => {
    const state = [
      {
        toto: 0,
        toto_lb: 'toto 0',
        titi: 'titi 0',
        tutu: '0',
        qty: 0,
        amt: 0
      },
      {
        toto: 0,
        toto_lb: 'toto 0',
        titi: 'titi 0',
        tutu: '1',
        qty: 1,
        amt: 1
      }
    ];
    expect(
      reducer(state, {
        type: SET_DATA,
        payload: [
          {
            toto: 0,
            toto_lb: 'toto 0',
            titi: 'titi 1',
            tutu: '0',
            qty: 10,
            amt: 10
          }
        ]
      })
    ).toEqual([
      {
        toto: 0,
        toto_lb: 'toto 0',
        titi: 'titi 1',
        tutu: '0',
        qty: 10,
        amt: 10
      }
    ]);
  });
  test('with pushData action', () => {
    const state = [
      {
        toto: 0,
        toto_lb: 'toto 0',
        titi: 'titi 0',
        tutu: '0',
        qty: 0,
        amt: 0
      },
      {
        toto: 0,
        toto_lb: 'toto 0',
        titi: 'titi 0',
        tutu: '1',
        qty: 1,
        amt: 1
      }
    ];
    expect(
      reducer(state, {
        type: PUSH_DATA,
        payload: [
          {
            toto: 0,
            toto_lb: 'toto 0',
            titi: 'titi 1',
            tutu: '0',
            qty: 10,
            amt: 10
          }
        ]
      })
    ).toEqual([
      {
        toto: 0,
        toto_lb: 'toto 0',
        titi: 'titi 0',
        tutu: '0',
        qty: 0,
        amt: 0
      },
      {
        toto: 0,
        toto_lb: 'toto 0',
        titi: 'titi 0',
        tutu: '1',
        qty: 1,
        amt: 1
      },
      {
        toto: 0,
        toto_lb: 'toto 0',
        titi: 'titi 1',
        tutu: '0',
        qty: 10,
        amt: 10
      }
    ]);
  });
  test('with __FOO__ action', () => {
    const bogusState = { foo: 'bar' };
    expect(reducer(bogusState, { type: '__FOO__' })).toEqual(bogusState);
  });
});
