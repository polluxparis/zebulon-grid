import { PUSH_DATA, SET_DATA } from '../constants';
import { pushData, setData } from './data.action';

describe('pushData create correct action', () => {
  test('when payload is an array of objects', () => {
    const payload = [
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
    ];
    expect(pushData(payload)).toEqual({
      type: PUSH_DATA,
      payload
    });
  });
  test('when payload is an object', () => {
    const payload = {
      toto: 0,
      toto_lb: 'toto 0',
      titi: 'titi 0',
      tutu: '0',
      qty: 0,
      amt: 0
    };
    expect(pushData(payload)).toEqual({
      type: PUSH_DATA,
      payload: [payload]
    });
  });
  test('when payload is an array of arrays of objects', () => {
    const payload = [
      [
        {
          toto: 0,
          toto_lb: 'toto 0',
          titi: 'titi 0',
          tutu: '0',
          qty: 0,
          amt: 0
        }
      ],
      [
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
      ]
    ];
    expect(pushData(payload)).toEqual({
      type: PUSH_DATA,
      payload
    });
  });
  test('when payload is null', () => {
    const payload = null;
    expect(pushData(payload)).toEqual({
      type: PUSH_DATA,
      payload: []
    });
  });
  test('when payload is a function', () => {
    const payload = () => {};
    expect(pushData(payload)).toEqual({
      type: PUSH_DATA,
      payload: []
    });
  });
});

describe('setData create correct action', () => {
  test('when payload is an array of objects', () => {
    const payload = [
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
    ];
    expect(setData(payload)).toEqual({
      type: SET_DATA,
      payload
    });
  });
  test('when payload is an object', () => {
    const payload = {
      toto: 0,
      toto_lb: 'toto 0',
      titi: 'titi 0',
      tutu: '0',
      qty: 0,
      amt: 0
    };
    expect(setData(payload)).toEqual({
      type: SET_DATA,
      payload: [payload]
    });
  });
  test('when payload is an array of arrays of objects', () => {
    const payload = [
      [
        {
          toto: 0,
          toto_lb: 'toto 0',
          titi: 'titi 0',
          tutu: '0',
          qty: 0,
          amt: 0
        }
      ],
      [
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
      ]
    ];
    expect(setData(payload)).toEqual({
      type: SET_DATA,
      payload
    });
  });
  test('when payload is null', () => {
    const payload = null;
    expect(setData(payload)).toEqual({
      type: SET_DATA,
      payload: []
    });
  });
  test('when payload is a function', () => {
    const payload = () => {};
    expect(setData(payload)).toEqual({
      type: SET_DATA,
      payload: []
    });
  });
});
