import reducer from './config.reducer';
import { SET_CONFIG_PROPERTY } from '../constants';

test('setConfigProperty reducer', () => {
  const state = reducer(
    {},
    {
      type: SET_CONFIG_PROPERTY,
      property: 'dataHeadersLocation',
      value: 'columns'
    }
  );
  expect(state).toEqual({
    dataHeadersLocation: 'columns'
  });
  test('with __FOO__ action', () => {
    const bogusState = { foo: 'bar' };
    expect(reducer(bogusState, { type: '__FOO__' })).toEqual(bogusState);
  });
});
