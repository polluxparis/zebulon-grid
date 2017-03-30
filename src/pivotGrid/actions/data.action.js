import { PUSH_DATA, SET_DATA } from '../constants';

const validateData = payload => {
  // typeof null is 'object'...
  const isObject = obj => typeof obj === 'object' && obj !== null;
  if (Array.isArray(payload)) {
    if (Array.isArray(payload[0]) || isObject(payload[0])) {
      // Payload is an array of arrays of objects or an array of object
      return payload;
    }
  } else if (isObject(payload)) {
    return [payload];
  }
  // Invalid payload
  return [];
};

export const pushData = payload => ({
  type: PUSH_DATA,
  payload: validateData(payload)
});

export const setData = payload => ({
  type: SET_DATA,
  payload: validateData(payload)
});
