import { PUSH_DATA, SET_DATA } from '../constants';

export const pushData = (payload) => {
  if (Array.isArray(payload) && (Array.isArray(payload[0]) || typeof payload[0] === 'object')) {
    // Payload is an array of arrays of objects
    return { type: PUSH_DATA, payload };
  } else if (Array.isArray(payload) || typeof payload === 'object') {
    // Payload is an array of objects
    return { type: PUSH_DATA, payload: [payload] };
  }
  // Invalided payload
  return { type: PUSH_DATA, payload: [] };
};

export const setData = (payload) => {
  if (Array.isArray(payload) && (Array.isArray(payload[0]) || typeof payload[0] === 'object')) {
    // Payload is an array of arrays of objects
    return { type: SET_DATA, payload };
  } else if (Array.isArray(payload) || typeof payload === 'object') {
    // Payload is an array of objects
    return { type: SET_DATA, payload: [payload] };
  }
  // Invalided payload
  return { type: SET_DATA, payload: [] };
};
