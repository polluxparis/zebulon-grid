import {
  PUSH_DATA,
  FETCH_SUCCESS,
  FETCH_DATA,
  FETCH_FAILURE
} from "../constants";

export const fetchData = () => ({ type: FETCH_DATA });
export const fetchFailure = error => ({ type: FETCH_FAILURE, error });

const validateData = payload => {
  // typeof null is 'object'...
  const isObject = obj => typeof obj === "object" && obj !== null;
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

export const fetchSuccess = payload => ({
  type: FETCH_SUCCESS,
  payload: validateData(payload)
});
