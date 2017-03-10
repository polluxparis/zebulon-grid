import { SET_CONFIG_PROPERTY, ZOOM_IN, ZOOM_OUT } from '../constants';

const zoomValues = [
  0.25,
  0.33,
  0.5,
  0.67,
  0.75,
  0.9,
  1,
  1.1,
  1.25,
  1.5,
  1.75,
  2,
  2.5,
  3,
  4,
  5
];

export default (state = {}, action) => {
  const { type, property, value } = action;
  switch (type) {
    case SET_CONFIG_PROPERTY:
      return { ...state, [property]: value };
    case ZOOM_IN:
      return { ...state, zoom: 1 };
    case ZOOM_OUT:
      return { ...state, zoom: 1 };

    default:
      return state;
  }
};
