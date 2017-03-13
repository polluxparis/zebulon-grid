import { SET_CONFIG_PROPERTY, ZOOM_IN, ZOOM_OUT } from '../constants';

function getNextZoom(previousZoom, zoomIn) {
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

  const zoomIndex = zoomValues.indexOf(previousZoom);
  let nextZoomIndex;
  if (zoomIn) {
    nextZoomIndex = Math.min(zoomIndex + 1, zoomValues.length - 1);
  } else {
    nextZoomIndex = Math.max(zoomIndex - 1, 0);
  }
  return zoomValues[nextZoomIndex];
}
export default (state = {}, action) => {
  const { type, property, value } = action;
  switch (type) {
    case SET_CONFIG_PROPERTY:
      return { ...state, [property]: value };
    case ZOOM_IN:
      return { ...state, zoom: getNextZoom(state.zoom, true) };
    case ZOOM_OUT:
      return { ...state, zoom: getNextZoom(state.zoom, false) };

    default:
      return state;
  }
};
