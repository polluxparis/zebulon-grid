import {
  SET_CONFIG_PROPERTY,
  ZOOM,
  // ZOOM_OUT,
  MOVE_DIMENSION,
  MEASURE_ID
} from "../constants";

// function getNextZoom(previousZoom, zoomIn) {
//   const zoomValues = [
//     0.25,
//     0.33,
//     0.5,
//     0.67,
//     0.75,
//     0.9,
//     1,
//     1.1,
//     1.25,
//     1.5,
//     1.75,
//     2,
//     2.5,
//     3,
//     4,
//     5
//   ];

//   const zoomIndex = zoomValues.indexOf(previousZoom);
//   let nextZoomIndex;
//   if (zoomIn) {
//     nextZoomIndex = Math.min(zoomIndex + 1, zoomValues.length - 1);
//   } else {
//     nextZoomIndex = Math.max(zoomIndex - 1, 0);
//   }
//   return zoomValues[nextZoomIndex];
// }
export default (state = {}, action) => {
  const { type, property, value, id, oldAxis, newAxis, zoomValue } = action;

  switch (type) {
    case MOVE_DIMENSION:
      if (id === MEASURE_ID && oldAxis !== newAxis) {
        return { ...state, measureHeadersAxis: newAxis };
      } else return state;
    case SET_CONFIG_PROPERTY:
      return { ...state, [property]: value };
    case ZOOM:
      return { ...state, zoom: zoomValue };
    // case ZOOM_OUT:
    //   return { ...state, zoom: getNextZoom(state.zoom, false) };

    default:
      return state;
  }
};
