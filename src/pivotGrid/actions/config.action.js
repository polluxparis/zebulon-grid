import { dimensionFactory, measureFactory } from '../utils/setConfig';
import {
  SET_AXIS,
  SET_DIMENSIONS,
  SET_MEASURES,
  SET_CONFIG_PROPERTY,
  TOGGLE_MEASURE,
  MOVE_DIMENSION,
  ZOOM_IN,
  ZOOM_OUT
} from '../constants';

export const setDimensions = (configObject, configFunctions) => ({
  type: SET_DIMENSIONS,
  dimensions: configObject.dimensions.map(dimension =>
    dimensionFactory(dimension, configFunctions)
  )
});

export const setMeasures = (configObject, configFunctions) => ({
  type: SET_MEASURES,
  measures: configObject.measures.map(measure =>
    measureFactory(measure, configFunctions)
  )
});
export const setConfigProperty = (configObject, property, defaultValue) => ({
  type: SET_CONFIG_PROPERTY,
  property,
  value: configObject[property] || defaultValue
});

export const toggleMeasure = measureId => ({
  type: TOGGLE_MEASURE,
  id: measureId
});

export const moveDimension = (dimensionId, oldAxis, newAxis, position) => ({
  type: MOVE_DIMENSION,
  id: dimensionId,
  oldAxis,
  newAxis,
  position
});

export const setAxis = axis => ({
  type: SET_AXIS,
  axis
});

export const zoomIn = () => {
  return { type: ZOOM_IN };
};
export const zoomOut = () => ({ type: ZOOM_OUT });
export const zoom = type => {
  return { type };
};
