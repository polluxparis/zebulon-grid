import { dimensionFactory, measureFactory } from "../utils/configuration";
import {
  SET_AXIS,
  SET_DIMENSIONS,
  SET_MEASURES,
  SET_CONFIG_PROPERTY,
  TOGGLE_MEASURE,
  MOVE_MEASURE,
  MOVE_DIMENSION,
  ZOOM_IN,
  ZOOM_OUT,
  LOADING_CONFIG
} from "../constants";
export const loadingConfig = loading => ({
  type: LOADING_CONFIG,
  loading
});
export const setDimensions = (configObject, configurationFunctions) => ({
  type: SET_DIMENSIONS,
  dimensions: configObject.dimensions.map(dimension =>
    dimensionFactory(dimension, configurationFunctions)
  )
});

export const setMeasures = (configObject, configurationFunctions) => ({
  type: SET_MEASURES,
  measures: configObject.measures.map(measure =>
    measureFactory(measure, configurationFunctions)
  )
});
export const setConfigurationProperty = (
  configObject,
  property,
  defaultValue
) => ({
  type: SET_CONFIG_PROPERTY,
  property,
  value: configObject[property] || defaultValue
});

export const toggleMeasure = measureId => ({
  type: TOGGLE_MEASURE,
  id: measureId
});
export const moveMeasure = (measureId, position) => ({
  type: MOVE_MEASURE,
  id: measureId,
  position
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
