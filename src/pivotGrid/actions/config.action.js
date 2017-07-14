import { dimensionFactory, measureFactory } from '../hydrateStore';
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

export const setDimensions = configObject => ({
  type: SET_DIMENSIONS,
  dimensions: configObject.dimensions.map(dimension =>
    dimensionFactory(dimension)
  )
});

export const setMeasures = configObject => ({
  type: SET_MEASURES,
  measures: configObject.measures.map(dimension => measureFactory(dimension))
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
  console.log('zoom in action');
  return { type: ZOOM_IN };
};
export const zoomOut = () => ({ type: ZOOM_OUT });
export const zoom = type => {
  console.log(['zoom action', type]);
  return { type };
};
