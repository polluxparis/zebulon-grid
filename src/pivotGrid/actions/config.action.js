import { fieldFactory, datafieldFactory } from '../fields';
import {
  SET_FIELDS,
  SET_DATAFIELDS,
  SET_CONFIG_PROPERTY,
  TOGGLE_DATAFIELD,
  MOVE_FIELD,
  ZOOM_IN,
  ZOOM_OUT
} from '../constants';

export const setFields = configObject => ({
  type: SET_FIELDS,
  fields: configObject.fields.map(field => fieldFactory(field))
});

export const setDatafields = configObject => ({
  type: SET_DATAFIELDS,
  datafields: configObject.datafields.map(field => datafieldFactory(field))
});

export const setConfigProperty = (configObject, property, defaultValue) => ({
  type: SET_CONFIG_PROPERTY,
  property,
  value: configObject[property] || defaultValue
});

export const toggleDatafield = datafieldId => ({
  type: TOGGLE_DATAFIELD,
  id: datafieldId
});

export const moveField = (fieldId, oldAxis, newAxis, position) => ({
  type: MOVE_FIELD,
  id: fieldId,
  oldAxis,
  newAxis,
  position
});

export const zoomIn = () => ({ type: ZOOM_IN });
export const zoomOut = () => ({ type: ZOOM_OUT });
