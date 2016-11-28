import { Config } from '../Config';
import { AxisType } from '../Axis';

const axisTypeToString = (type) => {
  switch (type) {
    case AxisType.ROWS:
      return 'rows';
    case AxisType.COLUMNS:
      return 'columns';
    default:
      return 'fields';
  }
};

export const SET_CONFIG = 'SET_CONFIG';
export const setConfig = configObject => ({
  type: SET_CONFIG,
  config: new Config(configObject),
});

export const SET_CONFIG_PROPERTY = 'SET_CONFIG_PROPERTY';
export const setConfigProperty = (configObject, property, defaultValue) => ({
  type: SET_CONFIG_PROPERTY,
  property,
  value: configObject[property] || defaultValue,
});

export const TOGGLE_DATAFIELD = 'TOGGLE_DATAFIELD';
export const toggleDatafield = datafieldId => ({
  type: TOGGLE_DATAFIELD,
  id: datafieldId,
});

export const ADD_FIELD = 'ADD_FIELD';
export const addField = (fieldId, axis, position) => ({
  type: ADD_FIELD,
  id: fieldId,
  axis: axisTypeToString(axis),
  position,
});

export const REMOVE_FIELD = 'REMOVE_FIELD';
export const removeField = (fieldId, axis) => ({
  type: REMOVE_FIELD,
  id: fieldId,
  axis: axisTypeToString(axis),
});
