import { Config } from '../Config';
import { AxisType } from '../Axis';

export const SET_CONFIG = 'SET_CONFIG';
export const setConfig = configObject => ({
  type: SET_CONFIG,
  config: new Config(configObject),
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
  axis: axis === AxisType.ROWS ? 'rows' : 'columns',
  position,
});

export const REMOVE_FIELD = 'REMOVE_FIELD';
export const removeField = (fieldId, axis) => ({
  type: REMOVE_FIELD,
  id: fieldId,
  axis: axis === AxisType.ROWS ? 'rows' : 'columns',
});
