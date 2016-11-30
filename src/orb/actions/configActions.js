import { Config } from '../Config';
import { AxisType } from '../Axis';
import { SET_CONFIG, SET_CONFIG_PROPERTY, TOGGLE_DATAFIELD, MOVE_FIELD } from '../constants';


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

export const setConfig = configObject => ({
  type: SET_CONFIG,
  config: new Config(configObject),
});

export const setConfigProperty = (configObject, property, defaultValue) => ({
  type: SET_CONFIG_PROPERTY,
  property,
  value: configObject[property] || defaultValue,
});

export const toggleDatafield = datafieldId => ({
  type: TOGGLE_DATAFIELD,
  id: datafieldId,
});

export const moveField = (fieldId, oldAxis, newAxis, position) => ({
  type: MOVE_FIELD,
  id: fieldId,
  oldAxis: axisTypeToString(oldAxis),
  newAxis: axisTypeToString(newAxis),
  position,
});
