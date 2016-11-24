import { connect } from 'react-redux';

import { AxisType } from '../Axis';
import GridConfiguration from '../components/GridConfiguration';
import { addField, removeField, toggleDatafield } from '../actions';

function getAvailableFields(rows, columns, fields) {
  return Object.keys(fields)
    .filter(fieldId => !(rows.includes(fieldId) || columns.includes(fieldId)))
    .map(fieldId => fields[fieldId]);
}

function getFields(axis, fields) {
  return axis.map(id => fields[id]);
}

function moveField(dispatch, fieldId, oldAxis, newAxis, position) {
  if ([AxisType.ROWS, AxisType.COLUMNS].includes(oldAxis)) {
    dispatch(removeField(fieldId, oldAxis));
  }
  if ([AxisType.ROWS, AxisType.COLUMNS].includes(newAxis)) {
    dispatch(addField(fieldId, newAxis, position));
  }
}

const mapStateToProps = state => ({
  datafields: state.datafields,
  rowFields: getFields(state.axis.rows, state.fields),
  columnFields: getFields(state.axis.columns, state.fields),
  availableFields: getAvailableFields(state.axis.rows, state.axis.columns, state.fields),
});

const mapDispatchToProps = dispatch => ({
  moveField: (fieldId, oldAxis, newAxis, position) => {
    moveField(dispatch, fieldId, oldAxis, newAxis, position);
  },
  toggleDatafield: (fieldId) => {
    dispatch(toggleDatafield(fieldId));
  },
});

export default connect(mapStateToProps, mapDispatchToProps)(GridConfiguration);
