import { connect } from 'react-redux';

import GridConfiguration from '../components/GridConfiguration';
import { addField, removeField, toggleDatafield } from '../actions';

const getRowFields = state => state.axis.rows.map(id => state.fields[id]);
const getColumnFields = state => state.axis.columns.map(id => state.fields[id]);
const getAvailableFields = state => state.axis.fields.map(id => state.fields[id]);

const mapStateToProps = state => ({
  datafields: state.datafields,
  rowFields: getRowFields(state),
  columnFields: getColumnFields(state),
  availableFields: getAvailableFields(state),
});

const mapDispatchToProps = dispatch => ({
  moveField: (fieldId, oldAxis, newAxis, position) => {
    dispatch(removeField(fieldId, oldAxis));
    dispatch(addField(fieldId, newAxis, position));
  },
  toggleDatafield: (fieldId) => {
    dispatch(toggleDatafield(fieldId));
  },
});

export default connect(mapStateToProps, mapDispatchToProps)(GridConfiguration);
