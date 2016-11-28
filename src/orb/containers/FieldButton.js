import { connect } from 'react-redux';

import FieldButton from '../components/FieldButton';
import { changeSortOrder } from '../actions';

const mapStateToProps = () => ({});

const mapDispatchToProps = dispatch => ({
  sort: fieldId => dispatch(changeSortOrder(fieldId)),
});

export default connect(mapStateToProps, mapDispatchToProps)(FieldButton);
