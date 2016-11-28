import { connect } from 'react-redux';

import FilterPanel from '../components/FilterPanel';
import { getFieldValues, getFilters } from '../selectors';
import { addFilter, deleteFilter } from '../actions';

const mapStateToProps = (state, ownProps) => ({
  values: getFieldValues(state)(ownProps.field),
  filter: getFilters(state)[ownProps.field.id],
});

const mapDispatchToProps = (dispatch, ownProps) => ({
  handleFilter: (all, operator, term, staticValue, excludeStatic) => {
    if (all) {
      dispatch(deleteFilter(ownProps.field.id));
    } else {
      dispatch(addFilter(ownProps.field.id, operator, term, staticValue, excludeStatic));
    }
    ownProps.onHide();
  },
});

export default connect(mapStateToProps, mapDispatchToProps)(FilterPanel);
