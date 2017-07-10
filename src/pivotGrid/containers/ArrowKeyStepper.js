import { connect } from 'react-redux';

import { selectedRangeSelector } from '../selectors';
import ArrowKeyStepper from '../components/ArrowKeyStepper/ArrowKeyStepper';

const mapStateToProps = (state, ownProps) => {
  // const rowDimensionHeaders = rowHeaders.dimensionHeaders;
  // const columnDimensionHeaders = columnHeaders.dimensionHeaders;
  return {
    selectedRange: selectedRangeSelector(state)
  };
};

export default connect(mapStateToProps)(ArrowKeyStepper);
