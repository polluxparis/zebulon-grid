import { connect } from 'react-redux';

import PivotGrid from '../components/Grid/Grid';
import { getLayout, getHeaderSizes } from '../selectors';

const mapStateToProps = state => ({
  height: state.config.height,
  width: state.config.width,
  layout: getLayout(state),
  sizes: getHeaderSizes(state),
});

export default connect(mapStateToProps)(PivotGrid);
