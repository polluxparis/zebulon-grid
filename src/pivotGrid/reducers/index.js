import { combineReducers } from 'redux';

import data from './data.reducer';
import config from './config.reducer';
import fields from './fields.reducer';
import datafields from './datafields.reducer';
import axis from './axis.reducer';
import sizes from './sizes.reducer';
import filters from './filters.reducer';

export default combineReducers({
  data,
  config,
  fields,
  datafields,
  axis,
  sizes,
  filters
});
