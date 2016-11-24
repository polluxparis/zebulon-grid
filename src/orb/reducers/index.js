import { combineReducers } from 'redux';

import data from './dataReducer';
import config from './configReducer';
import fields from './fieldsReducer';
import datafields from './datafieldsReducer';
import axis from './axisReducer';

export default combineReducers({ data, config, fields, datafields, axis });
