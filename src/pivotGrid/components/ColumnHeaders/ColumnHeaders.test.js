import React from 'react';
import { shallow } from 'enzyme';
import ColumnHeaders from './ColumnHeaders';

describe('ColumnHeaders', () => {
  it('renders without crashing', () => {
    shallow(<ColumnHeaders />);
  });
});
