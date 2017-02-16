import React from 'react';
import { shallow } from 'enzyme';
import DimensionHeaders from './DimensionHeaders';

describe('DimensionHeaders', () => {
  it('renders without crashing', () => {
    shallow(<DimensionHeaders />);
  });
});
