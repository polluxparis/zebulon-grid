import React from 'react';
import { shallow } from 'enzyme';
import DimensionHeader from './DimensionHeader';

describe('DimensionHeader', () => {
  it('renders without crashing', () => {
    shallow(<DimensionHeader />);
  });
});
