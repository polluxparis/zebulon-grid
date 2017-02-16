import React from 'react';
import { shallow } from 'enzyme';
import DataCells from './DataCells';

describe('DataCells', () => {
  it('renders without crashing', () => {
    shallow(<DataCells />);
  });
});
