import React from 'react';
import { shallow } from 'enzyme';
import DataCell from './index';

describe('DataCell', () => {
  it('renders without crashing', () => {
    shallow(<DataCell />);
  });
});
