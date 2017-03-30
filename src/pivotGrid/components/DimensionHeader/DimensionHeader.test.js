import React from 'react';
import { shallow } from 'enzyme';
import DimensionHeader from './DimensionHeader';

describe('DimensionHeader', () => {
  test('renders without crashing', () => {
    const props = {
      left: 100,
      top: 0,
      width: 100,
      height: 30,
      field: {
        id: 'titi',
        name: 'titi',
        caption: 'Titi',
        sort: { order: 'asc' },
        subTotal: {}
      },
      mainDirection: 'right',
      crossFieldId: 'tutu',
      previewSizes: { height: 586, width: 1084 }
    };
    shallow(<DimensionHeader {...props} />);
  });
});
