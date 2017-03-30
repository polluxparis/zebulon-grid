import React from 'react';
import { shallow } from 'enzyme';
import DataCells from './DataCells';

describe('DataCells', () => {
  test('renders without crashing', () => {
    shallow(
      <DataCells
        columnCount={2000}
        rowCount={4000}
        height={1400}
        width={3000}
        getColumnWidth={() => 200}
        getRowHeight={() => 30}
      />
    );
  });
});
