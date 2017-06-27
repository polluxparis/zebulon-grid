import React from 'react';
import { shallow } from 'enzyme';
import ColumnHeaders from './ColumnHeaders';

describe('ColumnHeaders', () => {
  test('renders without crashing', () => {
    shallow(
      <ColumnHeaders
        columnCount={2000}
        rowCount={4}
        height={800}
        width={3000}
        getColumnWidth={() => 200}
        getRowHeight={() => 30}
        getCrossSize={() => 60}
        getLastChildSize={() => 200}
        columnHeaders={[]}
        dimensionPositions={{ columns: {}, rows: {} }}
        previewSizes={{ height: 500, width: 1000 }}
        zoom={1}
        scrollLeft={0}
      />
    );
  });
});
