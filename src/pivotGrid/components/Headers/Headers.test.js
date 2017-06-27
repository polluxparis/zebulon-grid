import React from 'react';
import { shallow } from 'enzyme';
import RowHeaders from './RowHeaders';

describe('RowHeaders', () => {
  test('renders without crashing', () => {
    shallow(
      <RowHeaders
        columnCount={2}
        rowCount={4000}
        height={8000}
        width={300}
        getColumnWidth={() => 200}
        getRowHeight={() => 30}
        getCrossSize={() => 60}
        getLastChildSize={() => 30}
        rowHeaders={[]}
        dimensionPositions={{ columns: {}, rows: {} }}
        previewSizes={{ height: 5000, width: 100 }}
        zoom={1}
        scrollTop={0}
      />
    );
  });
});
