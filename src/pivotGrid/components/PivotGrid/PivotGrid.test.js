import React from 'react';
import { shallow } from 'enzyme';
import PivotGrid from './PivotGrid';

const OriginalPivotGrid = PivotGrid.DecoratedComponent;
describe('PivotGrid', () => {
  test('renders without crashing', () => {
    shallow(
      <OriginalPivotGrid
        connectDropTarget={i => i}
        width={1000}
        layout={{ columnHorizontalCount: 22, rowVerticalCount: 33 }}
        customFunctions={{}}
        drilldown={() => 33}
        columnDimensions={[]}
        columnHeaders={[]}
        dataDimensionsCount={1}
        rowDimensions={[]}
        rowHeaders={[]}
        setSizes={() => {}}
      />
    );
  });
});
