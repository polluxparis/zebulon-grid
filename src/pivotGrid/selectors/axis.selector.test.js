import { getAxisActivatedMeasuresSelector } from './axis.selector';
// import { measureFactory, dimensionFactory } from '../dimensions';
import { AxisType } from '../Axis';

describe('getAxisActivatedMeasuresSelector', () => {
  const measures = [{ id: 0 }];
  const measureHeadersAxis = 'rows';
  test('returns measures when axis has measures', () => {
    const res = getAxisActivatedMeasuresSelector(AxisType.ROWS).resultFunc(
      measures,
      measureHeadersAxis
    );
    expect(res).toEqual(measures);
  });
  test('returns null when axis is not measures axis', () => {
    const res = getAxisActivatedMeasuresSelector(AxisType.COLUMNS).resultFunc(
      measures,
      measureHeadersAxis
    );
    expect(res).toEqual(null);
  });
});

describe('buildAxisHeaders', () => {
  test('build the correct row headers', () => {
    const data = 1;
    const axisTree = 1;
    const dimensions = 1;
    const measures = 1;
    const areCollapsed = 1;
    const actual = buildAxisHeaders(
      data,
      axisTree,
      dimensions,
      measures,
      areCollapsed
    );
    // const expexted = 1;
    // expect(actual).toEqual(expexted);
    expect(actual).toMatchSnapshot();
  });
});
// xdescribe('axis with one dimension', () => {
//   test('works', () => {
//     const dimensions = {
//       dimension1: dimensionFactory({ accessor: 'dimension1' }),
//       dimension2: dimensionFactory({ accessor: 'dimension2' })
//     };
//     const data = [
//       { dimension1: 0, dimension2: 0 },
//       { dimension1: 1, dimension2: 2 },
//       { dimension1: 2, dimension2: 4 },
//       { dimension1: 1, dimension2: 8 }
//     ];
//     const axis = { rows: ['dimension1'] };
//     const rowAxis = getRowAxis({
//       dimensions,
//       data,
//       axis,
//       filters: {}
//     });
//     expect(rowAxis.type).toEqual(AxisType.ROWS);
//     expect(rowAxis.dimensions.length).toEqual(1);
//     expect(rowAxis.dimensions[0].id).toEqual('dimension1');
//     expect(rowAxis.root.values.length).toEqual(3);
//     expect(rowAxis.root.values).toEqual([0, 1, 2]);
//   });
//   test('with descending sorting', () => {
//     const dimensions = {
//       dimension1: dimensionFactory({
//         accessor: 'dimension1',
//         sort: { order: 'desc' }
//       }),
//       dimension2: dimensionFactory({ accessor: 'dimension2' })
//     };
//     const data = [
//       { dimension1: 0, dimension2: 0 },
//       { dimension1: 1, dimension2: 2 },
//       { dimension1: 2, dimension2: 4 },
//       { dimension1: 1, dimension2: 8 }
//     ];
//     const axis = { rows: ['dimension1'] };
//     const rowAxis = getRowAxis({
//       dimensions,
//       data,
//       axis,
//       filters: {}
//     });
//     expect(rowAxis.type).toEqual(AxisType.ROWS);
//     expect(rowAxis.dimensions.length).toEqual(1);
//     expect(rowAxis.dimensions[0].id).toEqual('dimension1');
//     expect(rowAxis.root.values.length).toEqual(3);
//     expect(rowAxis.root.values).toEqual([2, 1, 0]);
//   });
//   test('with custom sorting', () => {
//     const dimensions = {
//       // Sort by number order and not lexical order
//       dimension1: dimensionFactory({
//         accessor: 'dimension1',
//         sort: { custom: (a, b) => a - b }
//       }),
//       dimension2: dimensionFactory({ accessor: 'dimension2' })
//     };
//     const data = [
//       { dimension1: 0, dimension2: 0 },
//       { dimension1: 10, dimension2: 2 },
//       { dimension1: 2, dimension2: 4 },
//       { dimension1: 10, dimension2: 8 }
//     ];
//     const axis = { rows: ['dimension1'] };
//     const rowAxis = getRowAxis({
//       dimensions,
//       data,
//       axis,
//       filters: {}
//     });
//     expect(rowAxis.type).toEqual(AxisType.ROWS);
//     expect(rowAxis.dimensions.length).toEqual(1);
//     expect(rowAxis.dimensions[0].id).toEqual('dimension1');
//     expect(rowAxis.root.values.length).toEqual(3);
//     expect(rowAxis.root.values).toEqual([0, 2, 10]);
//   });

//   test('with sorting on another dimension', () => {
//     const dimensions = {
//       dimension1: dimensionFactory({
//         accessor: 'dimension1',
//         sort: { accessor: 'dimension2' }
//       }),
//       dimension2: dimensionFactory({ accessor: 'dimension2' })
//     };
//     const data = [
//       { dimension1: 0, dimension2: 9 },
//       { dimension1: 1, dimension2: 2 },
//       { dimension1: 2, dimension2: 4 }
//     ];
//     const axis = { rows: ['dimension1'] };
//     const rowAxis = getRowAxis({
//       dimensions,
//       data,
//       axis,
//       filters: {}
//     });
//     expect(rowAxis.type).toEqual(AxisType.ROWS);
//     expect(rowAxis.dimensions.length).toEqual(1);
//     expect(rowAxis.dimensions[0].id).toEqual('dimension1');
//     expect(rowAxis.root.values.length).toEqual(3);
//     expect(rowAxis.root.values).toEqual([1, 2, 0]);
//   });

//   test('with sorting on another dimension and custom function', () => {
//     const dimensions = {
//       dimension1: dimensionFactory({
//         accessor: 'dimension1',
//         // Normal number order and not lexical order
//         sort: { accessor: 'dimension2', custom: (a, b) => a - b }
//       }),
//       dimension2: dimensionFactory({ accessor: 'dimension2' })
//     };
//     const data = [
//       { dimension1: 0, dimension2: 10 },
//       { dimension1: 1, dimension2: 2 },
//       { dimension1: 2, dimension2: 4 }
//     ];
//     const axis = { rows: ['dimension1'] };
//     const rowAxis = getRowAxis({
//       dimensions,
//       data,
//       axis,
//       filters: {}
//     });
//     expect(rowAxis.type).toEqual(AxisType.ROWS);
//     expect(rowAxis.dimensions.length).toEqual(1);
//     expect(rowAxis.dimensions[0].id).toEqual('dimension1');
//     expect(rowAxis.root.values.length).toEqual(3);
//     expect(rowAxis.root.values).toEqual([1, 2, 0]);
//   });
// });

// describe('axis with two dimensions', () => {
//   test('works', () => {
//     const dimensions = {
//       dimension1: dimensionFactory({ accessor: 'dimension1' }),
//       dimension2: dimensionFactory({ accessor: 'dimension2' })
//     };
//     const data = [
//       { dimension1: 0, dimension2: 0 },
//       { dimension1: 1, dimension2: 2 },
//       { dimension1: 2, dimension2: 4 },
//       { dimension1: 1, dimension2: 8 }
//     ];
//     const axis = { rows: ['dimension2', 'dimension1'] };
//     const rowAxis = getRowAxis({
//       dimensions,
//       data,
//       axis
//     });
//     expect(rowAxis.type).toEqual(AxisType.ROWS);
//     expect(rowAxis.dimensions.length).toEqual(2);
//     expect(rowAxis.dimensions[0].id).toEqual('dimension2');
//     expect(rowAxis.root.values.length).toEqual(4);
//   });

//   test('and nested descending sorting', () => {
//     const dimensions = {
//       dimension1: dimensionFactory({ accessor: 'dimension1' }),
//       dimension2: dimensionFactory({
//         accessor: 'dimension2',
//         sort: { order: 'desc' }
//       })
//     };
//     const data = [
//       { dimension1: 0, dimension2: 0 },
//       { dimension1: 1, dimension2: 2 },
//       { dimension1: 2, dimension2: 4 },
//       { dimension1: 1, dimension2: 8 }
//     ];
//     const axis = { rows: ['dimension1', 'dimension2'] };
//     const rowAxis = getRowAxis({
//       dimensions,
//       data,
//       axis,
//       filters: {}
//     });
//     expect(rowAxis.type).toEqual(AxisType.ROWS);
//     expect(rowAxis.dimensions.length).toEqual(2);
//     expect(rowAxis.dimensions[0].id).toEqual('dimension1');
//     expect(rowAxis.root.values.length).toEqual(3);
//     expect(rowAxis.root.subdimvals[1].values[0]).toEqual(8);
//   });
// });

// describe('layout is computed correctly', () => {
//   test('2 dimensions on rows, 1 dimension on columns, 1 measure', () => {
//     const dimensions = {
//       dimension1: dimensionFactory({ accessor: 'dimension1' }),
//       dimension2: dimensionFactory({ accessor: 'dimension2' }),
//       dimension3: dimensionFactory({ accessor: 'dimension3' })
//     };
//     const df1 = measureFactory({ accessor: 'df1', aggregation: 'sum' });
//     df1.activated = true;
//     const measures = {
//       df1
//     };
//     const data = [
//       { dimension1: 0, dimension2: 0, dimension3: 0 },
//       { dimension1: 1, dimension2: 2, dimension3: 3 },
//       { dimension1: 2, dimension2: 4, dimension3: 6 },
//       { dimension1: 1, dimension2: 8, dimension3: 9 }
//     ];
//     const axis = {
//       rows: ['dimension2', 'dimension1'],
//       columns: ['dimension3']
//     };
//     const config = {
//       cellHeight: 30,
//       cellWidth: 100,
//       zoom: 1,
//       measureHeadersAxis: 'columns'
//     };
//     const {
//       rowHorizontalCount,
//       rowVerticalCount,
//       columnHorizontalCount,
//       columnVerticalCount
//     } = getLayout({ dimensions, data, axis, config, measures });
//     expect(rowHorizontalCount).toEqual(2);
//     expect(rowVerticalCount).toEqual(4);
//     expect(columnVerticalCount).toEqual(2);
//     expect(columnHorizontalCount).toEqual(4);
//   });

//   test('2 dimensions on rows, 1 dimension on columns, no measure', () => {
//     const dimensions = {
//       dimension1: dimensionFactory({ accessor: 'dimension1' }),
//       dimension2: dimensionFactory({ accessor: 'dimension2' }),
//       dimension3: dimensionFactory({ accessor: 'dimension3' })
//     };
//     const df1 = measureFactory({ accessor: 'df1', aggregation: 'sum' });
//     const measures = {
//       df1
//     };
//     const data = [
//       { dimension1: 0, dimension2: 0, dimension3: 0 },
//       { dimension1: 1, dimension2: 2, dimension3: 3 },
//       { dimension1: 2, dimension2: 4, dimension3: 6 },
//       { dimension1: 1, dimension2: 8, dimension3: 9 }
//     ];
//     const axis = {
//       rows: ['dimension2', 'dimension1'],
//       columns: ['dimension3']
//     };
//     const config = {
//       cellHeight: 30,
//       cellWidth: 100,
//       zoom: 1,
//       measureHeadersAxis: 'columns'
//     };
//     const {
//       rowHorizontalCount,
//       rowVerticalCount,
//       columnHorizontalCount,
//       columnVerticalCount
//     } = getLayout({ dimensions, data, axis, config, measures });
//     expect(rowHorizontalCount).toEqual(2);
//     expect(rowVerticalCount).toEqual(4);
//     expect(columnVerticalCount).toEqual(1);
//     expect(columnHorizontalCount).toEqual(4);
//   });

//   test('2 dimensions on rows, 1 dimension on columns, 1 measure on rows', () => {
//     const dimensions = {
//       dimension1: dimensionFactory({ accessor: 'dimension1' }),
//       dimension2: dimensionFactory({ accessor: 'dimension2' }),
//       dimension3: dimensionFactory({ accessor: 'dimension3' })
//     };
//     const df1 = measureFactory({ accessor: 'df1', aggregation: 'sum' });
//     df1.activated = true;
//     const measures = {
//       df1
//     };
//     const data = [
//       { dimension1: 0, dimension2: 0, dimension3: 0 },
//       { dimension1: 1, dimension2: 2, dimension3: 3 },
//       { dimension1: 2, dimension2: 4, dimension3: 6 },
//       { dimension1: 1, dimension2: 8, dimension3: 9 }
//     ];
//     const axis = {
//       rows: ['dimension2', 'dimension1'],
//       columns: ['dimension3']
//     };
//     const config = {
//       cellHeight: 30,
//       cellWidth: 100,
//       zoom: 1,
//       measureHeadersAxis: 'rows'
//     };
//     const {
//       rowHorizontalCount,
//       rowVerticalCount,
//       columnHorizontalCount,
//       columnVerticalCount
//     } = getLayout({ dimensions, data, axis, config, measures });
//     expect(rowHorizontalCount).toEqual(3);
//     expect(rowVerticalCount).toEqual(4);
//     expect(columnVerticalCount).toEqual(1);
//     expect(columnHorizontalCount).toEqual(4);
//   });
// });
