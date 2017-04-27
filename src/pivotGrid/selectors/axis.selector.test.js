import { getRowAxis, getLayout } from './axis.selector';
import { datafieldFactory, fieldFactory } from '../fields';
import { AxisType } from '../Axis';

describe('axis with one field', () => {
  test('works', () => {
    const fields = {
      field1: fieldFactory({ accessor: 'field1' }),
      field2: fieldFactory({ accessor: 'field2' })
    };
    const data = [
      { field1: 0, field2: 0 },
      { field1: 1, field2: 2 },
      { field1: 2, field2: 4 },
      { field1: 1, field2: 8 }
    ];
    const axis = { rows: ['field1'] };
    const rowAxis = getRowAxis({
      fields,
      data,
      axis,
      filters: {}
    });
    expect(rowAxis.type).toEqual(AxisType.ROWS);
    expect(rowAxis.fields.length).toEqual(1);
    expect(rowAxis.fields[0].id).toEqual('field1');
    expect(rowAxis.root.values.length).toEqual(3);
    expect(rowAxis.root.values).toEqual([0, 1, 2]);
  });
  test('with descending sorting', () => {
    const fields = {
      field1: fieldFactory({ accessor: 'field1', sort: { order: 'desc' } }),
      field2: fieldFactory({ accessor: 'field2' })
    };
    const data = [
      { field1: 0, field2: 0 },
      { field1: 1, field2: 2 },
      { field1: 2, field2: 4 },
      { field1: 1, field2: 8 }
    ];
    const axis = { rows: ['field1'] };
    const rowAxis = getRowAxis({
      fields,
      data,
      axis,
      filters: {}
    });
    expect(rowAxis.type).toEqual(AxisType.ROWS);
    expect(rowAxis.fields.length).toEqual(1);
    expect(rowAxis.fields[0].id).toEqual('field1');
    expect(rowAxis.root.values.length).toEqual(3);
    expect(rowAxis.root.values).toEqual([2, 1, 0]);
  });
  test('with custom sorting', () => {
    const fields = {
      // Sort by number order and not lexical order
      field1: fieldFactory({
        accessor: 'field1',
        sort: { custom: (a, b) => a - b }
      }),
      field2: fieldFactory({ accessor: 'field2' })
    };
    const data = [
      { field1: 0, field2: 0 },
      { field1: 10, field2: 2 },
      { field1: 2, field2: 4 },
      { field1: 10, field2: 8 }
    ];
    const axis = { rows: ['field1'] };
    const rowAxis = getRowAxis({
      fields,
      data,
      axis,
      filters: {}
    });
    expect(rowAxis.type).toEqual(AxisType.ROWS);
    expect(rowAxis.fields.length).toEqual(1);
    expect(rowAxis.fields[0].id).toEqual('field1');
    expect(rowAxis.root.values.length).toEqual(3);
    expect(rowAxis.root.values).toEqual([0, 2, 10]);
  });

  test('with sorting on another field', () => {
    const fields = {
      field1: fieldFactory({
        accessor: 'field1',
        sort: { accessor: 'field2' }
      }),
      field2: fieldFactory({ accessor: 'field2' })
    };
    const data = [
      { field1: 0, field2: 9 },
      { field1: 1, field2: 2 },
      { field1: 2, field2: 4 }
    ];
    const axis = { rows: ['field1'] };
    const rowAxis = getRowAxis({
      fields,
      data,
      axis,
      filters: {}
    });
    expect(rowAxis.type).toEqual(AxisType.ROWS);
    expect(rowAxis.fields.length).toEqual(1);
    expect(rowAxis.fields[0].id).toEqual('field1');
    expect(rowAxis.root.values.length).toEqual(3);
    expect(rowAxis.root.values).toEqual([1, 2, 0]);
  });

  test('with sorting on another field and custom function', () => {
    const fields = {
      field1: fieldFactory({
        accessor: 'field1',
        // Normal number order and not lexical order
        sort: { accessor: 'field2', custom: (a, b) => a - b }
      }),
      field2: fieldFactory({ accessor: 'field2' })
    };
    const data = [
      { field1: 0, field2: 10 },
      { field1: 1, field2: 2 },
      { field1: 2, field2: 4 }
    ];
    const axis = { rows: ['field1'] };
    const rowAxis = getRowAxis({
      fields,
      data,
      axis,
      filters: {}
    });
    expect(rowAxis.type).toEqual(AxisType.ROWS);
    expect(rowAxis.fields.length).toEqual(1);
    expect(rowAxis.fields[0].id).toEqual('field1');
    expect(rowAxis.root.values.length).toEqual(3);
    expect(rowAxis.root.values).toEqual([1, 2, 0]);
  });
});

describe('axis with two fields', () => {
  test('works', () => {
    const fields = {
      field1: fieldFactory({ accessor: 'field1' }),
      field2: fieldFactory({ accessor: 'field2' })
    };
    const data = [
      { field1: 0, field2: 0 },
      { field1: 1, field2: 2 },
      { field1: 2, field2: 4 },
      { field1: 1, field2: 8 }
    ];
    const axis = { rows: ['field2', 'field1'] };
    const rowAxis = getRowAxis({
      fields,
      data,
      axis
    });
    expect(rowAxis.type).toEqual(AxisType.ROWS);
    expect(rowAxis.fields.length).toEqual(2);
    expect(rowAxis.fields[0].id).toEqual('field2');
    expect(rowAxis.root.values.length).toEqual(4);
  });

  test('and nested descending sorting', () => {
    const fields = {
      field1: fieldFactory({ accessor: 'field1' }),
      field2: fieldFactory({ accessor: 'field2', sort: { order: 'desc' } })
    };
    const data = [
      { field1: 0, field2: 0 },
      { field1: 1, field2: 2 },
      { field1: 2, field2: 4 },
      { field1: 1, field2: 8 }
    ];
    const axis = { rows: ['field1', 'field2'] };
    const rowAxis = getRowAxis({
      fields,
      data,
      axis,
      filters: {}
    });
    expect(rowAxis.type).toEqual(AxisType.ROWS);
    expect(rowAxis.fields.length).toEqual(2);
    expect(rowAxis.fields[0].id).toEqual('field1');
    expect(rowAxis.root.values.length).toEqual(3);
    expect(rowAxis.root.subdimvals[1].values[0]).toEqual(8);
  });
});

describe('layout is computed correctly', () => {
  test('2 fields on rows, 1 field on columns, 1 datafield', () => {
    const fields = {
      field1: fieldFactory({ accessor: 'field1' }),
      field2: fieldFactory({ accessor: 'field2' }),
      field3: fieldFactory({ accessor: 'field3' })
    };
    const df1 = datafieldFactory({ accessor: 'df1', aggregateFunc: 'sum' });
    df1.activated = true;
    const datafields = {
      df1
    };
    const data = [
      { field1: 0, field2: 0, field3: 0 },
      { field1: 1, field2: 2, field3: 3 },
      { field1: 2, field2: 4, field3: 6 },
      { field1: 1, field2: 8, field3: 9 }
    ];
    const axis = { rows: ['field2', 'field1'], columns: ['field3'] };
    const config = {
      cellHeight: 30,
      cellWidth: 100,
      zoom: 1,
      dataHeadersLocation: 'columns'
    };
    const {
      rowHorizontalCount,
      rowVerticalCount,
      columnHorizontalCount,
      columnVerticalCount
    } = getLayout({ fields, data, axis, config, datafields });
    expect(rowHorizontalCount).toEqual(2);
    expect(rowVerticalCount).toEqual(4);
    expect(columnVerticalCount).toEqual(2);
    expect(columnHorizontalCount).toEqual(4);
  });

  test('2 fields on rows, 1 field on columns, no datafield', () => {
    const fields = {
      field1: fieldFactory({ accessor: 'field1' }),
      field2: fieldFactory({ accessor: 'field2' }),
      field3: fieldFactory({ accessor: 'field3' })
    };
    const df1 = datafieldFactory({ accessor: 'df1', aggregateFunc: 'sum' });
    const datafields = {
      df1
    };
    const data = [
      { field1: 0, field2: 0, field3: 0 },
      { field1: 1, field2: 2, field3: 3 },
      { field1: 2, field2: 4, field3: 6 },
      { field1: 1, field2: 8, field3: 9 }
    ];
    const axis = { rows: ['field2', 'field1'], columns: ['field3'] };
    const config = {
      cellHeight: 30,
      cellWidth: 100,
      zoom: 1,
      dataHeadersLocation: 'columns'
    };
    const {
      rowHorizontalCount,
      rowVerticalCount,
      columnHorizontalCount,
      columnVerticalCount
    } = getLayout({ fields, data, axis, config, datafields });
    expect(rowHorizontalCount).toEqual(2);
    expect(rowVerticalCount).toEqual(4);
    expect(columnVerticalCount).toEqual(1);
    expect(columnHorizontalCount).toEqual(4);
  });

  test('2 fields on rows, 1 field on columns, 1 datafield on rows', () => {
    const fields = {
      field1: fieldFactory({ accessor: 'field1' }),
      field2: fieldFactory({ accessor: 'field2' }),
      field3: fieldFactory({ accessor: 'field3' })
    };
    const df1 = datafieldFactory({ accessor: 'df1', aggregateFunc: 'sum' });
    df1.activated = true;
    const datafields = {
      df1
    };
    const data = [
      { field1: 0, field2: 0, field3: 0 },
      { field1: 1, field2: 2, field3: 3 },
      { field1: 2, field2: 4, field3: 6 },
      { field1: 1, field2: 8, field3: 9 }
    ];
    const axis = { rows: ['field2', 'field1'], columns: ['field3'] };
    const config = {
      cellHeight: 30,
      cellWidth: 100,
      zoom: 1,
      dataHeadersLocation: 'rows'
    };
    const {
      rowHorizontalCount,
      rowVerticalCount,
      columnHorizontalCount,
      columnVerticalCount
    } = getLayout({ fields, data, axis, config, datafields });
    expect(rowHorizontalCount).toEqual(3);
    expect(rowVerticalCount).toEqual(4);
    expect(columnVerticalCount).toEqual(1);
    expect(columnHorizontalCount).toEqual(4);
  });
});
