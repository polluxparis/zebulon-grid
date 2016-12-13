import { getMockDatasource } from '../../utils/mock';
import {
  getCellSizes,
  getFilteredData,
  getActivatedDataFields,
  getRowAxis,
  getRowFields,
  getColumnFields,
  getLayout,
  getLastChildSize,
 } from './index';
import { AxisType } from '../Axis';
import { datafieldFactory, fieldFactory } from '../fields';

describe('cell sizes are calculated with zoom equals', () => {
  it('1', () => {
    const config = { cellHeight: 30, cellWidth: 100, zoom: 1 };
    const actual = getCellSizes({ config });
    expect(actual).toEqual({ height: 30, width: 100 });
  });

  it('0.5', () => {
    const config = { cellHeight: 30, cellWidth: 100, zoom: 0.5 };
    const actual = getCellSizes({ config });
    expect(actual).toEqual({ height: 15, width: 50 });
  });
});

describe('axis fields are computed correctly', () => {
  it('1 field', () => {
    const axis = { rows: ['field1'], columns: ['field2'] };
    const fields = {
      field1: { id: 'field1' },
      field2: { id: 'field2' },
    };

    const rowFields = getRowFields({ axis, fields });
    expect(rowFields.length).toEqual(1);
    expect(rowFields[0].id).toEqual('field1');

    const columnFields = getColumnFields({ axis, fields });
    expect(columnFields.length).toEqual(1);
    expect(columnFields[0].id).toEqual('field2');
  });

  it('2 and 0 fields', () => {
    const axis = { rows: ['field1', 'field2'], columns: [] };
    const fields = {
      field1: { id: 'field1' },
      field2: { id: 'field2' },
    };

    const rowFields = getRowFields({ axis, fields });
    expect(rowFields.length).toEqual(2);

    const columnFields = getColumnFields({ axis, fields });
    expect(columnFields.length).toEqual(0);
  });
});

describe('activated datafields', () => {
  it('work with one activated field', () => {
    const datafields = {
      datafield1: { id: 'datafield1', activated: true },
      datafield2: { id: 'datafield2', activated: false },
    };
    const actual = getActivatedDataFields({ datafields });
    expect(actual.length).toEqual(1);
    expect(actual[0]).toEqual(datafields.datafield1);
  });
  it('work with no activated field', () => {
    const datafields = [
      { id: 'datafield1', activated: false },
      { id: 'datafield2', activated: false },
    ];
    const actual = getActivatedDataFields({ datafields });
    expect(actual.length).toEqual(0);
  });
});

describe('filtering data works', () => {
  it('with one static filter', () => {
    const actual = getFilteredData({
      data: getMockDatasource(1, 10, 10),
      filters: { toto: { fieldId: 'toto', staticValue: ['1'] } },
    });
    expect(actual.map(data => data.toto).includes('1')).toBe(true);
    expect(actual.map(data => data.toto).includes('0')).toBe(false);
    expect(actual.length).toEqual(20);
  });
});


describe('axis with one field', () => {
  it('is computed correctly', () => {
    const fields = {
      field1: fieldFactory({ id: 'field1' }),
      field2: fieldFactory({ id: 'field2' }),
    };
    const data = [
      { field1: 0, field2: 0 },
      { field1: 1, field2: 2 },
      { field1: 2, field2: 4 },
      { field1: 1, field2: 8 },
    ];
    const axis = { rows: ['field1'] };
    const rowAxis = getRowAxis({
      fields,
      data,
      axis,
      filters: {},
    });
    expect(rowAxis.type).toEqual(AxisType.ROWS);
    expect(rowAxis.fields.length).toEqual(1);
    expect(rowAxis.fields[0].id).toEqual('field1');
    expect(rowAxis.root.values.length).toEqual(3);
  });
});

describe('axis with two fields', () => {
  it('is computed correctly', () => {
    const fields = {
      field1: fieldFactory({ id: 'field1' }),
      field2: fieldFactory({ id: 'field2' }),
    };
    const data = [
      { field1: 0, field2: 0 },
      { field1: 1, field2: 2 },
      { field1: 2, field2: 4 },
      { field1: 1, field2: 8 },
    ];
    const axis = { rows: ['field2', 'field1'] };
    const rowAxis = getRowAxis({
      fields,
      data,
      axis,
    });
    expect(rowAxis.type).toEqual(AxisType.ROWS);
    expect(rowAxis.fields.length).toEqual(2);
    expect(rowAxis.fields[0].id).toEqual('field2');
    expect(rowAxis.root.values.length).toEqual(4);
  });
});

describe('layout is computed correctly', () => {
  it('2 fields on rows, 1 field on columns, 1 datafield', () => {
    const fields = {
      field1: fieldFactory({ id: 'field1' }),
      field2: fieldFactory({ id: 'field2' }),
      field3: fieldFactory({ id: 'field3' }),
    };
    const df1 = datafieldFactory({ id: 'df1', aggregateFunc: 'sum' });
    df1.activated = true;
    const datafields = {
      df1,
    };
    const data = [
      { field1: 0, field2: 0, field3: 0 },
      { field1: 1, field2: 2, field3: 3 },
      { field1: 2, field2: 4, field3: 6 },
      { field1: 1, field2: 8, field3: 9 },
    ];
    const axis = { rows: ['field2', 'field1'], columns: ['field3'] };
    const config = { cellHeight: 30, cellWidth: 100, zoom: 1, dataHeadersLocation: 'columns' };
    const {
      rowHorizontalCount,
      rowVerticalCount,
      columnHorizontalCount,
      columnVerticalCount,
    } = getLayout({ fields, data, axis, config, datafields });
    expect(rowHorizontalCount).toEqual(2);
    expect(rowVerticalCount).toEqual(4);
    expect(columnVerticalCount).toEqual(2);
    expect(columnHorizontalCount).toEqual(4);
  });

  it('2 fields on rows, 1 field on columns, no datafield', () => {
    const fields = {
      field1: fieldFactory({ id: 'field1' }),
      field2: fieldFactory({ id: 'field2' }),
      field3: fieldFactory({ id: 'field3' }),
    };
    const df1 = datafieldFactory({ id: 'df1', aggregateFunc: 'sum' });
    const datafields = {
      df1,
    };
    const data = [
      { field1: 0, field2: 0, field3: 0 },
      { field1: 1, field2: 2, field3: 3 },
      { field1: 2, field2: 4, field3: 6 },
      { field1: 1, field2: 8, field3: 9 },
    ];
    const axis = { rows: ['field2', 'field1'], columns: ['field3'] };
    const config = { cellHeight: 30, cellWidth: 100, zoom: 1, dataHeadersLocation: 'columns' };
    const {
      rowHorizontalCount,
      rowVerticalCount,
      columnHorizontalCount,
      columnVerticalCount,
    } = getLayout({ fields, data, axis, config, datafields });
    expect(rowHorizontalCount).toEqual(2);
    expect(rowVerticalCount).toEqual(4);
    expect(columnVerticalCount).toEqual(1);
    expect(columnHorizontalCount).toEqual(4);
  });

  it('2 fields on rows, 1 field on columns, 1 datafield on rows', () => {
    const fields = {
      field1: fieldFactory({ id: 'field1' }),
      field2: fieldFactory({ id: 'field2' }),
      field3: fieldFactory({ id: 'field3' }),
    };
    const df1 = datafieldFactory({ id: 'df1', aggregateFunc: 'sum' });
    df1.activated = true;
    const datafields = {
      df1,
    };
    const data = [
      { field1: 0, field2: 0, field3: 0 },
      { field1: 1, field2: 2, field3: 3 },
      { field1: 2, field2: 4, field3: 6 },
      { field1: 1, field2: 8, field3: 9 },
    ];
    const axis = { rows: ['field2', 'field1'], columns: ['field3'] };
    const config = { cellHeight: 30, cellWidth: 100, zoom: 1, dataHeadersLocation: 'rows' };
    const {
      rowHorizontalCount,
      rowVerticalCount,
      columnHorizontalCount,
      columnVerticalCount,
    } = getLayout({ fields, data, axis, config, datafields });
    expect(rowHorizontalCount).toEqual(3);
    expect(rowVerticalCount).toEqual(4);
    expect(columnVerticalCount).toEqual(1);
    expect(columnHorizontalCount).toEqual(4);
  });
});


describe('last child size is computed correctly', () => {
  const config = { cellHeight: 30, cellWidth: 100, zoom: 1, dataHeadersLocation: 'rows' };
  it('when no child', () => {
    const sizes = { rows: { leafs: {}, dimensions: {} }, columns: { leafs: {}, dimensions: {} } };
    const header = { key: 'field1a', subheaders: [] };
    const size = getLastChildSize({ sizes, config });
    expect(size(AxisType.ROWS, header)).toEqual(30);
  });

  it('when no child and custom size', () => {
    const sizes = {
      rows: { leafs: { field1a: 50 }, dimensions: {} },
      columns: { leafs: {}, dimensions: {} },
    };
    const header = { key: 'field1a', subheaders: [] };
    const size = getLastChildSize({ sizes, config });
    expect(size(AxisType.ROWS, header)).toEqual(50);
  });

  it('with children', () => {
    const sizes = { rows: { leafs: { 'field1a-/-field2a': 80 }, dimensions: {} }, columns: { leafs: {}, dimensions: {} } };
    const header = {
      key: 'field1a',
      subheaders: [
      { key: 'field1a-/-field2a', subheaders: [] },
      ],
    };
    const size = getLastChildSize({ sizes, config });
    expect(size(AxisType.ROWS, header)).toEqual(80);
  });
});
