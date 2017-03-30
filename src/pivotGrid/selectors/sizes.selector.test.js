import {
  getCellSizes,
  getLastChildSize,
  getDimensionSize,
  getDimensionPositions,
  getColumnWidth,
  getRowHeight,
  getHeaderSizes,
  getRowHeadersVisibleHeight,
  getColumnHeadersVisibleWidth,
  getPreviewSizes,
  getDataCellsHeight,
  getDataCellsWidth
} from './sizes.selector';
import { AxisType } from '../Axis';

describe('cell sizes are calculated with zoom equals', () => {
  test('1', () => {
    const config = { cellHeight: 30, cellWidth: 100, zoom: 1 };
    const actual = getCellSizes({ config });
    expect(actual).toEqual({ height: 30, width: 100 });
  });

  test('0.5', () => {
    const config = { cellHeight: 30, cellWidth: 100, zoom: 0.5 };
    const actual = getCellSizes({ config });
    expect(actual).toEqual({ height: 15, width: 50 });
  });
});

describe('dimension sizes are computed correctly', () => {
  const sizes = {
    columns: { dimensions: {} },
    rows: { dimensions: {} }
  };
  const cellSizes = {
    height: 30,
    width: 200
  };
  test('for columns with default size', () => {
    const actual = getDimensionSize.resultFunc(sizes, cellSizes)(
      AxisType.COLUMNS,
      'toto'
    );
    expect(actual).toEqual(30);
  });
  test('for rows with default size', () => {
    const actual = getDimensionSize.resultFunc(sizes, cellSizes)(
      AxisType.ROWS,
      'toto'
    );
    expect(actual).toEqual(200);
  });
  test('for columns with custom size', () => {
    sizes.dimensions.columns.toto = 66;
    const actual = getDimensionSize.resultFunc(sizes, cellSizes)(
      AxisType.COLUMNS,
      'toto'
    );
    expect(actual).toEqual(66);
  });
  test('for rows with custom size', () => {
    sizes.dimensions.rows.toto = 666;
    const actual = getDimensionSize.resultFunc(sizes, cellSizes)(
      AxisType.ROWS,
      'toto'
    );
    expect(actual).toEqual(666);
  });
});

describe('last child size is computed correctly', () => {
  const config = {
    cellHeight: 30,
    cellWidth: 100,
    zoom: 1,
    dataHeadersLocation: 'rows'
  };
  test('when no child', () => {
    const sizes = {
      rows: { leafs: {}, dimensions: {} },
      columns: { leafs: {}, dimensions: {} }
    };
    const header = { key: 'field1a', subheaders: [] };
    const size = getLastChildSize({ sizes, config });
    expect(size(AxisType.ROWS, header)).toEqual(30);
  });

  test('when no child and custom size', () => {
    const sizes = {
      rows: { leafs: { field1a: 50 }, dimensions: {} },
      columns: { leafs: {}, dimensions: {} }
    };
    const header = { key: 'field1a', subheaders: [] };
    const size = getLastChildSize({ sizes, config });
    expect(size(AxisType.ROWS, header)).toEqual(50);
  });

  test('with children in rows', () => {
    const sizes = {
      rows: { leafs: { 'field1a-/-field2a': 80 } },
      columns: { leafs: {} }
    };
    const header = {
      key: 'field1a',
      subheaders: [{ key: 'field1a-/-field2a', subheaders: [] }]
    };
    const size = getLastChildSize({ sizes, config });
    expect(size(AxisType.ROWS, header)).toEqual(80);
  });
  test('with children in columns', () => {
    const sizes = {
      columns: { leafs: { 'field1a-/-field2a': 80 } },
      rows: { leafs: {} }
    };
    const header = {
      key: 'field1a',
      subheaders: [{ key: 'field1a-/-field2a', subheaders: [] }]
    };
    const size = getLastChildSize({ sizes, config });
    expect(size(AxisType.COLUMNS, header)).toEqual(80);
  });
});

describe('dimension positions are computed correctly', () => {
  const dataHeadersLocation = 'columns';
  const sizes = {
    columns: { dimensions: {} },
    rows: { dimensions: {} }
  };
  const cellSizes = {
    height: 30,
    width: 100
  };
  const getDimensionSizeFunc = getDimensionSize.resultFunc(sizes, cellSizes);
  test('with fields on both axis', () => {
    const rowfields = [{ id: 'toto' }, { id: 'tutu' }];
    const columnFields = [{ id: 'titi' }];
    const actual = getDimensionPositions.resultFunc(
      getDimensionSizeFunc,
      dataHeadersLocation,
      columnFields,
      rowfields
    );
    const expected = {
      columns: { titi: 0, __measures__: 30 },
      rows: { toto: 0, tutu: 100 }
    };
    expect(actual).toEqual(expected);
  });
  test('with no field on row axis', () => {
    const rowfields = [];
    const columnFields = [{ id: 'titi' }];
    const actual = getDimensionPositions.resultFunc(
      getDimensionSizeFunc,
      dataHeadersLocation,
      columnFields,
      rowfields
    );
    const expected = {
      columns: { titi: 0, __measures__: 30 },
      rows: {}
    };
    expect(actual).toEqual(expected);
  });
  test('with no field on column axis', () => {
    const rowfields = [{ id: 'toto' }, { id: 'tutu' }];
    const columnFields = [];
    const actual = getDimensionPositions.resultFunc(
      getDimensionSizeFunc,
      dataHeadersLocation,
      columnFields,
      rowfields
    );
    const expected = {
      columns: { __measures__: 30 },
      rows: { toto: 0, tutu: 100 }
    };
    expect(actual).toEqual(expected);
  });
});

describe('column width is computed correctly', () => {
  const cellSizes = {
    height: 30,
    width: 200
  };
  const columnsUi = {
    headers: [[{ key: 'titi 0' }, { key: 'titi 0-/-qty' }]]
  };
  test('with standard size', () => {
    const sizes = {
      columns: { leafs: {} },
      rows: { leafs: {} }
    };
    const actual = getColumnWidth.resultFunc(columnsUi, sizes, cellSizes)({
      index: 0
    });
    expect(actual).toEqual(200);
  });
  test('with custom size', () => {
    const sizes = {
      columns: { leafs: { 'titi 0-/-qty': 180 } },
      rows: { leafs: {} }
    };
    const actual = getColumnWidth.resultFunc(columnsUi, sizes, cellSizes)({
      index: 0
    });
    expect(actual).toEqual(180);
  });
});

describe('row height is computed correctly', () => {
  const cellSizes = {
    height: 30,
    width: 200
  };
  const rowsUi = {
    headers: [[{ key: 'titi 0' }, { key: 'titi 0-/-toto 1' }]]
  };
  test('with standard size', () => {
    const sizes = {
      columns: { leafs: {} },
      rows: { leafs: {} }
    };
    const actual = getRowHeight.resultFunc(rowsUi, sizes, cellSizes)({
      index: 0
    });
    expect(actual).toEqual(30);
  });
  test('with custom size', () => {
    const sizes = {
      rows: { leafs: { 'titi 0-/-toto 1': 60 } },
      columns: { leafs: {} }
    };
    const actual = getRowHeight.resultFunc(rowsUi, sizes, cellSizes)({
      index: 0
    });
    expect(actual).toEqual(60);
  });
});

describe('headers sizes are computed correctly', () => {
  const sizes = {
    rows: { leafs: {}, dimensions: {} },
    columns: { leafs: {}, dimensions: {} }
  };
  const cellSizes = {
    height: 30,
    width: 200
  };
  const getDimensionSizeFunc = getDimensionSize.resultFunc(sizes, cellSizes);
  test('with data headers on column axis', () => {
    const rows = ['toto', 'tutu'];
    const columns = ['titi'];
    const dataHeadersLocation = 'columns';
    const rowsUi = {
      headers: [
        [{ key: 'toto 0' }, { key: 'toto 0-/-tutu 0' }],
        [{ key: 'toto 0-/-tutu 1' }]
      ]
    };
    const columnsUi = {
      headers: [[{ key: 'titi 0' }, { key: 'titi 0-/-qty' }]]
    };
    const actual = getHeaderSizes.resultFunc(
      dataHeadersLocation,
      sizes,
      rows,
      columns,
      rowsUi,
      columnsUi,
      cellSizes,
      getDimensionSizeFunc
    );
    const expected = {
      rowHeadersWidth: 400,
      columnHeadersWidth: 200,
      rowHeadersHeight: 60,
      columnHeadersHeight: 60
    };
    expect(actual).toEqual(expected);
  });

  test('with data headers on row axis', () => {
    const rows = ['toto', 'tutu'];
    const columns = ['titi'];
    const dataHeadersLocation = 'rows';
    const rowsUi = {
      headers: [
        [
          { key: 'toto 0' },
          { key: 'toto 0-/-tutu 0' },
          { key: 'toto 0-/-tutu 0-/-qty' }
        ],
        [{ key: 'toto 0-/-tutu 1' }, { key: 'toto 0-/-tutu 1-/-qty' }]
      ]
    };
    const columnsUi = {
      headers: [[{ key: 'titi 0' }]]
    };
    const actual = getHeaderSizes.resultFunc(
      dataHeadersLocation,
      sizes,
      rows,
      columns,
      rowsUi,
      columnsUi,
      cellSizes,
      getDimensionSizeFunc
    );
    const expected = {
      rowHeadersWidth: 600,
      columnHeadersWidth: 200,
      rowHeadersHeight: 60,
      columnHeadersHeight: 30
    };
    expect(actual).toEqual(expected);
  });
  test('with no field on column axis', () => {
    const rows = ['toto', 'tutu'];
    const columns = [];
    const dataHeadersLocation = 'columns';
    const rowsUi = {
      headers: [
        [{ key: 'toto 0' }, { key: 'toto 0-/-tutu 0' }],
        [{ key: 'toto 0-/-tutu 1' }]
      ]
    };
    const columnsUi = {
      headers: [
        [
          { key: '__total__-//-toto-/-tutu' },
          { key: '__total__-//-toto-/-tutu-/-qty' }
        ]
      ]
    };
    const actual = getHeaderSizes.resultFunc(
      dataHeadersLocation,
      sizes,
      rows,
      columns,
      rowsUi,
      columnsUi,
      cellSizes,
      getDimensionSizeFunc
    );
    const expected = {
      rowHeadersWidth: 400,
      columnHeadersWidth: 200,
      rowHeadersHeight: 60,
      columnHeadersHeight: 60
    };
    expect(actual).toEqual(expected);
  });
  test('with no field on row axis', () => {
    const columns = ['toto', 'tutu'];
    const rows = [];
    const dataHeadersLocation = 'columns';
    const columnsUi = {
      headers: [
        [
          { key: 'toto 0' },
          { key: 'toto 0-/-tutu 0' },
          { key: 'toto 0-/-tutu 0-/-qty' }
        ],
        [{ key: 'toto 0-/-tutu 1' }, { key: 'toto 0-/-tutu 0-/-qty' }]
      ]
    };
    const rowsUi = {
      headers: [[{ key: '__total__-/-toto-/-tutu' }]]
    };
    const actual = getHeaderSizes.resultFunc(
      dataHeadersLocation,
      sizes,
      rows,
      columns,
      rowsUi,
      columnsUi,
      cellSizes,
      getDimensionSizeFunc
    );
    const expected = {
      rowHeadersWidth: 200,
      columnHeadersWidth: 400,
      rowHeadersHeight: 30,
      columnHeadersHeight: 90
    };
    expect(actual).toEqual(expected);
  });
});

describe('row headers visible height is computed correctly', () => {
  test('when row headers are bigger than grid height', () => {
    const height = 500;
    const headerSizes = { columnHeadersHeight: 60, rowHeadersHeight: 6000 };
    // not in DOM so scrollbar size is 0 anyway
    const hasScrollbar = { bottom: false, right: false };
    const actual = getRowHeadersVisibleHeight.resultFunc(
      height,
      headerSizes,
      hasScrollbar
    );
    expect(actual).toEqual(440);
  });
  test('when row headers are smaller than grid height', () => {
    const height = 500;
    const headerSizes = { columnHeadersHeight: 60, rowHeadersHeight: 300 };
    // not in DOM so scrollbar size is 0 anyway
    const hasScrollbar = { bottom: false, right: false };
    const actual = getRowHeadersVisibleHeight.resultFunc(
      height,
      headerSizes,
      hasScrollbar
    );
    expect(actual).toEqual(300);
  });
});

describe('column headers visible width is computed correctly', () => {
  test('when column headers are bigger than grid width', () => {
    const width = 1000;
    const headerSizes = { columnHeadersWidth: 6000, rowHeadersWidth: 400 };
    // not in DOM so scrollbar size is 0 anyway
    const hasScrollbar = { bottom: false, right: false };
    const actual = getColumnHeadersVisibleWidth.resultFunc(
      width,
      headerSizes,
      hasScrollbar
    );
    expect(actual).toEqual(600);
  });
  test('when column headers are smaller than grid width', () => {
    const width = 1000;
    const headerSizes = { columnHeadersWidth: 500, rowHeadersWidth: 400 };
    // not in DOM so scrollbar size is 0 anyway
    const hasScrollbar = { bottom: false, right: false };
    const actual = getColumnHeadersVisibleWidth.resultFunc(
      width,
      headerSizes,
      hasScrollbar
    );
    expect(actual).toEqual(500);
  });
});

describe('preview sizes are computed correctly', () => {
  test('when headers sizes are bigger than grid sizes', () => {
    const width = 1000;
    const height = 600;
    const headerSizes = {
      columnHeadersWidth: 6000,
      rowHeadersWidth: 400,
      rowHeadersHeight: 6000,
      columnHeadersHeight: 60
    };
    // not in DOM so scrollbar size is 0 anyway
    const hasScrollbar = { bottom: false, right: false };
    const actual = getPreviewSizes.resultFunc(
      height,
      width,
      hasScrollbar,
      headerSizes
    );
    expect(actual).toEqual({ width, height });
  });
  test('when headers sizes are smaller than grid sizes', () => {
    const width = 1000;
    const height = 600;
    const headerSizes = {
      columnHeadersWidth: 400,
      rowHeadersWidth: 200,
      rowHeadersHeight: 300,
      columnHeadersHeight: 60
    };
    // not in DOM so scrollbar size is 0 anyway
    const hasScrollbar = { bottom: false, right: false };
    const actual = getPreviewSizes.resultFunc(
      height,
      width,
      hasScrollbar,
      headerSizes
    );
    expect(actual).toEqual({ width: 600, height: 360 });
  });
});

describe('data cells width is computed correctly', () => {
  test('when column headers are bigger than grid width', () => {
    const width = 1000;
    const headerSizes = { columnHeadersWidth: 6000, rowHeadersWidth: 400 };
    // not in DOM so scrollbar size is 0 anyway
    const hasScrollbar = { bottom: false, right: false };
    const actual = getDataCellsWidth.resultFunc(
      width,
      headerSizes,
      hasScrollbar
    );
    expect(actual).toEqual(600);
  });
  test('when column headers are smaller than grid width', () => {
    const width = 1000;
    const headerSizes = { columnHeadersWidth: 400, rowHeadersWidth: 400 };
    // not in DOM so scrollbar size is 0 anyway
    const hasScrollbar = { bottom: false, right: false };
    const actual = getDataCellsWidth.resultFunc(
      width,
      headerSizes,
      hasScrollbar
    );
    expect(actual).toEqual(400);
  });
});

describe('data cells height is computed correctly', () => {
  test('when row headers are bigger than grid height', () => {
    const height = 600;
    const headerSizes = { columnHeadersHeight: 60, rowHeadersHeight: 6000 };
    // not in DOM so scrollbar size is 0 anyway
    const hasScrollbar = { bottom: false, right: false };
    const actual = getDataCellsHeight.resultFunc(
      height,
      headerSizes,
      hasScrollbar
    );
    expect(actual).toEqual(540);
  });
  test('when row headers are smaller than grid height', () => {
    const height = 600;
    const headerSizes = { columnHeadersHeight: 60, rowHeadersHeight: 300 };
    // not in DOM so scrollbar size is 0 anyway
    const hasScrollbar = { bottom: false, right: false };
    const actual = getDataCellsHeight.resultFunc(
      height,
      headerSizes,
      hasScrollbar
    );
    expect(actual).toEqual(300);
  });
});
