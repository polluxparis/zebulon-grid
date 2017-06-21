import {
  getCellSizes,
  getLastChildSizeOnColumns,
  getLastChildSizeOnRows,
  getDimensionSize,
  getDimensionPositions,
  getColumnWidth,
  getRowHeight,
  getLeafHeaderSize,
  getColumnHeadersHeight,
  getColumnHeadersWidth,
  getRowHeadersHeight,
  getRowHeadersWidth,
  getRowHeadersVisibleHeight,
  getColumnHeadersVisibleWidth,
  getPreviewSizes,
  getDataCellsHeight,
  getDataCellsWidth
} from "./sizes.selector";
import { AxisType } from "../Axis";

describe("cell sizes are calculated with zoom equals", () => {
  test("1", () => {
    const config = { cellHeight: 30, cellWidth: 100, zoom: 1 };
    const actual = getCellSizes({ config });
    expect(actual).toEqual({ height: 30, width: 100 });
  });

  test("0.5", () => {
    const config = { cellHeight: 30, cellWidth: 100, zoom: 0.5 };
    const actual = getCellSizes({ config });
    expect(actual).toEqual({ height: 15, width: 50 });
  });
});

describe("dimension sizes are computed correctly", () => {
  const cellSizes = {
    height: 30,
    width: 200
  };
  test("for columns with default size", () => {
    const actual = getDimensionSize.resultFunc({}, {}, 1, cellSizes)(
      AxisType.COLUMNS,
      "toto"
    );
    expect(actual).toEqual(30);
  });
  test("for rows with default size", () => {
    const actual = getDimensionSize.resultFunc({}, {}, 1, cellSizes)(
      AxisType.ROWS,
      "toto"
    );
    expect(actual).toEqual(200);
  });
  test("for columns with custom size", () => {
    const actual = getDimensionSize.resultFunc({}, { toto: 66 }, 1, cellSizes)(
      AxisType.COLUMNS,
      "toto"
    );
    expect(actual).toEqual(66);
  });
  test("for rows with custom size", () => {
    const actual = getDimensionSize.resultFunc({ toto: 666 }, {}, 1, cellSizes)(
      AxisType.ROWS,
      "toto"
    );
    expect(actual).toEqual(666);
  });

  test("for columns with custom size and zoom", () => {
    const actual = getDimensionSize.resultFunc(
      {},
      { toto: 66 },
      0.5,
      cellSizes
    )(AxisType.COLUMNS, "toto");
    expect(actual).toEqual(33);
  });
  test("for rows with custom size and zoom", () => {
    const actual = getDimensionSize.resultFunc(
      { toto: 666 },
      {},
      0.5,
      cellSizes
    )(AxisType.ROWS, "toto");
    expect(actual).toEqual(333);
  });
});

describe("last child size is computed correctly", () => {
  const config = {
    cellHeight: 30,
    cellWidth: 100,
    zoom: 1,
    dataHeadersLocation: "rows"
  };
  test("when no child", () => {
    const sizes = {
      leafs: { rows: {} }
    };
    const header = { key: "dimension1a", subheaders: [] };
    const size = getLastChildSizeOnRows({ sizes, config });
    expect(size(header)).toEqual(30);
  });

  test("when no child and custom size", () => {
    const sizes = {
      leafs: { rows: { dimension1a: 50 } }
    };
    const header = { key: "dimension1a", subheaders: [] };
    const size = getLastChildSizeOnRows({ sizes, config });
    expect(size(header)).toEqual(50);
  });

  test("with children in rows", () => {
    const sizes = {
      leafs: { rows: { "dimension1a-/-dimension2a": 80 } }
    };
    const header = {
      key: "dimension1a",
      subheaders: [{ key: "dimension1a-/-dimension2a", subheaders: [] }]
    };
    const size = getLastChildSizeOnRows({ sizes, config });
    expect(size(header)).toEqual(80);
  });
  test("with children in columns", () => {
    const sizes = {
      leafs: { columns: { "dimension1a-/-dimension2a": 80 } }
    };
    const header = {
      key: "dimension1a",
      subheaders: [{ key: "dimension1a-/-dimension2a", subheaders: [] }]
    };
    const size = getLastChildSizeOnColumns({ sizes, config });
    expect(size(header)).toEqual(80);
  });
});

describe("dimension positions are computed correctly", () => {
  const dataHeadersLocation = "columns";
  const cellSizes = {
    height: 30,
    width: 100
  };
  const getDimensionSizeFunc = getDimensionSize.resultFunc(
    {},
    {},
    1,
    cellSizes
  );
  test("with dimensions on both axis", () => {
    const rowdimensions = [{ id: "toto" }, { id: "tutu" }];
    const columnDimensions = [{ id: "titi" }];
    const actual = getDimensionPositions.resultFunc(
      getDimensionSizeFunc,
      dataHeadersLocation,
      columnDimensions,
      rowdimensions
    );
    const expected = {
      columns: { titi: 0, __measures__: 30 },
      rows: { toto: 0, tutu: 100 }
    };
    expect(actual).toEqual(expected);
  });
  test("with no dimension on row axis", () => {
    const rowdimensions = [];
    const columnDimensions = [{ id: "titi" }];
    const actual = getDimensionPositions.resultFunc(
      getDimensionSizeFunc,
      dataHeadersLocation,
      columnDimensions,
      rowdimensions
    );
    const expected = {
      columns: { titi: 0, __measures__: 30 },
      rows: {}
    };
    expect(actual).toEqual(expected);
  });
  test("with no dimension on column axis", () => {
    const rowdimensions = [{ id: "toto" }, { id: "tutu" }];
    const columnDimensions = [];
    const actual = getDimensionPositions.resultFunc(
      getDimensionSizeFunc,
      dataHeadersLocation,
      columnDimensions,
      rowdimensions
    );
    const expected = {
      columns: { __measures__: 30 },
      rows: { toto: 0, tutu: 100 }
    };
    expect(actual).toEqual(expected);
  });
});

describe("column width is computed correctly", () => {
  const cellSizes = {
    height: 30,
    width: 200
  };
  const columnsUi = {
    headers: [[{ key: "titi 0" }, { key: "titi 0-/-qty" }]]
  };
  test("with standard size", () => {
    const actual = getColumnWidth.resultFunc(
      columnsUi,
      {},
      getLeafHeaderSize.resultFunc(1, cellSizes)
    )({
      index: 0
    });
    expect(actual).toEqual(200);
  });
  test("with custom size", () => {
    const actual = getColumnWidth.resultFunc(
      columnsUi,
      { "titi 0-/-qty": 180 },
      getLeafHeaderSize.resultFunc(1, cellSizes)
    )({
      index: 0
    });
    expect(actual).toEqual(180);
  });
});

describe("row height is computed correctly", () => {
  const cellSizes = {
    height: 30,
    width: 200
  };
  const rowsUi = {
    headers: [[{ key: "titi 0" }, { key: "titi 0-/-toto 1" }]]
  };
  test("with standard size", () => {
    const actual = getRowHeight.resultFunc(
      rowsUi,
      {},
      getLeafHeaderSize.resultFunc(1, cellSizes)
    )({
      index: 0
    });
    expect(actual).toEqual(30);
  });
  test("with custom size", () => {
    const actual = getRowHeight.resultFunc(
      rowsUi,
      { "titi 0-/-toto 1": 60 },
      getLeafHeaderSize.resultFunc(1, cellSizes)
    )({
      index: 0
    });
    expect(actual).toEqual(60);
  });
});

describe("headers sizes are computed correctly", () => {
  const cellSizes = {
    height: 30,
    width: 200
  };
  const getDimensionSizeFunc = getDimensionSize.resultFunc(
    {},
    {},
    1,
    cellSizes
  );
  test("with data headers on column axis", () => {
    const rows = ["toto", "tutu"];
    const columns = ["titi"];
    const dataHeadersLocation = "columns";
    const rowsUi = {
      headers: [
        [{ key: "toto 0" }, { key: "toto 0-/-tutu 0" }],
        [{ key: "toto 0-/-tutu 1" }]
      ]
    };
    const columnsUi = {
      headers: [[{ key: "titi 0" }, { key: "titi 0-/-qty" }]]
    };
    const actual = {
      rowHeadersWidth: getRowHeadersWidth.resultFunc(
        dataHeadersLocation,
        {},
        rows,
        cellSizes,
        getDimensionSizeFunc
      ),
      columnHeadersHeight: getColumnHeadersHeight.resultFunc(
        dataHeadersLocation,
        {},
        columns,
        cellSizes,
        getDimensionSizeFunc
      ),
      rowHeadersHeight: getRowHeadersHeight.resultFunc(
        {},
        rowsUi,
        getLeafHeaderSize.resultFunc(1, cellSizes)
      ),
      columnHeadersWidth: getColumnHeadersWidth.resultFunc(
        {},
        columnsUi,
        getLeafHeaderSize.resultFunc(1, cellSizes)
      )
    };
    const expected = {
      rowHeadersWidth: 400,
      columnHeadersWidth: 200,
      rowHeadersHeight: 60,
      columnHeadersHeight: 60
    };
    expect(actual).toEqual(expected);
  });

  test("with data headers on row axis", () => {
    const rows = ["toto", "tutu"];
    const columns = ["titi"];
    const dataHeadersLocation = "rows";
    const rowsUi = {
      headers: [
        [
          { key: "toto 0" },
          { key: "toto 0-/-tutu 0" },
          { key: "toto 0-/-tutu 0-/-qty" }
        ],
        [{ key: "toto 0-/-tutu 1" }, { key: "toto 0-/-tutu 1-/-qty" }]
      ]
    };
    const columnsUi = {
      headers: [[{ key: "titi 0" }]]
    };
    const actual = {
      rowHeadersWidth: getRowHeadersWidth.resultFunc(
        dataHeadersLocation,
        {},
        rows,
        cellSizes,
        getDimensionSizeFunc
      ),
      columnHeadersHeight: getColumnHeadersHeight.resultFunc(
        dataHeadersLocation,
        {},
        columns,
        cellSizes,
        getDimensionSizeFunc
      ),
      rowHeadersHeight: getRowHeadersHeight.resultFunc(
        {},
        rowsUi,
        getLeafHeaderSize.resultFunc(1, cellSizes)
      ),
      columnHeadersWidth: getColumnHeadersWidth.resultFunc(
        {},
        columnsUi,
        getLeafHeaderSize.resultFunc(1, cellSizes)
      )
    };
    const expected = {
      rowHeadersWidth: 600,
      columnHeadersWidth: 200,
      rowHeadersHeight: 60,
      columnHeadersHeight: 30
    };
    expect(actual).toEqual(expected);
  });
  test("with no dimension on column axis", () => {
    const rows = ["toto", "tutu"];
    const columns = [];
    const dataHeadersLocation = "columns";
    const rowsUi = {
      headers: [
        [{ key: "toto 0" }, { key: "toto 0-/-tutu 0" }],
        [{ key: "toto 0-/-tutu 1" }]
      ]
    };
    const columnsUi = {
      headers: [
        [
          { key: "__total__-//-toto-/-tutu" },
          { key: "__total__-//-toto-/-tutu-/-qty" }
        ]
      ]
    };
    const actual = {
      rowHeadersWidth: getRowHeadersWidth.resultFunc(
        dataHeadersLocation,
        {},
        rows,
        cellSizes,
        getDimensionSizeFunc
      ),
      columnHeadersHeight: getColumnHeadersHeight.resultFunc(
        dataHeadersLocation,
        {},
        columns,
        cellSizes,
        getDimensionSizeFunc
      ),
      rowHeadersHeight: getRowHeadersHeight.resultFunc(
        {},
        rowsUi,
        getLeafHeaderSize.resultFunc(1, cellSizes)
      ),
      columnHeadersWidth: getColumnHeadersWidth.resultFunc(
        {},
        columnsUi,
        getLeafHeaderSize.resultFunc(1, cellSizes)
      )
    };
    const expected = {
      rowHeadersWidth: 400,
      columnHeadersWidth: 200,
      rowHeadersHeight: 60,
      columnHeadersHeight: 60
    };
    expect(actual).toEqual(expected);
  });
  test("with no dimension on row axis", () => {
    const columns = ["toto", "tutu"];
    const rows = [];
    const dataHeadersLocation = "columns";
    const columnsUi = {
      headers: [
        [
          { key: "toto 0" },
          { key: "toto 0-/-tutu 0" },
          { key: "toto 0-/-tutu 0-/-qty" }
        ],
        [{ key: "toto 0-/-tutu 1" }, { key: "toto 0-/-tutu 0-/-qty" }]
      ]
    };
    const rowsUi = {
      headers: [[{ key: "__total__-/-toto-/-tutu" }]]
    };
    const actual = {
      rowHeadersWidth: getRowHeadersWidth.resultFunc(
        dataHeadersLocation,
        {},
        rows,
        cellSizes,
        getDimensionSizeFunc
      ),
      columnHeadersHeight: getColumnHeadersHeight.resultFunc(
        dataHeadersLocation,
        {},
        columns,
        cellSizes,
        getDimensionSizeFunc
      ),
      rowHeadersHeight: getRowHeadersHeight.resultFunc(
        {},
        rowsUi,
        getLeafHeaderSize.resultFunc(1, cellSizes)
      ),
      columnHeadersWidth: getColumnHeadersWidth.resultFunc(
        {},
        columnsUi,
        getLeafHeaderSize.resultFunc(1, cellSizes)
      )
    };
    const expected = {
      rowHeadersWidth: 200,
      columnHeadersWidth: 400,
      rowHeadersHeight: 30,
      columnHeadersHeight: 90
    };
    expect(actual).toEqual(expected);
  });
});

describe("row headers visible height is computed correctly", () => {
  test("when row headers are bigger than grid height", () => {
    const height = 500;
    const columnHeadersHeight = 60;
    const rowHeadersHeight = 6000;
    // not in DOM so scrollbar size is 0 anyway
    const hasScrollbar = false;
    const actual = getRowHeadersVisibleHeight.resultFunc(
      height,
      columnHeadersHeight,
      rowHeadersHeight,
      hasScrollbar
    );
    expect(actual).toEqual(440);
  });
  test("when row headers are smaller than grid height", () => {
    const height = 500;
    const columnHeadersHeight = 60;
    const rowHeadersHeight = 300;
    // not in DOM so scrollbar size is 0 anyway
    const hasScrollbar = false;
    const actual = getRowHeadersVisibleHeight.resultFunc(
      height,
      columnHeadersHeight,
      rowHeadersHeight,
      hasScrollbar
    );
    expect(actual).toEqual(300);
  });
});

describe("column headers visible width is computed correctly", () => {
  test("when column headers are bigger than grid width", () => {
    const width = 1000;
    const columnHeadersWidth = 6000;
    const rowHeadersWidth = 400;
    // not in DOM so scrollbar size is 0 anyway
    const hasScrollbar = false;
    const actual = getColumnHeadersVisibleWidth.resultFunc(
      width,
      rowHeadersWidth,
      columnHeadersWidth,
      hasScrollbar
    );
    expect(actual).toEqual(600);
  });
  test("when column headers are smaller than grid width", () => {
    const width = 1000;
    const columnHeadersWidth = 500;
    const rowHeadersWidth = 400;
    // not in DOM so scrollbar size is 0 anyway
    const hasScrollbar = false;
    const actual = getColumnHeadersVisibleWidth.resultFunc(
      width,
      rowHeadersWidth,
      columnHeadersWidth,
      hasScrollbar
    );
    expect(actual).toEqual(500);
  });
});

describe("preview sizes are computed correctly", () => {
  test("when headers sizes are bigger than grid sizes", () => {
    const width = 1000;
    const height = 600;

    const columnHeadersWidth = 6000;
    const rowHeadersWidth = 400;
    const rowHeadersHeight = 6000;
    const columnHeadersHeight = 60;
    // not in DOM so scrollbar size is 0 anyway
    const hasVerticalScrollbar = false;
    const hasHorizontalScrollbar = false;
    const actual = getPreviewSizes.resultFunc(
      height,
      width,
      hasVerticalScrollbar,
      hasHorizontalScrollbar,
      rowHeadersHeight,
      rowHeadersWidth,
      columnHeadersHeight,
      columnHeadersWidth
    );
    expect(actual).toEqual({ width, height });
  });
  test("when headers sizes are smaller than grid sizes", () => {
    const width = 1000;
    const height = 600;

    const columnHeadersWidth = 400;
    const rowHeadersWidth = 200;
    const rowHeadersHeight = 300;
    const columnHeadersHeight = 60; // not in DOM so scrollbar size is 0 anyway
    const hasVerticalScrollbar = false;
    const hasHorizontalScrollbar = false;
    const actual = getPreviewSizes.resultFunc(
      height,
      width,
      hasVerticalScrollbar,
      hasHorizontalScrollbar,
      rowHeadersHeight,
      rowHeadersWidth,
      columnHeadersHeight,
      columnHeadersWidth
    );
    expect(actual).toEqual({ width: 600, height: 360 });
  });
});

describe("data cells width is computed correctly", () => {
  test("when column headers are bigger than grid width", () => {
    const width = 1000;
    const columnHeadersWidth = 6000;
    const rowHeadersWidth = 400;
    // not in DOM so scrollbar size is 0 anyway
    const hasScrollbar = false;
    const actual = getDataCellsWidth.resultFunc(
      width,
      columnHeadersWidth,
      rowHeadersWidth,
      hasScrollbar
    );
    expect(actual).toEqual(600);
  });
  test("when column headers are smaller than grid width", () => {
    const width = 1000;
    const columnHeadersWidth = 400;
    const rowHeadersWidth = 400;
    // not in DOM so scrollbar size is 0 anyway
    const hasScrollbar = false;
    const actual = getDataCellsWidth.resultFunc(
      width,
      columnHeadersWidth,
      rowHeadersWidth,
      hasScrollbar
    );
    expect(actual).toEqual(400);
  });
});

describe("data cells height is computed correctly", () => {
  test("when row headers are bigger than grid height", () => {
    const height = 600;
    const columnHeadersHeight = 60;
    const rowHeadersHeight = 6000;
    // not in DOM so scrollbar size is 0 anyway
    const hasScrollbar = false;
    const actual = getDataCellsHeight.resultFunc(
      height,
      columnHeadersHeight,
      rowHeadersHeight,
      hasScrollbar
    );
    expect(actual).toEqual(540);
  });
  test("when row headers are smaller than grid height", () => {
    const height = 600;
    const columnHeadersHeight = 60;
    const rowHeadersHeight = 300;
    // not in DOM so scrollbar size is 0 anyway
    const hasScrollbar = false;
    const actual = getDataCellsHeight.resultFunc(
      height,
      columnHeadersHeight,
      rowHeadersHeight,
      hasScrollbar
    );
    expect(actual).toEqual(300);
  });
});
