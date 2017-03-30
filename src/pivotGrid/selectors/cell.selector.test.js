import { getCellInfos, getCellValue } from './cell.selector';
import { getMockDatasource } from '../../utils/mock';

describe('cells infos are computed correctly', () => {
  const cell = {
    caption: '100 $',
    columnDimension: {
      caption: 'titi 0',
      id: 'titi 0',
      isRoot: false,
      rowIndexes: [0, 1, 20, 21],
      parent: {
        isRoot: true
      },
      field: {
        caption: 'Titi',
        id: 'titi'
      }
    },
    rowDimension: {
      caption: '0',
      id: '0',
      isRoot: false,
      rowIndexes: [20, 22],
      parent: {
        caption: 'toto 1',
        id: 1,
        isRoot: false,
        rowIndexes: [20, 22, 40, 42],
        parent: {
          isRoot: true
        },
        field: {
          caption: 'Toto',
          id: 'toto'
        }
      },
      field: {
        caption: 'Tutu',
        id: 'tutu'
      }
    },
    datafield: {
      caption: 'Amount',
      id: 'amt'
    }
  };
  const datasource = getMockDatasource(1, 10, 10);

  test('in normal case', () => {
    const expected = {
      value: '100 $',
      data: [
        {
          amt: 1100,
          qty: 100,
          titi: 'titi 0',
          toto: 1,
          toto_lb: 'toto 1',
          tutu: '0'
        }
      ],
      dimensions: [
        {
          dimension: { caption: 'Tutu', id: 'tutu' },
          cell: { caption: '0', id: '0' }
        },
        {
          dimension: { caption: 'Toto', id: 'toto' },
          cell: { caption: 'toto 1', id: 1 }
        },
        {
          dimension: { caption: 'Titi', id: 'titi' },
          cell: { caption: 'titi 0', id: 'titi 0' }
        }
      ],
      measure: {
        caption: 'Amount',
        id: 'amt'
      }
    };
    const cellInfos = getCellInfos({ data: datasource })(cell);
    expect(cellInfos).toEqual(expected);
  });
});

describe('cell value is computed correctly', () => {
  const datafield = { id: 'qty' };
  const returnIntersection = (datafieldId, intersection) => intersection;

  test('with no rowDimension', () => {
    const cellValue = getCellValue.resultFunc([])(
      datafield,
      null,
      null,
      returnIntersection
    );
    expect(cellValue).toEqual(null);
  });
  test('with both dimensions as root', () => {
    const rowDimension = { isRoot: true };
    const columnDimension = { isRoot: true };
    const cellValue = getCellValue.resultFunc([])(
      datafield,
      rowDimension,
      columnDimension,
      returnIntersection
    );
    expect(cellValue).toEqual(null);
  });
  test('with rowDimension as root', () => {
    const rowDimension = { isRoot: true };
    const columnDimension = { isRoot: false, rowIndexes: [1, 2, 3] };
    const cellValue = getCellValue.resultFunc([])(
      datafield,
      rowDimension,
      columnDimension,
      returnIntersection
    );
    expect(cellValue).toEqual([1, 2, 3]);
  });
  test('with columnDimension as root', () => {
    const columnDimension = { isRoot: true };
    const rowDimension = { isRoot: false, rowIndexes: [1, 2, 3] };
    const cellValue = getCellValue.resultFunc([])(
      datafield,
      rowDimension,
      columnDimension,
      returnIntersection
    );
    expect(cellValue).toEqual([1, 2, 3]);
  });
  test('with rowDimension as root and columnDimension rowIndexes empty', () => {
    const rowDimension = { isRoot: true };
    const columnDimension = { isRoot: false, rowIndexes: [] };
    const cellValue = getCellValue.resultFunc([])(
      datafield,
      rowDimension,
      columnDimension,
      returnIntersection
    );
    expect(cellValue).toEqual(null);
  });
});
