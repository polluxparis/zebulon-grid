import { getCellInfosSelector, getCellValue } from './cell.selector';
import { getMockDatasource } from '../../utils/mock';
import { sum } from '../Aggregation';

describe('cells infos are computed correctly', () => {
  const cell = {
    caption: '1100 $',
    columnDimension: {
      caption: 'titi 0',
      id: 'titi 0',
      isRoot: false,
      rowIndexes: [0, 1, 20, 21],
      parent: {
        isRoot: true
      },
      dimension: {
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
        dimension: {
          caption: 'Toto',
          id: 'toto'
        }
      },
      dimension: {
        caption: 'Tutu',
        id: 'tutu'
      }
    },
    measure: {
      caption: 'Amount',
      id: 'amt'
    }
  };
  const datasource = getMockDatasource(1, 10, 10);

  test('in normal case', () => {
    const expected = {
      value: '1100 $',
      data: [
        {
          amt: 1100,
          qty: 101,
          titi: 'titi 0',
          toto: 1,
          toto_lb: 'toto 1',
          tutu: '0'
        }
      ],
      dimensions: [
        {
          dimension: { caption: 'Tutu', id: 'tutu' },
          cell: { caption: '0', id: '0' },
          axis: 'rows',
          index: 1
        },
        {
          dimension: { caption: 'Toto', id: 'toto' },
          cell: { caption: 'toto 1', id: 1 },
          axis: 'rows',
          index: 0
        },
        {
          dimension: { caption: 'Titi', id: 'titi' },
          cell: { caption: 'titi 0', id: 'titi 0' },
          axis: 'columns',
          index: 0
        }
      ],
      measure: {
        caption: 'Amount',
        id: 'amt',
        axis: 'columns'
      }
    };
    const cellInfos = getCellInfosSelector({
      data: datasource,
      config: { measureHeadersAxis: 'columns' }
    })(cell);
    expect(cellInfos).toEqual(expected);
  });
});

describe('cell value is computed correctly', () => {
  const accessor = row => row.qty;
  const data = [{ qty: 1 }, { qty: 2 }, { qty: 3 }, { qty: 4 }];

  test('with no rowDimension', () => {
    const cellValue = getCellValue.resultFunc([])(accessor, null, null, sum);
    expect(cellValue).toEqual(null);
  });
  test('with both dimensions as root', () => {
    const rowDimension = { isRoot: true };
    const columnDimension = { isRoot: true };
    const cellValue = getCellValue.resultFunc(data)(
      accessor,
      rowDimension,
      columnDimension,
      sum
    );
    expect(cellValue).toEqual(10);
  });
  test('with rowDimension as root', () => {
    const rowDimension = { isRoot: true };
    const columnDimension = { isRoot: false, rowIndexes: [1, 2, 3] };
    const cellValue = getCellValue.resultFunc(data)(
      accessor,
      rowDimension,
      columnDimension,
      sum
    );
    expect(cellValue).toEqual(9);
  });
  test('with columnDimension as root', () => {
    const columnDimension = { isRoot: true };
    const rowDimension = { isRoot: false, rowIndexes: [1, 2, 3] };
    const cellValue = getCellValue.resultFunc(data)(
      accessor,
      rowDimension,
      columnDimension,
      sum
    );
    expect(cellValue).toEqual(9);
  });
  test('with rowDimension as root and columnDimension rowIndexes empty', () => {
    const rowDimension = { isRoot: true };
    const columnDimension = { isRoot: false, rowIndexes: [] };
    const cellValue = getCellValue.resultFunc([])(
      accessor,
      rowDimension,
      columnDimension,
      sum
    );
    expect(cellValue).toEqual(null);
  });
});
