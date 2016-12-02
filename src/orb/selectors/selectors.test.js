import { getMockDatasource } from '../../utils/mock';
import { getCellSizes, getFilteredData } from './index';

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

it('filter correctly', () => {
  const actual = getFilteredData({
    data: getMockDatasource(1, 10, 10),
    filters: { toto: { fieldId: 'toto', staticValue: ['1'] } },
  });
  expect(actual.map(data => data.toto).includes('1')).toBe(true);
  expect(actual.map(data => data.toto).includes('0')).toBe(false);
  expect(actual.length).toEqual(20);
});
