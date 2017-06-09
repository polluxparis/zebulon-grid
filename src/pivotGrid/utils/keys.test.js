import { getCellInfosKey } from './keys';
import { TOTAL_ID, KEY_SEPARATOR, AXIS_SEPARATOR } from '../constants';

describe('obtain cell keys from cell infos', () => {
  it('with dimensions on both axis', () => {
    const cellInfos = {
      value: 0,
      dimensions: [
        {
          dimension: { caption: 'Tutu', id: 'tutu' },
          cell: { caption: '0', id: '0' },
          axis: 'rows',
          index: 0
        },
        {
          dimension: { caption: 'Toto', id: 'toto' },
          cell: { caption: 'toto 0', id: 0 },
          axis: 'rows',
          index: 1
        },
        {
          dimension: { caption: 'Titi', id: 'titi' },
          cell: { caption: 'titi 0', id: 'titi 0' },
          axis: 'columns',
          index: 0
        }
      ],
      data: [
        {
          toto: 0,
          toto_lb: 'toto 0',
          titi: 'titi 0',
          tutu: '0',
          qty: 0,
          amt: 1000
        }
      ],
      measure: { caption: 'Quantity', id: 'qty', axis: 'columns' }
    };
    expect(getCellInfosKey(cellInfos)).toEqual({
      columns: ['titi 0', 'qty'].join(KEY_SEPARATOR),
      rows: ['0', '0'].join(KEY_SEPARATOR)
    });
  });

  it('with dimensions only on row axis', () => {
    const cellInfos = {
      value: 0,
      dimensions: [
        {
          dimension: { caption: 'Tutu', id: 'tutu' },
          cell: { caption: '0', id: '0' },
          axis: 'rows',
          index: 0
        },
        {
          dimension: { caption: 'Toto', id: 'toto' },
          cell: { caption: 'toto 0', id: 0 },
          axis: 'rows',
          index: 1
        }
      ],
      data: [
        {
          toto: 0,
          toto_lb: 'toto 0',
          tutu: '0',
          qty: 0,
          amt: 1000
        }
      ],
      measure: { caption: 'Quantity', id: 'qty', axis: 'columns' }
    };
    expect(getCellInfosKey(cellInfos)).toEqual({
      columns: [
        [TOTAL_ID, 'qty'].join(KEY_SEPARATOR),
        ['tutu', 'toto'].join(KEY_SEPARATOR)
      ].join(AXIS_SEPARATOR),
      rows: ['0', '0'].join(KEY_SEPARATOR)
    });
  });

  it('with dimensions only on column axis', () => {
    const cellInfos = {
      value: 0,
      dimensions: [
        {
          dimension: { caption: 'Titi', id: 'titi' },
          cell: { caption: 'titi 0', id: 'titi 0' },
          axis: 'columns',
          index: 0
        }
      ],
      data: [
        {
          titi: 'titi 0',
          qty: 0,
          amt: 1000
        }
      ],
      measure: { caption: 'Quantity', id: 'qty', axis: 'columns' }
    };
    expect(getCellInfosKey(cellInfos)).toEqual({
      columns: ['titi 0', 'qty'].join(KEY_SEPARATOR),
      rows: `${TOTAL_ID}${AXIS_SEPARATOR}titi`
    });
  });

  it('with dimensions on both axis and data headers on rows', () => {
    const cellInfos = {
      value: 0,
      dimensions: [
        {
          dimension: { caption: 'Tutu', id: 'tutu' },
          cell: { caption: '0', id: '0' },
          axis: 'rows',
          index: 0
        },
        {
          dimension: { caption: 'Toto', id: 'toto' },
          cell: { caption: 'toto 0', id: 0 },
          axis: 'rows',
          index: 1
        },
        {
          dimension: { caption: 'Titi', id: 'titi' },
          cell: { caption: 'titi 0', id: 'titi 0' },
          axis: 'columns',
          index: 0
        }
      ],
      data: [
        {
          toto: 0,
          toto_lb: 'toto 0',
          titi: 'titi 0',
          tutu: '0',
          qty: 0,
          amt: 1000
        }
      ],
      measure: { caption: 'Quantity', id: 'qty', axis: 'rows' }
    };
    expect(getCellInfosKey(cellInfos)).toEqual({
      columns: ['titi 0'].join(KEY_SEPARATOR),
      rows: ['0', '0', 'qty'].join(KEY_SEPARATOR)
    });
  });
});
