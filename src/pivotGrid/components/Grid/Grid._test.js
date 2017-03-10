import React from 'react';
import renderer from 'react-test-renderer';
import { DragDropContext } from 'react-dnd';
import HTML5Backend from 'react-dnd-html5-backend';

import RawGrid from './Grid';
import { Store } from '../../index';
import { AxisType } from '../../Axis';
import {
  getMockDatasource,
  getObservableMockDatasource,
  basicConfig
} from '../../../utils/mock';

const Grid = DragDropContext(HTML5Backend)(RawGrid);

const datasource = getMockDatasource();

describe('works when config', () => {
  test('is empty', () => {
    const store = new Store({}, null, datasource);
    const tree = renderer
      .create(<Grid store={store} height={600} width={800} />)
      .toJSON();
    expect(tree).toMatchSnapshot();
  });
  test('has no columns', () => {
    const config = { ...basicConfig, columns: [] };
    const store = new Store(config, null, datasource);
    const tree = renderer
      .create(<Grid store={store} height={600} width={800} />)
      .toJSON();
    expect(tree).toMatchSnapshot();
  });
  test('has no rows', () => {
    const config = { ...basicConfig, rows: [] };
    const store = new Store(config, null, datasource);
    const tree = renderer
      .create(<Grid store={store} height={600} width={800} />)
      .toJSON();
    expect(tree).toMatchSnapshot();
  });

  test('has no data field', () => {
    const store = new Store({ ...basicConfig, data: [] }, null, datasource);
    const tree = renderer
      .create(<Grid store={store} height={600} width={800} />)
      .toJSON();
    expect(tree).toMatchSnapshot();
  });
  test('has one data field', () => {
    const store = new Store(
      { ...basicConfig, data: ['Quantity'] },
      null,
      datasource
    );
    const tree = renderer
      .create(<Grid store={store} height={600} width={800} />)
      .toJSON();
    expect(tree).toMatchSnapshot();
  });
  test('has two data fields', () => {
    const store = new Store(
      { ...basicConfig, data: ['Quantity', 'Amount'] },
      null,
      datasource
    );
    const tree = renderer
      .create(<Grid store={store} height={600} width={800} />)
      .toJSON();
    expect(tree).toMatchSnapshot();
  });
  test('has data fields on row axis', () => {
    const store = new Store(
      { ...basicConfig, dataHeadersLocation: 'rows' },
      null,
      datasource
    );
    const tree = renderer
      .create(<Grid store={store} height={600} width={800} />)
      .toJSON();
    expect(tree).toMatchSnapshot();
  });
});

describe('actions', () => {
  test('push a record', () => {
    const store = new Store(basicConfig, null, datasource);
    store.push({
      toto: '0',
      toto_lb: 'toto 0',
      qty: 100,
      amt: 100,
      titi: 'titi 0',
      tutu: '1'
    });
    const tree = renderer
      .create(<Grid store={store} height={600} width={800} />)
      .toJSON();
    expect(tree).toMatchSnapshot();
  });

  test('push an array of records', () => {
    const store = new Store(basicConfig, null, datasource);
    store.push([
      {
        toto: '0',
        toto_lb: 'toto 0',
        qty: 100,
        amt: 100,
        titi: 'titi 0',
        tutu: '1'
      },
      {
        toto: '0',
        toto_lb: 'toto 0',
        qty: 100,
        amt: 100,
        titi: 'titi 0',
        tutu: '0'
      }
    ]);
    const tree = renderer
      .create(<Grid store={store} height={600} width={800} />)
      .toJSON();
    expect(tree).toMatchSnapshot();
  });

  test('zoom in', () => {
    const store = new Store(basicConfig, null, datasource);
    store.handleZoom(true);
    const tree = renderer
      .create(<Grid store={store} height={600} width={800} />)
      .toJSON();
    expect(tree).toMatchSnapshot();
  });

  test('zoom out', () => {
    const store = new Store(basicConfig, null, datasource);
    store.handleZoom(false);
    const tree = renderer
      .create(<Grid store={store} height={600} width={800} />)
      .toJSON();
    expect(tree).toMatchSnapshot();
  });

  test('sort', () => {
    const store = new Store(basicConfig, null, datasource);
    store.sort(AxisType.COLUMNS, 'titi');
    const tree = renderer
      .create(<Grid store={store} height={600} width={800} />)
      .toJSON();
    expect(tree).toMatchSnapshot();
  });

  test('sort nested field', () => {
    const store = new Store(basicConfig, null, datasource);
    store.sort(AxisType.ROWS, 'tutu');
    const tree = renderer
      .create(<Grid store={store} height={600} width={800} />)
      .toJSON();
    expect(tree).toMatchSnapshot();
  });

  test('move field from row to column', () => {
    const store = new Store(basicConfig, null, datasource);
    store.moveField('tutu', AxisType.ROWS, AxisType.COLUMNS, 1);
    const tree = renderer
      .create(<Grid store={store} height={600} width={800} />)
      .toJSON();
    expect(tree).toMatchSnapshot();
  });

  test('move field from row to reserve', () => {
    const store = new Store(basicConfig, null, datasource);
    store.moveField('toto_lb', AxisType.ROWS, AxisType.FIELDS, 0);
    const tree = renderer
      .create(<Grid store={store} height={600} width={800} />)
      .toJSON();
    expect(tree).toMatchSnapshot();
  });

  test('toggle datafield', () => {
    const store = new Store(basicConfig, null, datasource);
    store.toggleDataField('amt');
    const tree = renderer
      .create(<Grid store={store} height={600} width={800} />)
      .toJSON();
    expect(tree).toMatchSnapshot();
  });

  test('resize header', () => {
    const store = new Store(basicConfig, null, datasource);
    store.updateCellSizes(
      {
        id: 'titi 1-/-qty',
        axis: AxisType.COLUMNS,
        leafSubheaders: [],
        position: 'right'
      },
      { x: 200 },
      { x: 0 }
    );
    const tree = renderer
      .create(<Grid store={store} height={600} width={800} />)
      .toJSON();
    expect(tree).toMatchSnapshot();
  });

  test('resize header in cross direction', () => {
    const store = new Store(basicConfig, null, datasource);
    store.updateCellSizes(
      { id: 'titi', axis: AxisType.COLUMNS, position: 'bottom' },
      { y: 200 },
      { y: 0 }
    );
    const tree = renderer
      .create(<Grid store={store} height={600} width={800} />)
      .toJSON();
    expect(tree).toMatchSnapshot();
  });
  // Not sure how to make this work, too complicated for too little value
  // describe('data updates', () => {
  //   test('highlight cells', () => {
  //     const observableDatasource = getObservableMockDatasource();
  //     const store = new Store(basicConfig, null, observableDatasource);
  //     const tree = renderer.create(<Grid store={store} height={600} width={800} />).toJSON();
  //     expect(tree).toMatchSnapshot();
  //   });
  // });
});
