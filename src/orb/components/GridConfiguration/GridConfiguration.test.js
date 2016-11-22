import React from 'react';
import renderer from 'react-test-renderer';
import { DragDropContext } from 'react-dnd';
import HTML5Backend from 'react-dnd-html5-backend';

import RawGridConfiguration from './GridConfiguration';
import { Store } from '../../index';
import { AxisType } from '../../Axis';
import { getMockDatasource, basicConfig } from '../../../utils/mock';

const GridConfiguration = DragDropContext(HTML5Backend)(RawGridConfiguration);

const datasource = getMockDatasource();

describe('works when config', () => {
  it('is empty', () => {
    const store = new Store({}, null, datasource);
    const tree = renderer.create(<GridConfiguration store={store} />).toJSON();
    expect(tree).toMatchSnapshot();
  });
  it('has no columns', () => {
    const config = { ...basicConfig, columns: [] };
    const store = new Store(config, null, datasource);
    const tree = renderer.create(<GridConfiguration store={store} />).toJSON();
    expect(tree).toMatchSnapshot();
  });
  it('has no rows', () => {
    const config = { ...basicConfig, rows: [] };
    const store = new Store(config, null, datasource);
    const tree = renderer.create(<GridConfiguration store={store} />).toJSON();
    expect(tree).toMatchSnapshot();
  });

  it('has no data field', () => {
    const store = new Store({ ...basicConfig, data: [] }, null, datasource);
    const tree = renderer.create(<GridConfiguration store={store} />).toJSON();
    expect(tree).toMatchSnapshot();
  });
  it('has one data field', () => {
    const store = new Store({ ...basicConfig, data: ['Quantity'] }, null, datasource);
    const tree = renderer.create(<GridConfiguration store={store} />).toJSON();
    expect(tree).toMatchSnapshot();
  });
  it('has two data fields', () => {
    const store = new Store({ ...basicConfig, data: ['Quantity', 'Amount'] }, null, datasource);
    const tree = renderer.create(<GridConfiguration store={store} />).toJSON();
    expect(tree).toMatchSnapshot();
  });
});

describe('actions', () => {
  it('move field from row to column', () => {
    const store = new Store(basicConfig, null, datasource);
    store.moveField('tutu', AxisType.ROWS, AxisType.COLUMNS, 1);
    const tree = renderer.create(<GridConfiguration store={store} />).toJSON();
    expect(tree).toMatchSnapshot();
  });

  it('move field from row to reserve', () => {
    const store = new Store(basicConfig, null, datasource);
    store.moveField('toto_lb', AxisType.ROWS, AxisType.FIELDS, 0);
    const tree = renderer.create(<GridConfiguration store={store} />).toJSON();
    expect(tree).toMatchSnapshot();
  });

  it('toggle datafield', () => {
    const store = new Store(basicConfig, null, datasource);
    store.toggleDataField('amt');
    const tree = renderer.create(<GridConfiguration store={store} />).toJSON();
    expect(tree).toMatchSnapshot();
  });
});
