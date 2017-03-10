import React from 'react';
import ReactDOM from 'react-dom';
import renderer from 'react-test-renderer';
import { mount } from 'enzyme';

import WrappedGrid from './WrappedGrid';
import { AxisType } from './Axis';

import { getMockDatasource, basicConfig } from '../utils/mock';

describe('WrappedGrid', () => {
  const data = getMockDatasource(1, 2, 5);
  test('renders without crashing', () => {
    const div = document.createElement('div');
    ReactDOM.render(
      <WrappedGrid
        data={data}
        config={basicConfig}
        drilldown={() => 33}
        id={0}
      />,
      div
    );
  });

  describe('actions', () => {
    test('push a record', () => {
      const wrapper = mount(
        <WrappedGrid data={data} drilldown={() => 33} config={basicConfig} />
      );
      expect(wrapper.find('DataCell').first().text()).toEqual('0');
      wrapper.instance().pushData({
        toto: '0',
        toto_lb: 'toto 0',
        qty: 100,
        amt: 100,
        titi: 'titi 0',
        tutu: '0'
      });
      expect(wrapper.find('DataCell').first().text()).toEqual('100');
    });

    test('push an array of records', () => {
      const wrapper = mount(
        <WrappedGrid data={data} drilldown={() => 33} config={basicConfig} />
      );
      expect(wrapper.find('DataCell').first().text()).toEqual('0');
      wrapper.instance().pushData([
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
      expect(wrapper.find('DataCell').first().text()).toEqual('100');
    });

    test('zoom in', () => {
      const wrapper = mount(
        <WrappedGrid data={data} drilldown={() => 33} config={basicConfig} />
      );
      expect(wrapper.find('DataCell').first().text()).toEqual('0');
      this.grid.handleZoom(true);
      expect(wrapper.find('DataCell').first().text()).toEqual('100');
    });

    test('zoom out', () => {
      this.grid.handleZoom(false);
      expect(tree).toMatchSnapshot();
    });

    test('sort', () => {
      this.grid.sort(AxisType.COLUMNS, 'titi');
      expect(tree).toMatchSnapshot();
    });

    test('sort nested field', () => {
      this.grid.sort(AxisType.ROWS, 'tutu');
      expect(tree).toMatchSnapshot();
    });

    test('move field from row to column', () => {
      this.grid.moveField('tutu', AxisType.ROWS, AxisType.COLUMNS, 1);
      expect(tree).toMatchSnapshot();
    });

    test('move field from row to reserve', () => {
      this.grid.moveField('toto_lb', AxisType.ROWS, AxisType.FIELDS, 0);
      expect(tree).toMatchSnapshot();
    });

    test('toggle datafield', () => {
      this.grid.toggleDataField('amt');
      expect(tree).toMatchSnapshot();
    });

    test('resize header', () => {
      this.grid.updateCellSizes(
        {
          id: 'titi 1-/-qty',
          axis: AxisType.COLUMNS,
          leafSubheaders: [],
          position: 'right'
        },
        { x: 200 },
        { x: 0 }
      );
      expect(tree).toMatchSnapshot();
    });

    test('resize header in cross direction', () => {
      this.grid.updateCellSizes(
        { id: 'titi', axis: AxisType.COLUMNS, position: 'bottom' },
        { y: 200 },
        { y: 0 }
      );
      expect(tree).toMatchSnapshot();
    });
    // Not sure how to make this work, too complicated for too little value
    // describe('data updates', () => {
    //   test('highlight cells', () => {
    //const observableDatasource = getObservableMockDatasource();
    //     const tree = renderer.create(<WrappedGrid ref={grid => {this.grid = grid}} data={data} drilldown={() => 33} config={basicConfig} />).toJSON();
    //     expect(tree).toMatchSnapshot();
    //   });
    // });*/
  });

  describe('returns correct markup when config', () => {
    test('has no columns', () => {
      const config = { ...basicConfig, columns: [] };
      const tree = renderer
        .create(
          <WrappedGrid data={data} drilldown={() => 33} config={config} />
        )
        .toJSON();
      expect(tree).toMatchSnapshot();
    });
    test('has no rows', () => {
      const config = { ...basicConfig, rows: [] };
      const tree = renderer
        .create(
          <WrappedGrid data={data} drilldown={() => 33} config={config} />
        )
        .toJSON();
      expect(tree).toMatchSnapshot();
    });

    test('has no data field', () => {
      const config = { ...basicConfig, data: [] };
      const tree = renderer
        .create(
          <WrappedGrid data={data} drilldown={() => 33} config={config} />
        )
        .toJSON();
      expect(tree).toMatchSnapshot();
    });
    test('has one data field', () => {
      const config = { ...basicConfig, data: ['Quantity'] };
      const tree = renderer
        .create(
          <WrappedGrid data={data} drilldown={() => 33} config={config} />
        )
        .toJSON();
      expect(tree).toMatchSnapshot();
    });
    test('has two data fields', () => {
      const config = { ...basicConfig, data: ['Quantity', 'Amount'] };
      const tree = renderer
        .create(
          <WrappedGrid data={data} drilldown={() => 33} config={config} />
        )
        .toJSON();
      expect(tree).toMatchSnapshot();
    });
    test('has data fields on row axis', () => {
      const config = { ...basicConfig, dataHeadersLocation: 'rows' };
      const tree = renderer
        .create(
          <WrappedGrid data={data} drilldown={() => 33} config={config} />
        )
        .toJSON();
      expect(tree).toMatchSnapshot();
    });
  });
});
