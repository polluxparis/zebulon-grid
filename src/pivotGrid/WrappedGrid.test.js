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
      expect(wrapper.find('DataCell').length).toEqual(36);
      wrapper.instance().zoomIn();
      expect(wrapper.find('DataCell').length).toEqual(32);
    });

    test('zoom out', () => {
      const wrapper = mount(
        <WrappedGrid data={data} drilldown={() => 33} config={basicConfig} />
      );
      expect(wrapper.find('DataCell').length).toEqual(36);
      wrapper.instance().zoomOut();
      expect(wrapper.find('DataCell').length).toEqual(40);
    });

    test('sort', () => {
      const wrapper = mount(
        <WrappedGrid data={data} drilldown={() => 33} config={basicConfig} />
      );
      expect(wrapper.find('Header').first().text()).toEqual('titi 0');
      wrapper.instance().changeSortOrder('titi');
      expect(wrapper.find('Header').first().text()).toEqual('titi 4');
    });

    test('sort nested field', () => {
      const wrapper = mount(
        <WrappedGrid data={data} drilldown={() => 33} config={basicConfig} />
      );
      expect(
        wrapper
          .find('Header')
          .filterWhere(wrapper => wrapper.props().axis === 2)
          .filterWhere(wrapper => wrapper.props().header.parent !== null)
          .first()
          .text()
      ).toEqual('0');
      wrapper.instance().changeSortOrder('tutu');
      expect(
        wrapper
          .find('Header')
          .filterWhere(wrapper => wrapper.props().axis === 2)
          .filterWhere(wrapper => wrapper.props().header.parent !== null)
          .first()
          .text()
      ).toEqual('1');
    });

    test('move field from row to column', () => {
      const wrapper = mount(
        <WrappedGrid data={data} drilldown={() => 33} config={basicConfig} />
      );
      expect(
        wrapper
          .find('Header')
          .filterWhere(wrapper => wrapper.props().axis === 2).length
      ).toEqual(6);
      wrapper.instance().moveField('tutu', 'rows', 'columns', 1);
      expect(
        wrapper
          .find('Header')
          .filterWhere(wrapper => wrapper.props().axis === 2).length
      ).toEqual(2);
    });

    test('move field from row to reserve', () => {
      const wrapper = mount(
        <WrappedGrid data={data} drilldown={() => 33} config={basicConfig} />
      );
      expect(
        wrapper
          .find('Header')
          .filterWhere(wrapper => wrapper.props().axis === 2).length
      ).toEqual(6);
      wrapper.instance().moveField('toto', 'rows', 'fields', 0);
      expect(
        wrapper
          .find('Header')
          .filterWhere(wrapper => wrapper.props().axis === 2).length
      ).toEqual(2);
    });

    test('toggle datafield', () => {
      const wrapper = mount(
        <WrappedGrid data={data} drilldown={() => 33} config={basicConfig} />
      );
      expect(
        wrapper
          .find('Header')
          .filterWhere(wrapper => wrapper.props().axis === 1)
          .filterWhere(
            wrapper => wrapper.props().header.subheaders === undefined
          ).length
      ).toEqual(9);
      wrapper.instance().toggleDatafield('amt');
      expect(
        wrapper
          .find('Header')
          .filterWhere(wrapper => wrapper.props().axis === 1)
          .filterWhere(
            wrapper => wrapper.props().header.subheaders === undefined
          ).length
      ).toEqual(5);
    });

    test('resize header', () => {
      const wrapper = mount(
        <WrappedGrid data={data} drilldown={() => 33} config={basicConfig} />
      );
      expect(
        wrapper
          .find('Header')
          .filterWhere(wrapper => wrapper.props().axis === 1)
          .filterWhere(
            wrapper => wrapper.props().header.subheaders === undefined
          ).length
      ).toEqual(9);
      wrapper.instance().updateCellSize({
        handle: {
          id: 'titi 0-/-qty',
          axis: AxisType.COLUMNS,
          leafSubheaders: [],
          position: 'right'
        },
        offset: { x: 200 },
        initialOffset: { x: 0 },
        defaultCellSizes: { width: 100, height: 30 },
        sizes: { leafs: { columns: {} } }
      });
      expect(
        wrapper
          .find('Header')
          .filterWhere(wrapper => wrapper.props().axis === 1)
          .filterWhere(
            wrapper => wrapper.props().header.subheaders === undefined
          ).length
      ).toEqual(7);
    });

    test('resize header in cross direction', () => {
      const wrapper = mount(
        <WrappedGrid data={data} drilldown={() => 33} config={basicConfig} />
      );
      expect(
        wrapper
          .find('Header')
          .filterWhere(wrapper => wrapper.props().axis === 2).length
      ).toEqual(6);
      wrapper.instance().updateCellSize({
        handle: {
          id: 'titi',
          position: 'bottom',
          axis: AxisType.COLUMNS
        },
        offset: { y: 540 },
        initialOffset: { y: 0 },
        defaultCellSizes: { width: 100, height: 30 },
        sizes: { dimensions: { columns: {} } }
      });
      expect(
        wrapper
          .find('Header')
          .filterWhere(wrapper => wrapper.props().axis === 2).length
      ).toEqual(2);
    });
    // Not sure how to make this work, too complicated for too little value
    // describe('data updates', () => {
    //   test('highlight cells', () => {
    //const observableDatasource = getObservableMockDatasource();
    //     const tree = renderer.create(<WrappedGrid ref={grid => {wrapper.instance() = grid}} data={data} drilldown={() => 33} config={basicConfig} />).toJSON();
    //     expect(tree).toMatchSnapshot();
    //   });
    // });
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
