import React from 'react';
import ReactDOM from 'react-dom';
import renderer from 'react-test-renderer';
import { mount } from 'enzyme';

import ZebulonGrid from './ZebulonGrid';
import { AxisType } from './constants';

import { getMockDatasource, basicConfig } from '../utils/mock';
import { configFunctions } from '../utils/configFunctions';
import { externalFunctions } from '../utils/externalFunctions';

describe('ZebulonGrid', () => {
  const data = getMockDatasource(1, 2, 5);
  test('renders without crashing', () => {
    const div = document.createElement('div');
    ReactDOM.render(
      <ZebulonGrid
        data={data}
        config={basicConfig}
        externalFunctions={externalFunctions}
        configFunctions={configFunctions}
        drilldown={() => 33}
        id={0}
      />,
      div
    );
  });

  describe('actions', () => {
    test('push a record', () => {
      const wrapper = mount(
        <ZebulonGrid
          externalFunctions={externalFunctions}
          configFunctions={configFunctions}
          data={data}
          drilldown={() => 33}
          config={basicConfig}
        />
      );
      expect(wrapper.find('DataCell').first().text()).toEqual('101');
      wrapper.instance().pushData(data[data.length - 1]);
      expect(wrapper.find('DataCell').first().text()).toEqual('202');
    });

    test('push an array of records', () => {
      const wrapper = mount(
        <ZebulonGrid
          externalFunctions={externalFunctions}
          configFunctions={configFunctions}
          data={data}
          drilldown={() => 33}
          config={basicConfig}
        />
      );
      expect(wrapper.find('DataCell').first().text()).toEqual('101');
      wrapper.instance().pushData(data);
      expect(wrapper.find('DataCell').first().text()).toEqual('202');
    });

    test('zoom in', () => {
      const wrapper = mount(
        <ZebulonGrid
          externalFunctions={externalFunctions}
          configFunctions={configFunctions}
          data={data}
          drilldown={() => 33}
          config={basicConfig}
        />
      );
      expect(wrapper.find('DataCell').length).toEqual(36);
      wrapper.instance().zoomIn();
      expect(wrapper.find('DataCell').length).toEqual(32);
    });

    test('zoom out', () => {
      const wrapper = mount(
        <ZebulonGrid
          externalFunctions={externalFunctions}
          configFunctions={configFunctions}
          data={data}
          drilldown={() => 33}
          config={basicConfig}
        />
      );
      expect(wrapper.find('DataCell').length).toEqual(36);
      wrapper.instance().zoomOut();
      expect(wrapper.find('DataCell').length).toEqual(44);
    });

    test('sort', () => {
      const wrapper = mount(
        <ZebulonGrid
          externalFunctions={externalFunctions}
          configFunctions={configFunctions}
          data={data}
          drilldown={() => 33}
          config={basicConfig}
        />
      );
      expect(wrapper.find('Header').first().text()).toEqual('titi 0');
      wrapper.instance().toggleSortOrder('titi');
      expect(wrapper.find('Header').first().text()).toEqual('titi 4');
    });

    test('sort nested dimension', () => {
      const wrapper = mount(
        <ZebulonGrid
          externalFunctions={externalFunctions}
          configFunctions={configFunctions}
          data={data}
          drilldown={() => 33}
          config={basicConfig}
        />
      );
      expect(
        wrapper
          .find('Header')
          .filterWhere(wrapper => wrapper.props().axis === 2)
          .filterWhere(wrapper => wrapper.props().header.parent !== null)
          .first()
          .text()
      ).toEqual('0');
      wrapper.instance().toggleSortOrder('tutu');
      expect(
        wrapper
          .find('Header')
          .filterWhere(wrapper => wrapper.props().axis === 2)
          .filterWhere(wrapper => wrapper.props().header.parent !== null)
          .first()
          .text()
      ).toEqual('1');
    });

    test('move dimension from row to column', () => {
      const wrapper = mount(
        <ZebulonGrid
          externalFunctions={externalFunctions}
          configFunctions={configFunctions}
          data={data}
          drilldown={() => 33}
          config={basicConfig}
        />
      );
      expect(
        wrapper
          .find('Header')
          .filterWhere(wrapper => wrapper.props().axis === 2).length
      ).toEqual(6);
      wrapper.instance().moveDimension('tutu', 'rows', 'columns', 1);
      expect(
        wrapper
          .find('Header')
          .filterWhere(wrapper => wrapper.props().axis === 2).length
      ).toEqual(2);
    });

    test('move dimension from row to reserve', () => {
      const wrapper = mount(
        <ZebulonGrid
          externalFunctions={externalFunctions}
          configFunctions={configFunctions}
          data={data}
          drilldown={() => 33}
          config={basicConfig}
        />
      );
      expect(
        wrapper
          .find('Header')
          .filterWhere(wrapper => wrapper.props().axis === 2).length
      ).toEqual(6);
      wrapper.instance().moveDimension('toto', 'rows', 'dimensions', 0);
      expect(
        wrapper
          .find('Header')
          .filterWhere(wrapper => wrapper.props().axis === 2).length
      ).toEqual(2);
    });

    test('toggle measure', () => {
      const wrapper = mount(
        <ZebulonGrid
          externalFunctions={externalFunctions}
          configFunctions={configFunctions}
          data={data}
          drilldown={() => 33}
          config={basicConfig}
        />
      );
      expect(
        wrapper
          .find('Header')
          .filterWhere(wrapper => wrapper.props().axis === 1)
          .filterWhere(
            wrapper => wrapper.props().header.subheaders === undefined
          )
          .filterWhere(wrapper => wrapper.props().header.caption === 'Amount')
          .length
      ).toEqual(3);
      wrapper.instance().toggleMeasure('amt');
      expect(
        wrapper
          .find('Header')
          .filterWhere(wrapper => wrapper.props().axis === 1)
          .filterWhere(
            wrapper => wrapper.props().header.subheaders === undefined
          )
          .filterWhere(wrapper => wrapper.props().header.caption === 'Amount')
          .length
      ).toEqual(0);
    });

    test('resize header', () => {
      const wrapper = mount(
        <ZebulonGrid
          externalFunctions={externalFunctions}
          configFunctions={configFunctions}
          data={data}
          drilldown={() => 33}
          config={basicConfig}
        />
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
        <ZebulonGrid
          externalFunctions={externalFunctions}
          configFunctions={configFunctions}
          data={data}
          drilldown={() => 33}
          config={basicConfig}
        />
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
    //     const observableDatasource = getObservableMockDatasource();
    //     const tree = renderer.create(<ZebulonGrid        externalFunctions={externalFunctions}
    // configFunctions={configFunctions} ref={grid => {wrapper.instance() = grid}} data={data} drilldown={() => 33} config={basicConfig} />).toJSON();
    //     expect(tree).toMatchSnapshot();
    //   });
    // });
  });

  describe('returns correct markup when config', () => {
    test('has no columns', () => {
      const config = { ...basicConfig, columns: [] };
      const tree = renderer
        .create(
          <ZebulonGrid
            externalFunctions={externalFunctions}
            configFunctions={configFunctions}
            data={data}
            drilldown={() => 33}
            config={config}
          />
        )
        .toJSON();
      expect(tree).toMatchSnapshot();
    });
    test('has no rows', () => {
      const config = { ...basicConfig, rows: [] };
      const tree = renderer
        .create(
          <ZebulonGrid
            externalFunctions={externalFunctions}
            configFunctions={configFunctions}
            data={data}
            drilldown={() => 33}
            config={config}
          />
        )
        .toJSON();
      expect(tree).toMatchSnapshot();
    });

    test('has no data dimension', () => {
      const config = { ...basicConfig, data: [] };
      const tree = renderer
        .create(
          <ZebulonGrid
            externalFunctions={externalFunctions}
            configFunctions={configFunctions}
            data={data}
            drilldown={() => 33}
            config={config}
          />
        )
        .toJSON();
      expect(tree).toMatchSnapshot();
    });
    test('has one data dimension', () => {
      const config = { ...basicConfig, data: ['Quantity'] };
      const tree = renderer
        .create(
          <ZebulonGrid
            externalFunctions={externalFunctions}
            configFunctions={configFunctions}
            data={data}
            drilldown={() => 33}
            config={config}
          />
        )
        .toJSON();
      expect(tree).toMatchSnapshot();
    });
    test('has two data dimensions', () => {
      const config = { ...basicConfig, data: ['Quantity', 'Amount'] };
      const tree = renderer
        .create(
          <ZebulonGrid
            externalFunctions={externalFunctions}
            configFunctions={configFunctions}
            data={data}
            drilldown={() => 33}
            config={config}
          />
        )
        .toJSON();
      expect(tree).toMatchSnapshot();
    });
    test('has data dimensions on row axis', () => {
      const config = { ...basicConfig, measureHeadersAxis: 'rows' };
      const tree = renderer
        .create(
          <ZebulonGrid
            externalFunctions={externalFunctions}
            configFunctions={configFunctions}
            data={data}
            drilldown={() => 33}
            config={config}
          />
        )
        .toJSON();
      expect(tree).toMatchSnapshot();
    });
  });
});
