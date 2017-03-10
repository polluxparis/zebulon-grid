import React from 'react';
import { shallow } from 'enzyme';
import Header from './Header';

describe('Header', () => {
  test('renders without crashing', () => {
    const props = {
      axis: 1,
      header: {
        axetype: 1,
        type: 2,
        template: 'cell-template-dataheader',
        value: {
          id: 'qty',
          name: 'qty',
          caption: 'Quantity',
          aggregateFuncName: 'sum',
          activated: true
        },
        expanded: true,
        cssclass: 'header header-col',
        key: 'titi 0-/-qty',
        parent: {
          axetype: 1,
          type: 5,
          template: 'cell-template-column-header',
          value: 'titi 0',
          expanded: true,
          cssclass: 'header header-col',
          key: 'titi 0',
          isOnRowAxis: false,
          subtotalHeader: null,
          parent: null,
          dim: {
            id: 'titi 0',
            parent: {
              id: -1,
              parent: null,
              caption: null,
              isRoot: true,
              isLeaf: false,
              field: null,
              depth: 2,
              values: ['titi 0', 'titi 1', 'titi 2', 'titi 3', 'titi 4'],
              subdimvals: {
                'titi 1': {
                  id: 'titi 1',
                  caption: 'titi 1',
                  isRoot: false,
                  isLeaf: true,
                  field: {
                    id: 'titi',
                    name: 'titi',
                    caption: 'Titi',
                    sort: { order: 'asc' },
                    subTotal: {}
                  },
                  depth: 1,
                  values: [],
                  subdimvals: {},
                  rowIndexes: [2, 3, 12, 13]
                },
                'titi 2': {
                  id: 'titi 2',
                  caption: 'titi 2',
                  isRoot: false,
                  isLeaf: true,
                  depth: 1,
                  values: [],
                  subdimvals: {},
                  rowIndexes: [4, 5, 14, 15]
                },
                'titi 3': {
                  id: 'titi 3',
                  caption: 'titi 3',
                  isRoot: false,
                  isLeaf: true,
                  depth: 1,
                  values: [],
                  subdimvals: {},
                  rowIndexes: [6, 7, 16, 17]
                },
                'titi 4': {
                  id: 'titi 4',
                  caption: 'titi 4',
                  isRoot: false,
                  isLeaf: true,
                  depth: 1,
                  values: [],
                  subdimvals: {},
                  rowIndexes: [8, 9, 18, 19]
                }
              },
              rowIndexes: null
            },
            caption: 'titi 0',
            isRoot: false,
            isLeaf: true,
            depth: 1,
            values: [],
            subdimvals: {},
            rowIndexes: [0, 1, 10, 11]
          },
          subheaders: [
            null,
            {
              axetype: 1,
              type: 2,
              template: 'cell-template-dataheader',
              value: {
                id: 'amt',
                name: 'amt',
                caption: 'Amount',
                aggregateFuncName: 'whatever',
                activated: true
              },
              expanded: true,
              cssclass: 'header header-col',
              key: 'titi 0-/-amt',
              caption: 'Amount',
              x: 2,
              y: 1
            }
          ],
          datafieldscount: 2,
          caption: 'titi 0',
          x: 0,
          y: 0
        },
        caption: 'Quantity',
        x: 1,
        y: 1
      },
      positionStyle: {
        position: 'absolute',
        left: 0,
        top: 30,
        height: 30,
        width: 100
      },
      span: 1,
      startIndex: 0,
      scrollLeft: 0,
      scrollTop: 0,
      previewSizes: { height: 180, width: 1099 },
      gridId: 0
    };
    shallow(<Header {...props} />);
  });
});
