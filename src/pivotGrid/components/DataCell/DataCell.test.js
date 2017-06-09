import React from 'react';
import { shallow } from 'enzyme';
import DataCell from './index';

describe('DataCell', () => {
  test('renders without crashing', () => {
    const props = {
      valueHasChanged: false,
      position: {
        height: 30,
        left: 300,
        position: 'absolute',
        top: 90,
        width: 100
      },
      rowIndex: 3,
      columnIndex: 3,
      cell: {
        axisType: null,
        type: 3,
        template: 'cell-template-datavalue',
        value: 111,
        expanded: true,
        cssclass: 'cell ',
        visible: true,
        key: 114,
        rowDimension: {
          id: '1',
          parent: {
            id: 1,
            parent: {
              id: -1,
              parent: null,
              caption: null,
              isRoot: true,
              isLeaf: false,
              field: null,
              depth: 3,
              values: [0, 1],
              subdimvals: {
                '0': {
                  id: 0,
                  caption: 'toto 0',
                  isRoot: false,
                  isLeaf: false,
                  field: {
                    id: 'toto',
                    name: 'toto_lb',
                    caption: 'Toto',
                    sort: { order: 'asc' },
                    subTotal: {}
                  },
                  depth: 2,
                  values: ['0', '1'],
                  subdimvals: {
                    '0': {
                      id: '0',
                      caption: '0',
                      isRoot: false,
                      isLeaf: true,
                      field: {
                        id: 'tutu',
                        name: 'tutu',
                        caption: 'Tutu',
                        sort: { order: 'asc' },
                        subTotal: {}
                      },
                      depth: 1,
                      values: [],
                      subdimvals: {},
                      rowIndexes: [0, 2, 4, 6, 8]
                    },
                    '1': {
                      id: '1',
                      caption: '1',
                      isRoot: false,
                      isLeaf: true,
                      depth: 1,
                      values: [],
                      subdimvals: {},
                      rowIndexes: [1, 3, 5, 7, 9]
                    }
                  },
                  rowIndexes: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
                }
              },
              rowIndexes: null
            },
            caption: 'toto 1',
            isRoot: false,
            isLeaf: false,
            depth: 2,
            values: ['0', '1'],
            subdimvals: {
              '0': {
                id: '0',
                caption: '0',
                isRoot: false,
                isLeaf: true,
                depth: 1,
                values: [],
                subdimvals: {},
                rowIndexes: [10, 12, 14, 16, 18]
              }
            },
            rowIndexes: [10, 11, 12, 13, 14, 15, 16, 17, 18, 19]
          },
          caption: '1',
          isRoot: false,
          isLeaf: true,
          depth: 1,
          values: [],
          subdimvals: {},
          rowIndexes: [11, 13, 15, 17, 19]
        },
        columnDimension: {
          id: 'titi 1',
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
              'titi 0': {
                id: 'titi 0',
                caption: 'titi 0',
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
                rowIndexes: [0, 1, 10, 11]
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
          caption: 'titi 1',
          isRoot: false,
          isLeaf: true,
          depth: 1,
          values: [],
          subdimvals: {},
          rowIndexes: [2, 3, 12, 13]
        },
        rowType: 5,
        colType: 5,
        datafield: {
          id: 'amt',
          name: 'amt',
          caption: 'Amount',
          aggregationName: 'whatever',
          activated: true
        },
        caption: '111 $'
      },
      selected: false
    };

    shallow(<DataCell {...props} />);
  });
});
