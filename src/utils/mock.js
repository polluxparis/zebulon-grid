import { Observable } from 'rx-lite';

export function getMockDatasource(dataRepetition = 1, nToto = 10, nTiti = 10) {
  const nTutu = 2;
  let obj = [];
  const res = [];
  for (let k = 0; k < dataRepetition; k += 1) {
    for (let o = 0; o < nToto; o += 1) {
      for (let i = 0; i < nTiti; i += 1) {
        for (let u = 0; u < nTutu; u += 1) {
          obj = {};
          obj.toto = String(o);
          obj.toto_lb = `toto ${String(o)}`;
          obj.titi = `titi ${String(i)}`;
          obj.tutu = String(u);
          obj.qty = u + (10 * i) + (100 * o); // +9999999999.1234567890123456
          obj.amt = u + (10 * i) + (100 * o);// +9999999999.1234567890123456
          res.push(obj);
        }
      }
    }
  }
  return res;
}

export function getObservableMockDatasource(interval) {
  const data = [
    getMockDatasource(),
    [
      { toto: '0', toto_lb: 'toto 0', qty: 100, amt: 100, titi: 'titi 0', tutu: '1' },
      { toto: '0', toto_lb: 'toto 0', qty: 100, amt: 100, titi: 'titi 0', tutu: '0' },
    ],
    { toto: '0', toto_lb: 'toto 0', qty: 1, amt: 2, titi: 'titi 0', tutu: '1' },
  ];
  return Observable.interval(interval || 100).take(3).map(i => data[i]);
}

export const basicConfig = {
  canMoveFields: true,
  dataHeadersLocation: 'columns',
  width: 1099,
  height: 601,
  cellHeight: 30,
  cellWidth: 100,
  theme: 'green',
  toolbar: {
    visible: true,
  },
  grandTotal: {
    rowsvisible: false,
    columnsvisible: false,
  },
  subTotal: {
    visible: false,
    collapsed: false,
    collapsible: false,
  },
  rowSettings: {
    subTotal: {
      visible: false,
      collapsed: false,
      collapsible: false,
    },
  },
  columnSettings: {
    subTotal: {
      visible: false,
      collapsed: false,
      collapsible: false,
    },
  },
  fields: [
    {
      name: 'toto_lb',
      code: 'toto',
      caption: 'Toto',
      sort: {
        order: 'asc',
      },
    },
    // {
    //     name: '1',
    //     caption: 'Product',
    // },
    // {
    //     name: '2',
    //     caption: 'Manufacturer',
    //     sort: {
    //         order: 'asc'
    //     },
    //     rowSettings: {
    //         subTotal: {
    //             visible: false,
    //             collapsed: true,
    //             collapsible: true
    //         }
    //     },
    // },
    {
      name: 'titi',
      caption: 'Titi',
    },
    {
      name: 'tutu',
      caption: 'Tutu',
    },
  // {
  //     name: '4',
  //     caption: 'Category',
  //     sort: {
  //         customfunc: function(a, b) {
  //             if(a.trim() == 'Touch Screen Phones'){
  //              return -1
  //             }
  //             if(a < b) return -1
  //             if(a > b) return 1
  //             return 0
  //         }
  //     }
  // },
  ],
  dataFields: [
    {
      name: 'qty',
      caption: 'Quantity',
      aggregateFunc: 'sum',
    },
    {
      name: 'amt',
      caption: 'Amount',
      aggregateFunc: 'sum',
      aggregateFuncName: 'whatever',
      formatFunc: (value) => {
        if (value || value === 0) {
          return `${Number(value).toFixed(0)} $`;
        }
        return '';
      },
    },
  ],
  columns: ['Titi'],
  rows: ['Toto', 'Tutu'],
  data: ['Quantity'],
  // drilldown: (cell) => { console.log('drilldown (config) on cell', cell); },
  preFilters: {
    // 'Titi': ['titi0']
    // 'Class': { 'Matches': 'Regular' },
    // 'Manufacturer': { 'Matches': /^a|^c/ },
    // 'Category': { 'Does Not Match': 'D' },
    // 'Amount': { '>': 40 },
    // 'Quantity': [4, 8, 12]
  },
};
