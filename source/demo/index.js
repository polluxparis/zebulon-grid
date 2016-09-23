import React from 'react'
import ReactDOM from 'react-dom'
import { Observable } from 'rx-lite'

// CSS files
import 'react-virtualized/styles.css'
import 'react-resizable/css/styles.css'

import '../styles.css'

import Main from './Main'
// const themeChangeCallbacks = {}

// Do not use the .less files because the compilation is too complicated (cf gulpactions/buildcss.js)
// require('../../../dist/orb.css')
// require('../../deps/bootstrap-3.3.1/css/bootstrap.css')

function getMockDataSource (dataRepetition, nToto) {
  const nTiti = 10
  const nTutu = 2
  let obj = []
  let res = []
  for (let k = 0; k < dataRepetition; k++) {
    for (let i = 0; i < 3; i++) {
      for (let u = 0; u < nTutu; u++) {
        obj = []
        obj['toto'] = 'toto ' + String(0)
        obj['titi'] = 'titi ' + String(i)
        obj['tutu'] = String(u)
        obj['qty'] = k + 10 * u + 100 * i * 1000 * 0 + 1 // +9999999999.1234567890123456
        obj['amt'] = k + 20 * u + 200 * i * 2000 * 0 + 2 // +9999999999.1234567890123456
        res.push(obj)
      }
    }
    for (let o = 1; o < nToto; o++) {
      for (let i = 0; i < nTiti; i++) {
        for (let u = 0; u < nTutu; u++) {
          obj = []
          obj['toto'] = 'toto' + String(o)
          obj['toto_cd'] = 'TOTO' + String(o)
          obj['titi'] = 'titi ' + String(i)
          obj['tutu'] = String(u)
          obj['qty'] = k + 10 * u + 100 * i * 1000 * o + 1 // +9999999999.1234567890123456
          obj['amt'] = k + 20 * u + 200 * i * 2000 * o + 2 // +9999999999.1234567890123456
          res.push(obj)
        }
      }
    }
  }
  return res
}

const dataArray = getMockDataSource(1, 100)
const datasourceArray = [
  dataArray.slice(0, 5000),
  // dataArray.slice(0, 1000),
  {toto: 'toto0', qty: 1, amt: 2, titi: 'titi000000000000 0', tutu: '0'},
  {toto: 'toto0', qty: 1, amt: 2, titi: 'titi000000000000 0', tutu: '0'},
  // ['toto11', 33, 666, 'titi0', '0'],
  [{toto: 'toto1', qty: 100, amt: 1000, titi: 'titi0', tutu: '0'}, {toto: 'toto12', qty: 44, amt: 777, titi: 'titi0',
  tutu: '0'}]
// ['toto2', 10, 100, 'titi0', '0']
]

const datasource = datasourceArray[0]

// const datasource = Observable.interval(2000).take(3)
//   .map(i => datasourceArray[i])
//   .do(data => console.log('data received', data))

let config = {
  canMoveFields: true,
  dataHeadersLocation: 'columns',
  width: 1099,
  height: 601,
  cellHeight: 30,
  cellWidth: 100,
  theme: 'green',
  toolbar: {
    visible: true
  },
  grandTotal: {
    rowsvisible: false,
    columnsvisible: false
  },
  subTotal: {
    visible: false,
    collapsed: false,
    collapsible: false
  },
  rowSettings: {
    subTotal: {
      visible: false,
      collapsed: false,
      collapsible: false
    }
  },
  columnSettings: {
    subTotal: {
      visible: false,
      collapsed: false,
      collapsible: false
    }
  },
  fields: [
    {
      name: 'toto',
      code: 'toto_cd',
      caption: 'Toto',
      sort: {
        order: 'asc'
      }
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
      caption: 'Titi'
    },
    {
      name: 'tutu',
      caption: 'Tutu'
    }
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
      aggregateFunc: 'sum'
    },
    {
      name: 'amt',
      caption: 'Amount',
      aggregateFunc: 'sum',
      aggregateFuncName: 'whatever',
      formatFunc: (value) => value ? Number(value).toFixed(0) + ' $' : ''
    }
  ],
  columns: ['Tutu'], // , 'Category' ],
  rows: ['Toto', 'Titi'],
  data: ['Quantity', 'Amount'],
  drilldown: (cell) => console.log('drilldown (config) on cell', cell),
  preFilters: {
    // 'Titi': ['titi0']
    // 'Class': { 'Matches': 'Regular' },
    // 'Manufacturer': { 'Matches': /^a|^c/ },
    // 'Category': { 'Does Not Match': 'D' },
    // 'Amount': { '>': 40 },
    // 'Quantity': [4, 8, 12]
  }
}

ReactDOM.render(<Main config={config} datasource={datasource} />, document.getElementById('root'))
