/* eslint-disable */

import React from 'react'
import ReactDOM from 'react-dom'
import {Observable} from 'rx-lite'

import { TestRunner } from 'fps-measurer';
import createScrollingTestCase from './fpsTests/tests'

import App from './App'

function getMockDataSource (dataRepetition, nToto) {
  const nTiti = 100
  const nTutu = 2
  let obj = []
  let res = []
  for (let k = 0; k < dataRepetition; k++) {
    for (let o = 0; o < nToto; o++) {
      for (let i = 0; i < nTiti; i++) {
        for (let u = 0; u < nTutu; u++) {
          obj = []
          obj['toto'] = String(o)
          obj['toto_lb'] = 'toto ' + String(o)
          obj['titi'] = 'titi ' + String(i)
          obj['tutu'] = String(u)
          obj['qty'] = u + 10 * i + 100 * o // +9999999999.1234567890123456
          obj['amt'] = u + 10 * i + 100 * o // +9999999999.1234567890123456
          res.push(obj)
        }
      }
    }
  }
  return res
}

const dataArray = getMockDataSource(1, 100)
const datasourceArray = [
  dataArray,
  // dataArray.slice(0, 5000),
  {toto: '0', toto_lb: 'toto 0', qty: 100, amt: 100, titi: 'titi 0', tutu: '1'},
  {toto: '0', toto_lb: 'toto 0', qty: 1, amt: 2, titi: 'titi 0', tutu: '1'},
  // [{toto: '1', toto_lb: 'toto 1', qty: 100, amt: 1000, titi: 'titi 0', tutu: '0'}, {toto: '12', toto_lb: 'toto 12', qty: 44, amt: 777, titi: 'titi 0',
  // tutu: '0'}]
]

const datasource = datasourceArray[0]

// const datasource = Observable.interval(2000).take(3)
//  .map(i => datasourceArray[i])
//  .do(data => console.log('data received', data))

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
      name: 'toto_lb',
      code: 'toto',
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
  columns: ['Titi'],
  rows: ['Toto', 'Tutu'],
  data: ['Quantity'],
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

ReactDOM.render(<App config={config} datasource={datasource} />, document.getElementById('root'))

if (process.env.REACT_APP_ORB_ENV === 'fps-test') {
const testCase = createScrollingTestCase(document.getElementsByClassName('.OrbGrid-data-cells')[0])
const testRunner = new TestRunner(testCase, 5)

let btn = document.createElement('button')
btn.innerText = 'Run FPS test'
document.body.prepend(btn)
btn.addEventListener('click', function (event) {
  if (testRunner.isRunning()) {
    testRunner.stop()
  } else {
    testRunner.start()
  }
})
}
