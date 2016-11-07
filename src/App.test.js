import React from 'react'
import ReactDOM from 'react-dom'
import App from './App'

console.log = function () {}

it('renders without crashing', () => {
  function getMockDataSource (dataRepetition, nToto) {
    const nTiti = 10
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

  const datasource = getMockDataSource(1, 15)
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
      {
        name: 'titi',
        caption: 'Titi'
      },
      {
        name: 'tutu',
        caption: 'Tutu'
      }
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
    drilldown: (cell) => console.log('drilldown (config) on cell', cell)
  }

  const div = document.createElement('div')
  ReactDOM.render(<App config={config} datasource={datasource} />, div)
})
