import React, { Component } from 'react'
import ReactDOM from 'react-dom'
import {Card, CardHeader, CardText} from 'material-ui/Card'
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider'
var injectTapEventPlugin = require('react-tap-event-plugin')
injectTapEventPlugin()
import { Observable } from 'rx-lite'

// CSS files
import 'react-virtualized/styles.css'
import 'react-resizable/css/styles.css'

import {Configuration, Grid, Store} from '../index'

let pivotId = 1
// const themeChangeCallbacks = {}

// Do not use the .less files because the compilation is too complicated (cf gulpactions/buildcss.js)
// require('../../../dist/orb.css')
// require('../../deps/bootstrap-3.3.1/css/bootstrap.css')

function getMockDataSource (dataRepetition, nToto) {
  const nTiti = 100
  const nTutu = 2
  var arr = []
  var res = []
  for (var k = 0; k < dataRepetition; k++) {
    for (var o = 0; o < nToto; o++) {
      for (var i = 0; i < nTiti; i++) {
        for (var u = 0; u < nTutu; u++) {
          arr = []
          arr[0] = 'toto' + String(o)
          arr[3] = 'titi' + String(i)
          arr[4] = String(u)
          arr[1] = k + 10 * u + 100 * i * 1000 * o + 1 // +9999999999.1234567890123456
          arr[2] = k + 10 * u + 100 * i * 1000 * o + 1 // +9999999999.1234567890123456
          res.push(arr)
        }
      }
    }
  }
  return res
}

const datasourceArray = [
  getMockDataSource(1, 100),
  ['toto11', 33, 666, 'titi0', 'tutu0'],
  ['toto0', 1, 10, 'titi0', 'tutu0'],
  [['toto1', 10, 100, 'titi0', 'tutu0'], ['toto12', 44, 777, 'titi0', 'tutu0']],
  ['toto2', 10, 100, 'titi0', 'tutu0']
]

const datasource = Observable.interval(0).take(1)
  .map(i => datasourceArray[i])
// .of(datasourceArray[0])

// const datasource = Observable.interval(2000).take(datasourceArray.length)

var config = {
  canMoveFields: true,
  dataHeadersLocation: 'columns',
  width: 1099,
  height: 601,
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
      name: '0',
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
      name: '3',
      caption: 'Titi'
    },
    {
      name: '4',
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
      name: '2',
      caption: 'Quantity',
      aggregateFunc: 'sum'
    },
    {
      name: '1',
      caption: 'Amount',
      aggregateFunc: 'sum',
      aggregateFuncName: 'whatever',
      formatFunc: (value) => value ? Number(value).toFixed(0) + ' $' : ''
    }
  ],
  columns: ['Titi', 'Tutu'], // , 'Category' ],
  rows: ['Toto'],
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

class Main extends Component {

  constructor (props) {
    super(props)
    this.id = pivotId++

    const store = new Store(props.config, this.forceUpdate.bind(this))
    store.subscribe(props.datasource)
    this.state = {store}

    this.onDrilldown = this.onDrilldown.bind(this)
  }

  componentWillReceiveProps (newProps) {
    console.log('main received props', newProps)
    const store = new Store(newProps.config, this.forceUpdate.bind(this))
    store.subscribe(newProps.datasource)
    this.setState({store})
  }

  sort (axetype, field) {
    this.state.store.sort(axetype, field)
  }

  toggleSubtotals (axetype) {
    this.state.store.toggleSubtotals(axetype)
  }

  toggleGrandtotal (axetype) {
    this.state.store.toggleGrandtotal(axetype)
  }

  onDrilldown (cell) {
    console.log('drilldown prop', cell)
  }

  render () {
    const {store} = this.state
    console.log(store)
    return (
      <MuiThemeProvider>
        <div>
          <Card>
            <CardHeader
              title='Configuration'
              expanded
              actAsExpander
              showExpandableButton
            />
            <CardText expandable>
              <Configuration store={store} />
            </CardText>
          </Card>
          <Card>
            <CardHeader
              title='Display'
              expanded
              actAsExpander
              showExpandableButton
            />
            <CardText expandable style={{height: 1000}}>
              <Grid store={store} drilldown={this.onDrilldown} />
            </CardText>
          </Card>
          <div className='orb-overlay orb-overlay-hidden' id={'drilldialog' + this.id}></div>
        </div>
      </MuiThemeProvider>
    )
  }
}

ReactDOM.render(<Main config={config} datasource={datasource} />, document.getElementById('root'))
