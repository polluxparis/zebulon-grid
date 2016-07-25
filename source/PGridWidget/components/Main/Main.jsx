import React, { Component } from 'react'
import ReactDOM from 'react-dom'
import {Card, CardHeader, CardText} from 'material-ui/Card'
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider'
var injectTapEventPlugin = require('react-tap-event-plugin')
injectTapEventPlugin()

// CSS files
import 'react-virtualized/styles.css'
import 'react-resizable/css/styles.css'

import Configuration from '../Configuration'
import Grid from '../Grid'
import Store from '../../stores/Store'

import * as domUtils from '../../Utils.dom'

let pivotId = 1
// const themeChangeCallbacks = {}

// Do not use the .less files because the compilation is too complicated (cf gulpactions/buildcss.js)
// require('../../../dist/orb.css')
// require('../../deps/bootstrap-3.3.1/css/bootstrap.css')

export default class Main extends Component {

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

  componentDidMount () {
    const fontInfos = domUtils.getStyle(ReactDOM.findDOMNode(this), ['font-family', 'font-size'], true)
    this.fontStyle = {
      fontFamily: fontInfos[0],
      fontSize: fontInfos[1]
    }
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
