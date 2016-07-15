import React, { Component } from 'react'
import ReactDOM from 'react-dom'

// CSS files
import 'react-virtualized/styles.css'
import 'react-resizable/css/styles.css'

import Configuration from '../Configuration'
import Grid from '../Grid'
import Store from '../../stores/Store'

import DragManager from '../../DragManager'
import * as domUtils from '../../Utils.dom'

let pivotId = 1
// const themeChangeCallbacks = {}

// Do not use the .less files because the compilation is too complicated (cf gulpactions/buildcss.js)
// require('../../../dist/orb.css')
// require('../../deps/bootstrap-3.3.1/css/bootstrap.css')

export default class Main extends Component {

  componentWillReceiveProps (newProps) {
    console.log('main received props', newProps)
    const store = new Store(newProps.config, this.forceUpdate.bind(this))
    store.subscribe(newProps.datasource)
    this.setState({store})
  }

  constructor (props) {
    super(props)
    this.id = pivotId++
    DragManager.init(this.moveButton.bind(this))

    const store = new Store(props.config, this.forceUpdate.bind(this))
    store.subscribe(props.datasource)
    this.state = {store}
  }

  sort (axetype, field) {
    this.state.store.sort(axetype, field)
  }

  moveButton (button, newAxeType, position) {
    this.state.store.moveField(button.props.field.name, button.props.axetype, newAxeType, position)
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

  render () {
    const {store} = this.state
    console.log(store)
    return (
      <div>
        <div className={'orb'}>
          <Configuration store={store} />
          <Grid store={store} />
          <div className='orb-overlay orb-overlay-hidden' id={'drilldialog' + this.id}></div>
        </div>
      </div>
    )
  }
}
