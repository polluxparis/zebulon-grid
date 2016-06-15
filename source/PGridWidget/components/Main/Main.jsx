import React, { Component } from 'react'
import ReactDOM from 'react-dom'

// CSS files
import 'react-virtualized/styles.css'
import 'react-resizable/css/styles.css'

import UpperButtons from '../UpperButtons'
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

  constructor (props) {
    super(props)
    this.id = pivotId++
    DragManager.init(this)

    this.store = new Store(this, props.config)
  }

  sort (axetype, field) {
    this.store.sort(axetype, field)
  }

  moveButton (button, newAxeType, position) {
    this.store.moveField(button.props.field.name, button.props.axetype, newAxeType, position)
  }

  toggleSubtotals (axetype) {
    this.store.toggleSubtotals(axetype)
  }

  toggleGrandtotal (axetype) {
    this.store.toggleGrandtotal(axetype)
  }

  componentDidMount () {
    const fontInfos = domUtils.getStyle(ReactDOM.findDOMNode(this), ['font-family', 'font-size'], true)
    this.fontStyle = {
      fontFamily: fontInfos[0],
      fontSize: fontInfos[1]
    }
  }

  render () {
    console.log(this.store)
    return (
      <div>
        <div className={'orb'}>
          <UpperButtons
            store={this.store}
          />
          <div style={{width: this.store.layout.pivotTable.width, height: this.store.layout.pivotTable.height}}>
            <Grid store={this.store} />
          </div>
          <div className='orb-overlay orb-overlay-hidden' id={'drilldialog' + this.id}></div>
        </div>
      </div>
    )
  }
}
