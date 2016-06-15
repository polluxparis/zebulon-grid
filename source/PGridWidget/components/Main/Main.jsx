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

    // themeChangeCallbacks[this.id] = []
    // this.registerThemeChanged(this.updateClasses)

    this.store = new Store(props.config)
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
  applyFilter (fieldname, operator, term, staticValue, excludeStatic) {
    this.store.applyFilter(fieldname, operator, term, staticValue, excludeStatic)
  }
  // registerThemeChanged (compCallback) {
  //   if (compCallback) {
  //     themeChangeCallbacks[this.id].push(compCallback)
  //   }
  // }
  // unregisterThemeChanged (compCallback) {
  //   let i
  //   if (compCallback && (i = themeChangeCallbacks[this.id].indexOf(compCallback)) >= 0) {
  //     themeChangeCallbacks[this.id].splice(i, 1)
  //   }
  // }
  // changeTheme (newTheme) {
  //   if (this.store.config.setTheme(newTheme)) {
  //     // notify self/sub-components of the theme change
  //     for (let i = 0; i < themeChangeCallbacks[this.id].length; i++) {
  //       themeChangeCallbacks[this.id][i]()
  //     }
  //   }
  // }
  // updateClasses () {
    // const thisnode = ReactDOM.findDOMNode(this)
    // const classes = this.store.config.theme.getPivotClasses()
    // thisnode.className = classes.container
    // thisnode.children[1].className = classes.table
  // }

  componentDidMount () {
    const fontInfos = domUtils.getStyle(ReactDOM.findDOMNode(this), ['font-family', 'font-size'], true)
    this.fontStyle = {
      fontFamily: fontInfos[0],
      fontSize: fontInfos[1]
    }
  }

  // shouldComponentUpdate (nextProps, nextState) {
  //   return shallowCompare(this, nextProps, nextState)
  // }

  // buildUi () {
    // const rows = new AxeUi(this.store.rows)
    // const columns = new AxeUi(this.store.columns)
    // const rowHeaders = {
    //   width: (this.store.rows.fields.length || 1) +
    //     (this.store.config.dataHeadersLocation === 'rows' && this.store.config.dataFieldsCount > 1 ? 1 : 0),
    //   height: rows.headers.length
    // }
    // const columnHeaders = {
    //   width: columns.headers.length,
    //   height: (this.store.columns.fields.length || 1) +
    //     (this.store.config.dataHeadersLocation === 'columns' && this.store.config.dataFieldsCount > 1 ? 1 : 0)
    // }
    // const pivotTable = {
    //   width: rowHeaders.width + columnHeaders.width,
    //   height: rowHeaders.height + columnHeaders.height
    // }
    // const layout = {columnHeaders, rowHeaders, pivotTable}
    // const cell = {
    //   height: 30,
    //   width: 100
    // }
    // const grid = {
    //   width: this.store.config.width,
    //   height: this.store.config.height
    // }
    // const sizes = {cell, grid}
    // return {rows, columns, layout, sizes}
  // }

  render () {
    // const {columns, rows, layout, sizes} = this.buildUi()
    console.log(this.store)
    return (
      <div>
        <div className={'orb'}>
          <UpperButtons store={this.store} />
          <div style={{width: this.store.layout.pivotTable.width, height: this.store.layout.pivotTable.height}}>
            <Grid store={this.store} />
          </div>
          <div className='orb-overlay orb-overlay-hidden' id={'drilldialog' + this.id}></div>
        </div>
      </div>
    )
  }
}
