import { Component } from 'react'
import * as ReactDOM from 'react-dom'
import DragManager from './orb.react.DragManager'
import UpperButtons from './orb.react.PivotTable.UpperButtons'
import * as domUtils from '../orb.utils.dom'

import { OrbGrid } from './orb.grid'

import DevTools from 'mobx-react-devtools'

let pivotId = 1
const themeChangeCallbacks = {}

// CSS files
import 'react-virtualized/styles.css'
import '../../css/react-dropdown.css'
import 'react-resizable/css/styles.css'

// Do not use the .less files because the compilation is too complicated (cf gulpactions/buildcss.js)
// require('../../../dist/orb.css')
// require('../../deps/bootstrap-3.3.1/css/bootstrap.css')

export class PivotTableComponent extends Component {

  constructor ({pgridwidgetstore}) {
    super({pgridwidgetstore})
    this.id = pivotId++
    DragManager.init(this)

    themeChangeCallbacks[this.id] = []
    this.registerThemeChanged(this.updateClasses)

    this.pgridwidgetstore = pgridwidgetstore
    this.pgrid = this.pgridwidgetstore.pgrid
  }

  sort (axetype, field) {
    this.pgridwidgetstore.sort(axetype, field)
  }
  moveButton (button, newAxeType, position) {
    this.pgridwidgetstore.moveField(button.props.field.name, button.props.axetype, newAxeType, position)
  }
  toggleSubtotals (axetype) {
    this.pgridwidgetstore.toggleSubtotals(axetype)
  }
  toggleGrandtotal (axetype) {
    this.pgridwidgetstore.toggleGrandtotal(axetype)
  }
  applyFilter (fieldname, operator, term, staticValue, excludeStatic) {
    this.pgridwidgetstore.applyFilter(fieldname, operator, term, staticValue, excludeStatic)
  }
  registerThemeChanged (compCallback) {
    if (compCallback) {
      themeChangeCallbacks[this.id].push(compCallback)
    }
  }
  unregisterThemeChanged (compCallback) {
    let i
    if (compCallback && (i = themeChangeCallbacks[this.id].indexOf(compCallback)) >= 0) {
      themeChangeCallbacks[this.id].splice(i, 1)
    }
  }
  changeTheme (newTheme) {
    if (this.pgridwidgetstore.pgrid.config.setTheme(newTheme)) {
      // notify self/sub-componenjs of the theme change
      for (let i = 0; i < themeChangeCallbacks[this.id].length; i++) {
        themeChangeCallbacks[this.id][i]()
      }
    }
  }
  updateClasses () {
    // const thisnode = ReactDOM.findDOMNode(this)
    // const classes = this.pgridwidgetstore.pgrid.config.theme.getPivotClasses()
    // thisnode.className = classes.container
    // thisnode.children[1].className = classes.table
  }

  componentDidMount () {
    const fontInfos = domUtils.getStyle(ReactDOM.findDOMNode(this), ['font-family', 'font-size'], true)
    this.fontStyle = {
      fontFamily: fontInfos[0],
      fontSize: fontInfos[1]
    }
  }

  shouldComponentUpdate (nextProps, nextState) {
    console.log('shouldComponentUpdate')
  // return shallowCompare(this, nextProps, nextState)
  }

  render () {
    const config = this.pgridwidgetstore.pgrid.config
    const classes = config.theme.getPivotClasses()

    let tblStyle = {height: undefined, width: undefined}
    if (config.width) { tblStyle.width = config.width }
    if (config.height) { tblStyle.height = config.height }

    return (
      <div className={classes.container}>
        <DevTools />
        <div className={'orb'}>
          <UpperButtons pivotTableComp={this} />
          <div style={tblStyle}>
            <OrbGrid pgridwidgetstore={this.props.pgridwidgetstore} />
          </div>
          <div className='orb-overlay orb-overlay-hidden' id={'drilldialog' + this.id}></div>
        </div>
      </div>
    )
  }
}
