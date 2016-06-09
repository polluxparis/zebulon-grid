import { Component } from 'react'
import ReactDOM from 'react-dom'
import DevTools from 'mobx-react-devtools'

// CSS files
import 'react-virtualized/styles.css'
import 'react-resizable/css/styles.css'

import UpperButtons from '../UpperButtons'
import Grid from '../Grid'
import Store from '../../stores/Store'

import DragManager from '../../DragManager'
import AxeUi from '../../AxeUi'
import * as domUtils from '../../Utils.dom'

let pivotId = 1
// const themeChangeCallbacks = {}

// Do not use the .less files because the compilation is too complicated (cf gulpactions/buildcss.js)
// require('../../../dist/orb.css')
// require('../../deps/bootstrap-3.3.1/css/bootstrap.css')

export default class Main extends Component {

  constructor ({config}) {
    super({config})
    this.id = pivotId++
    DragManager.init(this)

    // themeChangeCallbacks[this.id] = []
    // this.registerThemeChanged(this.updateClasses)

    this.store = new Store(config)
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
  //   if (this.pgridwidgetstore.pgrid.config.setTheme(newTheme)) {
  //     // notify self/sub-componenjs of the theme change
  //     for (let i = 0; i < themeChangeCallbacks[this.id].length; i++) {
  //       themeChangeCallbacks[this.id][i]()
  //     }
  //   }
  // }
  // updateClasses () {
    // const thisnode = ReactDOM.findDOMNode(this)
    // const classes = this.pgridwidgetstore.pgrid.config.theme.getPivotClasses()
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

  shouldComponentUpdate (nextProps, nextState) {
    console.log('shouldComponentUpdate')
  // return shallowCompare(this, nextProps, nextState)
  }

  buildUi () {
    const rows = new AxeUi(this.store.rows)
    const columns = new AxeUi(this.store.columns)
    const layout = {
      rowHeaders: {
        width: (this.store.rows.fields.length || 1) +
          (this.store.config.dataHeadersLocation === 'rows' && this.store.config.dataFieldsCount > 1 ? 1 : 0),
        height: this.rows.headers.length
      },
      columnHeaders: {
        width: this.columns.headers.length,
        height: (this.store.columns.fields.length || 1) +
          (this.store.config.dataHeadersLocation === 'columns' && this.store.config.dataFieldsCount > 1 ? 1 : 0)
      },
      pivotTable: {
        width: this.layout.rowHeaders.width + this.layout.columnHeaders.width,
        height: this.layout.rowHeaders.height + this.layout.columnHeaders.height
      }
    }
    return {rows, columns, layout}
  }

  render () {
    const {columns, rows, layout} = this.buildUi()
    return (
      <div>
        <DevTools />
        <div className={'orb'}>
          <UpperButtons store={this.store} />
          <div style={{width: layout.pivotTable.width, height: layout.pivotTable.height}}>
            <Grid columns={columns} rows={rows} layout={layout} />
          </div>
          <div className='orb-overlay orb-overlay-hidden' id={'drilldialog' + this.id}></div>
        </div>
      </div>
    )
  }
}
