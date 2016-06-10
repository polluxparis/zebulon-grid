import React, { Component } from 'react'
import * as ReactDOM from 'react-dom'
import { ResizableBox } from 'react-resizable'
import VirtualizedCheckbox from 'react-virtualized-checkbox'

import * as utils from '../../Utils'

export default class FilterPanelComponent extends Component {

  constructor (props) {
    super(props)
    console.log(props)
    this.startingHeight = 223
    this.startingWidth = 301

    this.onFilter = this.onFilter.bind(this)
    this.onMouseDown = this.onMouseDown.bind(this)
    this.toggleCheckbox = this.toggleCheckbox.bind(this)
  }

  destroy () {
    const container = ReactDOM.findDOMNode(this).parentNode
    ReactDOM.unmountComponentAtNode(container)
    container.parentNode.removeChild(container)
  }

  onFilter (operator, term, staticValue, excludeStatic) {
    console.log('onFilter')
    const {store} = this.props
    store.applyFilter(store.field, operator, term, staticValue, excludeStatic)
    this.destroy()
  }

  onMouseDown (e) {
    const container = ReactDOM.findDOMNode(this).parentNode
    let target = e.target || e.srcElement
    while (target !== null) {
      if (target === container) {
        return true
      }
      target = target.parentNode
    }

    this.destroy()
  }

  componentWillMount () {
    utils.addEventListener(document, 'mousedown', this.onMouseDown)
    // utils.addEventListener(document, 'wheel', this.onMouseWheel)
    utils.addEventListener(window, 'resize', this.destroy)
  }

  componentDidMount () {
    // this.filterManager.init(ReactDOM.findDOMNode(this))
  }

  componentWillUnmount () {
    utils.removeEventListener(document, 'mousedown', this.onMouseDown)
    // utils.removeEventListener(document, 'wheel', this.onMouseWheel)
    utils.removeEventListener(window, 'resize', this.destroy)
  }

  toggleCheckbox (value) {
    console.log(`toggling ${value.value}`)
  }

  getVisibleCheckboxes () {
    // this.checkboxes.filter(checkbox => )
  }

  render () {
    // previousFilter={store.getFieldFilter(field)}
    // values={store.getFieldValues(field)}
    // applyFilter={store.refresh(true, axetype)}

    const {store, field} = this.props
    const values = store.getFieldValues(field.name)
    this.checkboxes = values.map(val => ({checked: true, label: val}))

    const checkboxes =
      <VirtualizedCheckbox
        options={this.checkboxes}
        onOk={(result) => this.onFilter('', '', result, false)}
        onCancel={() => this.destroy()}
        maxHeight={this.startingHeight} />

    const divStyle = {
      backgroundColor: 'white',
      border: 'solid 1px',
      boxShadow: '0 5px 15px #9d9d9d',
      display: 'flex',
      flexDirection: 'column',
      fontSize: '90%',
      height: '100%',
      justifyContent: 'space-between',
      padding: '3px',
      width: '100%'
    }

    return (
      <ResizableBox width={this.startingWidth} height={this.startingHeight} minConstraints={[this.startingWidth, this.startingHeight]}>
        <div style={divStyle}>
          {checkboxes}
        </div>
      </ResizableBox>
    )
  }
}
