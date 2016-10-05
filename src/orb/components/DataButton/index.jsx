

import React, { Component } from 'react'

export default class DataButton extends Component {

  constructor (props) {
    super(props)
    this.onClick = this.onClick.bind(this)
  }

  onClick () {
    const {field, store} = this.props
    store.toggleDataField(field.name)
  }

  render () {
    const {active, field} = this.props
    const fieldAggFunc = <small>{' (' + field.aggregateFuncName + ')'}</small>
    const inactiveStyle = {
      backgroundColor: '#cccccc',
      borderRadius: 4,
      padding: 4,
      cursor: 'default'
    }
    const activeStyle = {
      border: 'solid #cccccc 1px',
      borderRadius: 4,
      padding: 4,
      cursor: 'default'
    }
    return (
      <div style={active ? activeStyle : inactiveStyle} onClick={this.onClick}>
        {field.caption}
        {fieldAggFunc}
      </div>
    )
  }
}
