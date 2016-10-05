import React, {Component} from 'react'

export default class DataCellComp extends Component {
  render () {
    const {cell, onDoubleClick} = this.props
    this._latestVisibleState = false

    this._latestVisibleState = cell.visible()

    const value = (cell.datafield && cell.datafield.formatFunc) ? cell.datafield.formatFunc()(cell.value) : cell.value

    const divcontent = [<div key='cell-value' ref='cellContent' className='cell-data'>{value}</div>]

    return (
      <div
        style={{ width: '100%', height: '100%', textAlign: 'right' }}
        onDoubleClick={onDoubleClick}
      >
        {divcontent}
      </div>
         )
  }

}
