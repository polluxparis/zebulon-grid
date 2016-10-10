import React, {Component} from 'react'

export default class DataCellComp extends Component {
  render () {
    const {cell, onDoubleClick} = this.props
    this._latestVisibleState = false

    this._latestVisibleState = cell.visible()

    const divcontent = [<div key='cell-value' ref='cellContent' className='cell-data'>{cell.caption}</div>]

    return (
      <div
        className='OrbGrid-data-cell'
        style={{ width: '100%', height: '100%', textAlign: 'right' }}
        onDoubleClick={onDoubleClick}
      >
        {divcontent}
      </div>
         )
  }

}
