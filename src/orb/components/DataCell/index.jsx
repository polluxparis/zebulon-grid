import React, {Component} from 'react'

export default class DataCell extends Component {

  constructor () {
    super()
    this.handleDoubleClick = this.handleDoubleClick.bind(this)
    this.handleMouseDown = this.handleMouseDown.bind(this)
    this.handleMouseOver = this.handleMouseOver.bind(this)
  }

  handleMouseDown (e) {
    this.props.handleMouseDown(e, [this.props.index[0], this.props.index[1]])
  }

  handleMouseOver () {
    this.props.handleMouseOver([this.props.index[0], this.props.index[1]])
  }

  handleDoubleClick () {
    this.props.drilldown(this.props.cell)
  }

  render () {
    const {cell, style, valueHasChanged} = this.props
    let className = 'OrbGrid-cell OrbGrid-data-cell'
    if (valueHasChanged) {
      className += ' OrbGrid-cell-highlighted'
    }
    return (
      <div
        className={className}
        style={style}
        onMouseDown={this.handleMouseDown}
        onMouseOver={this.handleMouseOver}
        onDoubleClick={this.handleDoubleClick}
      >
        {cell.caption}
      </div>
         )
  }

}
