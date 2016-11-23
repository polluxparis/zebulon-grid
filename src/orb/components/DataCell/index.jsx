import React, { PureComponent } from 'react';

export default class DataCell extends PureComponent {

  constructor() {
    super();
    this.handleDoubleClick = this.handleDoubleClick.bind(this);
    this.handleMouseDown = this.handleMouseDown.bind(this);
    this.handleMouseOver = this.handleMouseOver.bind(this);
  }

  handleMouseDown(e) {
    this.props.handleMouseDown(e, [this.props.columnIndex, this.props.rowIndex]);
  }

  handleMouseOver() {
    this.props.handleMouseOver([this.props.columnIndex, this.props.rowIndex]);
  }

  handleDoubleClick() {
    this.props.drilldown(this.props.cell);
  }

  render() {
    const { cell, position, rowIndex, selected, valueHasChanged } = this.props;
    let className = 'OrbGrid-cell OrbGrid-data-cell';
    if (valueHasChanged) {
      className += ' OrbGrid-cell-highlighted';
    }
    let style = {
      border: 'solid lightgrey thin',
      boxSizing: 'border-box',
      overflow: 'hidden',
    };
    const unEvenRowStyle = { backgroundColor: 'rgba(211, 211, 211, 0.4)' };
    const evenRowStyle = { backgroundColor: 'white' };

    if (rowIndex % 2) {
      style = { ...style, ...unEvenRowStyle };
    } else {
      style = { ...style, ...evenRowStyle };
    }

    const selectedStyle = { backgroundColor: 'lightcyan' };

    if (selected) {
      style = { ...style, ...selectedStyle };
    }


    return (
      <div
        className={className}
        style={{ ...style, ...position }}
        onMouseDown={this.handleMouseDown}
        onMouseOver={this.handleMouseOver}
        onDoubleClick={this.handleDoubleClick}
      >
        {cell.caption}
      </div>
    );
  }

}
