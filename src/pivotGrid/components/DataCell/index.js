import React, { PureComponent } from 'react';
import classnames from 'classnames';

export default class DataCell extends PureComponent {
  constructor() {
    super();
    this.handleDoubleClick = this.handleDoubleClick.bind(this);
    this.handleMouseDown = this.handleMouseDown.bind(this);
    this.handleMouseOver = this.handleMouseOver.bind(this);
  }

  handleMouseDown(e) {
    this.props.handleMouseDown(e, [
      this.props.columnIndex,
      this.props.rowIndex
    ]);
  }

  handleMouseOver() {
    this.props.handleMouseOver([this.props.columnIndex, this.props.rowIndex]);
  }

  handleDoubleClick() {
    this.props.drilldown(this.props.cell);
  }

  render() {
    const { cell, rowIndex, selected, valueHasChanged } = this.props;
    const style = {
      boxSizing: 'border-box',
      overflow: 'hidden'
    };

    const className = classnames('orb-cell', 'orb-data-cell', {
      'orb-data-cell-even': !(rowIndex % 2),
      'orb-data-cell-uneven': rowIndex % 2,
      'orb-data-cell-highlighted': valueHasChanged,
      'orb-data-cell-selected': selected
    });

    return (
      <div
        className={className}
        style={{ ...style, ...this.props.style }}
        onMouseDown={this.handleMouseDown}
        onMouseOver={this.handleMouseOver}
        onDoubleClick={this.handleDoubleClick}
      >
        {cell.caption}
      </div>
    );
  }
}
