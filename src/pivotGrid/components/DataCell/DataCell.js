import React, { PureComponent } from 'react';
import classnames from 'classnames';

export default class DataCell extends PureComponent {
  constructor() {
    super();
    this.handleDoubleClick = this.handleDoubleClick.bind(this);
    this.handleMouseDown = this.handleMouseDown.bind(this);
    this.handleMouseOver = this.handleMouseOver.bind(this);
  }

  shouldComponentUpdate(newProps) {
    return newProps.cell.caption !== this.props.cell.caption ||
      newProps.valueHasChanged ||
      newProps.selected !== this.props.selected;
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

    const className = classnames('pivotgrid-cell', 'pivotgrid-data-cell', {
      'pivotgrid-data-cell-even': !(rowIndex % 2),
      'pivotgrid-data-cell-uneven': rowIndex % 2,
      'pivotgrid-data-cell-highlighted': valueHasChanged,
      'pivotgrid-data-cell-selected': selected
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
