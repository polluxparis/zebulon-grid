import React, { PureComponent } from 'react';
import classnames from 'classnames';

export default class DataCell extends PureComponent {
  handleMouseDown = e => {
    this.props.handleMouseDown(e, {
      columnIndex: this.props.columnIndex,
      rowIndex: this.props.rowIndex
    });
  };
  handleMouseOver = e => {
    this.props.handleMouseOver(e, {
      columnIndex: this.props.columnIndex,
      rowIndex: this.props.rowIndex
    });
  };
  handleDoubleClick = () => {
    this.props.drilldown({
      columnIndex: this.props.columnIndex,
      rowIndex: this.props.rowIndex
    });
  };

  render() {
    const {
      caption,
      rowIndex,
      selected,
      focused,
      valueHasChanged
    } = this.props;
    const style = {
      boxSizing: 'border-box',
      overflow: 'hidden'
    };

    const className = classnames('pivotgrid-cell', 'pivotgrid-data-cell', {
      'pivotgrid-data-cell-even': !(rowIndex % 2),
      'pivotgrid-data-cell-uneven': rowIndex % 2,
      'pivotgrid-data-cell-highlighted': valueHasChanged,
      'pivotgrid-data-cell-selected': selected,
      'pivotgrid-data-cell-focused': focused
    });

    return (
      <div
        className={className}
        style={{ ...style, ...this.props.style }}
        onMouseDown={this.handleMouseDown}
        onMouseOver={this.handleMouseOver}
        onMouseUp={this.props.handleMouseUp}
        onDoubleClick={this.handleDoubleClick}
      >
        {caption}
      </div>
    );
  }
}
