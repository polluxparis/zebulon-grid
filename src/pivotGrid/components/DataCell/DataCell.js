import React, { PureComponent } from 'react';
import classnames from 'classnames';
import { ContextMenuTrigger } from 'react-contextmenu';

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
      columnIndex,
      selected,
      focused,
      valueHasChanged,
      collectMenu,
      gridId
    } = this.props;
    const style = {
      boxSizing: 'border-box',
      overflow: 'hidden'
    };

    const className = classnames(
      'zebulon-grid-cell',
      'zebulon-grid-data-cell',
      {
        'zebulon-grid-data-cell-even': !(rowIndex % 2),
        'zebulon-grid-data-cell-uneven': rowIndex % 2,
        'zebulon-grid-data-cell-highlighted': valueHasChanged,
        'zebulon-grid-data-cell-selected': selected,
        'zebulon-grid-data-cell-focused': focused
      }
    );

    return (
      <ContextMenuTrigger
        id={`context-menu-${gridId}`}
        holdToDisplay={-1}
        collect={collectMenu}
        onItemClick={this.props.handleClickMenu}
        type={'data-cell'}
        style={{ width: 'inherit' }}
        rowIndex={rowIndex}
        columnIndex={columnIndex}
      >
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
      </ContextMenuTrigger>
    );
  }
}
