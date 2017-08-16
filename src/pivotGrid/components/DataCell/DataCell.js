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
  // handleClickMenu = (e, data, target) => {
  //   if (e.button === 0) {
  //     if (data.action === 'drilldown') {
  //       this.handleDoubleClick();
  //     }
  //   }
  // };
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

    const className = classnames('pivotgrid-cell', 'pivotgrid-data-cell', {
      'pivotgrid-data-cell-even': !(rowIndex % 2),
      'pivotgrid-data-cell-uneven': rowIndex % 2,
      'pivotgrid-data-cell-highlighted': valueHasChanged,
      'pivotgrid-data-cell-selected': selected,
      'pivotgrid-data-cell-focused': focused
    });

    return (
      <ContextMenuTrigger
        id={`context-menu-data-cell-${gridId}`}
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
