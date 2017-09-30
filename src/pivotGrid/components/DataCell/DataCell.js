import React, { PureComponent } from "react";
import classnames from "classnames";
import { ContextMenuTrigger } from "react-contextmenu";

export default class DataCell extends PureComponent {
  collect = e => {
    const { button, shiftKey } = e;
    return {
      type: "cell",
      button,
      shiftKey,
      columnIndex: this.props.columnIndex,
      rowIndex: this.props.rowIndex
    };
  };
  handleMouseDown = e => this.props.handleMouseDown(this.collect(e));
  handleMouseOver = e => this.props.handleMouseOver(this.collect(e));
  handleMouseUp = e => this.props.handleMouseUp(this.collect(e));
  handleDoubleClick = e => this.props.handleDoubleClick(this.collect(e));
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
      boxSizing: "border-box",
      overflow: "hidden",
      padding: "02.em"
    };

    const className = classnames(
      "zebulon-grid-cell",
      "zebulon-grid-data-cell",
      {
        "zebulon-grid-data-cell-even": !(rowIndex % 2),
        "zebulon-grid-data-cell-uneven": rowIndex % 2,
        "zebulon-grid-data-cell-highlighted": valueHasChanged,
        "zebulon-grid-data-cell-selected": selected,
        "zebulon-grid-data-cell-focused": focused
      }
    );
    // console.log("render cell");
    return (
      <ContextMenuTrigger
        id={`context-menu-${gridId}`}
        holdToDisplay={-1}
        collect={collectMenu}
        onItemClick={this.props.handleClickMenu}
        type={"data-cell"}
        style={{ width: "inherit" }}
        rowIndex={rowIndex}
        columnIndex={columnIndex}
      >
        <div
          className={className}
          id={`${rowIndex} - ${columnIndex}`}
          style={{ ...style, ...this.props.style }}
          onMouseDown={this.handleMouseDown}
          onMouseUp={this.props.handleMouseUp}
          onDoubleClick={this.handleDoubleClick}
          onMouseOver={this.handleMouseOver}
        >
          {caption}
        </div>
      </ContextMenuTrigger>
    );
  }
}
// onMouseOver={this.handleMouseOver}
