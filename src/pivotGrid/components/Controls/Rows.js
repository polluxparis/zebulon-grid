import React, { Component } from "react";
import { ScrollableGrid } from "./ScrollableGrid";
import classnames from "classnames";
import { Input } from "./Input";
import { isInRange, dateToString, isDate } from "../../utils/generic";
// import { dateToString, stringToDate } from "../../utils/generic";
export const cell = (
  row,
  column,
  updatedRows,
  style,
  focused,
  selected,
  onClick,
  onMouseOver,
  onChange,
  onDoubleClick,
  onFocus,
  rowIndex
) => {
  const editable =
    focused &&
    (typeof (column.editable || false) === "function"
      ? column.editable(row, updatedRows[row.index_] || {})
      : column.editable);
  const className = classnames({
    "zebulon-table-cell": true,
    "zebulon-table-cell-selected": selected,
    "zebulon-table-cell-focused": focused,
    "zebulon-table-cell-editable": editable
  });
  let value = (column.formatFunction || (x => x))(
    column.accessor
      ? column.accessor(row, updatedRows[row.index_] || {})
      : row[column.id]
  );
  if (!editable && column.dataType === "date" && isDate(value)) {
    value = dateToString(value);
  }
  return editable ? (
    <Input
      style={style}
      className={className}
      id={column.id}
      value={value}
      dataType={column.dataType || "string"}
      format={column.format}
      editable={true}
      focused={focused}
      select={column.select}
      key={`cell-${row.index_}-${column.id}`}
      onChange={e => onChange(e, row, column)}
      row={row}
      // onFocus={this.props.onFocus}
      onFocus={e => onFocus(e, row, column, rowIndex)}
    />
  ) : (
    <div
      key={`cell-${row.index_}-${column.id}`}
      className={className}
      style={style}
      onClick={onClick}
      onDoubleClick={e => onDoubleClick(e, row, column)}
      onMouseOver={onMouseOver}
    >
      {value}
    </div>
  );
};

export class Rows extends ScrollableGrid {
  getRatios = () => {
    const { height, width, meta, rowHeight, scroll, data } = this.props;
    const lastColumn = meta[meta.length - 1];
    const columnsWidth =
      lastColumn.position + (lastColumn.hidden ? 0 : lastColumn.width || 0);
    return {
      vertical: {
        display: height / (data.length * rowHeight),
        position: scroll.rows.index / data.length
      },
      horizontal: {
        display:
          (width - (data.length * rowHeight > height ? 12 : 0)) / columnsWidth,
        position: scroll.columns.position / columnsWidth
      }
    };
  };
  onFocus = (e, row, column) => {
    let label;
    if (row.tp === "accessor") {
      label = "Parameters: (row)";
    } else if (row.tp === "format") {
      label = "Parameters: (value)";
    } else if (row.tp === "aggregation") {
      label = "Parameters: ([values])";
    } else if (row.tp === "sort") {
      label = "Parameters: (rowA, rowB)";
    }

    const text = {
      top:
        (0 + this.range.end.rows) * this.rowHeight +
        this.state.scroll.rows.shift,
      left:
        column.position + this.rowHeight - this.state.scroll.columns.position,
      v: (column.accessorFunction || (row => row[column.id]))(row),
      label,
      editable: column.editable,
      row,
      column
    };
    this.setState({ text });
  };
  rowRenderer = (
    row,
    updatedRows,
    meta,
    startIndex,
    shift,
    visibleWidth,
    rowHeight,
    rowIndex
    // selectedCell,
    // selectCell
  ) => {
    const cells = [];
    let left = shift,
      index = startIndex;
    while (index < meta.length && left < visibleWidth) {
      const column = meta[index];
      if (!column.hidden) {
        const columnIndex = index;
        const selectedRange = this.selectedRange();
        const selected = isInRange(
            {
              columns: index,
              rows: rowIndex
            },
            selectedRange.start,
            selectedRange.end
          ),
          focused =
            selectedRange.end.rows === rowIndex &&
            selectedRange.end.columns === index,
          onClick = e => {
            e.preventDefault();
            this.selectCell(
              { rows: rowIndex, columns: columnIndex },
              e.shiftKey
            );
          },
          onMouseOver = e => {
            e.preventDefault();
            if (e.buttons === 1) {
              this.selectCell({ rows: rowIndex, columns: columnIndex }, true);
            }
          },
          onFocus = column.dataType === "text" ? this.props.onFocus : () => {};
        cells.push(
          cell(
            row,
            column,
            updatedRows,
            {
              position: "absolute",
              left,
              width: column.width || 0,
              height: rowHeight,
              textAlign:
                column.alignement ||
                (column.dataType === "number"
                  ? "right"
                  : column.dataType === "number" ? "center" : "left")
            },
            focused,
            selected,
            onClick,
            onMouseOver,
            this.props.onChange,
            () => {}, //onDoubleClick,
            onFocus,
            rowIndex
          )
        );
        left += column.width || 0;
      }
      index += 1;
    }
    return (
      <div key={rowIndex} style={{ display: "flex", height: rowHeight }}>
        {cells}
      </div>
    );
  };

  getContent = () => {
    const items = [];
    let i = 0,
      index = this.props.scroll.rows.startIndex;
    const {
      data,
      meta,
      height,
      rowHeight,
      width,
      selectedCell,
      selectCell,
      updatedRows
    } = this.props;
    const visibleWidth = width - this.scrollbars.vertical.width;
    while (index < data.length && i < height / rowHeight) {
      items.push(
        this.rowRenderer(
          data[index],
          updatedRows,
          meta,
          this.props.scroll.columns.startIndex,
          this.props.scroll.columns.shift,
          visibleWidth,
          rowHeight,
          index,
          selectedCell,
          selectCell
        )
      );
      index++;
      i++;
    }
    return (
      <div
        style={{
          position: "absolute",
          top: this.props.scroll.rows.shift
        }}
        onWheel={this.onWheel}
      >
        {items}
      </div>
    );
  };
}
