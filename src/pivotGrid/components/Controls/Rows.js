import React, { Component } from "react";
import { ScrollableGrid } from "./ScrollableGrid";
import classnames from "classnames";
import { Input } from "./Input";
import { isInRange } from "../../utils/generic";

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
  onFocus
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
  return editable ? (
    <Input
      style={style}
      className={className}
      code={column.id}
      value={
        column.accessor ? (
          column.accessor(row, updatedRows[row.index_] || {})
        ) : (
          row[column.id]
        )
      }
      dataType={column.dataType || "string"}
      format={column.format}
      editable={true}
      select={column.select}
      key={`cell-${row.index_}-${column.id}`}
      onChange={e => onChange(e, row, column)}
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
      {(column.format || (x => x))(
        column.accessor
          ? column.accessor(row, updatedRows[row.index_] || {})
          : row[column.id]
      )}
    </div>
  );
};

export class Rows extends ScrollableGrid {
  getRatios = () => {
    const { height, width, meta, rowHeight, scroll, data } = this.props;
    const lastColumn = meta[meta.length - 1];
    const columnsWidth = lastColumn.position + lastColumn.width;
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
          this.selectCell({ rows: rowIndex, columns: columnIndex }, e.shiftKey);
        },
        onMouseOver = e => {
          e.preventDefault();
          if (e.buttons === 1) {
            console.log(e.button, e.buttons, rowIndex, columnIndex);
            this.selectCell({ rows: rowIndex, columns: columnIndex }, true);
          }
        };

      cells.push(
        cell(
          row,
          column,
          updatedRows,
          {
            position: "absolute",
            left,
            width: column.width,
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
          () => {} //onFocus,
        )
      );
      left += column.width;
      index += 1;
    }
    return (
      <div key={rowIndex} style={{ display: "flex", height: rowHeight }}>
        {cells}
      </div>
    );
  };
  // onKeyDown = e => console.log("keydown", e);
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
  // onWheel = e => {
  //   e.preventDefault();
  //   const sense = e.altKey || e.deltaX !== 0 ? "columns" : "rows";
  //   const { height, rowHeight, width, data, meta, scroll } = this.props;
  //   let { shift, index, startIndex, position } = scroll[sense];
  //   let direction =
  //     sense === "columns" ? -Math.sign(e.deltaX) : -Math.sign(e.deltaY);
  //   const visibleHeight = height - this.scrollbars.horizontal.width;
  //   const nRows = Math.ceil(visibleHeight / rowHeight);

  //   if (sense === "rows") {
  //     if (nRows > data.length) {
  //       direction = 1;
  //     }
  //     shift = direction === 1 ? 0 : visibleHeight - nRows * rowHeight;
  //     if (direction === -1) {
  //       startIndex = Math.max(
  //         Math.min(
  //           scroll.rows.startIndex + (scroll[sense].direction === direction),
  //           data.length - nRows
  //         ),
  //         0
  //       );
  //       index = startIndex + nRows - 1;
  //     } else {
  //       index = Math.max(
  //         scroll.rows.startIndex - (scroll[sense].direction === direction),
  //         0
  //       );
  //       startIndex = index;
  //     }
  //   } else {
  //     direction = 1;
  //     const lastColumn = meta[meta.length - 1];
  //     position = Math.min(
  //       Math.max(position + e.deltaX, 0),
  //       lastColumn.position + lastColumn.width - width
  //     );
  //     const column = meta.find(
  //       column =>
  //         position >= column.position &&
  //         position <= column.position + column.width
  //     );
  //     shift = column.position - position;
  //     index = column.index;
  //     startIndex = index;
  //   }
  //   scroll[sense] = { index, direction, startIndex, shift, position };
  //   this.props.onScroll(scroll);
  // };
}
