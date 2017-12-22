import React, { Component } from "react";
import classnames from "classnames";
import { Input, InputInterval } from "./Input";
export const editCell = (
  style,
  className,
  index,
  row,
  onClick,
  onDoubleClick
) => {
  let glyph;
  if (row.deleted_) {
    glyph = "X";
  } else if (row.new_) {
    glyph = "+";
  } else if (row.updated_) {
    glyph = "√";
  } else {
    glyph = null;
  }
  return (
    <div
      key={index}
      className={className}
      style={style}
      onClick={() => onClick(index)}
      onDoubleClick={e => onDoubleClick(e, row)}
    >
      {glyph}
    </div>
  );
};
const filter = (column, shift, width, filterTo, onChange, openFilter) => {
  const className = "zebulon-table-header zebulon-table-filter";
  const textAlign =
    column.filterType === "values"
      ? "right"
      : column.alignement ||
        (column.dataType === "number"
          ? "right"
          : column.dataType === "number" ? "center" : "left");
  // if (column.filterType === "interval") {
  //   return (
  //     <InputInterval
  //       key={column.id}
  //       className={className}
  //       style={{
  //         position: "absolute",
  //         left: shift,
  //         width
  //       }}
  //       dataType={column.dataType}
  //       format={column.format || (value => value)}
  //       onChange={() => {}} //onChange}
  //     />
  //   );
  // } else {
  return (
    <Input
      key={column.id}
      id={column.index_}
      className={className}
      style={{
        position: "absolute",
        left: shift,
        width,
        textAlign
      }}
      // hasFocus={hasFocus}
      editable={true}
      dataType={column.dataType || "string"}
      format={column.format}
      inputType="filter"
      value={
        column.filterType === "values" ? column.v ? (
          "Ỵ"
        ) : (
          ""
        ) : filterTo ? (
          column.vTo
        ) : (
          column.v
        )
      }
      onChange={e => {
        onChange(e, column, filterTo);
      }}
      onFocus={e => openFilter(e, column)}
      isEditable={true}
    />
  );
  // }
};
const header = (column, shift, width, handleClick) => {
  let sort = "";
  if (column.sort === "asc") {
    sort = "↑";
  } else if (column.sort === "desc") {
    sort = "↓";
  }
  return (
    <div
      key={column.id}
      className="zebulon-table-header"
      onClick={() => handleClick(column, false)}
      onDoubleClick={() => handleClick(column, true)}
      style={{
        position: "absolute",
        left: shift,
        width,
        justifyContent: "space-between",
        display: "flex"
      }}
    >
      <div>{column.caption || column.id}</div>
      <div>{sort}</div>
    </div>
  );
};

const filterEmpty = (id, shift, width) => {
  return (
    <div
      key={id}
      className="zebulon-table-header zebulon-table-filter-empty"
      style={{
        position: "absolute",
        left: shift,
        width
      }}
    />
  );
};

// ↑↓
export class Headers extends Component {
  constructor(props) {
    super(props);
  }
  handleClick = (column, double) => {
    const { onSort } = this.props;
    if (!double) {
      this.timer = setTimeout(() => {
        if (!this.prevent) {
          onSort(column, false);
        }
        this.prevent = false;
      }, 200);
    } else {
      clearTimeout(this.timer);
      this.prevent = true;
      onSort(column, true);
    }
  };
  // onDoubleClick = () => {
  //   onSort(e, column, true);
  // };
  render() {
    let { shift, startIndex: index } = this.props.scroll;
    const {
      meta,
      width,
      height,
      type,
      onChange,
      openFilter,
      filterTo
    } = this.props;
    const cells = [
      <div
        key="status"
        className="zebulon-table-corner"
        style={{
          position: "absolute",
          width: height,
          left: 0
        }}
      />
    ];
    shift += height;
    while (index < meta.length && shift < width) {
      const column = meta[index];
      if (!column.hidden) {
        const columnWidth = Math.min(
          index === 0 ? column.width - shift + height : column.width,
          width - shift
        );
        let div;
        if (type === "header") {
          div = header(column, shift, columnWidth, this.handleClick);
        } else if (type === "filter") {
          if (filterTo && column.filterType !== "between") {
            div = filterEmpty(column.id, shift, columnWidth);
          } else {
            div = filter(
              column,
              shift,
              columnWidth,
              filterTo,
              onChange,
              column.filterType === "values" ? openFilter : () => {}
            );
          }
        }
        // if (!filterTo || column.filterType === "between") {
        cells.push(div);
        // }
        shift += column.width;
      }
      index += 1;
    }
    if (cells.length) {
      return (
        <div
          key={-2}
          id={type}
          style={{
            width,
            height,
            overflow: "hidden"
            // position: "absolute"
          }}
        >
          {cells}
        </div>
      );
    } else {
      return null;
    }
  }
}
export class Status extends Component {
  constructor(props) {
    super(props);
  }
  onClick = index =>
    this.props.selectRange({
      end: { rows: index, columns: 0 },
      start: { rows: index, columns: this.props.meta.length - 1 }
    });
  render() {
    const { height, rowHeight, data, scroll, updatedRows } = this.props;
    let index = 0;

    // const shift = scroll.shift;
    const cells = [];
    while (index < Math.min(data.length, Math.ceil(height / rowHeight))) {
      const style = {
        position: "absolute",
        top: scroll.shift + index * rowHeight,
        width: rowHeight,
        height: rowHeight
      };
      cells.push(
        editCell(
          style,
          "zebulon-table-status",
          index + scroll.startIndex,
          updatedRows[data[index + scroll.startIndex].index_] || {},
          this.onClick,
          () => {}
        )
      );
      index += 1;
    }
    return (
      <div
        key={-1}
        style={{
          width: rowHeight,
          height,
          overflow: "hidden",
          position: "relative",
          diplay: "flex"
        }}
      >
        {cells}
      </div>
    );
  }
}
