import React, { Component } from "react";
import classnames from "classnames";
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
      onClick={e => onClick(e, row)}
      onDoubleClick={e => onDoubleClick(e, row)}
    >
      {glyph}
    </div>
  );
};

export class Headers extends Component {
  constructor(props) {
    super(props);
  }
  render() {
    let { shift, startIndex: index } = this.props.scroll;
    const { meta, width, height } = this.props;
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
      cells.push(
        <div
          key={`${-2} - ${index}`}
          className="zebulon-table-header"
          style={{
            position: "absolute",
            left: shift,
            width: column.width
          }}
        >
          {column.caption || column.id}
        </div>
      );
      shift += column.width;
      index += 1;
    }
    return (
      <div
        key={-2}
        style={{
          width,
          height,
          overflow: "hidden",
          position: "relative"
        }}
      >
        {cells}
      </div>
    );
  }
}
export class Status extends Component {
  constructor(props) {
    super(props);
  }
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
          () => {},
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
          position: "relative"
        }}
      >
        {cells}
      </div>
    );
  }
}
export class Filters extends Component {
  render() {
    return <div style={{ display: "flex" }}>titt</div>;
  }
}
