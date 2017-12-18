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
      onClick={e => onClick(e, row)}
      onDoubleClick={e => onDoubleClick(e, row)}
    >
      {glyph}
    </div>
  );
};
const filter = (column, shift, width, onChange) => {
  const className = "zebulon-table-header zebulon-table-filter";
  const textAlign =
    column.alignement ||
    (column.dataType === "number"
      ? "right"
      : column.dataType === "number" ? "center" : "left");
  if (column.filterType === "interval") {
    return (
      <InputInterval
        key={column.id}
        // code={column.id}
        className={className}
        // hasFocus={hasFocus}
        // className="zebulon-table-header"
        style={{
          position: "absolute",
          left: shift,
          width
        }}
        dataType={column.dataType}
        format={column.format || (value => value)}
        onChange={() => {}} //onChange}
      />
    );
  } else {
    return (
      <Input
        key={column.id}
        code={column.id}
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
        value={column.v}
        onChange={e => {
          onChange(e, column);
        }} //event => onChange(event)}
        isEditable={true}
      />
    );
  }
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
    const { meta, width, height, type, onChange } = this.props;
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
      const columnWidth =
        index === 0
          ? column.width - shift + height
          : Math.min(column.width, width - shift);
      let div;
      if (type === "header") {
        div = header(column, shift, columnWidth, this.handleClick);
      } else if (type === "filter") {
        div = filter(column, shift, columnWidth, onChange);
      }
      cells.push(div);
      shift += column.width;
      index += 1;
    }
    return (
      <div
        key={-2}
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
          position: "relative",
          diplay: "flex"
        }}
      >
        {cells}
      </div>
    );
  }
}

// export class Filters extends Component {
//   constructor(props) {
//     super(props);
//   }
//   render() {
//     let { shift, startIndex: index } = this.props.scroll;
//     const { meta, width, height } = this.props;
//     const cells = [
//       <div
//         key="status"
//         className="zebulon-table-corner"
//         style={{
//           position: "absolute",
//           width: height,
//           left: 0
//         }}
//       />
//     ];
//     shift += height;
//     while (index < meta.length && shift < width) {
//       const column = meta[index];
//       cells.push(filter(column, shift, false, { value: column.caption }));
//       shift += column.width;
//       index += 1;
//     }
//     return (
//       <div
//         key={-2}
//         style={{
//           width,
//           height,
//           overflow: "hidden",
//           position: "relative"
//         }}
//       >
//         {cells}
//       </div>
//     );
//   }
// render() {
//   let { shift, startIndex: index } = this.props.scroll;
//   const { meta, width, height } = this.props;
//   const cells = [
//     <div
//       key="status"
//       className="zebulon-table-corner"
//       style={{
//         position: "absolute",
//         width: height,
//         left: 0
//       }}
//     />
//   ];
//   shift += height;
//   while (index < meta.length && shift < width) {
//     const column = meta[index];
//     cells.push(filter(column, shift, false, { value: column.caption }));
//     shift += column.width;
//     index += 1;
//   }
//   return (
//     <div
//       key={-2}
//       style={{
//         width,
//         height,
//         overflow: "hidden",
//         position: "relative"
//       }}
//     >
//       {cells}
//     </div>
//   );
// }
// }
// export class Filters extends Component {
//   render() {
//     return <div style={{ display: "flex" }}>titt</div>;
//   }
// }
