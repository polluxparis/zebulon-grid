import React, { PureComponent } from "react";
import classnames from "classnames";
// import { ContextMenuTrigger } from "react-contextmenu";
import { rightArrow } from "../../icons";
// import { isNullOrUndefined } from "../../utils/generic";
import { ContextualMenuClient } from "zebulon-controls";
// const { isNullOrUndefined } = utils;
export default class DataCell extends PureComponent {
  // export default class DataCell extends ContextualMenuClient {

  collect = e => {
    const { button, shiftKey } = e;
    const {
      columnIndex,
      rowIndex,
      value,
      edited,
      comments,
      style,
      headersKey
    } = this.props;
    const element = document.getElementById(`${rowIndex} - ${columnIndex}`);
    const rect = element ? element.getBoundingClientRect() : {};
    // console.log(rect, rowIndex, columnIndex);
    const { x, y } = rect;
    return {
      type: "cell",
      button,
      shiftKey,
      columnIndex,
      rowIndex,
      headersKey,
      value,
      edited,
      comments,
      style,
      x,
      y
    };
  };
  constructor(props) {
    super(props);
    this.state = { value: props.value };
  }
  componentWillReceiveProps(nextProps) {
    this.setState({ value: nextProps.value });
  }
  componentDidUpdate(prevProps) {
    if (this.props.focused && !prevProps.focused) {
      const element = document.getElementById(
        `input ${this.props.rowIndex} - ${this.props.columnIndex}`
      );
      if (element) {
        element.focus();
        element.select();
      }
      // console.log("cell", element);
    }
  }
  handleComments = e => {
    const event = this.collect(e);
    event.editComment = true;
    // console.log("comment", event);
    this.props.handleComments(event);
  };
  handleMouseDown = e => {
    // if (e.button === 2) {
    //   e.preventDefault();
    //   e.persist();
    //   e.stopPropagation();
    // } else {
    this.props.handleMouseDown(this.collect(e));
    // }
  };

  handleMouseOver = e => this.props.handleMouseOver(this.collect(e));
  handleMouseUp = e => this.props.handleMouseUp(this.collect(e));
  handleDoubleClick = e => {
    const { columnIndex, rowIndex } = this.props;
    this.props.handleDoubleClick({ columns: columnIndex, rows: rowIndex });
  };
  handleOnChange = e => {
    const newValue =
      e.target.value === ""
        ? null
        : isNaN(Number(e.target.value))
          ? this.state.value
          : Number(e.target.value);
    this.setState({ value: newValue });
    this.props.handleOnChange(newValue, this.props.headersKey);
    // console.log(
    //   "changed",
    //   this.props.rowIndex,
    //   this.props.columnIndex,
    //   e.target.value
    // );
  };
  render() {
    const {
      caption,
      // value,
      rowIndex,
      columnIndex,
      // headersKey,
      selected,
      focused,
      valueHasChanged,
      isTotal,
      collectMenu,
      gridId,
      isEven,
      editable,
      edited,
      highlighted
      // handleOnChange,
      // handleComments
    } = this.props;
    const style = {
      boxSizing: "border-box",
      overflow: "hidden"
    };

    const className = classnames(
      "zebulon-grid-cell",
      "zebulon-grid-data-cell",
      {
        "zebulon-grid-data-cell-even": isEven,
        "zebulon-grid-data-cell-uneven": !isEven,
        "zebulon-grid-data-cell-selected": selected,
        "zebulon-grid-data-cell-focused": focused,
        "zebulon-grid-data-cell-total": isTotal === 1,
        "zebulon-grid-data-cell-grandtotal": isTotal === 2,
        "zebulon-grid-data-cell-changed": valueHasChanged === 1,
        "zebulon-grid-data-cell-changed2": valueHasChanged === 2,
        "zebulon-grid-data-cell-highlighted": highlighted,
        "zebulon-grid-data-cell-edited": edited
      }
    );
    let cell;
    if (focused && editable && !isTotal) {
      cell = (
        <div
          id={`${rowIndex} - ${columnIndex}`}
          style={{ ...style, ...this.props.style, display: "flex" }}
        >
          <div
            style={{
              background: rightArrow,
              backgroundSize: "cover",
              height: ".8em",
              width: ".8em",
              marginTop: "0.1em",
              marginRight: "0.1em",
              position: "absolute",
              zIndex: 3
            }}
            onClick={this.handleComments}
          />
          <input
            type="text"
            className={className}
            style={{
              height: "-webkit-fill-available",
              width: "-webkit-fill-available",
              position: "absolute"
            }}
            autoFocus={true}
            tabIndex="0"
            id={`input ${rowIndex} - ${columnIndex}`}
            value={this.state.value === null ? "" : this.state.value}
            onChange={this.handleOnChange}
            onMouseDown={this.handleMouseDown}
            onMouseUp={this.props.handleMouseUp}
            onDoubleClick={this.handleDoubleClick}
            onMouseOver={this.handleMouseOver}
            onDragStart={e => e.preventDefault()}
          />
        </div>
      );
    } else {
      cell = (
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
      );
    }

    return (
      <ContextualMenuClient
        menuId="grid-menu"
        id={`grid-menu-${rowIndex} - ${columnIndex}`}
        gridId={gridId}
        draggable={false}
        collect={() =>
          collectMenu({
            rowIndex,
            columnIndex
          })}
      >
        {cell}
      </ContextualMenuClient>
    );
  }
}
