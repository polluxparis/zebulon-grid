import React from "react";

import { isInRange, isUndefined } from "../../utils/generic";
import DataCell from "../DataCell/DataCell";
import { MEASURE_ID, AXIS_SEPARATOR, HeaderType } from "../../constants";
import { ScrollableArea } from "../Controls/ScrollableArea";
import { getSelectedText } from "../../services/copyService";
import { ReactHint } from "../Controls/ToolTip";
export class DataCells extends ScrollableArea {
  // -------------------------------------
  // life cycle
  // -------------------------------------
  constructor(props) {
    super(props);
    this.cellCache = {};
    this.isPushing = 0;
    this.editable =
      this.props.configuration.edition.editable &&
      this.props.configuration.edition.activated;
    this.commentsVisible = props.configuration.edition.comments;

    this.comment = undefined;
    this.state = { toolTip: { style: { opacity: 0 }, modal: false } };
  }
  componentWillReceiveProps(newProps) {
    this.isPushing =
      newProps.isPushing && !this.isEditing ? 1 + this.isPushing % 2 : 0;
    this.isEditing = false;
    if (
      !newProps.isPushing &&
      (newProps.rows !== this.props.rows ||
        newProps.columns !== this.props.columns)
    ) {
      this.cellCache = {};
    }
    if (
      this.editable &&
      (this.props.rows.leaves !== newProps.rows.leaves ||
        this.props.columns.leaves !== newProps.columns.leaves ||
        this.props.selectedRange.selectedCellEnd.rowIndex !==
          newProps.selectedRange.selectedCellEnd.rowIndex ||
        this.props.selectedRange.selectedCellEnd.columnIndex !==
          newProps.selectedRange.selectedCellEnd.columnIndex)
    ) {
      if (this.oldValue !== this.newValue || this.comment) {
        const {
          rowIndex,
          columnIndex
        } = this.props.selectedRange.selectedCellEnd;
        this.editData(
          this.props.rows.leaves[rowIndex],
          this.props.columns.leaves[columnIndex]
        );
      }
      this.focusChanged = true;
    }
    this.focusChanged = this.focusChanged || newProps.isPushing;
    this.editable =
      newProps.configuration.edition.editable &&
      newProps.configuration.edition.activated;
    this.commentsVisible = newProps.configuration.edition.comments;
  }
  // -----------------------------------------------------------
  // comments
  // -----------------------------------------------------------
  commentsClose = () => {
    this.comment = undefined;
    this.setState({
      toolTip: {
        style: { opacity: 0, zIndex: 0 }
      }
    });
  };
  handleOnCommentOk = e => {
    this.setState({
      toolTip: {
        style: { opacity: 0, zIndex: 0 }
      }
    });
  };
  handleOnCommentChange = e => {
    this.comment = e.target.value;
  };
  handleComments = e => {
    if ((this.commentsVisible && e.comments.length) || e.editComment) {
      const style = {
        ...e.style,
        opacity: 1,
        zIndex: 1000,
        height: "fit-content",
        width: "fit-content",
        position: "fixed",
        top: e.y + e.style.height,
        left: e.x
      };
      let comment = (
        <ul style={{ paddingLeft: 20, maxWidth: 300 }}>
          {e.comments.map((comment, index) => {
            return (
              <li key={index}>
                {comment
                  .split("\n")
                  .map((comment, index) => <div key={index}>{comment}</div>)}
              </li>
            );
          })}
        </ul>
      );
      let modal = false;
      if (e.editComment) {
        style.autofocus = true;
        comment = (
          <div>
            <div style={{ textAlign: "center", fontWeight: "bold" }}>
              Comments
            </div>
            {comment}
            <textarea
              rows="10"
              cols="50"
              id="comment"
              value={this.comment}
              onChange={this.handleOnCommentChange}
            />
            <div style={{ diplay: "flex" }} />
            <button
              style={{ marginRight: ".5em" }}
              onClick={this.handleOnCommentOk}
            >
              Ok
            </button>
            <button
              style={{ marginRight: ".5em" }}
              onClick={this.commentsClose}
            >
              Cancel
            </button>
          </div>
        );
        // this.isEditing = true;
        modal = true;
      }
      this.setState({
        toolTip: {
          comment,
          style,
          modal
        }
      });
    } else if (
      this.state.toolTip.style.opacity === 1 &&
      !this.state.toolTip.modal
    ) {
      this.commentsClose();
    }
  };
  // -----------------------------------------------------------
  // edition
  // -----------------------------------------------------------

  editData = (rowLeaf, columnLeaf) => {
    const { rows, columns } = this.props;
    const { newValue, oldValue, comment } = this;
    if (newValue - oldValue || this.comment) {
      const data = this.props.buidData(
        rowLeaf,
        columnLeaf,
        oldValue,
        newValue,
        comment
      );
      this.isEditing = true;
      this.props.editData([data]);
      if (this.props.configuration.callbacks.onEdit) {
      }
      this.props.configuration.callbacks.onEdit(data);
    }
    this.comment = undefined;
    this.oldValue = this.newValue;
    if (this.state.toolTip.modal) {
      this.commentsClose();
    }
  };
  handleOnChange = (value, key) => {
    this.newValue = value;
    this.cellCache[key].value = value;
  };
  // ------------------------------------------------
  // events
  // ------------------------------------------------
  handleMouseOver = e => {
    if (!this.state.toolTip.modal) {
      const { columnIndex, rowIndex, button, buttons } = e;
      if (this.isMouseDown && button === 0 && !e.openComment) {
        if (
          !(
            this.props.selectedRange.selectedCellEnd.columnIndex ===
              columnIndex &&
            this.props.selectedRange.selectedCellEnd.rowIndex === rowIndex
          )
        ) {
          this.props.selectRange({
            selectedCellEnd: { columnIndex, rowIndex },
            focusedCell: { columnIndex, rowIndex }
          });
        }
      } else if (this.commentsVisible) {
        this.handleComments(e);
      } else if (this.state.toolTip.style.opacity === 1) {
        this.commentsClose();
      }
    }
  };

  handleMouseDown = e => {
    if (!e.defaultPrevented) {
      this.onSelect(e);
    }
  };
  handleMouseUp = e => {
    this.isMouseDown = false;
  };

  onSelect = e => {
    // console.log(e);
    const { button, shiftKey, columnIndex, rowIndex, value } = e;
    if (button === 0) {
      // check if cell value has changed
      const focusedCell = this.props.selectedRange.selectedCellEnd;
      if (
        this.editable &&
        (focusedCell.columnIndex !== columnIndex ||
          focusedCell.rowIndex !== rowIndex) &&
        this.oldValue !== this.newValue
      ) {
        this.editData(
          this.props.rows.leaves[focusedCell.rowIndex],
          this.props.columns.leaves[focusedCell.columnIndex]
        );
      }
      this.isMouseDown = true;
      this.oldValue = value;
      this.newValue = value;
      if (shiftKey) {
        this.props.selectRange({
          selectedCellStart: this.props.selectedRange.selectedCellStart,
          selectedCellEnd: { columnIndex, rowIndex }
        });
      } else {
        // console.log("select", { columnIndex, rowIndex });
        this.props.selectCell({ columnIndex, rowIndex });
      }
    }
  };
  handleDoubleClick = cell => {
    const keys = Object.keys(this.props.menuFunctions.dataCellFunctions);
    if (keys.length) {
      const menuFunction = this.props.menuFunctions.dataCellFunctions[keys[0]]
        .function;
      return menuFunction(this.props.getCellInfos(cell));
    }
  };
  handleClickMenu = (e, data, target) => {
    if (e.button === 0) {
      if (data.functionType === "cell") {
        this.props.menuFunctions.dataCellFunctions[data.action].function(
          this.props.getCellInfos({
            columnIndex: data.columnIndex,
            rowIndex: data.rowIndex
          })
        );
      } else if (data.functionType === "range") {
        this.props.menuFunctions.rangeFunctions[data.action].function(
          this.props.getRangeInfos(this.props.selectedRange)
        );
      } else if (data.functionType === "grid") {
        this.props.menuFunctions.gridFunctions[data.action].function({
          grid: this.props.getGridInfos(),
          toText: getSelectedText
        });
      } else if (data.action === "toggle-totals") {
        this.props.setConfigProperty("totalsFirst", data.value);
      } else if (data.action === "toggle-edition-mode") {
        this.props.setConfigProperty("edition", {
          ...this.props.configuration.edition,
          activated: data.value
        });
      } else if (data.action === "toggle-comments") {
        this.props.setConfigProperty("edition", {
          ...this.props.configuration.edition,
          comments: data.value
        });
      }
    }
  };
  // -----------------------------------------
  // scrolling
  // -----------------------------------------
  nextVisible = (leaves, index, direction, offset) => {
    let ix = 0,
      n = 0;
    if (offset === 0) {
      ix = -direction;
      n = -1;
    }
    while (
      n < offset &&
      (direction === 1 ? index + ix < leaves.length - 1 : index + ix > 0)
    ) {
      ix += direction;
      n += leaves[index + ix].isVisible || 0;
    }
    return index + ix;
  };
  onScroll = e => {
    const { rows, columns, onScroll } = this.props;
    if (e.type === "scrollbar") {
      const scroll = {
        rows: { ...rows.scroll },
        columns: { ...columns.scroll }
      };
      let direction, index;
      // if (e.initiator === "bar") {
      if (e.direction === "horizontal") {
        index = Math.round(columns.length * e.positionRatio);
        direction = Math.sign(
          this.props.columns.startIndex +
            (scroll.columns.direction === -1 ? 1 : 0) -
            index
        );
        scroll.columns = {
          index:
            direction === -1
              ? Math.round(
                  columns.length * (e.positionRatio + columns.displayedRatio)
                )
              : index,
          direction
        };
      } else {
        index = Math.round(rows.length * e.positionRatio);
        direction = Math.sign(
          this.props.rows.startIndex +
            (scroll.rows.direction === -1 ? 1 : 0) -
            index
        );
        scroll.rows = {
          index:
            direction === -1
              ? Math.round(
                  rows.length * (e.positionRatio + rows.displayedRatio)
                )
              : index,
          direction
        };
      }
      if (direction) {
        onScroll(scroll.rows, scroll.columns);
        return true;
      }
    }
    return false;
  };
  collectMenu = props => {
    return {
      ...props,
      dimensions: this.props.dimensions,
      menuFunctions: this.props.menuFunctions,
      filters: this.props.filters,
      zoom: this.props.zoom,
      onItemClick: this.handleClickMenu,
      configuration: this.props.configuration
    };
  };
  // -----------------------------------------
  // rendering
  // -----------------------------------------
  cellRenderer = (
    rowHeader,
    columnHeader,
    // key,
    isEven,
    offsetRow,
    offsetColumn
  ) => {
    const {
      getCellValue,
      measures,
      selectedRange,
      gridId,
      isPushing
    } = this.props;
    const key = `${rowHeader.key}${AXIS_SEPARATOR}${columnHeader.key}`;
    const style = {
      position: "absolute",
      top: rowHeader.sizes.main.position - offsetRow,
      left: columnHeader.sizes.main.position - offsetColumn,
      height: rowHeader.sizes.main.size,
      width: columnHeader.sizes.main.size
    };
    let measure;
    if (rowHeader.type === HeaderType.MEASURE) {
      measure = measures[rowHeader.id];
    } else {
      measure = measures[columnHeader.id];
    }
    // empty measure
    if (isUndefined(measure)) {
      measure = { format: v => v, valueAccessor: () => null };
    }
    let selected = false;
    if (selectedRange.selectedCellStart && selectedRange.selectedCellEnd) {
      selected = isInRange(
        {
          columnIndex: columnHeader.index,
          rowIndex: rowHeader.index
        },
        selectedRange.selectedCellStart,
        selectedRange.selectedCellEnd
      );
    }
    let focused = false;
    if (selectedRange.selectedCellEnd) {
      focused =
        columnHeader.index === selectedRange.selectedCellEnd.columnIndex &&
        rowHeader.index === selectedRange.selectedCellEnd.rowIndex;
    }
    let valueHasChanged;
    const cell = this.cellCache[key] || {};
    if (cell.value === undefined || isPushing) {
      const value = getCellValue(
        measure.valueAccessor,
        rowHeader.dataIndexes,
        columnHeader.dataIndexes,
        measure.aggregation,
        measure.id
      );
      valueHasChanged = this.isPushing * (cell.value !== value.value);
      cell.value = value.value;
      cell.edited = value.edited;
      cell.comments = value.comments;
      cell.caption = measure.format(cell.value);
      this.cellCache[key] = cell;
    }
    if (focused && this.focusChanged) {
      this.oldValue = cell.value;
      this.newValue = cell.value;
      this.focusChanged = false;
    }
    const highlighted = false;
    return (
      <DataCell
        headersKey={key}
        key={key}
        valueHasChanged={valueHasChanged}
        style={style}
        rowIndex={rowHeader.index}
        columnIndex={columnHeader.index}
        caption={cell.caption}
        value={cell.value}
        gridId={gridId}
        selected={selected}
        focused={focused}
        highlighted={highlighted}
        isTotal={Math.max(rowHeader.isTotal, columnHeader.isTotal || 0)}
        handleMouseDown={this.handleMouseDown}
        handleMouseOver={this.handleMouseOver}
        handleMouseUp={this.handleMouseUp}
        handleDoubleClick={this.handleDoubleClick}
        handleOnChange={this.handleOnChange}
        collectMenu={this.collectMenu}
        isEven={isEven}
        editable={this.editable}
        edited={cell.edited}
        comments={cell.comments}
        handleComments={this.handleComments}
      />
    );
  };
  getContent = () => {
    const { rows, columns } = this.props;
    if (rows === undefined || columns === undefined) {
      return [];
    }
    this.positionRatios = {
      vertical: rows.positionRatio,
      horizontal: columns.positionRatio
    };
    const cells = [
      <div key={-1} className="tool-tip" style={this.state.toolTip.style}>
        {this.state.toolTip.comment}
      </div>
    ];
    // console.log("render", this.state.toolTip);
    rows.cells.forEach((row, rowIndex) =>
      columns.cells.map((column, columnIndex) =>
        cells.push(
          this.cellRenderer(
            row[0],
            column[0],
            !(rowIndex % 2),
            rows.offset,
            columns.offset
          )
        )
      )
    );
    return cells;
  };
  getRatios = () => {
    const { rows, columns } = this.props;
    return {
      vertical: { display: rows.displayedRatio, position: rows.positionRatio },
      horizontal: {
        display: columns.displayedRatio,
        position: columns.positionRatio
      }
    };
  };
}
