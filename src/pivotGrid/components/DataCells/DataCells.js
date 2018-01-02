import React from "react";
import { utils, constants } from "zebulon-controls";
import DataCell from "../DataCell/DataCell";
import { ScrollableGrid } from "zebulon-controls";
import { getSelectedText } from "../../services/copyService";
const { isInRange, isUndefined } = utils;
const { AXIS_SEPARATOR, HeaderType, AxisType, toAxis } = constants;
export class DataCells extends ScrollableGrid {
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
        this.props.selectedRange.end.rows !== newProps.selectedRange.end.rows ||
        this.props.selectedRange.end.columns !==
          newProps.selectedRange.end.columns)
    ) {
      if (this.oldValue !== this.newValue || this.comment) {
        const { rows, columns } = this.props.selectedRange.end;
        this.editData(
          this.props.rows.leaves[rows],
          this.props.columns.leaves[columns]
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
  componentDidMount() {
    this.props.getRef(this);
  }
  // -----------------------------------------------------------
  // comments
  // -----------------------------------------------------------
  commentsClose = () => {
    this.comment = undefined;
    this.handleOnCommentOk();
    this.isMouseDown = false;
  };
  handleOnCommentOk = e => {
    const toolTip = {
      style: { opacity: 0, zIndex: 0 }
    };
    this.setState({ toolTip });
    this.props.setToolTip(toolTip);
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
        // style.autofocus = true;
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
              tabIndex={0}
              autoFocus={true}
              // value={this.comment}
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
      const toolTip = {
        comment,
        style,
        modal
      };
      this.setState({ toolTip });
      this.props.setToolTip(toolTip);
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
  handleOnChange = (value, key) => {
    this.newValue = value;
    this.cellCache[key].value = value;
  };
  editData = (rowLeaf, columnLeaf) => {
    // const { rows, columns } = this.props;
    const { newValue, oldValue, comment } = this;
    if (newValue !== oldValue || this.comment) {
      const data = this.props.buildData(
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

  // ------------------------------------------------
  // events
  // ------------------------------------------------
  handleMouseOver = e => {
    if (!this.state.toolTip.modal) {
      const { columnIndex, rowIndex, button } = e;
      if (this.isMouseDown && button === 0 && !e.openComment) {
        if (
          !(
            this.props.selectedRange.end.columns === columnIndex &&
            this.props.selectedRange.end.rows === rowIndex
          )
        ) {
          this.props.selectRange({
            end: { columns: columnIndex, rows: rowIndex },
            focusedCell: { columns: columnIndex, rows: rowIndex }
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
    // console.log("mouse up", e.target);
    this.isMouseDown = false;
    e.stopPropagation();
  };

  onSelect = e => {
    // console.log(e);
    const { button, shiftKey, columnIndex, rowIndex, value } = e;
    if (button === 0) {
      // check if cell value has changed
      const focusedCell = this.props.selectedRange.end;
      if (
        this.editable &&
        (focusedCell.columns !== columnIndex ||
          focusedCell.rows !== rowIndex) &&
        this.oldValue !== this.newValue
      ) {
        this.editData(
          this.props.rows.leaves[focusedCell.rows],
          this.props.columns.leaves[focusedCell.columns]
        );
      }
      this.isMouseDown = true;
      this.oldValue = value;
      this.newValue = value;
      if (shiftKey) {
        this.props.selectRange({
          start: this.props.selectedRange.start,
          end: { columns: columnIndex, rows: rowIndex }
        });
      } else {
        // console.log("select", { columnIndex, rowIndex });
        this.props.selectCell({ columns: columnIndex, rows: rowIndex });
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

  collectMenu = props => {
    return {
      ...props,
      dimensions: this.props.dimensions,
      menuFunctions: this.props.menuFunctions,
      filters: this.props.filters,
      // zoom: this.props.zoom,
      onItemClick: this.handleClickMenu,
      configuration: this.props.configuration
    };
  };
  handleClickMenu = (type, fct, data) => {
    if (type === "Cell") {
      this.props.menuFunctions.dataCellFunctions[fct].function(
        this.props.getCellInfos({
          columns: data.columnIndex,
          rows: data.rowIndex
        })
      );
    } else if (type === "Range") {
      this.props.menuFunctions.rangeFunctions[fct].function(
        this.props.getRangeInfos(this.props.selectedRange)
      );
    } else if (type === "Grid") {
      this.props.menuFunctions.gridFunctions[fct].function({
        grid: this.props.getGridInfos(),
        toText: getSelectedText
      });
    } else if (type === "Totals") {
      this.props.setConfigProperty("totalsFirst", data.value);
    } else if (type === "Edition") {
      this.props.setConfigProperty("edition", {
        ...this.props.configuration.edition,
        activated: data.value
      });
    } else if (type === "Comments") {
      this.props.setConfigProperty("edition", {
        ...this.props.configuration.edition,
        comments: data.value
      });
    }
  };
  // -----------------------------------------
  // scrolling and selection
  // -----------------------------------------
  selectedRange = () => this.props.selectedRange;
  selectedCell = () => ({ ...this.props.selectedRange.end });
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
  lastIndex = (axis, direction) => {
    const axisLeaves =
      axis === AxisType.COLUMNS ? this.props.columns : this.props.rows;
    return this.nextVisible(
      axisLeaves.leaves,
      direction === 1 ? axisLeaves.length - 1 : 0,
      -direction,
      0
    );
  };
  nextIndex = (axis, direction, index, offset) => {
    const axisLeaves =
      axis === AxisType.COLUMNS ? this.props.columns : this.props.rows;
    return this.nextVisible(axisLeaves.leaves, index, direction, offset);
  };
  nextPageIndex = (axis, direction, index) => {
    const axisLeaves =
      axis === AxisType.COLUMNS ? this.props.columns : this.props.rows;
    return this.nextVisible(
      axisLeaves.leaves,
      index,
      direction,
      axisLeaves.cells.length - 1
    );
  };
  scrollOnKey = (cell, axis, direction) => {
    const { rows, columns } = this.props;
    const axisLeaves = axis === AxisType.COLUMNS ? columns : rows;
    const scroll = { rows: rows.scroll, columns: columns.scroll };
    if (
      axisLeaves.hasScrollbar &&
      ((direction === 1 && cell[toAxis(axis)] >= axisLeaves.stopIndex) ||
        (direction === -1 && cell[toAxis(axis)] <= axisLeaves.startIndex))
    ) {
      scroll[toAxis(axis)] = {
        index: cell[toAxis(axis)],
        direction: -direction
      };
      this.props.onScroll(scroll.rows, scroll.columns);
    }
  };
  onWheel = e => {
    if (!e.defaultPrevented) {
      e.preventDefault();
      const { rows, columns, onScroll } = this.props;
      const sense = e.altKey || e.deltaX !== 0 ? "columns" : "rows";
      const leaves = sense === "columns" ? columns : rows;
      const direction = Math.sign(sense === "columns" ? e.deltaX : e.deltaY);

      const prevIndex = direction === 1 ? leaves.stopIndex : leaves.startIndex;
      const offset = leaves.direction === direction && leaves.offset > 2;
      const index = this.nextVisible(
        leaves.leaves,
        prevIndex - offset,
        direction,
        1
      );
      const scroll = { rows: rows.scroll, columns: columns.scroll };
      if (sense === "columns") {
        scroll.columns = { index, direction: -direction };
      } else {
        scroll.rows = { index, direction: -direction };
      }
      onScroll(scroll.rows, scroll.columns);
    }
  };
  onScroll = (axis, dir, ix, positionRatio) => {
    const { rows, columns, onScroll } = this.props;
    // if (e.type === "scrollbar") {
    const scroll = {
      rows: { ...rows.scroll },
      columns: { ...columns.scroll }
    };
    let direction, index;
    // if (e.initiator === "bar") {
    if (axis === AxisType.COLUMNS) {
      index = Math.round(columns.length * positionRatio);
      direction = Math.sign(
        this.props.columns.startIndex +
          (scroll.columns.direction === -1 ? 1 : 0) -
          index
      );
      scroll.columns = {
        index:
          direction === -1
            ? Math.round(
                columns.length * (positionRatio + columns.displayedRatio)
              )
            : index,
        direction
      };
    } else {
      index = Math.round(rows.length * positionRatio);
      direction = Math.sign(
        this.props.rows.startIndex +
          (scroll.rows.direction === -1 ? 1 : 0) -
          index
      );
      scroll.rows = {
        index:
          direction === -1
            ? Math.round(rows.length * (positionRatio + rows.displayedRatio))
            : index,
        direction
      };
    }
    if (direction) {
      onScroll(scroll.rows, scroll.columns);
      return true;
    }
    // }
    return false;
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
    if (selectedRange.start && selectedRange.end) {
      selected = isInRange(
        {
          columns: columnHeader.index,
          rows: rowHeader.index
        },
        selectedRange.start,
        selectedRange.end
      );
    }
    let focused = false;
    if (selectedRange.end) {
      focused =
        columnHeader.index === selectedRange.end.columns &&
        rowHeader.index === selectedRange.end.rows;
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
  _getContent = () => {
    const { rows, columns } = this.props;
    if (rows === undefined || columns === undefined) {
      return [];
    }
    this.positionRatios = {
      vertical: rows.positionRatio,
      horizontal: columns.positionRatio
    };
    const cells = [];
    // (  <div
    //     key={"tool-tip"}
    //     className="zebulon-tool-tip"
    //     style={this.state.toolTip.style}
    //   >
    //     {this.state.toolTip.comment}
    //   </div>)
    // ];
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
