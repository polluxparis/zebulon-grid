import React, { Component } from "react";

import { isInRange, isUndefined } from "../../utils/generic";
import DataCell from "../DataCell/DataCell";
import { AXIS_SEPARATOR, HeaderType, AxisType } from "../../constants";
import { ScrollBar } from "./ScrollBar";

export class DataCells extends Component {
  constructor(props) {
    super(props);
    this.cellCache = {};
    // this.scrollbarWidth = 10;
    // this.scrollArrowSize = 10;
    this.scroll = {
      row: {},
      column: {}
    };
    this.state = {
      scroll: {
        row: {
          index: 0,
          direction: 1
        },
        column: {
          index: 0,
          direction: 1
        }
      }
    };
  }

  componentWillReceiveProps(newProps) {
    if (
      newProps.rowHeaders !== this.props.rowHeaders ||
      newProps.columnHeaders !== this.props.columnHeaders
    ) {
      this.cellCache = {};
    }
  }
  componentDidUpdate() {
    // this.cells = this.cellsRenderer();
    this.props.onScroll(this.scroll.row, this.scroll.column);
  }

  handleMouseDown = e => {
    // console.log("scrollbar handleMouseDown", e);
    if (!this.startScroll(e)) {
      this.startSelect(e);
    }
  };

  handleMouseUp = e => {
    // console.log("scrollbar handleMouseup", e);
    this.isMouseDown = false;
    if (this.scroll.isScrolling) {
      this.endScroll(e);
    }
  };
  handleMouseOver = ({ columnIndex, rowIndex, button }) => {
    if (this.isMouseDown && button === 0) {
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
    }
  };
  handleKeyDown = e => {
    if (e.metaKey || e.ctrlKey) {
      // ctrl A -> select all
      if (e.which === 65) {
        this.props.selectRange({
          selectedCellStart: { columnIndex: 0, rowIndex: 0 },
          selectedCellEnd: {
            columnIndex: this.props.columnHeaders.length - 1,
            rowIndex: this.props.rowHeaders.length - 1
          }
        });
        e.preventDefault();
      }
      // arrow keys
    } else if (e.which > 32 && e.which < 41) {
      let direction,
        cell,
        canApply = false,
        offset,
        axis;
      if (e.key === "ArrowDown" || e.key === "ArrowUp") {
        direction = e.key === "ArrowDown" ? -1 : 1;
        axis = AxisType.ROWS;
        cell = {
          columnIndex: this.props.selectedRange.selectedCellEnd.columnIndex,
          rowIndex:
            this.props.selectedRange.selectedCellEnd.rowIndex - direction
        };
      } else if (e.key === "ArrowRight" || e.key === "ArrowLeft") {
        direction = e.key === "ArrowRight" ? -1 : 1;
        axis = AxisType.COLUMNS;
        cell = {
          columnIndex:
            this.props.selectedRange.selectedCellEnd.columnIndex - direction,
          rowIndex: this.props.selectedRange.selectedCellEnd.rowIndex
        };
      } else if (e.key === "PageUp" || e.key === "PageDown") {
        direction = e.key === "PageDown" ? -1 : 1;
        if (e.altKey) {
          axis = AxisType.COLUMNS;
          cell = {
            columnIndex: Math.max(
              Math.min(
                this.props.selectedRange.selectedCellEnd.rowIndex -
                  direction *
                    (this.scroll.column.stopIndex -
                      this.scroll.column.startIndex),
                this.props.columnHeaders.length - 1
              ),
              0
            ),
            rowIndex: this.props.selectedRange.selectedCellEnd.rowIndex
          };
        } else {
          axis = AxisType.ROWS;
          cell = {
            columnIndex: this.props.selectedRange.selectedCellEnd.columnIndex,
            rowIndex: Math.max(
              Math.min(
                this.props.selectedRange.selectedCellEnd.rowIndex -
                  direction *
                    (this.scroll.row.stopIndex - this.scroll.row.startIndex),
                this.props.rowHeaders.length - 1
              ),
              0
            )
          };
        }
      } else if (e.key === "Home" || e.key === "End") {
        direction = e.key === "End" ? -1 : 1;
        if (e.altKey) {
          axis = AxisType.COLUMNS;
          cell = {
            columnIndex:
              direction === 1 ? 0 : this.props.columnHeaders.length - 1,
            rowIndex: this.props.selectedRange.selectedCellEnd.rowIndex
          };
        } else {
          axis = AxisType.ROWS;
          cell = {
            columnIndex: this.props.selectedRange.selectedCellEnd.columnIndex,
            rowIndex: direction === 1 ? 0 : this.props.rowHeaders.length - 1
          };
        }
      }
      offset =
        axis === AxisType.ROWS
          ? (this.scroll.row.direction * direction < 0) *
            direction *
            (this.props.scrollbarSizes.vertical !== 0)
          : (this.scroll.column.direction * direction < 0) *
            direction *
            (this.props.scrollbarSizes.horizontal !== 0);
      canApply =
        axis === AxisType.ROWS
          ? cell.rowIndex + offset >= 0 &&
            cell.rowIndex + offset < this.props.rowHeaders.length
          : cell.columnIndex + offset >= 0 &&
            cell.columnIndex + offset < this.props.columnHeaders.length;
      if (canApply) {
        // selection
        if (e.shiftKey) {
          this.props.selectRange({
            ...this.props.selectedRange,
            selectedCellEnd: cell
          });
        } else {
          this.props.selectCell(cell);
        }
        // shift display at end
        const scroll = {};
        if (
          this.props.scrollbarSizes.vertical &&
          (cell.rowIndex - offset > this.scroll.row.stopIndex ||
            cell.rowIndex - offset < this.scroll.row.startIndex)
        ) {
          this.scrollTo({
            scroll: {
              row: { index: cell.rowIndex, direction },
              column: this.state.scroll.column
            }
          });
        } else if (
          this.props.scrollbarSizes.horizontal &&
          (cell.columnIndex - offset > this.scroll.column.stopIndex ||
            cell.columnIndex - offset < this.scroll.column.startIndex)
        ) {
          this.scrollTo({
            scroll: {
              row: this.state.scroll.row,
              column: { index: cell.columnIndex, direction }
            }
          });
        }
      }
      e.preventDefault();
    }

    // Page down
    //   if (e.key === 'PageDown') {
    //     handleScrollToChange({});

    //     e.preventDefault();
    // }
  };
  handleWheel = e => {
    // console.log(e);
    e.preventDefault();
    const direction = -Math.sign(e.deltaY);
    const prevScroll = e.altKey ? this.scroll.column : this.scroll.row;
    const headersLenght = e.altKey
      ? this.props.columnHeaders.length
      : this.props.rowHeaders.length;
    const scroll = {};
    const offset = prevScroll.direction * direction < 0;
    if (direction === -1 && prevScroll.stopIndex - offset < headersLenght - 1) {
      scroll.index = prevScroll.stopIndex - offset + 1;
      scroll.direction = direction;
    } else if (direction === 1 && prevScroll.startIndex + offset > 0) {
      scroll.index = prevScroll.startIndex + offset - 1;
      scroll.direction = direction;
    } else {
      return;
    }
    this.scrollTo({
      scroll: {
        row: e.altKey ? this.state.scroll.row : scroll,
        column: e.altKey ? scroll : this.state.scroll.column
      }
    });
  };

  //
  startScroll = e => {
    if (e.type === "scrollbar") {
      // console.log(e);
      if (e.initiator === "bar")
        if (e.direction === "horizontal") {
          const index = Math.round(
            this.props.columnHeaders.length * e.positionRatio
          );
          this.scrollTo({
            scroll: {
              row: this.state.scroll.row,
              column: { index, direction: 1 }
            }
          });
        } else {
          const index = Math.round(
            this.props.rowHeaders.length * e.positionRatio
          );

          this.scrollTo({
            scroll: {
              row: { index, direction: 1 },
              column: this.state.scroll.column
            }
          });
        }
    } else {
      return false;
    }
    return true;
  };
  scrollTo = scroll => {
    this.setState(scroll);
    // this.props.onScroll(scroll);
  };
  startSelect = ({ button, shiftKey, columnIndex, rowIndex }) => {
    if (button === 0) {
      this.isMouseDown = true;
      if (shiftKey) {
        this.props.selectRange({
          selectedCellStart: this.props.selectedRange.selectedCellStart,
          selectedCellEnd: { columnIndex, rowIndex }
        });
      } else {
        this.props.selectCell({ columnIndex, rowIndex });
      }
    }
  };

  handleDoubleClick = cell => {
    return this.props.drilldown(this.props.getCellInfos(cell));
  };

  handleClickMenu = (e, data, target) => {
    if (e.button === 0) {
      if (data.action === "drilldown") {
        this.handleDrilldown({
          columnIndex: data.columnIndex,
          rowIndex: data.rowIndex
        });
      } else if (data.functionType === "cell") {
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
      } else if (data.functionType === "function") {
        this.props.menuFunctions.functions[data.action].function();
      }
    }
  };
  collectMenu = props => {
    return {
      ...props,
      dimensions: this.props.dimensions,
      menuFunctions: this.props.menuFunctions,
      filters: this.props.filters,
      zoom: this.props.zoom
    };
  };
  cellRenderer = ({ rowHeader, columnHeader, top, left }) => {
    const { getCellValue, measures, selectedRange, gridId } = this.props;
    const style = {
      position: "absolute",
      top,
      left,
      height: rowHeader.main.size,
      width: columnHeader.main.size
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
        { columnIndex: columnHeader.index, rowIndex: rowHeader.index },
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
    const cellKey = `${rowHeader.index}${AXIS_SEPARATOR}${columnHeader.index}`;
    const cell = this.cellCache[cellKey] || {};
    if (cell.value === undefined) {
      cell.value = getCellValue(
        measure.valueAccessor,
        rowHeader.dataIndexes,
        columnHeader.dataIndexes,
        measure.aggregation
      );
      cell.caption = measure.format(cell.value);
      this.cellCache[cellKey] = cell;
    }
    return (
      <DataCell
        key={cellKey}
        valueHasChanged={false}
        style={style}
        rowIndex={rowHeader.index}
        columnIndex={columnHeader.index}
        caption={cell.caption}
        gridId={gridId}
        selected={selected}
        focused={focused}
        handleMouseDown={this.handleMouseDown}
        handleMouseOver={this.handleMouseOver}
        handleMouseUp={this.handleMouseUp}
        handleDoubleClick={this.handleDoubleClick}
        collectMenu={this.collectMenu}
      />
    );
  };
  cellsRenderer = scroll => {
    // console.log("cells renderer");
    const { rowHeaders, columnHeaders, height, width } = this.props;
    const { row, column } = scroll;
    const cells = [];
    if (row.index >= rowHeaders.length) {
      row.index = 0;
      row.direction = 1;
    }
    if (column.index >= columnHeaders.length) {
      column.index = 0;
      column.direction = 1;
    }
    let rowsSize = 0,
      columnsSize = 0,
      rowIndex = row.index,
      columnIndex = column.index;

    // a voir changement de longueur

    while (rowsSize < height && rowIndex < rowHeaders.length && rowIndex >= 0) {
      const rowHeader = rowHeaders[rowIndex][0].header;
      columnIndex = column.index;
      columnsSize = 0;
      while (
        columnsSize < width &&
        columnIndex < columnHeaders.length &&
        columnIndex >= 0
      ) {
        const columnHeader = columnHeaders[columnIndex][0].header;
        cells.push(
          this.cellRenderer({
            top:
              row.direction === 1
                ? rowsSize
                : height - rowsSize - rowHeader.main.size,
            left:
              column.direction === 1
                ? columnsSize
                : width - columnsSize - columnHeader.main.size,
            rowHeader,
            columnHeader
          })
        );
        columnsSize += columnHeader.main.size;
        columnIndex += column.direction;
      }
      rowsSize += rowHeader.main.size;
      rowIndex += row.direction;
    }
    // when height(or widht) increase and the grid is scrolled, it may not rest enough cells to feed the grid
    if (rowsSize < height || columnsSize < width) {
      console.log(rowIndex - row.index, height, rowsSize);
      const scroll2 = { ...scroll };
      if (rowsSize < height) {
        scroll2.row = { index: rowHeaders.length - 1, direction: -1 };
      }
      if (columnsSize < width) {
        scroll2.column = { index: columnHeaders.length - 1, direction: -1 };
      }
      return this.cellsRenderer(scroll2);
    }
    if (row.direction === 1) {
      this.scroll.row.startIndex = row.index;
      this.scroll.row.stopIndex = rowIndex - 1;
      this.scroll.row.direction = rowsSize - width < 3 ? 1 : 0;
      this.scroll.row.offset = 0;
    } else {
      this.scroll.row.startIndex = rowIndex + 1;
      this.scroll.row.stopIndex = row.index;
      this.scroll.row.direction = rowsSize - width < 3 ? 1 : 0;
      this.scroll.row.offset = height - rowsSize;
    }
    this.scroll.row.direction = row.direction;
    if (column.direction === 1) {
      this.scroll.column.startIndex = column.index;
      this.scroll.column.stopIndex = columnIndex - 1;
      this.scroll.column.direction = columnsSize - width < 3 ? 1 : 0;
      this.scroll.column.offset = 0;
    } else {
      this.scroll.column.startIndex = columnIndex + 1;
      this.scroll.column.stopIndex = column.index;
      this.scroll.column.direction = columnsSize - width < 3 ? 1 : 0;
      this.scroll.column.offset = width - columnsSize;
    }
    this.scroll.column.direction = column.direction;
    // console.log(rowHeaders);
    return cells;
  };

  render() {
    const { height, width, gridId } = this.props;
    const style = {
      position: "relative",
      overflow: "hidden",
      height,
      width
      // borderRight: "solid 0.03em grey",
      // borderBottom: "solid 0.03em grey",
      // borderRadius: " 0.25rem"
    };
    // if (!this.cells) {
    this.cells = this.cellsRenderer(this.state.scroll);
    // }
    const rowDisplayedCount =
      this.scroll.row.stopIndex - this.scroll.row.startIndex + 1;
    const columnDisplayedCount =
      this.scroll.column.stopIndex - this.scroll.column.startIndex + 1;
    return (
      <div
        id={`grid ${gridId}`}
        onWheel={this.handleWheel}
        tabIndex={0}
        onKeyDown={this.handleKeyDown}
        onKeyUp={this.handleKeyUp}
      >
        <div style={{ display: "flex" }}>
          <div style={style}>{this.cells}</div>
          <ScrollBar
            direction="vertical"
            width={this.props.scrollbarSizes.vertical}
            length={height}
            offset={width}
            // visible={this.scroll.visbleScrollbarV}
            positionRatio={
              this.scroll.row.startIndex / this.props.rowHeaders.length
            }
            displayedRatio={rowDisplayedCount / this.props.rowHeaders.length}
            id={`vertical-scrollbar ${gridId}`}
            handleMouseDown={this.handleMouseDown}
            handleMouseOver={this.handleMouseOver}
          />
        </div>
        <ScrollBar
          direction="horizontal"
          width={this.props.scrollbarSizes.horizontal}
          length={width}
          offset={height}
          // visible={this.scroll.visbleScrollbarH}
          positionRatio={
            this.scroll.column.startIndex / this.props.columnHeaders.length
          }
          displayedRatio={
            columnDisplayedCount / this.props.columnHeaders.length
          }
          id={`horizontal-scrollbar ${gridId}`}
          handleMouseDown={this.handleMouseDown}
          handleMouseOver={this.handleMouseOver}
        />
      </div>
    );
  }
}
