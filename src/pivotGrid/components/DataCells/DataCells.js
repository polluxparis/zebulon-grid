import React, { Component } from "react";

import { isInRange, isUndefined } from "../../utils/generic";
import DataCell from "../DataCell/DataCell";
import { AXIS_SEPARATOR, HeaderType } from "../../constants";
import { ScrollBar } from "./ScrollBar";

export class DataCells extends Component {
  constructor(props) {
    super(props);
    this.cellCache = {};
    // this.scrollbarWidth = 10;
    // this.scrollArrowSize = 10;
    this.scroll = { rowStartIndex: 0, columnStartIndex: 0 };
  }

  componentWillReceiveProps(newProps) {
    if (
      newProps.rowHeaders !== this.props.rowHeaders ||
      newProps.columnHeaders !== this.props.columnHeaders
    ) {
      this.cellCache = {};
    }
  }
  // componentDidMount() {
  //   this.document = document.getElementById(`grid ${this.props.gridId}`);
  //   this.document.addEventListener("mouseenter", this.handleMouseEnter);
  //   this.document.addEventListener("mouseleave", this.handleMouseLeave);
  //   // this.document.addEventListener("mousedown", this.handleMouseDown);
  //   // this.document.addEventListener("mouseup", this.handleMouseUp);
  //   // this.document.addEventListener("mouseover", this.handleMouseOver);
  //   // this.document.addEventListener("wheel", this.handleWheel);
  // }

  // componentDidUnMount() {
  //   this.document.removeEventListener("mouseenter", this.handleMouseEnter);
  //   this.document.removeEventListener("mouseleave", this.handleMouseLeave);
  // this.document.removeEventListener("mousedown", this.handleMouseDown);
  // this.document.removeEventListener("mouseup", this.handleMouseUp);
  // this.document.removeEventListener("mouseover", this.handleMouseOver);
  // this.document.removeEventListener("wheel", this.handleWheel);
  // }
  // handleMouseEnter = e => {
  //   // console.log("scrollbar handleMouseEnter", e);
  // };
  // handleMouseLeave = e => {
  //   // console.log("scrollbar handleMouseLeave", e);
  // };

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
    } else if (e.which > 36 && e.which < 41) {
      let direction,
        cell,
        canApply = false;
      if (e.key === "ArrowDown" || e.key === "ArrowUp") {
        direction = e.key === "ArrowDown" ? 1 : -1;
        cell = {
          columnIndex: this.props.selectedRange.selectedCellEnd.columnIndex,
          rowIndex:
            this.props.selectedRange.selectedCellEnd.rowIndex + direction
        };
        canApply =
          cell.rowIndex >= 0 && cell.rowIndex <= this.scroll.rowLastIndex;
      } else if (e.key === "ArrowRight" || e.key === "ArrowLeft") {
        direction = e.key === "ArrowRight" ? 1 : -1;
        cell = {
          columnIndex:
            this.props.selectedRange.selectedCellEnd.columnIndex + direction,
          rowIndex: this.props.selectedRange.selectedCellEnd.rowIndex
        };
        canApply =
          cell.columnIndex >= 0 &&
          cell.columnIndex <= this.scroll.columnLastIndex;
      }
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
        if (
          cell.rowIndex > this.scroll.rowStopIndex - 1 ||
          cell.rowIndex < this.scroll.rowStartIndex
        ) {
          this.scrollTo(
            this.scroll.rowStartIndex + direction,
            this.scroll.columnStartIndex
          );
        } else if (
          cell.columnIndex > this.scroll.columnStopIndex - 1 ||
          cell.columnIndex < this.scroll.columnStartIndex
        ) {
          this.scrollTo(
            this.scroll.rowStartIndex,
            this.scroll.columnStartIndex + direction
          );
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
  // handleKeyUp = e => {
  //   if (e.which === 17) {
  //     this.modifierKeyIsPressed = false;
  //   }
  //   if (e.which === 16) {
  //     this.shiftKeyIsPressed = false;
  //   }
  // };

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

  // handleDrilldown = cell => {
  //   return this.props.drilldown(this.props.getCellInfos(cell));
  // };
  // collectMenu = props => {
  //   return {
  //     ...props,
  //     dimensions: this.props.dimensions,
  //     menuFunctions: this.props.menuFunctions,
  //     filters: this.props.filters,
  //     zoom: this.props.zoom
  //   };
  // };
  // handleClickMenu = (e, data, target) => {
  //   if (e.button === 0) {
  //     if (data.action === "drilldown") {
  //       this.handleDrilldown({
  //         columnIndex: data.columnIndex,
  //         rowIndex: data.rowIndex
  //       });
  //     } else if (data.functionType === "cell") {
  //       this.props.menuFunctions.dataCellFunctions[data.action].function(
  //         this.props.getCellInfos({
  //           columnIndex: data.columnIndex,
  //           rowIndex: data.rowIndex
  //         })
  //       );
  //     } else if (data.functionType === "range") {
  //       this.props.menuFunctions.rangeFunctions[data.action].function(
  //         this.props.getRangeInfos(this.props.selectedRange)
  //       );
  //     } else if (data.functionType === "function") {
  //       this.props.menuFunctions.functions[data.action].function();
  //     }
  //   }
  // };
  collectMenu = props => {
    return {
      ...props,
      dimensions: this.props.dimensions,
      menuFunctions: this.props.menuFunctions,
      filters: this.props.filters,
      zoom: this.props.zoom
    };
  };
  cellRenderer = ({ rowHeader, columnHeader, rowSize, columnSize }) => {
    const { getCellValue, measures, selectedRange, gridId } = this.props;
    const style = {
      position: "absolute",
      top: rowSize,
      left: columnSize,
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
        collectMenu={this.collectMenu}
      />
    );
  };
  cellsRenderer = () => {
    console.log("cells renderer");
    const { rowHeaders, columnHeaders, height, width } = this.props;
    const cells = [];
    let rowSize = 0,
      columnSize = 0,
      rowIndex = this.scroll.rowStartIndex,
      columnIndex = this.scroll.columnStartIndex,
      visbleScrollbarV = false,
      visbleScrollbarH = false;
    if (rowIndex >= rowHeaders.length) {
      rowIndex = 0;
      this.scroll.rowStartIndex = 0;
    }
    if (columnIndex >= columnHeaders.length) {
      columnIndex = 0;
      this.scroll.columnStartIndex = 0;
    }
    while (rowSize < height && rowIndex < rowHeaders.length) {
      const rowHeader = rowHeaders[rowIndex][0].header;
      columnIndex = this.scroll.columnStartIndex;
      columnSize = 0;
      while (columnSize < width && columnIndex < columnHeaders.length) {
        const columnHeader = columnHeaders[columnIndex][0].header;
        cells.push(
          this.cellRenderer({
            rowSize,
            columnSize,
            rowHeader,
            columnHeader
          })
        );
        columnSize += columnHeader.main.size;
        columnIndex++;
      }
      visbleScrollbarV = columnSize > width;
      rowSize += rowHeader.main.size;
      rowIndex++;
    }
    visbleScrollbarH = rowSize > height;
    this.scroll = {
      ...this.scroll,
      rowStopIndex: rowIndex - 1,
      columnStopIndex: columnIndex - 1,
      rowLastIndex: rowHeaders.length - 1,
      columnLastIndex: columnHeaders.length - 1,
      visbleScrollbarV,
      visbleScrollbarH
    };
    console.log(rowHeaders);
    return cells;
  };

  handleWheel = e => {
    // console.log(e);
    const direction = Math.sign(e.deltaY);
    if (
      (this.scroll.rowStartIndex === 0 && direction === -1) ||
      (this.scroll.rowStopIndex === this.scroll.rowLastIndex && direction === 1)
    ) {
      return;
    }
    const scrollToRow = this.scroll.rowStartIndex + direction,
      scrollToColumn = this.scroll.columnStartIndex;
    e.preventDefault();
    this.scrollTo(scrollToRow, scrollToColumn);
  };
  scrollTo = (scrollToRow, scrollToColumn) => {
    this.setState({
      scroll: {
        scrollToRow,
        scrollToColumn
      }
    });
    this.scroll.rowStartIndex = scrollToRow;
    this.scroll.columnStartIndex = scrollToColumn;
    this.props.onScroll(scrollToRow, scrollToColumn);
  };
  startScroll = e => {
    if (e.type === "scrollbar") {
      console.log(e);
      if (e.initiator === "bar")
        if (e.direction === "horizontal") {
          const scrollToRow = this.scroll.rowStartIndex,
            scrollToColumn = Math.round(
              (this.scroll.columnLastIndex + 1) * e.positionRatio
            );

          this.scrollTo(scrollToRow, scrollToColumn);
        } else {
          const scrollToRow = Math.round(
              (this.scroll.rowLastIndex + 1) * e.positionRatio
            ),
            scrollToColumn = this.scroll.columnStartIndex;
          this.scrollTo(scrollToRow, scrollToColumn);
        }
    } else {
      return false;
    }
    return true;
  };
  endScroll = e => {};
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
    const cells = this.cellsRenderer();
    const rowDisplayedCount =
      this.scroll.rowStopIndex - this.scroll.rowStartIndex + 1;
    const columnDisplayedCount =
      this.scroll.columnStopIndex - this.scroll.columnStartIndex + 1;
    return (
      <div
        id={`grid ${gridId}`}
        onWheel={this.handleWheel}
        tabIndex={0}
        onKeyDown={this.handleKeyDown}
        onKeyUp={this.handleKeyUp}
      >
        <div style={{ display: "flex" }}>
          <div style={style}>{cells}</div>
          <ScrollBar
            direction="vertical"
            width={this.props.scrollbarSizes.vertical}
            length={height}
            offset={width}
            // visible={this.scroll.visbleScrollbarV}
            positionRatio={
              this.scroll.rowStartIndex / (this.scroll.rowLastIndex + 1)
            }
            displayedRatio={rowDisplayedCount / (this.scroll.rowLastIndex + 1)}
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
            this.scroll.columnStartIndex / (this.scroll.columnLastIndex + 1)
          }
          displayedRatio={
            columnDisplayedCount / (this.scroll.columnLastIndex + 1)
          }
          id={`horizontal-scrollbar ${gridId}`}
          handleMouseDown={this.handleMouseDown}
          handleMouseOver={this.handleMouseOver}
        />
      </div>
    );
  }
}
