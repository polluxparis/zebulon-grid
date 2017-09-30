import React, { Component } from "react";
import { Grid as ReactVirtualizedGrid } from "react-virtualized/dist/commonjs/Grid";

import { isInRange, isUndefined } from "../../utils/generic";
import DataCell from "../DataCell/DataCell";
import { AXIS_SEPARATOR, HeaderType } from "../../constants";
import { ScrollBar } from "./ScrollBar";

export class DataCells2 extends Component {
  constructor(props) {
    super(props);
    this.cellCache = {};
    this.scroll = { shouldUpdate: true };
    this.scrollbarWidth = 10;
    this.scrollArrowSize = 10;
    this.state = { scroll: { scrollToRow: 0, scrollToColumn: 0 } };
  }

  // componentWillReceiveProps() {
  //   this.setState({ cellCache: this.cellCache });
  // }
  componentDidMount() {
    this.document = document.getElementById(`grid ${this.props.gridId}`);
    this.document.addEventListener("mouseenter", this.handleMouseEnter);
    this.document.addEventListener("mouseleave", this.handleMouseLeave);
    // this.document.addEventListener("mousedown", this.handleMouseDown);
    // this.document.addEventListener("mouseup", this.handleMouseUp);
    // this.document.addEventListener("mouseover", this.handleMouseOver);
    // this.document.addEventListener("wheel", this.handleWheel);
  }

  componentDidUnMount() {
    this.document.removeEventListener("mouseenter", this.handleMouseEnter);
    this.document.removeEventListener("mouseleave", this.handleMouseLeave);
    // this.document.removeEventListener("mousedown", this.handleMouseDown);
    // this.document.removeEventListener("mouseup", this.handleMouseUp);
    // this.document.removeEventListener("mouseover", this.handleMouseOver);
    // this.document.removeEventListener("wheel", this.handleWheel);
  }
  handleMouseEnter = e => {
    // console.log("scrollbar handleMouseEnter", e);
  };
  handleMouseLeave = e => {
    // console.log("scrollbar handleMouseLeave", e);
  };

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
  // , { columnIndex, rowIndex })
  handleMouseOver = e => {
    // console.log("scrollbar handleMouseOver", e);
    // buttons = 1 to help when mouse is up outside of the grid
    // if (this.isMouseDown && e.buttons === 1) {
    //   if (
    //     !(
    //       this.props.selectedRange.selectedCellEnd.columnIndex ===
    //         columnIndex &&
    //       this.props.selectedRange.selectedCellEnd.rowIndex === rowIndex
    //     )
    //   ) {
    //     this.props.selectRange({
    //       selectedCellEnd: { columnIndex, rowIndex },
    //       focusedCell: { columnIndex, rowIndex }
    //     });
    //   }
    // }
  };

  startSelect = ({ button, shiftKey, columnIndex, rowIndex }) => {
    if (button === 0) {
      this.isMouseDown = true;
      if (shiftKey) {
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
        handleMouseDown={this.handleMouseDown}
        handleMouseOver={this.handleMouseOver}
        handleMouseUp={this.handleMouseUp}
        collectMenu={this.collectMenu}
      />
    );
  };
  cellsRenderer = ({ scrollToRow, scrollToColumn, height, width }) => {
    const { rowHeaders, columnHeaders } = this.props;
    const cells = [];
    let rowSize = 0,
      columnSize = 0,
      rowIndex = scrollToRow,
      columnIndex = scrollToColumn,
      visbleScrollbarV = false,
      visbleScrollbarH = false;
    while (rowSize < height && rowIndex < rowHeaders.length) {
      const rowHeader = rowHeaders[rowIndex][0].header;
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
      columnIndex = scrollToColumn;
      visbleScrollbarV = columnSize > width;
      columnSize = 0;
      rowSize += rowHeader.main.size;
      rowIndex++;
    }
    visbleScrollbarH = rowSize > height;
    this.scroll = {
      rowStartIndex: scrollToRow,
      rowStopIndex: rowIndex,
      columnStartIndex: scrollToColumn,
      columnStopIndex: columnIndex,
      rowLastIndex: rowHeaders.length - 1,
      columnLastIndex: columnHeaders.length - 1,
      visbleScrollbarV,
      visbleScrollbarH
    };
    return cells;
  };
  // getIndex = (prevIndex, index, shouldUpdate, direction, length) => {
  //   const offset =
  //     !shouldUpdate ||
  //     (index === 0 && direction === -1) ||
  //     (index === length - 1 && direction === 1)
  //       ? 0
  //       : direction;
  //   return direction === 1
  //     ? Math.max(prevIndex, index + offset)
  //     : Math.min(prevIndex, index + offset);
  // };

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
    this.setState({
      scroll: {
        scrollToRow,
        scrollToColumn
      }
    });
    e.preventDefault();
    this.props.onScroll(scrollToRow, scrollToColumn);
  };

  startScroll = e => {
    if (e.type === "scrollbar") {
      if (e.initiator === "bar")
        if (e.direction === "horizontal") {
          const scrollToRow = this.scroll.rowStartIndex,
            scrollToColumn = Math.round(
              (this.scroll.columnLastIndex + 1) * e.positionRatio
            );
          this.setState({
            scroll: {
              scrollToRow,
              scrollToColumn
            }
          });
          this.props.onScroll(scrollToRow, scrollToColumn);
        } else {
          const scrollToRow = Math.round(
              (this.scroll.rowLastIndex + 1) * e.positionRatio
            ),
            scrollToColumn = this.scroll.columnStartIndex;
          this.setState({
            scroll: {
              scrollToRow,
              scrollToColumn
            }
          });
          this.props.onScroll(scrollToRow, scrollToColumn);
        }
    } else {
      return false;
    }
    return true;
  };
  endScroll = e => {};
  render() {
    const {
      rowHeaders,
      columnHeaders,
      height,
      width,
      scrollToColumn,
      scrollToRow,
      selectedRange,
      scrollTop,
      gridId
    } = this.props;
    // console.log("cells render", scrollTop, scrollLeft);
    // this.scroll.shouldUpdate = true;
    const style = {
      position: "relative",
      overflow: "hidden",
      height,
      width,
      borderRight: "solid 0.03em grey",
      borderBottom: "solid 0.03em grey",
      borderRadius: " 0.25rem"
    };
    const cells = this.cellsRenderer({
      scrollToRow: this.state.scroll.scrollToRow,
      scrollToColumn: this.state.scroll.scrollToColumn,
      height:
        height - this.scrollbarWidth * (this.scroll.visbleScrollbarH || false),
      width:
        width - this.scrollbarWidth * (this.scroll.visbleScrollbarV || false)
    });

    const rowDisplayedCount =
      this.scroll.rowStopIndex - this.scroll.rowStartIndex + 1;
    const columnDisplayedCount =
      this.scroll.columnStopIndex - this.scroll.columnStartIndex + 1;
    return (
      <div id={`grid ${gridId}`} style={style} onWheel={this.handleWheel}>
        <div display="flex">
          {cells}
          <ScrollBar
            direction="vertical"
            width={this.scrollbarWidth}
            length={height - this.scroll.visbleScrollbarH * this.scrollbarWidth}
            offset={width - this.scrollbarWidth}
            visible={this.scroll.visbleScrollbarV}
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
          width={this.scrollbarWidth}
          length={width - this.scroll.visbleScrollbarV * this.scrollbarWidth}
          offset={height - this.scrollbarWidth}
          visible={this.scroll.visbleScrollbarH}
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
        />
      </div>
    );
  }
}

// export default DataCells;scrollToColumnscrollToColumn
