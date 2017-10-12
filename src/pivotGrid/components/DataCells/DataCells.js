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
          direction: -1
        },
        column: {
          index: 0,
          direction: -1
        }
      }
    };
  }
  componentWillReceiveProps(newProps) {
    if (
      newProps.rows !== this.props.rows ||
      newProps.columns !== this.props.columns
    ) {
      this.cellCache = {};
    }
  }
  handleMouseDown = e => {
    this.onSelect(e);
  };
  handleMouseUp = e => {
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

  onScroll = e => {
    const { rows, columns, onScroll } = this.props;
    if (e.type === "scrollbar") {
      const scroll = { rows: rows.scroll, columns: columns.scroll };
      if (e.initiator === "bar") {
        if (e.direction === "horizontal") {
          scroll.columns = {
            index: Math.round(columns.length * e.positionRatio),
            direction: 1
          };
        } else {
          scroll.rows = {
            index: Math.round(rows.length * e.positionRatio),
            direction: 1
          };
        }
        onScroll(scroll.rows, scroll.columns);
      }
      return true;
    }
    return false;
  };
  onSelect = ({ button, shiftKey, columnIndex, rowIndex }) => {
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
  cellRenderer = (rowHeader, columnHeader, isEven, offsetRow, offsetColumn) => {
    const { getCellValue, measures, selectedRange, gridId } = this.props;
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
        isEven={isEven}
      />
    );
  };
  cellsRenderer = () => {
    const { rows, columns } = this.props;
    if (rows === undefined || columns === undefined) {
      return [];
    }
    const cells = [];
    // const offsetRow =
    //   rows.scroll.direction === 1 ? 0 : scrollbarsWidth.horizontal;
    // const offsetColumn =
    //   columns.scroll.direction === 1 ? 0 : scrollbarsWidth.vertical;
    rows.cells.map((row, index) =>
      columns.cells.map(column =>
        cells.push(
          this.cellRenderer(
            row[0],
            column[0],
            !(index % 2),
            rows.offset,
            columns.offset
          )
        )
      )
    );
    return cells;
  };

  render() {
    const {
      height,
      width,
      gridId,
      scrollbarsWidth,
      rows,
      columns
    } = this.props;
    const style = {
      position: "relative",
      overflow: "hidden",
      height,
      width
    };
    const cells = this.cellsRenderer();
    return (
      <div
        id={`grid ${gridId}`}

        // tabIndex={0}
        // onKeyDown={this.handleKeyDown}
        // onKeyUp={this.handleKeyUp}
        // onWheel={this.handleWheel}
      >
        <div style={{ display: "flex" }}>
          <div style={style}>{cells}</div>
          <ScrollBar
            direction="vertical"
            width={scrollbarsWidth.vertical}
            length={style.height}
            offset={style.width}
            positionRatio={rows.positionRatio}
            displayedRatio={rows.displayedRatio}
            id={`vertical-scrollbar ${gridId}`}
            onScroll={this.onScroll}
            // handleMouseOver={this.handleMouseOver}
          />
        </div>
        <ScrollBar
          direction="horizontal"
          width={scrollbarsWidth.horizontal}
          length={style.width}
          offset={style.height}
          positionRatio={columns.positionRatio}
          displayedRatio={columns.displayedRatio}
          id={`horizontal-scrollbar ${gridId}`}
          onScroll={this.onScroll}
        />
      </div>
    );
  }
}
