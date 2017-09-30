import React, { PureComponent } from "react";
import { Grid as ReactVirtualizedGrid } from "react-virtualized/dist/commonjs/Grid";

import { isInRange, isUndefined } from "../../utils/generic";
import DataCell from "../DataCell/DataCell";
import { AXIS_SEPARATOR, HeaderType } from "../../constants";

class DataCells extends PureComponent {
  constructor(props) {
    super(props);
    this.cellCache = {};
    this.shouldUpdate = true;
    this.scroll = { shouldUpdate: true };
  }

  // componentWillReceiveProps() {
  //   this.setState({ cellCache: this.cellCache });
  // }

  componentDidUpdate(prevProps) {
    if (
      prevProps.sizes !== this.props.sizes ||
      prevProps.dimensions !== this.props.dimensions ||
      prevProps.zoom !== this.props.zoom
    ) {
      this.grid.recomputeGridSize();
    }
    this.isUpdating = false;
  }
  componentDidMount() {
    this.grid.shouldComponentUpdate = () => this.scroll.shouldUpdate;
  }
  handleKeyDown = e => {
    // Page down
    if (e.key === "PageDown") {
      if (e.shiftKey) {
      } else {
      }
      e.preventDefault();
    }

    // Page up
    if (e.key === "PageUp") {
      if (e.shiftKey) {
      } else {
      }
      e.preventDefault();
    }
  };

  handleMouseDown = (e, { columnIndex, rowIndex }) => {
    if (e.button === 0) {
      this.isMouseDown = true;
      if (e.shiftKey) {
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

  handleMouseUp = () => {
    this.isMouseDown = false;
  };

  handleMouseOver = (e, { columnIndex, rowIndex }) => {
    // buttons = 1 to help when mouse is up outside of the grid
    if (this.isMouseDown && e.buttons === 1) {
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

  handleDrilldown = cell => {
    return this.props.drilldown(this.props.getCellInfos(cell));
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
  cellRenderer = ({
    columnIndex,
    key,
    rowIndex,
    verticalOffsetAdjustment,
    horizontalOffsetAdjustment
  }) => {
    const {
      getCellValue,
      rowHeaders,
      columnHeaders,
      measures,
      selectedRange,
      gridId
    } = this.props;
    const rowHeader = rowHeaders[rowIndex][0].header;
    const columnHeader = columnHeaders[columnIndex][0].header;
    const style = {
      position: "absolute",
      top: rowHeader.main.position + verticalOffsetAdjustment,
      left: columnHeader.main.position + horizontalOffsetAdjustment,
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
        { columnIndex, rowIndex },
        selectedRange.selectedCellStart,
        selectedRange.selectedCellEnd
      );
    }
    let focused = false;
    if (selectedRange.focusedCell) {
      focused =
        columnIndex === selectedRange.focusedCell.columnIndex &&
        rowIndex === selectedRange.focusedCell.rowIndex;
    }
    // This causes all the data cells to be rendered when new cells are selected via mouse actions
    // It is not optimal, we could implement a memoizer so that cells are not recalculated but it would
    // bring complexity and this is good enough at the time.
    // const value = getCellValue(
    //   measure.valueAccessor,
    //   rowHeader.dataIndexes,
    //   columnHeader.dataIndexes,
    //   measure.aggregation
    // );
    // const cellKey = `${rowHeader.key}${AXIS_SEPARATOR}${columnHeader.key}`;

    // this.cellCache[cellKey] = value;
    // let valueHasChanged = false;
    // if (this.isUpdating) {
    //   const oldValue = this.state.cellCache[cellKey];
    //   // NaN is not equal to NaN... hence the last condition
    //   if (oldValue !== undefined && value !== oldValue && !isNaN(oldValue)) {
    //     valueHasChanged = true;
    //   }
    // }
    //  const caption = measure.format(value);

    const cellKey = `${rowHeader.key}${AXIS_SEPARATOR}${columnHeader.key}`;
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
        key={key}
        valueHasChanged={false}
        style={style}
        rowIndex={rowIndex}
        columnIndex={columnIndex}
        caption={cell.caption}
        drilldown={this.handleDrilldown}
        handleMouseDown={this.handleMouseDown}
        handleMouseOver={this.handleMouseOver}
        handleMouseUp={this.handleMouseUp}
        handleClickMenu={this.handleClickMenu}
        selected={selected}
        focused={focused}
        collectMenu={this.collectMenu}
        gridId={gridId}
        noContentRenderer={() => null}
        ref={ref => (this.grid = ref)}
      />
    );
  };

  cellsRenderer = ({
    isScrolling,
    rowStartIndex,
    rowStopIndex,
    columnStartIndex,
    columnStopIndex,
    scrollTop,
    scrollLeft,
    verticalOffsetAdjustment,
    horizontalOffsetAdjustment
  }) => {
    const { rowHeaders, columnHeaders } = this.props;
    const rowHeader = rowHeaders[rowStartIndex][0].header;
    if (
      isScrolling &&
      this.scrollToRow === rowStartIndex &&
      this.scrollToColumn === columnStartIndex
    ) {
      return this.cells;
    }
    this.scrollToRow = rowStartIndex;
    this.scrollToColumn = columnStartIndex;
    const columnHeader = columnHeaders[columnStartIndex][0].header;
    this.scrollLeft = columnHeader.main.position;
    console.log(
      "cellsRenderer",
      // rowStartIndex,
      // rowStopIndex,
      // scrollTop,
      // rowHeader.main.position,
      columnStartIndex,
      columnStopIndex,
      scrollLeft,
      columnHeader.main.position,
      this.scroll.shouldUpdate,
      isScrolling,
      verticalOffsetAdjustment,
      horizontalOffsetAdjustment
    );
    const cells = [];
    for (let rowIndex = rowStartIndex; rowIndex <= rowStopIndex; rowIndex++) {
      this.scrollToRow = rowStartIndex;
      for (
        let columnIndex = columnStartIndex;
        columnIndex <= columnStopIndex;
        columnIndex++
      ) {
        cells.push(
          this.cellRenderer({
            rowIndex,
            columnIndex,
            key: `${rowIndex}-${columnIndex}`,
            verticalOffsetAdjustment: verticalOffsetAdjustment,
            horizontalOffsetAdjustment: horizontalOffsetAdjustment
          })
        );
      }
    }
    this.cells = cells;
    return cells;
  };
  getColumnWidth = ({ index }) => {
    const n = this.props.columnHeaders[index][0].header.main.size;
    console.log(
      "getColumnWidth",
      n,
      this.props.columnHeaders[index][0].header.key,
      index
    );
    return n;
  };
  getRowHeight = ({ index }) =>
    this.props.rowHeaders[index][0].header.main.size;

  getIndex = (prevIndex, index, shouldUpdate, direction, length) => {
    const offset =
      !shouldUpdate ||
      (index === 0 && direction === -1) ||
      (index === length - 1 && direction === 1)
        ? 0
        : direction;
    return direction === 1
      ? Math.max(prevIndex, index + offset)
      : Math.min(prevIndex, index + offset);
  };
  onScroll = e => {
    const scroll = this.scroll;
    let updated = false;
    const { prevRowIndex, prevColumnIndex } = scroll;
    if (scroll.shouldUpdate) {
      scroll.rowDirection = Math.sign(e.scrollTop - (scroll.scrollTop || 0));
      scroll.columnDirection = Math.sign(
        e.scrollLeft - (scroll.scrollLeft || 0)
      );
    }
    if (scroll.rowDirection) {
      const rowIndex = this.props.rowHeaders.findIndex(header => {
        const main = header[0].header.main;
        return (
          e.scrollTop >= main.position &&
          e.scrollTop <= main.position + main.size
        );
      });
      scroll.rowIndex = this.getIndex(
        scroll.rowIndex || 0,
        rowIndex,
        scroll.shouldUpdate,
        scroll.rowDirection,
        this.props.rowHeaders.length
      );
      updated = scroll.rowIndex !== (prevRowIndex || 0);
      scroll.scrollTop = this.props.rowHeaders[
        (scroll, rowIndex)
      ][0].header.main.position;
    }
    if (scroll.columnDirection) {
      const columnIndex = this.props.columnHeaders.findIndex(header => {
        const main = header[0].header.main;
        return (
          e.scrollTop >= main.position &&
          e.scrollTop <= main.position + main.size
        );
      });
      scroll.columnIndex = this.getIndex(
        scroll.columnIndex || 0,
        columnIndex,
        scroll.shouldUpdate,
        scroll.columnDirection,
        this.props.columnHeaders.length
      );
      scroll.scrollLeft = this.props.columnHeaders[
        scroll.columnIndex
      ][0].header.main.position;
      updated = scroll.columnIndex !== (prevColumnIndex || 0);
    }
    if (updated) {
      scroll.shouldUpdate = false;
    }
    e = { ...e, ...scroll };

    // e.scrollTop = this.scrollTop;
    console.log("onScroll", e);
    this.props.onScroll(e);
  };
  render() {
    const {
      rowHeaders,
      columnHeaders,
      // onScroll,
      height,
      width,
      scrollToColumn,
      scrollToRow,
      onSectionRendered,
      selectedRange,
      scrollTop,
      scrollLeft
      // rowsSize,
      // columnsSize
    } = this.props;
    console.log("cells render", scrollTop, scrollLeft);
    this.scroll.shouldUpdate = true;
    return (
      <ReactVirtualizedGrid
        cellRenderer={() => {}}
        cellRangeRenderer={this.cellsRenderer}
        className="zebulon-grid-data-cells"
        columnCount={columnHeaders.length}
        rowCount={rowHeaders.length}
        rowHeight={this.getRowHeight}
        columnWidth={this.getColumnWidth}
        height={height}
        width={width}
        onScroll={this.onScroll}
        ref={ref => {
          this.grid = ref;
        }}
        onSectionRendered={onSectionRendered}
        scrollToColumn={scrollToColumn}
        scrollToRow={scrollToRow}
        selectedRange={selectedRange}
        overscanRowCount={0}
        overscanColumnCount={0}
        scrollTop={scrollTop}
        scrollLeft={scrollLeft}
        // estimatedColumnSize={columnsSize}
        // estimatedRowSize={rowsSize}
      />
    );
  }
}

export default DataCells;
