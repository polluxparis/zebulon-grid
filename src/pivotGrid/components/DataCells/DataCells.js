import React from "react";

import { isInRange, isUndefined } from "../../utils/generic";
import DataCell from "../DataCell/DataCell";
import { AXIS_SEPARATOR, HeaderType } from "../../constants";
import { ScrollableArea } from "../Controls/ScrollableArea";
import { getSelectedText } from "../../services/copyService";
export class DataCells extends ScrollableArea {
  constructor(props) {
    super(props);
    this.cellCache = {};
    this.isPushing = 0;
  }
  componentWillReceiveProps(newProps) {
    this.isPushing = newProps.isPushing ? 1 + this.isPushing % 2 : 0;
    if (
      !newProps.isPushing &&
      (newProps.rows !== this.props.rows ||
        newProps.columns !== this.props.columns)
    ) {
      this.cellCache = {};
    }
  }
  handleMouseDown = e => {
    this.onSelect(e);
  };
  handleMouseUp = e => {
    this.isMouseDown = false;
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
      const scroll = { rows: rows.scroll, columns: columns.scroll };
      let direction;
      // if (e.initiator === "bar") {
      if (e.direction === "horizontal") {
        const index = Math.round(columns.length * e.positionRatio);
        direction = Math.sign(this.props.columns.startIndex - index);

        scroll.columns = {
          index:
            direction === -1
              ? this.nextVisible(
                  columns.leaves,
                  index,
                  -direction,
                  columns.cells.length - 1
                )
              : index,
          direction
        };
      } else {
        const index = Math.round(rows.length * e.positionRatio);
        direction = Math.sign(this.props.rows.startIndex - index);
        scroll.rows = {
          index:
            direction === -1
              ? this.nextVisible(
                  rows.leaves,
                  index,
                  -direction,
                  rows.cells.length - 1
                )
              : index,
          direction
        };
      }
      if (direction) {
        onScroll(scroll.rows, scroll.columns);
        return true;
      }
      // }
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
      }
    }
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
        measure.aggregation
      );
      valueHasChanged = this.isPushing * (cell.value !== value);
      cell.value = value;
      cell.caption = measure.format(cell.value);
      this.cellCache[key] = cell;
    }
    const highlighted = false;
    return (
      <DataCell
        key={key}
        valueHasChanged={valueHasChanged}
        style={style}
        rowIndex={rowHeader.index}
        columnIndex={columnHeader.index}
        caption={cell.caption}
        gridId={gridId}
        selected={selected}
        focused={focused}
        highlighted={highlighted}
        isTotal={Math.max(rowHeader.isTotal, columnHeader.isTotal || 0)}
        handleMouseDown={this.handleMouseDown}
        handleMouseOver={this.handleMouseOver}
        handleMouseUp={this.handleMouseUp}
        handleDoubleClick={this.handleDoubleClick}
        collectMenu={this.collectMenu}
        isEven={isEven}
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
    const cells = [];
    rows.cells.map((row, rowIndex) =>
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
