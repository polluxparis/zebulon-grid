import React, { Component } from 'react';
import { Grid as ReactVirtualizedGrid } from 'react-virtualized';

import { isInRange } from '../../Utils';
import { DataCell } from '../../Cells';
import DataCellComponent from '../DataCell';

class DataCells extends Component {
  constructor(props) {
    super(props);
    this.cellRenderer = this.cellRenderer.bind(this);

    this.state = {};
  }

  cellRenderer({
    columnIndex,
    key,
    rowIndex,
    style: positionStyle,
   }) {
    const { store, drilldown, selectedCellStart, selectedCellEnd } = this.props;
    const { rowsUi, columnsUi } = store;
    const rowHeaderRow = rowsUi.headers[rowIndex];
    const rowHeader = rowHeaderRow[rowHeaderRow.length - 1];
    const columnHeaderColumn = columnsUi.headers[columnIndex];
    const columnHeader = columnHeaderColumn[columnHeaderColumn.length - 1];
    let selected = false;
    if (selectedCellStart && selectedCellEnd) {
      selected = isInRange([columnIndex, rowIndex], selectedCellStart, selectedCellEnd);
    }

    const cell = new DataCell(
      store,
      true,
      rowHeader,
      columnHeader);
    let style = {
      border: 'solid lightgrey thin',
      boxSizing: 'border-box',
      overflow: 'hidden',
    };
    const unEvenRowStyle = { backgroundColor: 'rgba(211, 211, 211, 0.4)' };
    const evenRowStyle = { backgroundColor: 'white' };

    if (rowIndex % 2) {
      style = { ...style, ...unEvenRowStyle };
    } else {
      style = { ...style, ...evenRowStyle };
    }

    const selectedStyle = { backgroundColor: 'lightcyan' };

    if (selected) {
      style = { ...style, ...selectedStyle };
    }

    const cellKey = `${rowHeader.key}-//-${columnHeader.key}`;
    this.datacellsCache[cellKey] = cell.value;
    let valueHasChanged = false;
    if (this.isUpdating) {
      const oldcell = this.state.cellsCache[cellKey];
      if (oldcell !== undefined && cell.value !== oldcell) {
        valueHasChanged = true;
      }
    }
    return (
      <DataCellComponent
        key={key}
        valueHasChanged={valueHasChanged}
        style={{ ...style, ...positionStyle }}
        index={[columnIndex, rowIndex]}
        cell={cell}
        drilldown={drilldown}
        handleMouseDown={this.props.handleMouseDown}
        handleMouseOver={this.props.handleMouseOver}
      />
    );
  }

  render() {
    const {
      store,
      onScroll,
      columnCount,
      rowCount,
      scrollLeft,
      scrollTop,
      height,
      width,
     } = this.props;
    this.datacellsCache = {};
    return (
      <ReactVirtualizedGrid
        cellRenderer={this.cellRenderer}
        className="OrbGrid-data-cells"
        columnCount={columnCount}
        columnWidth={store.getColumnWidth}
        height={height}
        onScroll={onScroll}
        ref={(ref) => { this.grid = ref; }}
        rowCount={rowCount}
        rowHeight={store.getRowHeight}
        scrollLeft={scrollLeft}
        scrollTop={scrollTop}
        style={{ fontSize: `${this.props.store.zoom * 100}%` }}
        width={width}
      />);
  }
}

export default DataCells;
