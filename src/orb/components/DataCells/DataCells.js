import React, { PureComponent } from 'react';
import { findDOMNode } from 'react-dom';
import { Grid as ReactVirtualizedGrid } from 'react-virtualized';

import copy from '../../services/copyService';
import { isInRange } from '../../utils/generic';
import { DataCell } from '../../Cells';
import DataCellComponent from '../DataCell';

class DataCells extends PureComponent {
  constructor(props) {
    super(props);
    this.cellRenderer = this.cellRenderer.bind(this);
    this.handleCopy = this.handleCopy.bind(this);
    this.handleMouseDown = this.handleMouseDown.bind(this);
    this.handleMouseUp = this.handleMouseUp.bind(this);
    this.handleMouseOver = this.handleMouseOver.bind(this);
    this.handleKeyDown = this.handleKeyDown.bind(this);
    this.handleDocumentMouseDown = this.handleDocumentMouseDown.bind(this);

    this.state = { cellsCache: {}, selectedCellStart: null, selectedCellEnd: null };
  }

  componentDidMount() {
    document.addEventListener('mouseup', this.handleMouseUp);
    document.addEventListener('mousedown', this.handleDocumentMouseDown);
    document.addEventListener('keydown', this.handleKeyDown);
    document.addEventListener('copy', this.handleCopy);
  }

  componentWillReceiveProps() {
    this.setState({ cellsCache: this.datacellsCache });
  }

  componentWillUpdate() {
    this.isUpdating = true;
  }

  componentDidUpdate() {
    this.isUpdating = false;
  }

  componentDidUnMount() {
    document.removeEventListener('mouseup', this.handleMouseUp);
    document.removeEventListener('mousedown', this.handleDocumentMouseDown);
    document.removeEventListener('keydown', this.handleKeyDown);
    document.removeEventListener('copy', this.handleCopy);
  }

  handleMouseDown(e, [columnIndex, rowIndex]) {
    if (e.button === 0) {
      this.isMouseDown = true;
      this.setState({ selectedCellStart: [columnIndex, rowIndex] });
      this.setState({ selectedCellEnd: [columnIndex, rowIndex] });
    }
  }

  handleMouseUp() {
    this.isMouseDown = false;
  }

  handleMouseOver([columnIndex, rowIndex]) {
    if (this.isMouseDown) {
      this.setState({ selectedCellEnd: [columnIndex, rowIndex] });
    }
  }

  handleDocumentMouseDown(e) {
    if (e.button === 0 && this.state.selectedCellStart) {
      if (!this.isMouseDown) {
        this.setState({ selectedCellStart: null, selectedCellEnd: null });
      }
    }
  }

  handleKeyDown(e) {
    const { rowsUi, columnsUi } = this.props.store;
    if (e.which === 69) {
      if (!this.perf) {
        window.Perf.start();
        this.perf = true;
      } else {
        this.perf = false;
        window.Perf.stop();
      }
    }
    if (e.which === 65 && (e.metaKey || e.ctrlKey)) {
      if (
        // Works only if the data cells are focused
        // Later we could make it work if any part of the grid
        // (row and columns headers...) are focused
        findDOMNode(this.grid) === e.target
      ) {
        this.setState({
          selectedCellStart: [0, 0],
          selectedCellEnd: [columnsUi.headers.length, rowsUi.headers.length],
        });
      }
      e.preventDefault();
    }
  }

  handleCopy() {
    try {
      if (
        // Works only if the data cells are focused
        // Later we could make it work if any part of the grid
        // (row and columns headers...) are focused
        findDOMNode(this.grid) === document.activeElement
      ) {
        const { selectedCellStart, selectedCellEnd } = this.state;
        copy({ selectedCellStart, selectedCellEnd, store: this.props.store });
      }
    } catch (error) {
      // console.error('error in handleCopy', error);
    }
  }


  cellRenderer({
    columnIndex,
    key,
    rowIndex,
    style: position,
   }) {
    const { selectedCellStart, selectedCellEnd } = this.state;
    const { store, drilldown } = this.props;
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
        position={position}
        rowIndex={rowIndex}
        columnIndex={columnIndex}
        cell={cell}
        drilldown={drilldown}
        handleMouseDown={this.handleMouseDown}
        handleMouseOver={this.handleMouseOver}
        selected={selected}
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
