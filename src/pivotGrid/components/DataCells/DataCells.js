import React, { PureComponent } from 'react';
import { findDOMNode } from 'react-dom';
import {
  Grid as ReactVirtualizedGrid
} from 'react-virtualized/dist/commonjs/Grid';
import { CellMeasurer } from 'react-virtualized/dist/commonjs/CellMeasurer';

import { isInRange } from '../../utils/generic';
import { DataCell } from '../../Cells';
import DataCellComponent from '../DataCell';
import { AXIS_SEPARATOR } from '../../constants';

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
    this.handleDrilldown = this.handleDrilldown.bind(this);

    this.state = {
      cellsCache: {},
      selectedCellStart: null,
      selectedCellEnd: null
    };
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

  componentDidUpdate(prevProps) {
    this.isUpdating = false;
    if (
      prevProps.zoom !== this.props.zoom ||
      prevProps.sizes.leafs.rows !== this.props.sizes.leafs.rows ||
      prevProps.sizes.leafs.columns !== this.props.sizes.leafs.columns
    ) {
      this.grid.recomputeGridSize();
    }
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
    const { columnHeaders, rowHeaders } = this.props;
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
          selectedCellEnd: [columnHeaders.length, rowHeaders.length]
        });
      }
      e.preventDefault();
    }
  }

  handleCopy() {
    if (
      // Works only if the data cells are focused
      // Later we could make it work if any part of the grid
      // (row and columns headers...) are focused
      findDOMNode(this.grid) === document.activeElement
    ) {
      const { selectedCellStart, selectedCellEnd } = this.state;
      this.props.copy({
        selectedCellStart,
        selectedCellEnd
      });
    }
  }

  handleDrilldown(cell) {
    return this.props.drilldown(this.props.getCellInfos(cell));
  }

  cellRenderer(
    {
      columnIndex,
      key,
      parent,
      rowIndex,
      style
    }
  ) {
    const { selectedCellStart, selectedCellEnd } = this.state;
    const {
      getCellValue,
      dataHeadersLocation,
      customFunctions
    } = this.props;
    const { rowHeaders, columnHeaders, measuredSizesCache } = this.props;
    const rowHeaderRow = rowHeaders[rowIndex];
    const rowHeader = rowHeaderRow[rowHeaderRow.length - 1];
    const columnHeaderColumn = columnHeaders[columnIndex];
    const columnHeader = columnHeaderColumn[columnHeaderColumn.length - 1];
    let selected = false;
    if (selectedCellStart && selectedCellEnd) {
      selected = isInRange(
        [columnIndex, rowIndex],
        selectedCellStart,
        selectedCellEnd
      );
    }

    // This causes all the data cells to be rendered when new cells are selected via mouse actions
    // It is not optimal, we could implement a memoizer so that cells are not recalculated but it would
    // bring complexity and this is good enough at the time.
    const cell = new DataCell(
      getCellValue,
      dataHeadersLocation,
      rowHeader,
      columnHeader,
      customFunctions
    );
    const cellKey = `${rowHeader.key}${AXIS_SEPARATOR}${columnHeader.key}`;
    this.datacellsCache[cellKey] = cell.value;
    let valueHasChanged = false;
    if (this.isUpdating) {
      const oldcell = this.state.cellsCache[cellKey];
      // NaN is not equal to NaN... hence the last condition
      if (oldcell !== undefined && cell.value !== oldcell && !isNaN(oldcell)) {
        valueHasChanged = true;
      }
    }
    return (
      <CellMeasurer
        cache={measuredSizesCache}
        columnIndex={columnIndex}
        key={key}
        parent={parent}
        rowIndex={rowIndex}
      >
        <DataCellComponent
          cell={cell}
          columnIndex={columnIndex}
          drilldown={this.handleDrilldown}
          handleMouseDown={this.handleMouseDown}
          handleMouseOver={this.handleMouseOver}
          key={key}
          rowIndex={rowIndex}
          selected={selected}
          style={{ ...style, whiteSpace: 'nowrap' }}
          valueHasChanged={valueHasChanged}
        />
      </CellMeasurer>
    );
  }

  render() {
    const {
      getColumnWidth,
      getRowHeight,
      onScroll,
      columnCount,
      rowCount,
      height,
      width,
      scrollToColumn,
      scrollToRow,
      measuredSizesCache,
      onSectionRendered,
      zoom
    } = this.props;
    this.datacellsCache = {};
    return (
      <ReactVirtualizedGrid
        cellRenderer={this.cellRenderer}
        className="pivotgrid-data-cells"
        columnCount={columnCount}
        columnWidth={getColumnWidth}
        deferredMeasurementCache={measuredSizesCache}
        height={height}
        onScroll={onScroll}
        ref={ref => {
          this.grid = ref;
        }}
        rowCount={rowCount}
        rowHeight={getRowHeight}
        scrollToAlignment="start"
        onSectionRendered={onSectionRendered}
        scrollToColumn={scrollToColumn}
        scrollToRow={scrollToRow}
        style={{ fontSize: `${zoom * 100}%` }}
        width={width}
      />
    );
  }
}

export default DataCells;
