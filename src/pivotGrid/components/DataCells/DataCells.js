import React, { PureComponent } from 'react';
import { findDOMNode } from 'react-dom';
import { Grid as ReactVirtualizedGrid } from 'react-virtualized/dist/commonjs/Grid';

import { isInRange, isUndefined } from '../../utils/generic';
import { DataCell, HeaderType } from '../../Cells';
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
      valuesCache: {}
      // selectedCellStart: null,
      // selectedCellEnd: null
    };
  }

  componentDidMount() {
    document.addEventListener('mouseup', this.handleMouseUp);
    document.addEventListener('mousedown', this.handleDocumentMouseDown);
    document.addEventListener('keydown', this.handleKeyDown);
    document.addEventListener('copy', this.handleCopy);
  }

  componentWillReceiveProps() {
    this.setState({ valuesCache: this.valuesCache });
  }

  componentWillUpdate() {
    this.isUpdating = true;
  }

  componentDidUpdate(prevProps) {
    this.isUpdating = false;
    if (
      prevProps.zoom !== this.props.zoom ||
      prevProps.sizes.heights !== this.props.sizes.heights ||
      prevProps.sizes.widths !== this.props.sizes.widths
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

  handleMouseDown(e, { columnIndex, rowIndex }) {
    if (e.button === 0) {
      this.isMouseDown = true;
      this.props.selectRange({
        selectedCellStart: { columnIndex, rowIndex },
        selectedCellEnd: { columnIndex, rowIndex }
      });
      // this.setState({ selectedCellStart: { columnIndex, rowIndex } });
      // this.setState({ selectedCellEnd: { columnIndex, rowIndex } });
    }
  }

  handleMouseUp() {
    this.isMouseDown = false;
  }

  handleMouseOver({ columnIndex, rowIndex }) {
    if (this.isMouseDown) {
      this.props.selectRange({
        selectedCellEnd: { columnIndex, rowIndex }
      });
    }
  }

  handleDocumentMouseDown(e) {
    if (e.button === 0 && this.state.selectedCellStart) {
      if (!this.isMouseDown) {
        this.props.selectRange({
          selectedCellStart: null,
          selectedCellEnd: null
        });
      }
    }
  }

  handleKeyDown(e) {
    const { columnLeaves, rowLeaves } = this.props;
    if (e.which === 69) {
      if (!this.perf) {
        window.Perf.start();
        this.perf = true;
      } else {
        this.perf = false;
        window.Perf.stop();
      }
    }
    // ctrl+A
    if (e.which === 65 && (e.metaKey || e.ctrlKey)) {
      if (
        // Works only if the data cells are focused
        // Later we could make it work if any part of the grid
        // (row and columns headers...) are focused
        findDOMNode(this.grid) === e.target
      ) {
        this.props.selectRange({
          selectedCellStart: { columnIndex: 0, rowIndex: 0 },
          selectedCellEnd: {
            columnIndex: columnLeaves.length,
            rowIndex: rowLeaves.length
          }
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

  cellRenderer({ columnIndex, key, rowIndex, style }) {
    // const { selectedCellStart, selectedCellEnd } = this.state;

    const { getCellValue, customFunctions, focusCellKeys } = this.props;
    const { rowLeaves, columnLeaves, measures, selectedRange } = this.props;
    const rowHeader = rowLeaves[rowIndex];
    const columnHeader = columnLeaves[columnIndex];

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
    // const focused = !!focusCellKeys.filter(
    //   ({ columns, rows }) =>
    //     columns === columnHeader.key && rows === rowHeader.key
    // ).length;

    // This causes all the data cells to be rendered when new cells are selected via mouse actions
    // It is not optimal, we could implement a memoizer so that cells are not recalculated but it would
    // bring complexity and this is good enough at the time.
    // const cell = new DataCell(
    //   getCellValue,
    //   rowHeader,
    //   columnHeader,
    //   customFunctions
    // );
    const value = getCellValue(
      measure.valueAccessor,
      rowHeader.dataIndexes,
      columnHeader.dataIndexes,
      customFunctions.aggregation[measure.id]
    );
    const cellKey = `${rowHeader.key}${AXIS_SEPARATOR}${columnHeader.key}`;
    this.valuesCache[cellKey] = value;
    let valueHasChanged = false;
    if (this.isUpdating) {
      const oldValue = this.state.valuesCache[cellKey];
      // NaN is not equal to NaN... hence the last condition
      if (oldValue !== undefined && value !== oldValue && !isNaN(oldValue)) {
        valueHasChanged = true;
      }
    }
    const caption = measure.format(value);
    return (
      <DataCellComponent
        key={key}
        valueHasChanged={valueHasChanged}
        style={style}
        rowIndex={rowIndex}
        columnIndex={columnIndex}
        caption={caption}
        drilldown={this.handleDrilldown}
        handleMouseDown={this.handleMouseDown}
        handleMouseOver={this.handleMouseOver}
        selected={selected}
      />
    );
  }
  // focused={focused}
  render() {
    const {
      getColumnWidth,
      getRowHeight,
      onScroll,
      height,
      width,
      scrollToColumn,
      scrollToRow,
      onSectionRendered,
      zoom,
      columnLeaves,
      rowLeaves
    } = this.props;
    this.valuesCache = {};
    return (
      <ReactVirtualizedGrid
        cellRenderer={this.cellRenderer}
        className="pivotgrid-data-cells"
        columnCount={columnLeaves.length}
        columnWidth={getColumnWidth}
        height={height}
        onScroll={onScroll}
        ref={ref => {
          this.grid = ref;
        }}
        rowCount={rowLeaves.length}
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
