import React, { Component } from 'react';
import { findDOMNode } from 'react-dom';
import { Grid as ReactVirtualizedGrid, AutoSizer } from 'react-virtualized';
import { DropTarget } from 'react-dnd';

import HeaderCellComponent from '../HeaderCell';
import DataCellComponent from '../DataCell';
import { DataCell } from '../../Cells';
import { isInRange } from '../../Utils';
import DragLayer from './DragLayer';
import ResizeHandle from './ResizeHandle';
import { AxisType } from '../../Axis';
import { MEASURE_ID, TOTAL_ID } from '../../stores/Store';

function replaceNullAndUndefined(val) {
  if (val === null || val === undefined) {
    return '';
  }
  return val;
}

function getHeaderSize(sizeAndPositionManager, index, span) {
  let res = 0;
  for (let i = 0; i < span; i += 1) {
    res += sizeAndPositionManager.getSizeAndPositionOfCell(index + i).size;
  }
  return res;
}

function getSelectedText({ selectedCellStart, selectedCellEnd, store }) {
  const { columnsUi, rowsUi } = store;

  // Build rows headers array
  const rowsRange = [
    Math.min(selectedCellStart[0], selectedCellEnd[0]),
    Math.max(selectedCellStart[0], selectedCellEnd[0]) + 1];
  const rowHeaderLeafs = rowsUi.headers.slice(...rowsRange)
    .map(headers => headers[headers.length - 1]);
  const rows = rowHeaderLeafs.map((header) => {
    const res = [];
    let currentHeader = header;
    while (currentHeader) {
      res.unshift(currentHeader.caption);
      currentHeader = currentHeader.parent;
    }
    return res;
  });

  // Build columns headers array
  const columnsRange = [
    Math.min(selectedCellStart[1], selectedCellEnd[1]),
    Math.max(selectedCellStart[1], selectedCellEnd[1]) + 1];
  const columnHeaderLeafs = columnsUi.headers.slice(...columnsRange)
    .map(headers => headers[headers.length - 1]);
  const columns = columnHeaderLeafs.map((header) => {
    const res = [];
    let currentHeader = header;
    while (currentHeader) {
      res.unshift(currentHeader.caption);
      currentHeader = currentHeader.parent;
    }
    return res;
  });

  // Build data array
  const cells = rowHeaderLeafs.map(rowHeader =>
    columnHeaderLeafs
      .map(columnHeader => new DataCell(store, true, rowHeader, columnHeader).caption));
  const rowDimensions = rowsUi.dimensionHeaders.map(header => header.value.caption);
  const columnDimensions = columnsUi.dimensionHeaders.map(header => header.value.caption);

  // Format data to text
  let output = '';
  // First rows with only the dimension and columns headers
  const depth = columns[0].length;
  const width = rows[0].length;
  for (let y = 0; y < depth; y += 1) {
    for (let x = 0; x < width; x += 1) {
      if (x === width - 1 && y < depth - 1) {
        output += `${replaceNullAndUndefined(columnDimensions[y])}\t`;
      } else if (y === depth - 1 && x < width - 1) {
        output += `${replaceNullAndUndefined(rowDimensions[x])}\t`;
      } else if (y === depth - 1 && x === width - 1) {
        // Handle corner case
        // Dimension header in bottom right cell can refer to a column header
        // or a row header depending on data headers location
        if (store.config.dataHeadersLocation === 'columns') {
          output += `${replaceNullAndUndefined(rowDimensions[x])}\t`;
        } else {
          output += `${replaceNullAndUndefined(columnDimensions[y])}\t`;
        }
      } else {
        output += '\t';
      }
    }
    output = columns.reduce((column, current) => `${current}+${replaceNullAndUndefined(column[y])}\t`);
    output = output.slice(0, -1);
    output += '\n';
  }
  // Other rows with rows headers and data
  for (let y = 0; y < rows.length; y += 1) {
    for (let x = 0; x < width; x += 1) {
      output += `${replaceNullAndUndefined(rows[y][x])}\t`;
    }
    for (let x = 0; x < columnHeaderLeafs.length; x += 1) {
      output += `${replaceNullAndUndefined(cells[y][x])}\t`;
    }
    output = output.slice(0, -1);
    output += '\n';
  }
  output = output.slice(0, -1);
  return output;
}

function getLeafSubheaders(header, result) {
  if (header.subheaders && header.subheaders.length) {
    header.subheaders.forEach(subheader => getLeafSubheaders(subheader, result));
    return result;
  }
  result.push(header);
  return result;
}

export class Grid extends Component {
  constructor(props) {
    super(props);
    const { store } = props;

    this.state = {
      rowVerticalCount: store.layout.rowVerticalCount,
      rowHorizontalCount: store.layout.rowHorizontalCount,
      columnVerticalCount: store.layout.columnVerticalCount,
      columnHorizontalCount: store.layout.columnHorizontalCount,
      cellsCache: { },
      selectedCellStart: null,
      selectedCellEnd: null,
    };

    this.scrollLeft = 0;
    this.scrollTop = 0;

    this.isMouseDown = false;

    this.cellRangeRenderer = this.cellRangeRenderer.bind(this);
    this.dataCellRenderer = this.dataCellRenderer.bind(this);
    this.headerRenderer = this.headerRenderer.bind(this);
    this.dimensionHeaderRenderer = this.dimensionHeaderRenderer.bind(this);
    this.handleMouseDown = this.handleMouseDown.bind(this);
    this.handleMouseUp = this.handleMouseUp.bind(this);
    this.handleMouseOver = this.handleMouseOver.bind(this);
    this.handleKeyDown = this.handleKeyDown.bind(this);
    this.handleDocumentMouseDown = this.handleDocumentMouseDown.bind(this);
    this.handleCopy = this.handleCopy.bind(this);
  }

  componentDidMount() {
    document.addEventListener('mouseup', this.handleMouseUp);
    document.addEventListener('mousedown', this.handleDocumentMouseDown);
    document.addEventListener('copy', this.handleCopy);
    document.addEventListener('keydown', this.handleKeyDown);
  }


  componentWillReceiveProps(nextProps) {
    // Change scroll values to stay at the same position when modifying the layout
    this.scrollLeft = this.gridRef.state.scrollLeft
      * (this.props.store.layout.columnHorizontalCount / this.state.columnHorizontalCount);
    this.scrollTop = this.gridRef.state.scrollTop
      * (this.props.store.layout.rowVerticalCount / this.state.rowVerticalCount);

    this.setState({
      rowVerticalCount: nextProps.store.layout.rowVerticalCount,
      rowHorizontalCount: nextProps.store.layout.rowHorizontalCount,
      columnVerticalCount: nextProps.store.layout.columnVerticalCount,
      columnHorizontalCount: nextProps.store.layout.columnHorizontalCount,
      cellsCache: this.datacellsCache || { },
    });
  }

  componentWillUpdate() {
    this.isUpdating = true;
    // Clean cache for cell sizes
    // Call forceUpdate on the grid, so cannot be done in render
    this.gridRef.recomputeGridSize();
  }

  componentDidUpdate() {
    this.isUpdating = false;
  }

  componentDidUnMount() {
    document.removeEventListener('mouseup', this.handleMouseUp);
    document.removeEventListener('mousedown', this.handleDocumentMouseDown);
    document.removeEventListener('copy', this.handleCopy);
    document.removeEventListener('keydown', this.handleKeyDown);
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
    if (e.which === 65 && (e.metaKey || e.ctrlKey)) {
      if (findDOMNode(this.gridRef) === e.target) {
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
      if (findDOMNode(this.gridRef) === document.activeElement) {
        const bodyElement = document.getElementsByTagName('body')[0];
        const clipboardTextArea = document.createElement('textarea');
        clipboardTextArea.style.position = 'absolute';
        clipboardTextArea.style.left = '-10000px';
        bodyElement.appendChild(clipboardTextArea);
        const { selectedCellStart, selectedCellEnd } = this.state;
        clipboardTextArea.innerHTML = getSelectedText({
          selectedCellStart,
          selectedCellEnd,
          store: this.props.store,
        });
        clipboardTextArea.select();
        window.setTimeout(() => { bodyElement.removeChild(clipboardTextArea); }, 0);
      }
    } catch (error) {
      console.error('error in handleCopy', error);
    }
  }

  cellRangeRenderer({
    cellCache,
    columnSizeAndPositionManager,
    columnStartIndex,
    columnStopIndex,
    horizontalOffsetAdjustment,
    isScrolling,
    rowSizeAndPositionManager,
    rowStartIndex,
    rowStopIndex,
    scrollLeft,
    scrollTop,
    verticalOffsetAdjustment,
   }) {
    const { columnVerticalCount, rowHorizontalCount } = this.state;
    const { store } = this.props;
    const { columnsUi, rowsUi, layout } = store;
    const columnHeaders = columnsUi.headers;
    const rowHeaders = rowsUi.headers;
    const columnDimensionHeaders = columnsUi.dimensionHeaders;
    const rowDimensionHeaders = rowsUi.dimensionHeaders;


    const renderedCells = [];

    // Because of the offset caused by the fixed headers,
    // we have to make the cell count artificially higher.
    // This ensures that we don't render inexistent headers.
    const correctColumnStopIndex = Math.min(
      columnStopIndex - layout.rowHorizontalCount,
      columnHeaders.length - 1);
    const correctRowStopIndex = Math.min(
      rowStopIndex - layout.columnVerticalCount,
      rowHeaders.length - 1);

    // Top-left corner piece
    renderedCells.push(
      <div
        key="fixed-fixed"
        className={'OrbGrid-cell'}
        style={{
          position: 'fixed',
          left: scrollLeft,
          top: scrollTop,
          width: this.props.store.sizes.rowHeadersWidth,
          height: this.props.store.sizes.columnHeadersHeight,
          zIndex: 2,
          backgroundColor: '#fff',
        }}
      />);

    // Render dimension headers

    // Get width for column dimension headers
    let fieldWhoseWidthToGet;
    if (store.config.dataHeadersLocation === 'rows') {
      // Dimension headers are on top of the measures column
      fieldWhoseWidthToGet = MEASURE_ID;
    } else if (store.rows.fields.length) {
      // Dimension headers are on top of the column of the last field of the row headers
      fieldWhoseWidthToGet = store.rows.fields[store.rows.fields.length - 1].code;
    } else {
      // Dimension headers are on top of the Total header --> get default width
      fieldWhoseWidthToGet = null;
    }
    const width = this.props.store.getDimensionSize(AxisType.ROWS, fieldWhoseWidthToGet);
    const left = scrollLeft + (this.props.store.sizes.rowHeadersWidth - width);
    renderedCells.push(
      ...columnDimensionHeaders.map((dimensionHeader) => {
        const field = dimensionHeader.value;
        const top = scrollTop + this.props.store.dimensionPositions.columns[field.code];
        const height = this.props.store.getDimensionSize(AxisType.COLUMNS, field.code);
        return this.dimensionHeaderRenderer({ left, top, width, height, field, mainDirection: 'right', crossFieldCode: fieldWhoseWidthToGet, scrollLeft, scrollTop });
      }));

    // Get height for row dimension headers in different cases
    let fieldWhoseHeightToGet;
    if (store.config.dataHeadersLocation === 'columns') {
      // Dimension headers are to the left of the measures row
      fieldWhoseHeightToGet = MEASURE_ID;
    } else if (store.columns.fields.length) {
      // Dimension headers are to the left of the row of the last field of the column headers
      fieldWhoseHeightToGet = store.columns.fields[store.columns.fields.length - 1].code;
    } else {
      // Dimension headers are to the left of the Total header --> get default height
      fieldWhoseHeightToGet = null;
    }
    const height = this.props.store.getDimensionSize(AxisType.COLUMNS, fieldWhoseHeightToGet);
    const top = scrollTop + (this.props.store.sizes.columnHeadersHeight - height);
    renderedCells.push(
      ...rowDimensionHeaders.map((dimensionHeader) => {
        const field = dimensionHeader.value;
        const left = scrollLeft + this.props.store.dimensionPositions.rows[field.code];
        const width = this.props.store.getDimensionSize(AxisType.ROWS, field.code);
        return this.dimensionHeaderRenderer({
          left,
          top,
          height,
          width,
          field,
          mainDirection: 'down',
          crossFieldCode: fieldWhoseHeightToGet,
          scrollLeft,
          scrollTop,
        });
      }));

    // Render fixed header rows

    // Render big cells on top of current cells if necessary
    // The check on the presence of the header is necessary
    // because it can be out of bounds when the headers array is modified
    if (columnHeaders[columnStartIndex]
      && columnHeaders[columnStartIndex].length < columnVerticalCount) {
      let header = columnHeaders[columnStartIndex][0];
      while (header.parent) {
        header = header.parent;
        const main = columnSizeAndPositionManager.getSizeAndPositionOfCell(header.x);
        const left = main.offset
        + horizontalOffsetAdjustment + this.props.store.sizes.rowHeadersWidth;
        const span = header.hspan();
        const width = getHeaderSize(columnSizeAndPositionManager, header.x, span);
        const top = scrollTop
        + this.props.store.dimensionPositions.columns[header.dim.field.code];
        const height = this.props.store.getDimensionSize(AxisType.COLUMNS, header.dim.field.code);
        const positionStyle = {
          position: 'fixed',
          left,
          top,
          height,
          width,
        };
        renderedCells.push(this.headerRenderer({
          axis: AxisType.COLUMNS,
          header,
          positionStyle,
          span,
          startIndex: rowStartIndex,
          scrollLeft,
          scrollTop,
        }));
      }
    }

    for (
      let columnIndex = columnStartIndex;
      columnIndex <= correctColumnStopIndex;
      columnIndex += 1
    ) {
      const main = columnSizeAndPositionManager.getSizeAndPositionOfCell(columnIndex);
      const left = main.offset + horizontalOffsetAdjustment
      + this.props.store.sizes.rowHeadersWidth;
      renderedCells.push(
        ...columnHeaders[columnIndex].map((header) => {
          const span = header.hspan();
          const width = getHeaderSize(columnSizeAndPositionManager, columnIndex, span);
          // 3 cases: normal dimension header, measure header or total header
          let top = scrollTop;
          let height;
          if (!header.dim) {
            // Measure header
            height = this.props.store.getDimensionSize(AxisType.COLUMNS, MEASURE_ID);
            top += this.props.store.dimensionPositions.columns[MEASURE_ID];
          } else if (header.dim.field) {
            // Normal dimension header
            height = this.props.store.getDimensionSize(AxisType.COLUMNS, header.dim.field.code);
            top += this.props.store.dimensionPositions.columns[header.dim.field.code];
          } else {
            // Total header
            height = this.props.store.getDimensionSize(AxisType.COLUMNS, TOTAL_ID);
          }
          const positionStyle = {
            position: 'fixed',
            left,
            top,
            height,
            width,
          };
          return this.headerRenderer({
            axis: AxisType.COLUMNS,
            header,
            positionStyle,
            span,
            startIndex: columnStartIndex,
            scrollLeft,
            scrollTop,
          });
        }));
    }

    // Render fixed left columns

    // Render big cells on the left of current cells if necessary
    // The check on the presence of the header is necessary
    // because it can be out of bounds when the headers array is modified
    if (rowHeaders[rowStartIndex] && rowHeaders[rowStartIndex].length < rowHorizontalCount) {
      let header = rowHeaders[rowStartIndex][0];
      while (header.parent) {
        header = header.parent;
        const main = rowSizeAndPositionManager.getSizeAndPositionOfCell(header.x);
        const span = header.vspan();
        const top = main.offset + verticalOffsetAdjustment
        + this.props.store.sizes.columnHeadersHeight;
        const height = getHeaderSize(rowSizeAndPositionManager, header.x, span);
        const width = this.props.store.getDimensionSize(AxisType.ROWS, header.dim.field.code);
        const left = scrollLeft + this.props.store.dimensionPositions.rows[header.dim.field.code];
        const positionStyle = {
          position: 'fixed',
          left,
          top,
          height,
          width,
        };
        renderedCells.push(this.headerRenderer({
          axis: AxisType.ROWS,
          header,
          positionStyle,
          span,
          startIndex: rowStartIndex,
          scrollLeft,
          scrollTop,
        }));
      }
    }

    for (let rowIndex = rowStartIndex; rowIndex <= correctRowStopIndex; rowIndex += 1) {
      const main = rowSizeAndPositionManager.getSizeAndPositionOfCell(rowIndex);
      const top = main.offset + verticalOffsetAdjustment
      + this.props.store.sizes.columnHeadersHeight;
      renderedCells.push(
        ...rowHeaders[rowIndex].map((header) => {
          const span = header.vspan();
          const height = getHeaderSize(rowSizeAndPositionManager, rowIndex, span);
          // 3 cases: normal dimension header, measure header or total header
          let width;
          let left = scrollLeft;
          if (!header.dim) {
            // Measure header
            width = this.props.store.getDimensionSize(AxisType.ROWS, MEASURE_ID);
            left += this.props.store.dimensionPositions.rows[MEASURE_ID];
          } else if (header.dim.field) {
            // Normal dimension header
            width = this.props.store.getDimensionSize(AxisType.ROWS, header.dim.field.code);
            left += this.props.store.dimensionPositions.rows[header.dim.field.code];
          } else {
            // Total header
            width = this.props.store.getDimensionSize(AxisType.ROWS, TOTAL_ID);
          }
          const positionStyle = {
            position: 'fixed',
            left,
            top,
            height,
            width,
          };
          return (this.headerRenderer({
            axis: AxisType.ROWS,
            header,
            positionStyle,
            span,
            startIndex: rowStartIndex,
            scrollLeft,
            scrollTop,
          }));
        }));
    }

    // Render data cells
    this.datacellsCache = { };

    // if (!isScrolling) {
    for (let rowIndex = rowStartIndex; rowIndex <= correctRowStopIndex; rowIndex += 1) {
      const rowDatum = rowSizeAndPositionManager.getSizeAndPositionOfCell(rowIndex);
      for (
        let columnIndex = columnStartIndex;
        columnIndex <= correctColumnStopIndex;
        columnIndex += 1
      ) {
        const columnDatum = columnSizeAndPositionManager.getSizeAndPositionOfCell(columnIndex);
        const key = `${rowIndex}-${columnIndex}`;
        const positionStyle = {
          height: rowDatum.size,
          // The modulos allow discrete scrolling
          // left: columnDatum.offset + this.props.store.sizes.rowHeadersWidth
          // + horizontalOffsetAdjustment + (scrollLeft % this.defaultCellWidth),
          // top: rowDatum.offset + this.props.store.sizes.columnHeadersHeight
          // + verticalOffsetAdjustment + (scrollTop % this.defaultCellHeight)
          left: columnDatum.offset + horizontalOffsetAdjustment
          + this.props.store.sizes.rowHeadersWidth,
          top: rowDatum.offset + verticalOffsetAdjustment
          + this.props.store.sizes.columnHeadersHeight,
          position: 'fixed',
          width: columnDatum.size,
        };
        const dataCellRendererParams = {
          key,
          columnIndex,
          rowIndex,
          positionStyle,
        };

        // let renderedCell;
        // if (
        //   isScrolling &&
        //   !horizontalOffsetAdjustment &&
        //   !verticalOffsetAdjustment
        // ) {
        //   if (!cellCache[key]) {
        //     cellCache[key] = this.dataCellRenderer(dataCellRendererParams);
        //   }
        //   renderedCell = cellCache[key];
        // } else {
        //   renderedCell = this.dataCellRenderer(dataCellRendererParams);
        // }

        const renderedCell = this.dataCellRenderer(dataCellRendererParams);

        renderedCells.push(renderedCell);
      }
      // }
    }
    return renderedCells;
  }

  dataCellRenderer({
    columnIndex,
    key,
    rowIndex,
    positionStyle,
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
        handleMouseDown={this.handleMouseDown}
        handleMouseOver={this.handleMouseOver}
      />
    );
  }

  headerRenderer({
    axis,
    header,
    positionStyle,
    span,
    startIndex,
    scrollLeft,
    scrollTop,
   }) {
    const { left, top, width, height } = positionStyle;
    const { x, y } = header;
    const renderedCell = <HeaderCellComponent key={`${axis}-${x}-${y}`} cell={header} />;
    let innerHeader = renderedCell;
    const previewSize = { };
    if (axis === AxisType.COLUMNS) {
      previewSize.right = this.props.store.sizes.columnHeadersHeight;
      previewSize.bottom = this.width;
    } else {
      previewSize.right = this.height;
      previewSize.bottom = this.props.store.sizes.rowHeadersWidth;
    }
    // Handle affix
    if (span > 1 && x <= startIndex) {
      let style;
      let offset;
      const lastChildSize = this.props.store.getLastChildSize(axis, header);
      if (axis === AxisType.COLUMNS) {
        offset = Math.min(scrollLeft + (this.props.store.sizes.rowHeadersWidth - left),
          width - (lastChildSize || 0));
        style = { position: 'relative', left: offset };
      } else {
        offset = Math.min(scrollTop + (this.props.store.sizes.columnHeadersHeight - top),
          height - (lastChildSize || 0));
        style = { position: 'relative', top: offset };
      }
      innerHeader = <div style={style}>{renderedCell}</div>;
    }
    const leafHeaderId = header.key;
    let dimensionId;
    if (!header.dim) {
      // Measure header
      dimensionId = MEASURE_ID;
    } else if (header.dim.field) {
      // Normal header
      dimensionId = header.dim.field.code;
    } else {
      // Total header
      dimensionId = TOTAL_ID;
    }
    const leafSubheaders = header.subheaders ? getLeafSubheaders(header, []) : [];
    return (
      <div
        key={`fixed-${axis}-${x}-${y}`}
        className={'OrbGrid-cell OrbGrid-cell-header'}
        style={{
          boxSizing: 'border-box',
          overflow: 'hidden',
          border: 'solid lightgrey thin',
          backgroundColor: '#eef8fb',
          zIndex: 1,
          display: 'flex',
          ...positionStyle,
        }}
      >
        {innerHeader}
        <ResizeHandle
          position="right"
          size={height}
          id={axis === AxisType.COLUMNS ? leafHeaderId : dimensionId}
          leafSubheaders={leafSubheaders}
          axis={axis}
          previewSize={previewSize.right}
          previewOffset={top - scrollTop}
        />
        <ResizeHandle
          position="bottom"
          size={width}
          id={axis === AxisType.ROWS ? leafHeaderId : dimensionId}
          leafSubheaders={leafSubheaders}
          axis={axis}
          previewSize={previewSize.bottom}
          previewOffset={left - scrollLeft}
        />
      </div>
    );
  }

  dimensionHeaderRenderer({
    field,
    left,
    top,
    height,
    width,
    crossFieldCode,
    mainDirection,
    scrollLeft,
    scrollTop,
   }) {
    const ids = {};
    if (mainDirection === 'down') {
      ids.right = field.code;
      ids.bottom = crossFieldCode;
    } else {
      ids.bottom = field.code;
      ids.right = crossFieldCode;
    }
    return (
      <div
        key={`fixed-dim-${field.code}`}
        className={'OrbGrid-cell'}
        style={{
          position: 'fixed',
          left,
          top,
          width,
          height,
          zIndex: 3,
          border: 'lightgrey 0.1em solid',
          boxSizing: 'border-box',
          textAlign: 'left',
          display: 'flex',
          backgroundColor: '#fafad2',
        }}
      >
        <span>{ field.caption }</span>
        <ResizeHandle
          position="right"
          size={height}
          id={ids.right}
          isOnDimensionHeader
          axis={AxisType.ROWS}
          previewSize={this.height}
          previewOffset={top - scrollTop}
        />
        <ResizeHandle
          position="bottom"
          size={width}
          id={ids.bottom}
          isOnDimensionHeader
          axis={AxisType.COLUMNS}
          previewSize={this.width}
          previewOffset={left - scrollLeft}
        />
      </div>
    );
  }

  render() {
    const { connectDropTarget, store } = this.props;
    const {
      columnHorizontalCount,
      columnVerticalCount,
      rowHorizontalCount,
      rowVerticalCount,
    } = this.state;
    return connectDropTarget(
      <div style={{ height: 'inherit' }}>
        <DragLayer />
        <AutoSizer>
          {({ width, height }) => {
            // Add 15 pixels to avoid that the grid go under a scrollbar
            // We should use the scrollbar size instead on Windows and Linux and nothing on macOs
            // TO SEE
            this.height = Math.min(height,
            store.sizes.rowHeadersHeight + store.sizes.columnHeadersHeight + 15);
            this.width = Math.min(width,
            store.sizes.columnHeadersWidth + store.sizes.rowHeadersWidth + 15);
            return (
              <ReactVirtualizedGrid
                cellRangeRenderer={this.cellRangeRenderer}
                cellRenderer={() => 33}
                columnCount={columnHorizontalCount + rowHorizontalCount}
                columnWidth={store.getColumnWidth.bind(store)}
                height={this.height}
                overscanRowCount={0}
                overscanColumnCount={0}
                ref={(ref) => { this.gridRef = ref; }}
                rowCount={rowVerticalCount + columnVerticalCount}
                rowHeight={store.getRowHeight.bind(store)}
                scrollLeft={this.scrollLeft}
                scrollTop={this.scrollTop}
                style={{ fontSize: `${this.props.store.zoom * 100}%` }}
                width={this.width}
              />);
          }
          }
        </AutoSizer>
      </div>);
  }

 }


const gridSpec = {
  drop(props, monitor, component) {
    const handle = monitor.getItem();
    const initialOffset = monitor.getInitialClientOffset();
    const offset = monitor.getClientOffset();
    component.props.store.updateCellSizes(handle, offset, initialOffset);
  },
};

const collect = connect => ({
  connectDropTarget: connect.dropTarget(),
});

export default DropTarget('cell-resize-handle', gridSpec, collect)(Grid);
