import React, { Component } from 'react';
import { findDOMNode } from 'react-dom';
import { Grid as ReactVirtualizedGrid, ScrollSync } from 'react-virtualized';
import { DropTarget } from 'react-dnd';

import HeaderCellComponent from '../HeaderCell';
import DataCells from '../DataCells';
import DimensionHeaders from '../DimensionHeaders';
import DragLayer from './DragLayer';
import ResizeHandle from '../ResizeHandle';
import { AxisType } from '../../Axis';
import { MEASURE_ID, TOTAL_ID } from '../../stores/Store';
import { scrollbarSize } from '../../Utils.dom';
import copy from '../../services/copyService';

function getLeafSubheaders(header, result) {
  if (header.subheaders && header.subheaders.length) {
    header.subheaders.forEach(subheader => getLeafSubheaders(subheader, result));
    return result;
  }
  result.push(header);
  return result;
}

function getHeaderSize(sizeAndPositionManager, index, span) {
  let res = 0;
  for (let i = 0; i < span; i += 1) {
    res += sizeAndPositionManager.getSizeAndPositionOfCell(index + i).size;
  }
  return res;
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

    this.columnHeadersRenderer = this.columnHeadersRenderer.bind(this);
    this.rowHeadersRenderer = this.rowHeadersRenderer.bind(this);
    this.headerRenderer = this.headerRenderer.bind(this);
    this.handleCopy = this.handleCopy.bind(this);
    this.handleMouseDown = this.handleMouseDown.bind(this);
    this.handleMouseUp = this.handleMouseUp.bind(this);
    this.handleMouseOver = this.handleMouseOver.bind(this);
    this.handleKeyDown = this.handleKeyDown.bind(this);
    this.handleDocumentMouseDown = this.handleDocumentMouseDown.bind(this);
    store.getColumnWidth = store.getColumnWidth.bind(store);
    store.getRowHeight = store.getRowHeight.bind(store);
  }

  componentDidMount() {
    document.addEventListener('mouseup', this.handleMouseUp);
    document.addEventListener('mousedown', this.handleDocumentMouseDown);
    document.addEventListener('keydown', this.handleKeyDown);
    document.addEventListener('copy', this.handleCopy);
  }


  componentWillReceiveProps(nextProps) {
    // // Change scroll values to stay at the same position when modifying the layout
    // // The current implementation only works when all cells have the same size
    // // A better implementation would be to find which cells are at the beginning
    // // upon receiving props and jumping there after
    // this.scrollLeft = this.dataCellsRef.state.scrollLeft
    //   * (this.props.store.layout.columnHorizontalCount / this.state.columnHorizontalCount);
    // this.scrollTop = this.dataCellsRef.state.scrollTop
    //   * (this.props.store.layout.rowVerticalCount / this.state.rowVerticalCount);

    this.setState({
      rowVerticalCount: nextProps.store.layout.rowVerticalCount,
      rowHorizontalCount: nextProps.store.layout.rowHorizontalCount,
      columnVerticalCount: nextProps.store.layout.columnVerticalCount,
      columnHorizontalCount: nextProps.store.layout.columnHorizontalCount,
      cellsCache: this.datacellsCache || { },
    });
  }

  componentWillUpdate() {
    // this.isUpdating = true;
    // Clean cache for cell sizes
    // Call forceUpdate on the grid, so cannot be done in render
    this.columnHeadersRef.recomputeGridSize();
    this.rowHeadersRef.recomputeGridSize();
    this.dataCellsRef.grid.recomputeGridSize();
  }

  componentDidUpdate() {
    // this.isUpdating = false;
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
    if (e.which === 65 && (e.metaKey || e.ctrlKey)) {
      if (
        findDOMNode(this.dataCellsRef) === e.target
        || findDOMNode(this.columnHeadersRef) === e.target
        || findDOMNode(this.rowHeadersRef) === e.target
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
        findDOMNode(this.dataCellsRef) === document.activeElement
        || findDOMNode(this.columnHeadersRef) === document.activeElement
        || findDOMNode(this.rowHeadersRef) === document.activeElement
      ) {
        const { selectedCellStart, selectedCellEnd } = this.state;
        copy({ selectedCellStart, selectedCellEnd, store: this.props.store });
      }
    } catch (error) {
      // console.error('error in handleCopy', error);
    }
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
    const previewOffset = { };
    if (axis === AxisType.COLUMNS) {
      previewOffset.right = top - scrollTop;
      previewOffset.bottom = (left - scrollLeft) + this.props.store.sizes.rowHeadersWidth;
    } else {
      previewOffset.right = (top - scrollTop) + this.props.store.sizes.columnHeadersHeight;
      previewOffset.bottom = left - scrollLeft;
    }
    // Handle affix
    let style;
    if (span > 1 && x <= startIndex) {
      let offset;
      const lastChildSize = this.props.store.getLastChildSize(axis, header);
      if (axis === AxisType.COLUMNS) {
        offset = Math.min(scrollLeft - left, width - (lastChildSize || 0));
        style = { position: 'relative', left: offset };
      } else {
        offset = Math.min(scrollTop - top, height - (lastChildSize || 0));
        style = { position: 'relative', top: offset };
      }
    }
    const innerHeader = <HeaderCellComponent key={`${axis}-${x}-${y}`} cell={header} style={style} />;
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
          previewSize={this.height}
          previewOffset={previewOffset.right}
        />
        <ResizeHandle
          position="bottom"
          size={width}
          id={axis === AxisType.ROWS ? leafHeaderId : dimensionId}
          leafSubheaders={leafSubheaders}
          axis={axis}
          previewSize={this.width}
          previewOffset={previewOffset.bottom}
        />
      </div>
    );
  }

  columnHeadersRenderer({
    columnSizeAndPositionManager,
    columnStartIndex,
    columnStopIndex,
    horizontalOffsetAdjustment,
    scrollLeft,
    scrollTop,
   }) {
    const { columnVerticalCount } = this.state;
    const { store } = this.props;
    const { columnsUi } = store;
    const columnHeaders = columnsUi.headers;


    const renderedCells = [];

    // Because of the offset caused by the fixed headers,
    // we have to make the cell count artificially higher.
    // This ensures that we don't render inexistent headers.
    const correctColumnStopIndex = Math.min(
      columnStopIndex,
      columnHeaders.length - 1);

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
        const left = main.offset + horizontalOffsetAdjustment;
        const span = header.hspan();
        const width = getHeaderSize(columnSizeAndPositionManager, header.x, span);
        const top = scrollTop
        + this.props.store.dimensionPositions.columns[header.dim.field.code];
        const height = this.props.store.getDimensionSize(AxisType.COLUMNS, header.dim.field.code);
        const positionStyle = {
          position: 'absolute',
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
          startIndex: columnStartIndex,
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
      const left = main.offset + horizontalOffsetAdjustment;
      // + this.props.store.sizes.rowHeadersWidth;
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
            position: 'absolute',
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
    return renderedCells;
  }

  rowHeadersRenderer({
    // cellCache,
    // columnSizeAndPositionManager,
    // columnStartIndex,
    // columnStopIndex,
    // horizontalOffsetAdjustment,
    // isScrolling,
    rowSizeAndPositionManager,
    rowStartIndex,
    rowStopIndex,
    scrollLeft,
    scrollTop,
    verticalOffsetAdjustment,
   }) {
    const { rowHorizontalCount } = this.state;
    const { store } = this.props;
    const { rowsUi } = store;
    const rowHeaders = rowsUi.headers;


    const renderedCells = [];

    // Because of the offset caused by the fixed headers,
    // we have to make the cell count artificially higher.
    // This ensures that we don't render inexistent headers.
    const correctRowStopIndex = Math.min(
      rowStopIndex,
      rowHeaders.length - 1);


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
        const top = main.offset + verticalOffsetAdjustment;
        const height = getHeaderSize(rowSizeAndPositionManager, header.x, span);
        const width = this.props.store.getDimensionSize(AxisType.ROWS, header.dim.field.code);
        const left = scrollLeft + this.props.store.dimensionPositions.rows[header.dim.field.code];
        const positionStyle = {
          position: 'absolute',
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
      const top = main.offset + verticalOffsetAdjustment;
      // + this.props.store.sizes.columnHeadersHeight;
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
            position: 'absolute',
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
    return renderedCells;
  }

  render() {
    const { connectDropTarget, store } = this.props;
    const {
      columnHeadersHeight,
      columnHeadersWidth,
      rowHeadersHeight,
      rowHeadersWidth,
     } = store.sizes;
    const {
      columnHorizontalCount,
      columnVerticalCount,
      rowHorizontalCount,
      rowVerticalCount,
    } = this.state;
    const height = this.props.height !== undefined ? this.props.height : store.config.height;
    const width = this.props.width !== undefined ? this.props.width : store.config.width;
    const hasScrollbarAtBottom = width
    < columnHeadersWidth + rowHeadersWidth + scrollbarSize();
    const hasScrollbarAtRight = height
    < columnHeadersHeight + rowHeadersHeight + scrollbarSize();
    const rowHeadersVisibleHeight = Math.min(
      height - columnHeadersHeight - (hasScrollbarAtBottom ? scrollbarSize() : 0),
      rowHeadersHeight);
    const columnHeadersVisibleWidth = Math.min(
      width - rowHeadersWidth - (hasScrollbarAtRight ? scrollbarSize() : 0),
      columnHeadersWidth);
    const previewSizes = {};
    previewSizes.height = Math.min(height - (hasScrollbarAtBottom ? scrollbarSize() : 0),
      rowHeadersHeight + columnHeadersHeight);
    previewSizes.width = Math.min(width - (hasScrollbarAtRight ? scrollbarSize() : 0),
      columnHeadersWidth + rowHeadersWidth);

    return connectDropTarget(
      <div>
        <DragLayer />
        <ScrollSync>
          {({ onScroll, scrollLeft, scrollTop }) => {
            this.datacellsCache = {};
            return (
              <div>
                <div style={{ display: 'flex' }}>
                  <div style={{ height: columnHeadersHeight, width: rowHeadersWidth }}>
                    <DimensionHeaders
                      store={store}
                      scrollLeft={scrollLeft}
                      scrollTop={scrollTop}
                      previewSizes={previewSizes}
                    />
                  </div>
                  {/* Column headers */}
                  <ReactVirtualizedGrid
                    cellRangeRenderer={this.columnHeadersRenderer}
                    cellRenderer={function mock() {}}
                    className="OrbGrid-column-headers"
                    columnCount={columnHorizontalCount}
                    columnWidth={store.getColumnWidth}
                    height={columnHeadersHeight}
                    overscanColumnCount={0}
                    ref={(ref) => { this.columnHeadersRef = ref; }}
                    rowCount={columnVerticalCount}
                    rowHeight={store.getRowHeight}
                    scrollLeft={scrollLeft}
                    // We set overflowX and overflowY and not overflow
                    // because react-virtualized sets them during render
                    style={{ fontSize: `${this.props.store.zoom * 100}%`, overflowX: 'hidden', overflowY: 'hidden' }}
                    width={columnHeadersVisibleWidth}
                  />
                </div>
                <div style={{ display: 'flex' }}>
                  {/* Row headers */}
                  <ReactVirtualizedGrid
                    cellRangeRenderer={this.rowHeadersRenderer}
                    cellRenderer={function mock() {}}
                    className="OrbGrid-row-headers"
                    columnCount={rowHorizontalCount}
                    columnWidth={store.getColumnWidth}
                    height={rowHeadersVisibleHeight}
                    overscanRowCount={0}
                    ref={(ref) => { this.rowHeadersRef = ref; }}
                    rowCount={rowVerticalCount}
                    rowHeight={store.getRowHeight}
                    scrollTop={scrollTop}
                    // We set overflowX and overflowY and not overflow
                    // because react-virtualized sets them during render
                    style={{ fontSize: `${this.props.store.zoom * 100}%`, overflowX: 'hidden', overflowY: 'hidden' }}
                    width={rowHeadersWidth}
                  />
                  <DataCells
                    onScroll={onScroll}
                    store={store}
                    columnCount={columnHorizontalCount}
                    rowCount={rowVerticalCount}
                    ref={(ref) => { this.dataCellsRef = ref; }}
                    height={Math.min(height - columnHeadersHeight,
                      rowHeadersHeight + scrollbarSize())}
                    width={Math.min(width - rowHeadersWidth,
                      columnHeadersWidth + scrollbarSize())}
                    handleMouseDown={this.handleMouseDown}
                    handleMouseOver={this.handleMouseOver}
                    selectedCellStart={this.state.selectedCellStart}
                    selectedCellEnd={this.state.selectedCellEnd}
                  />
                </div>
              </div>
            );
          }
         }
        </ScrollSync>
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
