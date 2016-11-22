import React, { PureComponent } from 'react';
import { Grid as ReactVirtualizedGrid } from 'react-virtualized';

import { AxisType } from '../../Axis';
import { MEASURE_ID, TOTAL_ID } from '../../stores/Store';
import Header from '../Header';
import getHeaderSize from '../../utils/headerSize';

class ColumnHeaders extends PureComponent {
  constructor() {
    super();
    this.rowHeadersRenderer = this.rowHeadersRenderer.bind(this);
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
    const { store, columnCount, previewSizes } = this.props;
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
    if (rowHeaders[rowStartIndex] && rowHeaders[rowStartIndex].length < columnCount) {
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
        const previewOffsets = { };
        previewOffsets.right = (top - scrollTop) + store.sizes.columnHeadersHeight;
        previewOffsets.bottom = left - scrollLeft;
        renderedCells.push(
          <Header
            key={`header-${header.key}?`}
            axis={AxisType.ROWS}
            header={header}
            positionStyle={positionStyle}
            span={span}
            startIndex={rowStartIndex}
            scrollLeft={scrollLeft}
            scrollTop={scrollTop}
            previewSizes={previewSizes}
            previewOffsets={previewOffsets}
            getLastChildSize={store.getLastChildSize}
          />);
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
          const previewOffsets = { };
          previewOffsets.right = (top - scrollTop) + store.sizes.columnHeadersHeight;
          previewOffsets.bottom = left - scrollLeft;
          return (
            <Header
              key={`header-${header.key}?`}
              axis={AxisType.ROWS}
              header={header}
              positionStyle={positionStyle}
              span={span}
              startIndex={rowStartIndex}
              scrollLeft={scrollLeft}
              scrollTop={scrollTop}
              previewSizes={previewSizes}
              previewOffsets={previewOffsets}
              getLastChildSize={store.getLastChildSize}
            />);
        }));
    }
    return renderedCells;
  }


  render() {
    const { store, columnCount, rowCount, scrollTop, height, width } = this.props;
    return (
      <ReactVirtualizedGrid
        cellRangeRenderer={this.rowHeadersRenderer}
        cellRenderer={function mock() {}}
        className="OrbGrid-row-headers"
        columnCount={columnCount}
        columnWidth={store.getColumnWidth}
        height={height}
        overscanColumnCount={0}
        ref={(ref) => { this.grid = ref; }}
        rowCount={rowCount}
        rowHeight={store.getRowHeight}
        scrollTop={scrollTop}
        // We set overflowX and overflowY and not overflow
        // because react-virtualized sets them during render
        style={{ fontSize: `${this.props.store.zoom * 100}%`, overflowX: 'hidden', overflowY: 'hidden' }}
        width={width}
      />
    );
  }
}

export default ColumnHeaders;
