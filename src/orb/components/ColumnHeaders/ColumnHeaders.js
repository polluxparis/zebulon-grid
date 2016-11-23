import React, { PureComponent } from 'react';
import { Grid as ReactVirtualizedGrid } from 'react-virtualized';

import { AxisType } from '../../Axis';
import { MEASURE_ID, TOTAL_ID } from '../../stores/Store';
import Header from '../Header';
import getHeaderSize from '../../utils/headerSize';

class ColumnHeaders extends PureComponent {
  constructor() {
    super();
    this.columnHeadersRenderer = this.columnHeadersRenderer.bind(this);
  }

  columnHeadersRenderer({
    columnSizeAndPositionManager,
    columnStartIndex,
    columnStopIndex,
    horizontalOffsetAdjustment,
    scrollLeft,
    scrollTop,
   }) {
    const { store, rowCount, previewSizes } = this.props;
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
      && columnHeaders[columnStartIndex].length < rowCount) {
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
        const previewOffsets = { };
        previewOffsets.right = top - scrollTop;
        previewOffsets.bottom = (left - scrollLeft) + store.sizes.rowHeadersWidth;
        renderedCells.push(
          <Header
            key={`header-${header.key}`}
            axis={AxisType.COLUMNS}
            header={header}
            positionStyle={positionStyle}
            span={span}
            startIndex={columnStartIndex}
            scrollLeft={scrollLeft}
            scrollTop={scrollTop}
            previewSizes={previewSizes}
            previewOffsets={previewOffsets}
            getLastChildSize={store.getLastChildSize}
          />);
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
          const previewOffsets = { };
          previewOffsets.right = top - scrollTop;
          previewOffsets.bottom = (left - scrollLeft) + store.sizes.rowHeadersWidth;
          return (
            <Header
              key={`header-${header.key}`}
              axis={AxisType.COLUMNS}
              header={header}
              positionStyle={positionStyle}
              span={span}
              startIndex={columnStartIndex}
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
    const { store, columnCount, rowCount, scrollLeft, height, width } = this.props;
    return (
      <ReactVirtualizedGrid
        cellRangeRenderer={this.columnHeadersRenderer}
        cellRenderer={function mock() {}}
        className="OrbGrid-column-headers"
        columnCount={columnCount}
        columnWidth={store.getColumnWidth}
        height={height}
        overscanColumnCount={0}
        ref={(ref) => { this.grid = ref; }}
        rowCount={rowCount}
        rowHeight={store.getRowHeight}
        scrollLeft={scrollLeft}
        // We set overflowX and overflowY and not overflow
        // because react-virtualized sets them during render
        style={{ fontSize: `${this.props.store.zoom * 100}%`, overflowX: 'hidden', overflowY: 'hidden' }}
        width={width}
      />
    );
  }
}

export default ColumnHeaders;
