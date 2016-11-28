import React, { PureComponent } from 'react';
import { Grid as ReactVirtualizedGrid } from 'react-virtualized';

import { AxisType } from '../../Axis';
import { MEASURE_ID, TOTAL_ID } from '../../constants';
import Header from '../Header';
import getHeaderSize from '../../utils/headerSize';

class ColumnHeaders extends PureComponent {
  constructor() {
    super();
    this.columnHeadersRenderer = this.columnHeadersRenderer.bind(this);
  }

  componentDidUpdate() {
    this.grid.recomputeGridSize();
  }

  columnHeadersRenderer({
    columnSizeAndPositionManager,
    columnStartIndex,
    columnStopIndex,
    horizontalOffsetAdjustment,
    scrollLeft,
   }) {
    const {
      columnHeaders,
      dimensionPositions,
      getDimensionSize,
      getLastChildSize,
      previewSizes,
      rowCount,
      rowHeadersWidth,
     } = this.props;

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
        const top = 0
        + dimensionPositions.columns[header.dim.field.id];
        const height = getDimensionSize(AxisType.COLUMNS, header.dim.field.id);
        const positionStyle = {
          position: 'absolute',
          left,
          top,
          height,
          width,
        };
        const previewOffsets = { };
        previewOffsets.right = top - 0;
        previewOffsets.bottom = (left - scrollLeft) + rowHeadersWidth;
        renderedCells.push(
          <Header
            key={`header-${header.key}`}
            axis={AxisType.COLUMNS}
            header={header}
            positionStyle={positionStyle}
            span={span}
            startIndex={columnStartIndex}
            scrollLeft={scrollLeft}
            scrollTop={0}
            previewSizes={previewSizes}
            previewOffsets={previewOffsets}
            getLastChildSize={getLastChildSize}
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
      renderedCells.push(
        ...columnHeaders[columnIndex].map((header) => {
          const span = header.hspan();
          const width = getHeaderSize(columnSizeAndPositionManager, columnIndex, span);
          // 3 cases: normal dimension header, measure header or total header
          let top = 0;
          let height;
          if (!header.dim) {
            // Measure header
            height = getDimensionSize(AxisType.COLUMNS, MEASURE_ID);
            top += dimensionPositions.columns[MEASURE_ID];
          } else if (header.dim.field) {
            // Normal dimension header
            height = getDimensionSize(AxisType.COLUMNS, header.dim.field.id);
            top += dimensionPositions.columns[header.dim.field.id];
          } else {
            // Total header
            height = getDimensionSize(AxisType.COLUMNS, TOTAL_ID);
          }
          const positionStyle = {
            position: 'absolute',
            left,
            top,
            height,
            width,
          };
          const previewOffsets = { };
          previewOffsets.right = top - 0;
          previewOffsets.bottom = (left - scrollLeft) + rowHeadersWidth;
          return (
            <Header
              key={`header-${header.key}`}
              axis={AxisType.COLUMNS}
              header={header}
              positionStyle={positionStyle}
              span={span}
              startIndex={columnStartIndex}
              scrollLeft={scrollLeft}
              scrollTop={0}
              previewSizes={previewSizes}
              previewOffsets={previewOffsets}
              getLastChildSize={getLastChildSize}
            />);
        }));
    }
    return renderedCells;
  }

  render() {
    const {
      columnCount,
      getColumnWidth,
      getRowHeight,
      height,
      rowCount,
      scrollLeft,
      width,
      zoom,
    } = this.props;
    return (
      <ReactVirtualizedGrid
        cellRangeRenderer={this.columnHeadersRenderer}
        cellRenderer={function mock() {}}
        className="OrbGrid-column-headers"
        columnCount={columnCount}
        columnWidth={getColumnWidth}
        height={height}
        overscanColumnCount={0}
        ref={(ref) => { this.grid = ref; }}
        rowCount={rowCount}
        rowHeight={getRowHeight}
        scrollLeft={scrollLeft}
        scrollTop={0}
        // We set overflowX and overflowY and not overflow
        // because react-virtualized sets them during render
        style={{ fontSize: `${zoom * 100}%`, overflowX: 'hidden', overflowY: 'hidden' }}
        width={width}
      />
    );
  }
}

export default ColumnHeaders;
