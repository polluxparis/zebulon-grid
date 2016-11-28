import React, { PureComponent } from 'react';
import { Grid as ReactVirtualizedGrid } from 'react-virtualized';

import { AxisType } from '../../Axis';
import { MEASURE_ID, TOTAL_ID } from '../../constants';
import Header from '../Header';
import getHeaderSize from '../../utils/headerSize';

class RowHeaders extends PureComponent {
  constructor() {
    super();
    this.rowHeadersRenderer = this.rowHeadersRenderer.bind(this);
  }

  componentDidUpdate() {
    this.grid.recomputeGridSize();
  }

  rowHeadersRenderer({
    rowSizeAndPositionManager,
    rowStartIndex,
    rowStopIndex,
    scrollTop,
    verticalOffsetAdjustment,
   }) {
    const {
     rowHeaders,
     columnHeadersHeight,
     columnCount,
     previewSizes,
     getLastChildSize,
     getDimensionSize,
     dimensionPositions,
    } = this.props;

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
        const width = getDimensionSize(AxisType.ROWS, header.dim.field.id);
        const left = 0 + dimensionPositions.rows[header.dim.field.id];
        const positionStyle = {
          position: 'absolute',
          left,
          top,
          height,
          width,
        };
        const previewOffsets = { };
        previewOffsets.right = (top - scrollTop) + columnHeadersHeight;
        previewOffsets.bottom = left - 0;
        renderedCells.push(
          <Header
            key={`header-${header.key}`}
            axis={AxisType.ROWS}
            header={header}
            positionStyle={positionStyle}
            span={span}
            startIndex={rowStartIndex}
            scrollLeft={0}
            scrollTop={scrollTop}
            previewSizes={previewSizes}
            previewOffsets={previewOffsets}
            getLastChildSize={getLastChildSize}
          />);
      }
    }

    for (let rowIndex = rowStartIndex; rowIndex <= correctRowStopIndex; rowIndex += 1) {
      const main = rowSizeAndPositionManager.getSizeAndPositionOfCell(rowIndex);
      const top = main.offset + verticalOffsetAdjustment;
      renderedCells.push(
        ...rowHeaders[rowIndex].map((header) => {
          const span = header.vspan();
          const height = getHeaderSize(rowSizeAndPositionManager, rowIndex, span);
          // 3 cases: normal dimension header, measure header or total header
          let width;
          let left = 0;
          if (!header.dim) {
            // Measure header
            width = getDimensionSize(AxisType.ROWS, MEASURE_ID);
            left += dimensionPositions.rows[MEASURE_ID];
          } else if (header.dim.field) {
            // Normal dimension header
            width = getDimensionSize(AxisType.ROWS, header.dim.field.id);
            left += dimensionPositions.rows[header.dim.field.id];
          } else {
            // Total header
            width = getDimensionSize(AxisType.ROWS, TOTAL_ID);
          }
          const positionStyle = {
            position: 'absolute',
            left,
            top,
            height,
            width,
          };
          const previewOffsets = { };
          previewOffsets.right = (top - scrollTop) + columnHeadersHeight;
          previewOffsets.bottom = left - 0;
          return (
            <Header
              key={`header-${header.key}`}
              axis={AxisType.ROWS}
              header={header}
              positionStyle={positionStyle}
              span={span}
              startIndex={rowStartIndex}
              scrollLeft={0}
              scrollTop={scrollTop}
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
      zoom,
      getRowHeight,
      getColumnWidth,
      columnCount,
      rowCount,
      scrollTop,
      height,
      width,
    } = this.props;
    return (
      <ReactVirtualizedGrid
        cellRangeRenderer={this.rowHeadersRenderer}
        cellRenderer={function mock() {}}
        className="OrbGrid-row-headers"
        columnCount={columnCount}
        columnWidth={getColumnWidth}
        height={height}
        overscanRowCount={0}
        ref={(ref) => { this.grid = ref; }}
        rowCount={rowCount}
        rowHeight={getRowHeight}
        scrollLeft={0}
        scrollTop={scrollTop}
        // We set overflowX and overflowY and not overflow
        // because react-virtualized sets them during render
        style={{ fontSize: `${zoom * 100}%`, overflowX: 'hidden', overflowY: 'hidden' }}
        width={width}
      />
    );
  }
}

export default RowHeaders;
