import React, { PropTypes, PureComponent } from 'react';
import {
  Grid as ReactVirtualizedGrid
} from 'react-virtualized/dist/commonjs/Grid';
import { CellMeasurer } from 'react-virtualized/dist/commonjs/CellMeasurer';

import { AxisType } from '../../Axis';
import { Header, DataHeader } from '../../Cells';
import { MEASURE_ID, TOTAL_ID } from '../../constants';
import HeaderComponent from '../Header';
import getHeaderSize from '../../utils/headerSize';

class RowHeaders extends PureComponent {
  constructor() {
    super();
    this.rowHeadersRenderer = this.rowHeadersRenderer.bind(this);
  }

  componentDidUpdate(prevProps) {
    if (
      prevProps.sizesRowsLeafs !== this.props.sizesRowsLeafs ||
      prevProps.zoom !== this.props.zoom
    ) {
      this.grid.recomputeGridSize();
    }
  }

  rowHeadersRenderer(
    {
      deferredMeasurementCache,
      isScrolling,
      parent,
      rowSizeAndPositionManager,
      rowStartIndex,
      rowStopIndex,
      scrollTop,
      styleCache,
      verticalOffsetAdjustment
    }
  ) {
    const {
      rowHeaders,
      autoResizeColumn,
      columnCount,
      previewSizes,
      getLastChildSize,
      getDimensionSize,
      dimensionPositions,
      gridId,
      measuredSizesCache
    } = this.props;

    const deferredMode = typeof deferredMeasurementCache !== 'undefined';
    this.firstLeafHeader = rowHeaders[rowStartIndex][
      rowHeaders[rowStartIndex].length - 1
    ];

    const renderedCells = [];

    // Because of the offset caused by the fixed headers,

    // we have to make the cell count artificially higher.
    // This ensures that we don't render inexistent headers.
    const correctRowStopIndex = Math.min(rowStopIndex, rowHeaders.length - 1);

    // Render fixed left columns

    // Render big cells on the left of current cells if necessary
    // The check on the presence of the header is necessary
    // because it can be out of bounds when the headers array is modified
    if (
      rowHeaders[rowStartIndex] &&
      rowHeaders[rowStartIndex].length < columnCount
    ) {
      let header = rowHeaders[rowStartIndex][0];
      while (header.parent) {
        header = header.parent;
        const main = rowSizeAndPositionManager.getSizeAndPositionOfCell(
          header.x
        );
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
          width
        };
        renderedCells.push(
          <CellMeasurer
            cache={measuredSizesCache}
            columnIndex={header.y}
            key={header.key}
            parent={parent}
            rowIndex={header.x}
          >
            <HeaderComponent
              resizeCell={autoResizeColumn}
              key={`header-${header.key}`}
              axis={AxisType.ROWS}
              header={header}
              positionStyle={positionStyle}
              span={span}
              startIndex={rowStartIndex}
              scrollLeft={0}
              scrollTop={scrollTop}
              previewSizes={previewSizes}
              getLastChildSize={getLastChildSize}
              gridId={gridId}
            />
          </CellMeasurer>
        );
      }
    }

    const canCacheStyle = !isScrolling;
    for (
      let rowIndex = rowStartIndex;
      rowIndex <= correctRowStopIndex;
      rowIndex += 1
    ) {
      const main = rowSizeAndPositionManager.getSizeAndPositionOfCell(rowIndex);
      const top = main.offset + verticalOffsetAdjustment;
      renderedCells.push(
        ...rowHeaders[rowIndex].map(header => {
          const span = header.vspan();
          let positionStyle;
          if (canCacheStyle && styleCache[header.key]) {
            positionStyle = styleCache[header.key];
          } else if (
            deferredMode && !deferredMeasurementCache.has(header.x, header.y)
          ) {
            // Position not-yet-measured cells at top/left 0,0,
            // And give them width/height of 'auto' so they can grow larger than the parent Grid if necessary.
            // Positioning them further to the right/bottom influences their measured size.
            positionStyle = {
              height: 'auto',
              left: 0,
              position: 'absolute',
              top: 0,
              width: 'auto'
            };
          } else {
            const height = getHeaderSize(
              rowSizeAndPositionManager,
              rowIndex,
              span
            );
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
            positionStyle = {
              position: 'absolute',
              left,
              top,
              height,
              width
            };
            /* eslint-disable no-param-reassign */
            styleCache[header.key] = positionStyle;
            /* eslint-enable */
          }
          return (
            <CellMeasurer
              cache={measuredSizesCache}
              columnIndex={header.y}
              key={header.key}
              parent={parent}
              rowIndex={header.x}
            >
              <HeaderComponent
                resizeCell={autoResizeColumn}
                key={`header-${header.key}`}
                axis={AxisType.ROWS}
                header={header}
                positionStyle={positionStyle}
                span={span}
                startIndex={rowStartIndex}
                scrollLeft={0}
                scrollTop={scrollTop}
                previewSizes={previewSizes}
                getLastChildSize={getLastChildSize}
                gridId={gridId}
              />
            </CellMeasurer>
          );
        })
      );
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
      measuredSizesCache,
      width
    } = this.props;
    return (
      <ReactVirtualizedGrid
        cellRangeRenderer={this.rowHeadersRenderer}
        cellRenderer={function mock() {}}
        className="pivotgrid-row-headers"
        columnCount={columnCount}
        columnWidth={getColumnWidth}
        // The position of inner style was set to static in react-virtualized 9.2.3
        // This broke the grid because the height of the inner container was not reset
        // when the height prop changed
        // This is a workaround
        containerStyle={{ position: 'static' }}
        deferredMeasurementCache={measuredSizesCache}
        height={height}
        overscanRowCount={0}
        ref={ref => {
          this.grid = ref;
        }}
        rowCount={rowCount}
        rowHeight={getRowHeight}
        scrollLeft={0}
        scrollTop={scrollTop}
        // We set overflowX and overflowY and not overflow
        // because react-virtualized sets them during render
        style={{
          fontSize: `${zoom * 100}%`,
          overflowX: 'hidden',
          overflowY: 'hidden'
        }}
        width={width}
      />
    );
  }
}

RowHeaders.propTypes = {
  columnCount: PropTypes.number.isRequired,
  rowHeaders: PropTypes.arrayOf(
    PropTypes.arrayOf(
      PropTypes.oneOfType([
        PropTypes.instanceOf(Header),
        PropTypes.instanceOf(DataHeader)
      ])
    )
  ).isRequired,
  dimensionPositions: PropTypes.shape({
    columns: PropTypes.objectOf(PropTypes.number),
    rows: PropTypes.objectOf(PropTypes.number)
  }).isRequired,
  getColumnWidth: PropTypes.func.isRequired,
  getDimensionSize: PropTypes.func.isRequired,
  getLastChildSize: PropTypes.func.isRequired,
  getRowHeight: PropTypes.func.isRequired,
  height: PropTypes.number.isRequired,
  previewSizes: PropTypes.objectOf(PropTypes.number).isRequired,
  rowCount: PropTypes.number.isRequired,
  scrollTop: PropTypes.number.isRequired,
  width: PropTypes.number.isRequired,
  zoom: PropTypes.number.isRequired
};

export default RowHeaders;
