import React, { PureComponent, PropTypes } from 'react';
import {
  Grid as ReactVirtualizedGrid
} from 'react-virtualized/dist/commonjs/Grid';
import { CellMeasurer } from 'react-virtualized/dist/commonjs/CellMeasurer';
import { AxisType } from '../../Axis';
import { Header, DataHeader } from '../../Cells';
import { MEASURE_ID, TOTAL_ID } from '../../constants';
import HeaderComponent from '../Header';
import getHeaderSize from '../../utils/headerSize';

class ColumnHeaders extends PureComponent {
  constructor() {
    super();
    this.columnHeadersRenderer = this.columnHeadersRenderer.bind(this);
    this.handleResizeCell = this.handleResizeCell.bind(this);
  }

  componentDidUpdate(prevProps) {
    if (
      prevProps.sizesColumnsLeafs !== this.props.sizesColumnsLeafs ||
      prevProps.zoom !== this.props.zoom
    ) {
      this.grid.recomputeGridSize();
    }
  }

  handleResizeCell(header) {
    this.props.autoResizeColumn(header);
  }

  columnHeadersRenderer({
    columnSizeAndPositionManager,
    columnStartIndex,
    columnStopIndex,
    deferredMeasurementCache,
    horizontalOffsetAdjustment,
    isScrolling,
    parent,
    scrollLeft,
    styleCache
  }) {
    const {
      columnHeaders,
      dimensionPositions,
      getDimensionSize,
      getLastChildSize,
      gridId,
      measuredSizesCache,
      previewSizes,
      autoResizeColumn,
      rowCount
    } = this.props;
    const deferredMode = typeof deferredMeasurementCache !== 'undefined';

    const renderedCells = [];

    // Because of the offset caused by the fixed headers,
    // we have to make the cell count artificially higher.
    // This ensures that we don't render inexistent headers.
    const correctColumnStopIndex = Math.min(
      columnStopIndex,
      columnHeaders.length - 1
    );

    // Render fixed header rows

    // Render big cells on top of current cells if necessary
    // The check on the presence of the header is necessary
    // because it can be out of bounds when the headers array is modified
    if (
      columnHeaders[columnStartIndex] &&
      columnHeaders[columnStartIndex].length < rowCount
    ) {
      let header = columnHeaders[columnStartIndex][0];
      while (header.parent) {
        header = header.parent;
        const main = columnSizeAndPositionManager.getSizeAndPositionOfCell(
          header.x
        );
        const left = main.offset + horizontalOffsetAdjustment;
        const span = header.hspan();
        const width = getHeaderSize(
          columnSizeAndPositionManager,
          header.x,
          span
        );
        let top;
        let height;
        if (header.dim.field) {
          // Normal header
          top = dimensionPositions.columns[header.dim.field.id];
          height = getDimensionSize(AxisType.COLUMNS, header.dim.field.id);
        } else {
          // Total header
          // Important when the size of the grid is smaller than the width of the measure cells
          top = 0;
          height = getDimensionSize(AxisType.COLUMNS, TOTAL_ID);
        }
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
            columnIndex={header.x}
            key={header.key}
            parent={parent}
            rowIndex={header.y}
          >
            <HeaderComponent
              resizeCell={autoResizeColumn}
              key={`header-${header.key}`}
              axis={AxisType.COLUMNS}
              header={header}
              positionStyle={positionStyle}
              span={span}
              startIndex={columnStartIndex}
              scrollLeft={scrollLeft}
              scrollTop={0}
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
      let columnIndex = columnStartIndex;
      columnIndex <= correctColumnStopIndex;
      columnIndex += 1
    ) {
      const main = columnSizeAndPositionManager.getSizeAndPositionOfCell(
        columnIndex
      );
      const left = main.offset + horizontalOffsetAdjustment;
      renderedCells.push(
        ...columnHeaders[columnIndex].map(header => {
          const span = header.hspan();
          let positionStyle;
          if (canCacheStyle && styleCache[header.key]) {
            positionStyle = styleCache[header.key];
          } else if (
            deferredMode &&
            !deferredMeasurementCache.has(header.y, header.x)
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
            const width = getHeaderSize(
              columnSizeAndPositionManager,
              columnIndex,
              span
            );
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
              columnIndex={header.x}
              key={header.key}
              parent={parent}
              rowIndex={header.y}
            >
              <HeaderComponent
                resizeCell={autoResizeColumn}
                key={`header-${header.key}`}
                axis={AxisType.COLUMNS}
                header={header}
                positionStyle={positionStyle}
                span={span}
                startIndex={columnStartIndex}
                scrollLeft={scrollLeft}
                scrollTop={0}
                previewSizes={previewSizes}
                gridId={gridId}
                getLastChildSize={getLastChildSize}
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
      columnCount,
      getColumnWidth,
      getRowHeight,
      height,
      measuredSizesCache,
      rowCount,
      scrollLeft,
      width,
      zoom
    } = this.props;
    return (
      <ReactVirtualizedGrid
        cellRangeRenderer={this.columnHeadersRenderer}
        cellRenderer={function mock() {}}
        className="pivotgrid-column-headers"
        columnCount={columnCount}
        columnWidth={getColumnWidth}
        // The position of inner style was set to static in react-virtualized 9.2.3
        // This broke the grid because the height of the inner container was not reset
        // when the height prop changed
        // This is a workaround
        containerStyle={{ position: 'static' }}
        deferredMeasurementCache={measuredSizesCache}
        height={height}
        overscanColumnCount={0}
        ref={ref => {
          this.grid = ref;
        }}
        rowCount={rowCount}
        rowHeight={getRowHeight}
        scrollLeft={scrollLeft}
        scrollTop={0}
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

ColumnHeaders.propTypes = {
  columnCount: PropTypes.number.isRequired,
  columnHeaders: PropTypes.arrayOf(
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
  scrollLeft: PropTypes.number.isRequired,
  width: PropTypes.number.isRequired,
  zoom: PropTypes.number.isRequired
};

export default ColumnHeaders;
