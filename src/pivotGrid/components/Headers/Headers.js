import PropTypes from 'prop-types';
import React, { PureComponent } from 'react';
import { Grid as ReactVirtualizedGrid } from 'react-virtualized/dist/commonjs/Grid';

import { AxisType, toAxis } from '../../Axis';
import { Header, DataHeader, HeaderType } from '../../Cells';
import { MEASURE_ID, TOTAL_ID, ROOT_ID, KEY_SEPARATOR } from '../../constants';
import HeaderComponent from '../Header';
import getHeaderSize from '../../utils/headerSize';

function toComponent(
  header,
  caption,
  axisType,
  startIndex,
  scrollTop,
  scrollLeft,
  previewSizes,
  getLastChildSize,
  span,
  positionStyle,
  dimensionKey,
  gridId
) {
  return (
    <HeaderComponent
      key={`header-${header.key}`}
      axis={axisType} //{AxisType.ROWS}
      header={header}
      caption={caption}
      positionStyle={positionStyle}
      span={span}
      startIndex={startIndex}
      scrollLeft={scrollLeft}
      scrollTop={scrollTop}
      previewSizes={previewSizes}
      getLastChildSize={getLastChildSize}
      dimensionKey={dimensionKey}
      gridId={gridId}
    />
  );
}

class Headers extends PureComponent {
  constructor() {
    super();
    this.headersRenderer = this.headersRenderer.bind(this);
  }

  componentDidUpdate(prevProps) {
    if (
      prevProps.sizes !== this.props.sizes ||
      prevProps.zoom !== this.props.zoom
    ) {
      this.grid.recomputeGridSize();
    }
  }

  headersRenderer({
    rowSizeAndPositionManager,
    rowStartIndex,
    rowStopIndex,
    columnSizeAndPositionManager,
    columnStartIndex,
    columnStopIndex,
    scrollTop,
    scrollLeft,
    verticalOffsetAdjustment,
    horizontalOffsetAdjustment
  }) {
    const {
      headers,
      leaves,
      rowCount,
      columnCount,
      previewSizes,
      getLastChildSize,
      getCrossSize,
      crossPositions,
      gridId,
      dimensions,
      measures,
      data,
      axisType
    } = this.props;

    // this.firstLeafHeader =
    //   headers[rowStartIndex][headers[rowStartIndex].length - 1];

    const renderedCells = [];
    let startIndex,
      stopIndex,
      sizeAndPositionManager,
      offsetAdjustment,
      crossSizeAndPositionManager;
    if (axisType === AxisType.ROWS) {
      startIndex = rowStartIndex;
      stopIndex = rowStopIndex;
      sizeAndPositionManager = rowSizeAndPositionManager;
      crossSizeAndPositionManager = columnSizeAndPositionManager;
      offsetAdjustment = verticalOffsetAdjustment;
    } else {
      startIndex = columnStartIndex;
      stopIndex = columnStopIndex;
      sizeAndPositionManager = columnSizeAndPositionManager;
      crossSizeAndPositionManager = rowSizeAndPositionManager;
      offsetAdjustment = horizontalOffsetAdjustment;
    }
    // Because of the offset caused by the fixed headers,

    // we have to make the cell count artificially higher.
    // This ensures that we don't render inexistent headers.
    const correctStopIndex = Math.min(stopIndex, leaves.length - 1);

    // Render fixed left columns

    // // Render big cells on the left of current cells if necessary
    // // The check on the presence of the header is necessary
    // // because it can be out of bounds when the headers array is modified
    // if (
    //   headers[rowStartIndex] &&
    //   headers[rowStartIndex].length < columnCount
    // ) {
    //   let header = headers[rowStartIndex][0];
    //   while (header.parent) {
    //     header = header.parent;
    //     const main = rowSizeAndPositionManager.getSizeAndPositionOfCell(
    //       header.x
    //     );
    //     const span = header.vspan();
    //     const top = main.offset + verticalOffsetAdjustment;
    //     const height = getHeaderSize(rowSizeAndPositionManager, header.x, span);
    //     const width = getCrossSize(AxisType.ROWS, header.dim.dimension.id);
    //     const left = 0 + dimensionPositions.rows[header.dim.dimension.id];
    //     const positionStyle = {
    //       position: "absolute",
    //       left,
    //       top,
    //       height,
    //       width
    //     };
    //     renderedCells.push(
    //       <HeaderComponent
    //         key={`header-${header.key}`}
    //         axis={AxisType.ROWS}
    //         header={header}
    //         positionStyle={positionStyle}
    //         span={span}
    //         startIndex={rowStartIndex}
    //         scrollLeft={0}
    //         scrollTop={scrollTop}
    //         previewSizes={previewSizes}
    //         getLastChildSize={getLastChildSize}
    //         gridId={gridId}
    //       />
    //     );
    //   }
    // }

    // row headers
    // loop on leaves + leave parents
    const renderedHeaderKeys = {};
    let header;
    let dimensionIndex;
    for (let index = startIndex; index <= correctStopIndex; index += 1) {
      header = leaves[index];
      dimensionIndex = dimensions.length - 1;
      while (header.id !== ROOT_ID && !renderedHeaderKeys[header.key]) {
        renderedHeaderKeys[header.key] = true;
        const dimension = dimensions[dimensionIndex];

        let caption;
        if (header.type === HeaderType.DIMENSION) {
          caption = dimension.format(
            dimension.labelAccessor(data[header.dataIndexes[0]])
          );
        } else {
          caption = measures[header.id].caption;
        }

        const main = sizeAndPositionManager.getSizeAndPositionOfCell(index);
        const mainOffset = main.offset + offsetAdjustment;

        // const span = header.vspan();
        const span = header.orderedChildrenIds.length || 1;
        const mainSize = getHeaderSize(sizeAndPositionManager, index, span);
        // 3 cases: normal dimension header, measure header or total header
        // let left = 0;
        // if (!header.dim) {
        //   // Measure header
        //   width = getCrossSize(AxisType.ROWS, MEASURE_ID);
        //   left += dimensionPositions.rows[MEASURE_ID];
        // } else if (header.dim.dimension) {
        // Normal dimension header
        //
        // const crossSize = getCrossSize(axisType, dimension.id);
        const cross = crossSizeAndPositionManager.getSizeAndPositionOfCell(
          dimensionIndex
        );
        // const crossPosition = crossPositions[axisType][dimension.id];
        // } else {
        //   // Total header
        //   width = getCrossSize(AxisType.ROWS, TOTAL_ID);
        // }
        let positionStyle;
        if (axisType === AxisType.ROWS) {
          positionStyle = {
            position: 'absolute',
            left: cross.offset,
            top: mainOffset,
            height: mainSize,
            width: cross.size
          };
        } else {
          positionStyle = {
            position: 'absolute',
            left: mainOffset,
            top: cross.offset,
            height: cross.size,
            width: mainSize
          };
        }

        renderedCells.push(
          toComponent(
            header,
            caption,
            axisType,
            rowStartIndex,
            scrollTop,
            scrollLeft,
            previewSizes,
            getLastChildSize,
            span,
            positionStyle,
            dimension.id,
            gridId
          )
        );
        header = header.parent;
        dimensionIndex -= 1;
      }
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
      scrollLeft,
      height,
      width
    } = this.props;
    return (
      <ReactVirtualizedGrid
        cellRangeRenderer={this.headersRenderer}
        cellRenderer={function mock() {}}
        // className="pivotgrid-row-headers"
        // The position of inner style was set to static in react-virtualized 9.2.3
        // This broke the grid because the height of the inner container was not reset
        // when the height prop changed
        // This is a workaround
        containerStyle={{ position: 'static' }}
        height={height}
        width={width}
        rowCount={rowCount}
        rowHeight={getRowHeight}
        columnCount={columnCount}
        columnWidth={getColumnWidth}
        overscanRowCount={0}
        ref={ref => {
          this.grid = ref;
        }}
        scrollLeft={scrollLeft}
        scrollTop={scrollTop}
        // We set overflowX and overflowY and not overflow
        // because react-virtualized sets them during render
        style={{
          fontSize: `${zoom * 100}%`,
          overflowX: 'hidden',
          overflowY: 'hidden'
        }}
      />
    );
  }
}

Headers.propTypes = {
  columnCount: PropTypes.number.isRequired,
  headers: PropTypes.arrayOf(
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
  getCrossSize: PropTypes.func.isRequired,
  getLastChildSize: PropTypes.func.isRequired,
  getRowHeight: PropTypes.func.isRequired,
  height: PropTypes.number.isRequired,
  previewSizes: PropTypes.objectOf(PropTypes.number).isRequired,
  rowCount: PropTypes.number.isRequired,
  scrollTop: PropTypes.number.isRequired,
  width: PropTypes.number.isRequired,
  zoom: PropTypes.number.isRequired
};

export default Headers;
