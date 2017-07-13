import PropTypes from 'prop-types';
import React, { PureComponent } from 'react';
import { Grid as ReactVirtualizedGrid } from 'react-virtualized/dist/commonjs/Grid';

import { AxisType, toAxis } from '../../Axis';

import { Header, DataHeader, HeaderType } from '../../Cells';
import { MEASURE_ID, TOTAL_ID, ROOT_ID, KEY_SEPARATOR } from '../../constants';
import HeaderComponent from '../Header';
// import getHeaderSize from '../../utils/headerSize';
import { isNullOrUndefined, isNull, getLeaves } from '../../utils/generic';
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
  isNotCollapsible,
  isAffixManaged,
  toggleCollapse,
  moveDimension,
  selectAxis,
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
      isNotCollapsible={isNotCollapsible}
      isCollapsed={header.isCollapsed}
      isAffixManaged={isAffixManaged}
      gridId={gridId}
      toggleCollapse={toggleCollapse}
      selectAxis={selectAxis}
      moveDimension={moveDimension}
    />
  );
}

class Headers extends PureComponent {
  constructor() {
    super();
    // this.headersRenderer = this.headersRenderer.bind(this);
    this.cellCache = {};
  }
  componentWillReceiveProps(nextProps) {
    if (nextProps.headers !== this.props.headers) {
      this.cellCache = {};
    }
    // this.cellCache = this.getState(cellCache);
  }
  componentDidUpdate(prevProps) {
    console.log(33);
    if (
      prevProps.sizes !== this.props.sizes ||
      prevProps.headers !== this.props.headers ||
      prevProps.zoom !== this.props.zoom
    ) {
      this.grid.recomputeGridSize();
    }
  }

  headersRenderer = ({
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
  }) => {
    const {
      headers,
      leaves,
      rowCount,
      columnCount,
      previewSizes,
      getLastChildSize,
      getColumnWidth,
      getRowHeight,
      gridId,
      dimensions,
      measures,
      data,
      axisType,
      toggleCollapse,
      selectAxis,
      moveDimension,
      getSizeByKey,
      crossPositions
    } = this.props;

    // this.firstLeafHeader =
    //   headers[rowStartIndex][headers[rowStartIndex].length - 1];

    const renderedCells = [];
    let startIndex, stopIndex, sizeAndPositionManager, offsetAdjustment;
    // ,
    // crossSizeAndPositionManager;
    if (axisType === AxisType.ROWS) {
      startIndex = rowStartIndex;
      stopIndex = rowStopIndex;
      sizeAndPositionManager = rowSizeAndPositionManager;
      // crossSizeAndPositionManager = columnSizeAndPositionManager;
      offsetAdjustment = verticalOffsetAdjustment;
    } else {
      startIndex = columnStartIndex;
      stopIndex = columnStopIndex;
      sizeAndPositionManager = columnSizeAndPositionManager;
      // crossSizeAndPositionManager = rowSizeAndPositionManager;
      offsetAdjustment = horizontalOffsetAdjustment;
    }
    // Because of the offset caused by the fixed headers,

    // we have to make the cell count artificially higher.
    // This ensures that we don't render inexistent headers.
    const correctStopIndex = Math.min(stopIndex, leaves.length - 1);

    const renderedHeaderKeys = {};
    let header, leafKey;
    let mainLeafOffset = 0;
    let dimensionIndex;
    // collapse expand management
    let collapsedSize = 0;
    let collapsedOffset = 0;
    let nextDimensionIsAttribute, size;
    const collapsedSizes = [];
    const getSize = axisType === AxisType.ROWS ? getColumnWidth : getRowHeight;
    const lastNotMeasureDimensionIndex =
      dimensions.length - 1 - !isNull(measures);
    const measuresCount = isNull(measures)
      ? 1
      : Object.keys(measures).length || 1;

    // collapse management -> size of collapsed dimension until measures columns or end
    for (let index = lastNotMeasureDimensionIndex; index >= 0; index -= 1) {
      if (nextDimensionIsAttribute) {
        collapsedSizes.push(0);
      } else {
        collapsedSizes.push(collapsedSize);
      }
      nextDimensionIsAttribute = dimensions[index].isAttribute;
      size = getSize({ index: [index] });
      collapsedSize += size;
    }
    collapsedSizes.reverse();
    // console.log([rowStartIndex, rowStopIndex, correctStopIndex]);
    // loop on leaves + parent leaf
    // if (axisType === 2) {
    //   console.log([getColumnWidth({ index: 1 }), getColumnWidth({ index: 2 })]);
    // }
    for (let index = startIndex; index <= correctStopIndex; index += 1) {
      header = leaves[index];
      leafKey = header.key;
      while (header.id !== ROOT_ID && !renderedHeaderKeys[header.key]) {
        // if (header.isCollapsed) {
        //   header.depth = header.depth;
        // }
        renderedHeaderKeys[header.key] = true;
        // let cell = this.cellCache[header.key];
        // if (isNullOrUndefined(cell) || true) {
        const dimension = dimensions[header.depth];

        let caption;
        if (header.type === HeaderType.DIMENSION) {
          caption = dimension.format(
            dimension.labelAccessor(data[header.dataIndexes[0]])
          );
        } else {
          caption = measures[header.id].caption;
        }
        let mainSize, mainOffset, isAffixManaged;
        if (leafKey === header.key) {
          const main = sizeAndPositionManager.getSizeAndPositionOfCell(index);
          mainSize = main.size;
          mainOffset = main.offset + offsetAdjustment;
          isAffixManaged = false;
          mainLeafOffset = mainOffset;
        } else {
          const headerLeaves = getLeaves(header);
          mainSize = headerLeaves.reduce(
            (acc, leaf) => acc + getSizeByKey(leaf.key),
            0
          );
          mainOffset = mainLeafOffset;
          isAffixManaged =
            leafKey !== headerLeaves[headerLeaves.length - 1].key;
        }
        const span = header.span;
        // const span = header.orderedChildrenIds.length || 1;
        // if (toto === 2) {
        //   console.log(header.key, index, span);
        // }

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
        // const cross = crossSizeAndPositionManager.getSizeAndPositionOfCell(
        //   header.depth
        // );
        const cross = crossPositions[dimension.id];
        collapsedSize = 0;
        // collapsedOffset = 0;
        if (header.isCollapsed && collapsedSizes.length > 0) {
          if (header.type !== HeaderType.MEASURE) {
            //   collapsedOffset = collapsedSizes[0];
            // } else {
            collapsedSize = collapsedSizes[header.depth];
          }
        }

        let positionStyle;
        // if (axisType === AxisType.ROWS) {
        //   positionStyle = {
        //     position: 'absolute',
        //     left: cross.offset,
        //     top: mainOffset,
        //     height: mainSize,
        //     width: cross.size + collapsedSize
        //   };
        // } else {
        //   positionStyle = {
        //     position: 'absolute',
        //     left: mainOffset,
        //     top: cross.offset,
        //     height: cross.size + collapsedSize,
        //     width: mainSize
        //   };
        // }
        //   let positionStyle;
        if (axisType === AxisType.ROWS) {
          positionStyle = {
            position: 'absolute',
            left: cross.position,
            top: mainOffset,
            height: mainSize,
            width: cross.size + collapsedSize
          };
        } else {
          positionStyle = {
            position: 'absolute',
            left: mainOffset,
            top: cross.position,
            height: cross.size + collapsedSize,
            width: mainSize
          };
        }
        const isNotCollapsible =
          header.type === HeaderType.MEASURE ||
          dimension.isAttribute ||
          header.depth >= lastNotMeasureDimensionIndex ||
          (!header.isCollapsed && span === measuresCount);
        const isDragTarget = dimension.id === MEASURE_ID && header.depth === 0;
        // cell = toComponent(
        //   header,
        //   caption,
        //   axisType,
        //   rowStartIndex,
        //   scrollTop,
        //   scrollLeft,
        //   previewSizes,
        //   getLastChildSize,
        //   span,
        //   positionStyle,
        //   dimension.id,
        //   isNotCollapsible,
        //   index === startIndex && header.depth < lastNotMeasureDimensionIndex,
        //   toggleCollapse,
        //   isDragTarget ? moveDimension : null,
        //   selectAxis,
        //   gridId
        // );

        //   this.cellCache[header.key] = cell;
        // }
        // read parents (but only once)
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
            isNotCollapsible,
            index === startIndex && isAffixManaged,
            toggleCollapse,
            isDragTarget ? moveDimension : null,
            selectAxis,
            gridId
          )
        );
        header = header.parent;
      }
    }
    return renderedCells;
  };

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
        ref={ref => {
          this.grid = ref;
        }}
        scrollLeft={scrollLeft}
        scrollTop={scrollTop}
        overscanRowCount={0}
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
// /  overscanRowCount={0}
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
  // getCrossSize: PropTypes.func.isRequired,
  // getLastChildSize: PropTypes.func.isRequired,
  getRowHeight: PropTypes.func.isRequired,
  height: PropTypes.number.isRequired,
  previewSizes: PropTypes.objectOf(PropTypes.number).isRequired,
  rowCount: PropTypes.number.isRequired,
  scrollTop: PropTypes.number.isRequired,
  width: PropTypes.number.isRequired,
  zoom: PropTypes.number.isRequired
};

export default Headers;
