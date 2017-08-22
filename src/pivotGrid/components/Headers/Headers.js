import React, { PureComponent } from 'react';
import { Grid as ReactVirtualizedGrid } from 'react-virtualized/dist/commonjs/Grid';
import classnames from 'classnames';

import { MEASURE_ID, ROOT_ID, HeaderType, AxisType } from '../../constants';
import HeaderComponent from '../Header/Header';
import { isNull } from '../../utils/generic';
import { getLeaves } from '../../utils/headers';
class Headers extends PureComponent {
  componentWillReceiveProps = nextProps => {
    if (nextProps.headers !== this.props.headers) {
      this.cellCache = {};
    }
  };
  // shouldComponentUpdate = nextProps => {
  //   return (
  //     nextProps.height !== this.props.height ||
  //     nextProps.width !== this.props.width ||
  //     nextProps.zoom !== this.props.zoom ||
  //     nextProps.leaves !== this.props.leaves ||
  //     (nextProps.scrollTop !== this.props.scrollTop &&
  //       this.props.axisType === AxisType.ROWS) ||
  //     (nextProps.scrollLeft !== this.props.scrollLeft &&
  //       this.props.axisType === AxisType.COLUMNS)
  //   );
  // };

  componentDidUpdate = prevProps => {
    if (
      prevProps.sizes !== this.props.sizes ||
      prevProps.dimensions !== this.props.dimensions ||
      prevProps.zoom !== this.props.zoom
    ) {
    }
    this.grid.recomputeGridSize();
  };

  collectMenu = props => {
    return {
      ...props,
      availableMeasures: this.props.availableMeasures,
      measures: this.props.measures
    };
  };
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
      leaves,
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
      toggleMeasure,
      getSizeByKey,
      crossPositions
    } = this.props;
    const renderedCells = [];

    let startIndex, stopIndex, sizeAndPositionManager, offsetAdjustment;
    if (axisType === AxisType.ROWS) {
      startIndex = rowStartIndex;
      stopIndex = rowStopIndex;
      sizeAndPositionManager = rowSizeAndPositionManager;
      offsetAdjustment = verticalOffsetAdjustment;
    } else {
      startIndex = columnStartIndex;
      stopIndex = columnStopIndex;
      sizeAndPositionManager = columnSizeAndPositionManager;
      offsetAdjustment = horizontalOffsetAdjustment;
    }

    // Because of the offset caused by the fixed headers,
    // we have to make the cell count artificially higher.
    // This ensures that we don't render inexistent headers.
    const correctStopIndex = Math.min(stopIndex, leaves.length - 1);

    const renderedHeaderKeys = {};

    let header,
      leafKey,
      mainLeafOffset = 0,
      collapsedSize = 0,
      nextDimensionIsAttribute,
      size;
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

    // Loop on leaves
    for (let index = startIndex; index <= correctStopIndex; index += 1) {
      header = leaves[index];
      leafKey = header.key;

      // Loop on leaf and parents
      while (header.id !== ROOT_ID && !renderedHeaderKeys[header.key]) {
        renderedHeaderKeys[header.key] = true;
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
        const cross = crossPositions[dimension.id];
        collapsedSize = 0;
        if (header.isCollapsed && collapsedSizes.length > 0) {
          if (header.type !== HeaderType.MEASURE) {
            collapsedSize = collapsedSizes[header.depth];
          }
        }

        let positionStyle;
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
        const isDropTarget = dimension.id === MEASURE_ID && header.depth === 0;
        // read parents (but only once)
        renderedCells.push(
          <HeaderComponent
            key={`header-${header.key}`}
            axis={axisType}
            header={header}
            caption={caption}
            positionStyle={positionStyle}
            span={span}
            scrollLeft={scrollLeft}
            scrollTop={scrollTop}
            previewSizes={previewSizes}
            getLastChildSize={getLastChildSize}
            dimensionId={dimension.id}
            isNotCollapsible={isNotCollapsible}
            isCollapsed={header.isCollapsed}
            isAffixManaged={index === startIndex && isAffixManaged}
            gridId={gridId}
            toggleCollapse={toggleCollapse}
            selectAxis={selectAxis}
            moveDimension={moveDimension}
            toggleMeasure={toggleMeasure}
            isDropTarget={isDropTarget}
            collectMenu={this.collectMenu}
          />
        );
        header = header.parent;
      }
    }
    return renderedCells;
  };

  render() {
    const {
      getRowHeight,
      getColumnWidth,
      columnCount,
      rowCount,
      scrollTop,
      scrollLeft,
      height,
      width,
      axisType
    } = this.props;
    return (
      <div
        className={classnames({
          'pivotgrid-column-headers': axisType === AxisType.COLUMNS,
          'pivotgrid-row-headers': axisType === AxisType.ROWS
        })}
      >
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
            overflowX: 'hidden',
            overflowY: 'hidden',
            outline: 'none'
          }}
        />
      </div>
    );
  }
}

export default Headers;
