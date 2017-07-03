import React, { Component } from 'react';

import { AxisType } from '../../Axis';
import { MEASURE_ID } from '../../constants';
import DimensionHeader from '../DimensionHeader';

class DimensionHeaders extends Component {
  constructor() {
    super();
    this.headersRenderer = this.headersRenderer.bind(this);
  }
  shouldComponentUpdate(nextProps) {
    return nextProps.crossPositions !== this.props.crossPositions;
  }
  headersRenderer(axis, dimensions, lastCrossDimensionId) {
    const {
      previewSizes,
      gridId,
      toggleCollapseDimension,
      crossPositions,
      height,
      width
    } = this.props;
    const headers = [];
    dimensions.forEach((dimension, index) => {
      if (dimension.id !== MEASURE_ID) {
        const isNotCollapsible =
          !dimension.isCollapsed &&
          (dimension.isAttribute ||
            index >=
              dimensions.length -
                1 -
                (dimensions[dimensions.length - 1].id === MEASURE_ID) ||
            !dimensions[index + 1].isAttribute);
        let positions;
        if (axis === AxisType.ROWS) {
          positions = {
            left: crossPositions[AxisType.ROWS][dimension.id].position,
            top:
              height -
                crossPositions[AxisType.COLUMNS][lastCrossDimensionId].size,
            width: crossPositions[AxisType.ROWS][dimension.id].size,
            height: crossPositions[AxisType.COLUMNS][lastCrossDimensionId].size
          };
        } else {
          positions = {
            left:
              width - crossPositions[AxisType.ROWS][lastCrossDimensionId].size,
            top: crossPositions[AxisType.COLUMNS][dimension.id].position,
            width: crossPositions[AxisType.ROWS][lastCrossDimensionId].size,
            height: crossPositions[AxisType.COLUMNS][dimension.id].size
          };
        }
        headers.push(
          <DimensionHeader
            key={`dimension-header-${dimension.id}`}
            left={positions.left}
            top={positions.top}
            width={positions.width}
            height={positions.height}
            dimensionId={dimension.id}
            caption={dimension.caption}
            axis={axis}
            crossDimensionId={lastCrossDimensionId}
            isNotCollapsible={isNotCollapsible}
            isCollapsed={dimension.isCollapsed}
            previewSizes={previewSizes}
            gridId={gridId}
            toggleCollapseDimension={toggleCollapseDimension}
            isAttribute={dimension.isAttribute}
          />
        );
      }
    });
    return headers;
  }

  render() {
    const {
      columnDimensions,
      crossPositions,
      getColumnWidth,
      getRowHeight,
      rowDimensions,
      previewSizes,
      height,
      width,
      zoom,
      gridId,

      toggleCollapseDimension
    } = this.props;

    let headers = [];
    let lastCrossDimensionId = rowDimensions[rowDimensions.length - 1].id;

    headers = this.headersRenderer(
      AxisType.COLUMNS,
      columnDimensions,
      lastCrossDimensionId
    );
    // columnDimensions.forEach((dimension, index) => {
    //   if (dimension.id !== MEASURE_ID) {
    //     const isNotCollapsible =
    //       dimension.isAttribute ||
    //       index >=
    //         columnDimensions.length -
    //           1 -
    //           (columnDimensions[columnDimensions.length - 1].id ===
    //             columnDimensions.length - 1) ||
    //       !columnDimensions[index + 1].isAttribute;
    //     headers.push(
    //       <DimensionHeader
    //         key={`dimension-header-${dimension.id}`}
    //         left={
    //           width - crossPositions[AxisType.ROWS][lastCrossDimensionId].size
    //         }
    //         top={crossPositions[AxisType.COLUMNS][dimension.id].position}
    //         width={crossPositions[AxisType.ROWS][lastCrossDimensionId].size}
    //         height={crossPositions[AxisType.COLUMNS][dimension.id].size}
    //         dimensionId={dimension.id}
    //         caption={dimension.caption}
    //         axis="columns"
    //         crossDimensionId={lastCrossDimensionId}
    //         isNotCollapsible={isNotCollapsible}
    //         isCollapsed={dimension.isCollapsed}
    //         previewSizes={previewSizes}
    //         gridId={gridId}
    //         toggleCollapseDimension={toggleCollapseDimension}
    //       />
    //     );
    //   }
    // });

    lastCrossDimensionId = columnDimensions[columnDimensions.length - 1].id;

    headers = headers.concat(
      this.headersRenderer(AxisType.ROWS, rowDimensions, lastCrossDimensionId)
    );
    // rowDimensions.forEach((dimension, index) => {
    //   if (dimension.id !== MEASURE_ID) {
    //     const isNotCollapsible =
    //       dimension.isAttribute ||
    //       index >=
    //         rowDimensions.length -
    //           1 -
    //           (rowDimensions[rowDimensions.length - 1].id ===
    //             rowDimensions.length - 1) ||
    //       !rowDimensions[index + 1].isAttribute;
    //     headers.push(
    //       <DimensionHeader
    //         key={`dimension-header-${dimension.id}`}
    //         left={crossPositions[AxisType.ROWS][dimension.id].position}
    //         top={
    //           height -
    //           crossPositions[AxisType.COLUMNS][lastCrossDimensionId].size
    //         }
    //         width={crossPositions[AxisType.ROWS][dimension.id].size}
    //         height={crossPositions[AxisType.COLUMNS][lastCrossDimensionId].size}
    //         dimensionId={dimension.id}
    //         caption={dimension.caption}
    //         axis="rows"
    //         crossDimensionId={lastCrossDimensionId}
    //         isNotCollapsible={isNotCollapsible}
    //         isCollapsed={dimension.isCollapsed}
    //         previewSizes={previewSizes}
    //         gridId={gridId}
    //         toggleCollapseDimension={toggleCollapseDimension}
    //       />
    //     );
    //   }
    // });
    return (
      // Putting position as relative here allows its children (the dimension headers)
      // to be absolutely positioned relatively to their parent
      <div
        style={{
          position: 'relative',
          height,
          width,
          fontSize: `${zoom * 100}%`,
          overflow: 'hidden'
        }}
        className="pivotgrid-dimension-headers"
      >
        {headers}
      </div>
    );
  }
}

export default DimensionHeaders;
