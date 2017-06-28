import React, { Component } from 'react';

import { AxisType } from '../../Axis';
import { MEASURE_ID } from '../../constants';
import DimensionHeader from '../DimensionHeader';

class DimensionHeaders extends Component {
  shouldComponentUpdate(nextProps) {
    return nextProps.crossPositions !== this.props.crossPositions;
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
      gridId
    } = this.props;
    // let headers = [];

    // Get width for column dimension headers
    // let dimensionWhoseWidthToGet;
    // // if (measureHeadersAxis === 'rows') {
    // //   // Dimension headers are on top of the measures column
    // //   dimensionWhoseWidthToGet = MEASURE_ID;
    // // } else
    // if (rowDimensions.length) {
    //   // Dimension headers are on top of the column of the last dimension of the row headers
    //   dimensionWhoseWidthToGet = rowDimensions[rowDimensions.length - 1].id;
    // } else {
    //   // Dimension headers are on top of the Total header --> get default width
    //   dimensionWhoseWidthToGet = null;
    // }
    // const crossWidth =
    //   crossPositions[AxisType.COLUMNS][
    //     columnDimensions[columnDimensions.length - 1].id
    //   ].size;
    // const crossHeight =
    //   crossPositions[AxisType.ROWS][rowDimensions[rowDimensions.length - 1].id]
    //     .size;
    // columns header
    let headers = [];
    let lastCrossDimensionId = rowDimensions[rowDimensions.length - 1].id;
    columnDimensions.forEach(dimension => {
      if (dimension.id !== MEASURE_ID) {
        headers.push(
          <DimensionHeader
            key={`dimension-header-${dimension.id}`}
            left={
              width - crossPositions[AxisType.ROWS][lastCrossDimensionId].size
            }
            top={crossPositions[AxisType.COLUMNS][dimension.id].position}
            width={crossPositions[AxisType.ROWS][lastCrossDimensionId].size}
            height={crossPositions[AxisType.COLUMNS][dimension.id].size}
            dimensionId={dimension.id}
            caption={dimension.caption}
            mainDirection="right"
            // crossDimensionId={dimensionWhoseWidthToGet}
            previewSizes={previewSizes}
            gridId={gridId}
          />
        );
      }
    });

    lastCrossDimensionId = columnDimensions[columnDimensions.length - 1].id;
    rowDimensions.forEach(dimension => {
      if (dimension.id !== MEASURE_ID) {
        headers.push(
          <DimensionHeader
            key={`dimension-header-${dimension.id}`}
            left={crossPositions[AxisType.ROWS][dimension.id].position}
            top={
              height -
              crossPositions[AxisType.COLUMNS][lastCrossDimensionId].size
            }
            width={crossPositions[AxisType.ROWS][dimension.id].size}
            height={crossPositions[AxisType.COLUMNS][lastCrossDimensionId].size}
            dimensionId={dimension.id}
            caption={dimension.caption}
            mainDirection="down"
            // crossDimensionId={dimensionWhoseWidthToGet}
            previewSizes={previewSizes}
            gridId={gridId}
          />
        );
      }
    });
    // headers = headers.concat(
    //   columnDimensions.filter(dim => dim.id !== MEASURE_ID).map(dimension => {
    //     const cross = crossPositions[AxisType.ROWS][dimension.id];
    //     // const headerHeight = getCrossSize(AxisType.COLUMNS, dimension.id);
    //     return (
    //       <DimensionHeader
    //         key={`dimension-header-${dimension.id}`}
    //         left={width - crossWidth}
    //         top={cross.position}
    //         width={crossWidth}
    //         height={cross.size}
    //         dimensionId={dimension.id}
    //         caption={dimension.caption}
    //         mainDirection="right"
    //         // crossDimensionId={dimensionWhoseWidthToGet}
    //         previewSizes={previewSizes}
    //         gridId={gridId}
    //       />
    //     );
    //   })
    // );
    // Get height for row dimension headers in different cases
    // let dimensionWhoseHeightToGet;
    // // if (measureHeadersAxis === 'columns') {
    // //   // Dimension headers are to the left of the measures row
    // //   dimensionWhoseHeightToGet = MEASURE_ID;
    // // } else
    // if (columnDimensions.length) {
    //   // Dimension headers are to the left of the row of the last dimension of the column headers
    //   dimensionWhoseHeightToGet =
    //     columnDimensions[columnDimensions.length - 1].id;
    // } else {
    //   // Dimension headers are to the left of the Total header --> get default height
    //   dimensionWhoseHeightToGet = null;
    // }

    //  getCrossSize(
    //   AxisType.COLUMNS,
    //   dimensionWhoseHeightToGet
    // );

    // headers = headers.concat(
    //   columnDimensions.filter(dim => dim.id !== MEASURE_ID).map(dimension => {
    //     const cross = crossPositions[AxisType.COLUMNS][dimension.id];
    //     return (
    //       <DimensionHeader
    //         top={height - crossHeight}
    //         // crossDimensionId={dimensionWhoseHeightToGet}
    //         dimensionId={dimension.id}
    //         caption={dimension.caption}
    //         key={`dimension-header-${dimension.id}`}
    //         left={cross.position}
    //         mainDirection="down"
    //         previewSizes={previewSizes}
    //         height={crossHeight}
    //         width={cross.size}
    //         gridId={gridId}
    //       />
    //     );
    //   })
    // );

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
