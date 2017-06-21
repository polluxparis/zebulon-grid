import React, { Component } from "react";

import { AxisType } from "../../Axis";
import { MEASURE_ID } from "../../constants";
import DimensionHeader from "../DimensionHeader";

class DimensionHeaders extends Component {
  shouldComponentUpdate(nextProps) {
    return nextProps.dimensionPositions !== this.props.dimensionPositions;
  }
  render() {
    const {
      columnDimensionHeaders,
      columnDimensions,
      dataHeadersLocation,
      dimensionPositions,
      getDimensionSize,
      height,
      previewSizes,
      rowDimensionHeaders,
      rowDimensions,
      width,
      zoom,
      gridId
    } = this.props;
    let headers = [];

    // Get width for column dimension headers
    let dimensionWhoseWidthToGet;
    if (dataHeadersLocation === "rows") {
      // Dimension headers are on top of the measures column
      dimensionWhoseWidthToGet = MEASURE_ID;
    } else if (rowDimensions.length) {
      // Dimension headers are on top of the column of the last dimension of the row headers
      dimensionWhoseWidthToGet = rowDimensions[rowDimensions.length - 1].id;
    } else {
      // Dimension headers are on top of the Total header --> get default width
      dimensionWhoseWidthToGet = null;
    }
    const headerWidth = getDimensionSize(
      AxisType.ROWS,
      dimensionWhoseWidthToGet
    );
    headers = headers.concat(
      columnDimensionHeaders.map(dimensionHeader => {
        const dimension = dimensionHeader.value;
        const top = dimensionPositions.columns[dimension.id];
        const headerHeight = getDimensionSize(AxisType.COLUMNS, dimension.id);
        return (
          <DimensionHeader
            key={`dimension-header-${dimension.id}`}
            left={width - headerWidth}
            top={top}
            width={headerWidth}
            height={headerHeight}
            dimension={dimension}
            mainDirection="right"
            crossDimensionId={dimensionWhoseWidthToGet}
            previewSizes={previewSizes}
            gridId={gridId}
          />
        );
      })
    );
    // Get height for row dimension headers in different cases
    let dimensionWhoseHeightToGet;
    if (dataHeadersLocation === "columns") {
      // Dimension headers are to the left of the measures row
      dimensionWhoseHeightToGet = MEASURE_ID;
    } else if (columnDimensions.length) {
      // Dimension headers are to the left of the row of the last dimension of the column headers
      dimensionWhoseHeightToGet =
        columnDimensions[columnDimensions.length - 1].id;
    } else {
      // Dimension headers are to the left of the Total header --> get default height
      dimensionWhoseHeightToGet = null;
    }
    const headerHeight = getDimensionSize(
      AxisType.COLUMNS,
      dimensionWhoseHeightToGet
    );
    headers = headers.concat(
      rowDimensionHeaders.map(dimensionHeader => {
        const dimension = dimensionHeader.value;
        const left = dimensionPositions.rows[dimension.id];
        const headerWidth = getDimensionSize(AxisType.ROWS, dimension.id);
        return (
          <DimensionHeader
            top={height - headerHeight}
            crossDimensionId={dimensionWhoseHeightToGet}
            dimension={dimension}
            height={headerHeight}
            key={`dimension-header-${dimension.id}`}
            left={left}
            mainDirection="down"
            previewSizes={previewSizes}
            width={headerWidth}
            gridId={gridId}
          />
        );
      })
    );
    return (
      // Putting position as relative here allows its children (the dimension headers)
      // to be absolutely positioned relatively to their parent
      <div
        style={{
          position: "relative",
          height,
          width,
          fontSize: `${zoom * 100}%`,
          overflow: "hidden"
        }}
        className="pivotgrid-dimension-headers"
      >
        {headers}
      </div>
    );
  }
}

export default DimensionHeaders;
