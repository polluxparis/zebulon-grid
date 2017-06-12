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
      columnFields,
      dataHeadersLocation,
      dimensionPositions,
      getDimensionSize,
      height,
      previewSizes,
      rowDimensionHeaders,
      rowFields,
      width,
      zoom,
      gridId
    } = this.props;
    let headers = [];

    // Get width for column dimension headers
    let fieldWhoseWidthToGet;
    if (dataHeadersLocation === "rows") {
      // Dimension headers are on top of the measures column
      fieldWhoseWidthToGet = MEASURE_ID;
    } else if (rowFields.length) {
      // Dimension headers are on top of the column of the last field of the row headers
      fieldWhoseWidthToGet = rowFields[rowFields.length - 1].id;
    } else {
      // Dimension headers are on top of the Total header --> get default width
      fieldWhoseWidthToGet = null;
    }
    const headerWidth = getDimensionSize(AxisType.ROWS, fieldWhoseWidthToGet);
    headers = headers.concat(
      columnDimensionHeaders.map(dimensionHeader => {
        const field = dimensionHeader.value;
        const top = dimensionPositions.columns[field.id];
        const headerHeight = getDimensionSize(AxisType.COLUMNS, field.id);
        return (
          <DimensionHeader
            key={`dimension-header-${field.id}`}
            left={width - headerWidth}
            top={top}
            width={headerWidth}
            height={headerHeight}
            field={field}
            mainDirection="right"
            crossFieldId={fieldWhoseWidthToGet}
            previewSizes={previewSizes}
            gridId={gridId}
          />
        );
      })
    );
    // Get height for row dimension headers in different cases
    let fieldWhoseHeightToGet;
    if (dataHeadersLocation === "columns") {
      // Dimension headers are to the left of the measures row
      fieldWhoseHeightToGet = MEASURE_ID;
    } else if (columnFields.length) {
      // Dimension headers are to the left of the row of the last field of the column headers
      fieldWhoseHeightToGet = columnFields[columnFields.length - 1].id;
    } else {
      // Dimension headers are to the left of the Total header --> get default height
      fieldWhoseHeightToGet = null;
    }
    const headerHeight = getDimensionSize(
      AxisType.COLUMNS,
      fieldWhoseHeightToGet
    );
    headers = headers.concat(
      rowDimensionHeaders.map(dimensionHeader => {
        const field = dimensionHeader.value;
        const left = dimensionPositions.rows[field.id];
        const headerWidth = getDimensionSize(AxisType.ROWS, field.id);
        return (
          <DimensionHeader
            top={height - headerHeight}
            crossFieldId={fieldWhoseHeightToGet}
            field={field}
            height={headerHeight}
            key={`dimension-header-${field.id}`}
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
