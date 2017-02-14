import React, { PureComponent } from 'react';

import { AxisType } from '../../Axis';
import { MEASURE_ID } from '../../constants';
import DimensionHeader from '../DimensionHeader';

class DimensionHeaders extends PureComponent {
  render() {
    const {
      columnDimensionHeaders,
      columns,
      dataHeadersLocation,
      dimensionPositions,
      getDimensionSize,
      height,
      previewSizes,
      rowDimensionHeaders,
      rows,
      width,
      zoom,
    } = this.props;
    const headers = [];

    // Get width for column dimension headers
    let fieldWhoseWidthToGet;
    if (dataHeadersLocation === 'rows') {
      // Dimension headers are on top of the measures column
      fieldWhoseWidthToGet = MEASURE_ID;
    } else if (rows.fields.length) {
      // Dimension headers are on top of the column of the last field of the row headers
      fieldWhoseWidthToGet = rows.fields[rows.fields.length - 1].id;
    } else {
      // Dimension headers are on top of the Total header --> get default width
      fieldWhoseWidthToGet = null;
    }
    const headerWidth = getDimensionSize(AxisType.ROWS, fieldWhoseWidthToGet);
    headers.push(
      ...columnDimensionHeaders.map(dimensionHeader => {
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
          />
        );
      }),
    );
    // Get height for row dimension headers in different cases
    let fieldWhoseHeightToGet;
    if (dataHeadersLocation === 'columns') {
      // Dimension headers are to the left of the measures row
      fieldWhoseHeightToGet = MEASURE_ID;
    } else if (columns.fields.length) {
      // Dimension headers are to the left of the row of the last field of the column headers
      fieldWhoseHeightToGet = columns.fields[columns.fields.length - 1].id;
    } else {
      // Dimension headers are to the left of the Total header --> get default height
      fieldWhoseHeightToGet = null;
    }
    const headerHeight = getDimensionSize(
      AxisType.COLUMNS,
      fieldWhoseHeightToGet,
    );
    headers.push(
      ...rowDimensionHeaders.map(dimensionHeader => {
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
          />
        );
      }),
    );
    return (
      // Putting position as relative here allows its children (the dimension headers)
      // to be absolutely positioned relatively to their parent
      (
        <div
          style={{
            position: 'relative',
            height,
            width,
            fontSize: `${zoom * 100}%`,
            overflow: 'hidden',
          }}
          className="orb-dimension-headers"
        >
          {headers}
        </div>
      )
    );
  }
}

export default DimensionHeaders;
