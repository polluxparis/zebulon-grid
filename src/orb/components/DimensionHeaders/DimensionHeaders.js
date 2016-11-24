import React, { PureComponent } from 'react';

import { AxisType } from '../../Axis';
import { MEASURE_ID } from '../../stores/Store';
import DimensionHeader from '../DimensionHeader';

class DimensionHeaders extends PureComponent {
  render() {
    const { store, height, width, previewSizes, dataHeadersLocation } = this.props;
    const rowDimensionHeaders = store.rowsUi.dimensionHeaders;
    const columnDimensionHeaders = store.columnsUi.dimensionHeaders;
    const headers = [];

    // Get width for column dimension headers
    let fieldWhoseWidthToGet;
    if (dataHeadersLocation === 'rows') {
      // Dimension headers are on top of the measures column
      fieldWhoseWidthToGet = MEASURE_ID;
    } else if (store.rows.fields.length) {
      // Dimension headers are on top of the column of the last field of the row headers
      fieldWhoseWidthToGet = store.rows.fields[store.rows.fields.length - 1].code;
    } else {
      // Dimension headers are on top of the Total header --> get default width
      fieldWhoseWidthToGet = null;
    }
    const headerWidth = store.getDimensionSize(AxisType.ROWS, fieldWhoseWidthToGet);
    headers.push(
      ...columnDimensionHeaders.map((dimensionHeader) => {
        const field = dimensionHeader.value;
        const top = store.dimensionPositions.columns[field.code];
        const headerHeight = store.getDimensionSize(AxisType.COLUMNS, field.code);
        return (
          <DimensionHeader
            key={`dimension-header-${field.code}`}
            right={0}
            top={top}
            width={headerWidth}
            height={headerHeight}
            field={field}
            mainDirection="right"
            crossFieldCode={fieldWhoseWidthToGet}
            previewSizes={previewSizes}
          />);
      }));
    // Get height for row dimension headers in different cases
    let fieldWhoseHeightToGet;
    if (dataHeadersLocation === 'columns') {
      // Dimension headers are to the left of the measures row
      fieldWhoseHeightToGet = MEASURE_ID;
    } else if (store.columns.fields.length) {
      // Dimension headers are to the left of the row of the last field of the column headers
      fieldWhoseHeightToGet = store.columns.fields[store.columns.fields.length - 1].code;
    } else {
      // Dimension headers are to the left of the Total header --> get default height
      fieldWhoseHeightToGet = null;
    }
    const headerHeight = store.getDimensionSize(AxisType.COLUMNS, fieldWhoseHeightToGet);
    headers.push(
      ...rowDimensionHeaders.map((dimensionHeader) => {
        const field = dimensionHeader.value;
        const left = store.dimensionPositions.rows[field.code];
        const headerWidth = store.getDimensionSize(AxisType.ROWS, field.code);
        return (
          <DimensionHeader
            bottom={0}
            crossFieldCode={fieldWhoseHeightToGet}
            field={field}
            height={headerHeight}
            key={`dimension-header-${field.code}`}
            left={left}
            mainDirection="down"
            previewSizes={previewSizes}
            width={headerWidth}
          />);
      }));
    return (
      // Putting position as relative here allows its children (the dimension headers)
      // to be absolutely positioned relatively to their parent
      <div style={{ position: 'relative', height, width, fontSize: `${store.zoom * 100}%`, overflow: 'hidden' }}>
        {headers}
      </div>
    );
  }
}

export default DimensionHeaders;
