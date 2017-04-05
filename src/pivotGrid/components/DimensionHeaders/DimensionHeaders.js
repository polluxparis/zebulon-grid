import React, { Component } from 'react';
import { CellMeasurer } from 'react-virtualized/dist/commonjs/CellMeasurer';

import { AxisType } from '../../Axis';
import { MEASURE_ID } from '../../constants';
import DimensionHeader from '../DimensionHeader';

class DimensionHeaders extends Component {
  constructor() {
    super();
    // Start with a render cylce to measure cells
    this.measureCycle = true;
  }

  componentDidMount() {
    this.measureCycle = false;
    this.forceUpdate();
  }

  componentWillReceiveProps() {
    // If new props were received, do another measure cycle
    this.measureCycle = true;
  }

  shouldComponentUpdate(nextProps) {
    return nextProps.dimensionPositions !== this.props.dimensionPositions;
  }

  componentDidUpdate() {
    // Rerender component to get correct sizes on cells
    // Will only run if component has received new props
    // i.e. update came from the outside, not from forceUpdate
    if (this.measureCycle === true) {
      this.measureCycle = false;
      this.forceUpdate();
    }
  }

  render() {
    const {
      autoResizeColumn,
      columnDimensionHeaders,
      columnFields,
      dataHeadersLocation,
      dimensionPositions,
      getDimensionSize,
      height,
      measuredSizesCache,
      previewSizes,
      rowDimensionHeaders,
      rowFields,
      width,
      zoom,
      gridId
    } = this.props;
    const headers = [];

    const columnFieldsColumnIndex = rowFields.length +
      (dataHeadersLocation === 'columns' ? 0 : 1) -
      1;
    const rowFieldsRowIndex = columnDimensionHeaders.length -
      dataHeadersLocation ===
      'columns'
      ? 0
      : 1;
    // Get width for column dimension headers
    let fieldWhoseWidthToGet;
    if (dataHeadersLocation === 'rows') {
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
    headers.push(
      ...columnDimensionHeaders.map((dimensionHeader, index) => {
        const field = dimensionHeader.value;
        let style;
        if (!this.measureCycle) {
          const top = dimensionPositions.columns[field.id];
          const headerHeight = getDimensionSize(AxisType.COLUMNS, field.id);
          style = {
            top,
            left: width - headerWidth,
            width: headerWidth,
            height: headerHeight,
            position: 'absolute'
          };
        } else {
          style = {
            height: 'auto',
            left: 0,
            position: 'absolute',
            top: 0,
            width: 'auto'
          };
        }
        return (
          <CellMeasurer
            cache={measuredSizesCache}
            columnIndex={columnFieldsColumnIndex}
            key={field.id}
            rowIndex={index}
          >
            <DimensionHeader
              key={`dimension-header-${field.id}`}
              columnIndex={columnFieldsColumnIndex}
              resizeCell={autoResizeColumn}
              style={style}
              field={field}
              mainDirection="right"
              crossFieldId={fieldWhoseWidthToGet}
              previewSizes={previewSizes}
              gridId={gridId}
            />
          </CellMeasurer>
        );
      })
    );
    // Get height for row dimension headers in different cases
    let fieldWhoseHeightToGet;
    if (dataHeadersLocation === 'columns') {
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
    headers.push(
      ...rowDimensionHeaders.map((dimensionHeader, index) => {
        const field = dimensionHeader.value;
        let style;
        if (!this.measureCycle) {
          const left = dimensionPositions.rows[field.id];
          const headerWidth = getDimensionSize(AxisType.ROWS, field.id);
          style = {
            top: height - headerHeight,
            left,
            height: headerHeight,
            width: headerWidth,
            position: 'absolute'
          };
        } else {
          style = {
            height: 'auto',
            left: 0,
            position: 'absolute',
            top: 0,
            width: 'auto'
          };
        }

        return (
          <CellMeasurer
            cache={measuredSizesCache}
            columnIndex={index}
            key={field.id}
            rowIndex={rowFieldsRowIndex}
          >
            <DimensionHeader
              crossFieldId={fieldWhoseHeightToGet}
              columnIndex={index}
              resizeCell={autoResizeColumn}
              field={field}
              key={`dimension-header-${field.id}`}
              mainDirection="down"
              previewSizes={previewSizes}
              style={style}
              gridId={gridId}
            />
          </CellMeasurer>
        );
      })
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
            overflow: 'hidden'
          }}
          className="pivotgrid-dimension-headers"
        >
          {headers}
        </div>
      )
    );
  }
}

export default DimensionHeaders;
