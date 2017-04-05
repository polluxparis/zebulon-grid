import React, { PropTypes, Component } from 'react';
import { ScrollSync } from 'react-virtualized/dist/commonjs/ScrollSync';
import {
  CellMeasurerCache
} from 'react-virtualized/dist/commonjs/CellMeasurer';
import { DropTarget } from 'react-dnd';

import ArrowKeyStepper from '../ArrowKeyStepper/ArrowKeyStepper';
import DataCells from '../../containers/DataCells';
import DimensionHeaders from '../../containers/DimensionHeaders';
import ColumnHeaders from '../../containers/ColumnHeaders';
import RowHeaders from '../../containers/RowHeaders';
import DragLayer from './DragLayer';
import { Header, DataHeader, getDimensionId } from '../../Cells';
import { keyToIndex } from '../../AxisUi';
import { KEY_SEPARATOR, MINIMUM_CELL_SIZE } from '../../constants';

function getNextKey(current, next) {
  const firstLeafHeader = current.firstHeaderRow[
    current.firstHeaderRow.length - 1
  ];
  const keys = firstLeafHeader.key.split(KEY_SEPARATOR);
  let nextKey = '';
  if (current.fields.length > next.fields.length) {
    const nextFieldIds = next.fields.map(field => field.id);
    const missingFieldPosition = current.fields.findIndex(
      field => !nextFieldIds.includes(field.id)
    );
    nextKey = keys.slice(0, missingFieldPosition).join(KEY_SEPARATOR);
  } else if (current.fields.length < next.fields.length) {
    const previousFieldIds = current.fields.map(field => field.id);
    const newFieldPosition = next.fields.findIndex(
      field => !previousFieldIds.includes(field.id)
    );
    nextKey = keys.slice(0, newFieldPosition).join(KEY_SEPARATOR);
  } else if (current.dataFieldsCount !== next.dataFieldsCount) {
    // A data field has been toggled
    nextKey = keys.slice(0, -1).join(KEY_SEPARATOR);
  } else {
    // A filter has been modified
    // For the moment, do nothing
    nextKey = '';
  }
  return nextKey;
}

class PivotGrid extends Component {
  constructor() {
    super();
    this.rowStartIndex = 0;
    this.columnStartIndex = 0;
    // Putting default width at MINIMUM_CELL_SIZE allows size cache to work when some columns are sparse
    // for example in column headers.
    // Since the width of a column is the max of the width of the cell, if a column has no cell
    // in its upper row, this will put the width of the column as the width of the cell as desired.
    this.measuredSizeCaches = {
      dimensions: new CellMeasurerCache({
        defaultWidth: MINIMUM_CELL_SIZE,
        fixedHeight: true
      }),
      columns: new CellMeasurerCache({
        defaultWidth: MINIMUM_CELL_SIZE,
        fixedHeight: true
      }),
      rows: new CellMeasurerCache({
        defaultWidth: MINIMUM_CELL_SIZE,
        fixedHeight: true
      }),
      datacells: new CellMeasurerCache({
        defaultWidth: MINIMUM_CELL_SIZE,
        fixedHeight: true
      })
    };

    this.handleAutoResizeColumnHeader = this.handleAutoResizeColumnHeader.bind(
      this
    );
    this.handleAutoResizeRowHeaderColumn = this.handleAutoResizeRowHeaderColumn.bind(
      this
    );
  }

  componentWillReceiveProps(nextProps) {
    let nextRowStartIndex;
    let nextColumnStartIndex;
    const current = {};
    const next = {};
    current.dataFieldsCount = this.props.dataFieldsCount;
    next.dataFieldsCount = nextProps.dataFieldsCount;
    if (this.props.rowHeaders.length !== nextProps.rowHeaders.length) {
      current.fields = this.props.rowFields;
      next.fields = nextProps.rowFields;
      current.firstHeaderRow = this.props.rowHeaders[this.rowStartIndex];
      const nextFirstHeaderKey = getNextKey(current, next);
      nextRowStartIndex = keyToIndex(nextProps.rowHeaders, nextFirstHeaderKey);
    }
    if (this.props.columnHeaders.length !== nextProps.columnHeaders.length) {
      current.fields = this.props.columnFields;
      next.fields = nextProps.columnFields;
      current.firstHeaderRow = this.props.columnHeaders[this.columnStartIndex];
      const nextFirstHeaderKey = getNextKey(current, next);
      nextColumnStartIndex = keyToIndex(
        nextProps.columnHeaders,
        nextFirstHeaderKey
      );
    }
    // If keyToIndex does not find the key in the headers, it returns -1
    // In this case, do nothing
    if (nextRowStartIndex >= 0) this.rowStartIndex = nextRowStartIndex;
    if (nextColumnStartIndex >= 0) this.columnStartIndex = nextColumnStartIndex;
  }

  componentDidUpdate(prevProps) {
    const { height, width, setSizes } = this.props;
    if (height !== prevProps.height || width !== prevProps.width) {
      setSizes({ height, width });
    }
  }

  handleAutoResizeRowHeaderColumn(header) {
    const headerMeasuredWidth = this.measuredSizeCaches.rows.columnWidth({
      index: header.y
    });
    const dimensionHeadersMeasuredWidth = this.measuredSizeCaches.dimensions.columnWidth(
      { index: header.y }
    );
    this.props.setCellSize({
      cell: { key: getDimensionId(header), axisType: header.axisType },
      size: Math.max(headerMeasuredWidth, dimensionHeadersMeasuredWidth),
      direction: 'dimensions'
    });
  }

  handleAutoResizeColumnHeader(header) {
    const autoResizeOneColumn = header => {
      const dataCellsMeasuredColumnWidth = this.measuredSizeCaches.datacells.columnWidth(
        {
          index: header.x
        }
      );
      const leafHeaderMeasuredWidth = this.measuredSizeCaches.columns.columnWidth(
        {
          index: header.x
        }
      );
      this.props.setCellSize({
        cell: header,
        size: Math.max(dataCellsMeasuredColumnWidth, leafHeaderMeasuredWidth),
        direction: 'leafs'
      });
    };
    if (header.subheaders && header.subheaders.length) {
      header.subheaders.forEach(subheader =>
        this.handleAutoResizeColumnHeader(subheader));
    } else {
      autoResizeOneColumn(header);
    }
  }

  handleSectionRendered(onSectionRendered) {
    return indexes => {
      const { rowStartIndex, columnStartIndex } = indexes;
      this.rowStartIndex = rowStartIndex;
      this.columnStartIndex = columnStartIndex;
      onSectionRendered(indexes);
    };
  }

  render() {
    const {
      connectDropTarget,
      customFunctions,
      drilldown,
      id: gridId,
      layout,
      width
    } = this.props;

    const {
      columnHorizontalCount,
      rowVerticalCount
    } = layout;

    return connectDropTarget(
      // Width has to be set in order to render correctly in a resizable box
      // Position must be relative so that the absolutely positioned DragLayer behaves correctly
      <div style={{ width, position: 'relative' }}>
        <DragLayer gridId={gridId} />
        <ArrowKeyStepper
          columnCount={columnHorizontalCount}
          mode="align:top-left"
          rowCount={rowVerticalCount}
          scrollToRow={this.rowStartIndex}
          scrollToColumn={this.columnStartIndex}
        >
          {({ onSectionRendered, scrollToColumn, scrollToRow }) => (
            <ScrollSync>
              {({ onScroll, scrollLeft, scrollTop }) => (
                <div className="pivotgrid-pivotgrid">
                  <div style={{ display: 'flex' }}>
                    <DimensionHeaders
                      gridId={gridId}
                      measuredSizesCache={this.measuredSizeCaches.dimensions}
                      autoResizeColumn={this.handleAutoResizeRowHeaderColumn}
                    />
                    <ColumnHeaders
                      autoResizeColumn={this.handleAutoResizeColumnHeader}
                      gridId={gridId}
                      measuredSizesCache={this.measuredSizeCaches.columns}
                      scrollLeft={scrollLeft}
                      scrollToColumn={scrollToColumn}
                    />
                  </div>
                  <div style={{ display: 'flex' }}>
                    <RowHeaders
                      autoResizeColumn={this.handleAutoResizeRowHeaderColumn}
                      measuredSizesCache={this.measuredSizeCaches.rows}
                      scrollTop={scrollTop}
                      scrollToRow={scrollToRow}
                      gridId={gridId}
                    />
                    <DataCells
                      measuredSizesCache={this.measuredSizeCaches.datacells}
                      customFunctions={customFunctions}
                      onSectionRendered={this.handleSectionRendered(
                        onSectionRendered
                      )}
                      scrollToColumn={scrollToColumn}
                      scrollToRow={scrollToRow}
                      onScroll={onScroll}
                      drilldown={drilldown}
                    />
                  </div>
                </div>
              )}
            </ScrollSync>
          )}
        </ArrowKeyStepper>
      </div>
    );
  }
}

const gridSpec = {
  drop(props, monitor, component) {
    const handle = monitor.getItem();
    const initialOffset = monitor.getInitialClientOffset();
    const offset = monitor.getClientOffset();
    component.props.updateCellSize({ handle, offset, initialOffset });
  }
};

const collect = connect => ({
  connectDropTarget: connect.dropTarget()
});

PivotGrid.propTypes = {
  columnFields: PropTypes.arrayOf(PropTypes.object).isRequired,
  columnHeaders: PropTypes.arrayOf(
    PropTypes.arrayOf(
      PropTypes.oneOfType([
        PropTypes.instanceOf(Header),
        PropTypes.instanceOf(DataHeader)
      ])
    )
  ).isRequired,
  connectDropTarget: PropTypes.func.isRequired,
  customFunctions: PropTypes.shape({
    aggregation: PropTypes.object,
    format: PropTypes.object,
    sort: PropTypes.object
  }).isRequired,
  dataFieldsCount: PropTypes.number.isRequired,
  drilldown: PropTypes.func.isRequired,
  id: PropTypes.oneOfType([PropTypes.number, PropTypes.string]),
  layout: PropTypes.shape({
    columnHorizontalCount: PropTypes.number,
    columnVerticalCount: PropTypes.number,
    rowHorizontalCount: PropTypes.number,
    rowVerticalCount: PropTypes.number
  }).isRequired,
  rowFields: PropTypes.arrayOf(PropTypes.object).isRequired,
  rowHeaders: PropTypes.arrayOf(
    PropTypes.arrayOf(
      PropTypes.oneOfType([
        PropTypes.instanceOf(Header),
        PropTypes.instanceOf(DataHeader)
      ])
    )
  ).isRequired,
  setSizes: PropTypes.func.isRequired,
  width: PropTypes.number.isRequired
};

PivotGrid.defaultProps = { id: 0 };

// Add grid id to the type to ensure only correct drop target is used
export default DropTarget(
  props => `cell-resize-handle--${props.id || 0}`,
  gridSpec,
  collect
)(PivotGrid);
