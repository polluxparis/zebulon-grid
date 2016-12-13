import React, { PureComponent } from 'react';
// import { findDOMNode } from 'react-dom';
import { ScrollSync, ArrowKeyStepper } from 'react-virtualized';
import { DropTarget } from 'react-dnd';

import DataCells from '../../containers/DataCells';
import DimensionHeaders from '../../containers/DimensionHeaders';
import ColumnHeaders from '../../containers/ColumnHeaders';
import RowHeaders from '../../containers/RowHeaders';
import DragLayer from './DragLayer';
import { keyToIndex } from '../../AxisUi';
import { KEY_SEPARATOR } from '../../constants';


function getNextKey(current, next) {
  const firstLeafHeader = current.firstHeaderRow[current.firstHeaderRow.length - 1];
  const keys = firstLeafHeader.key.split(KEY_SEPARATOR);
  let nextKey = '';
  if (current.fields.length > next.fields.length) {
    const nextFieldIds = next.fields.map(field => field.id);
    const missingFieldPosition = current.fields
      .findIndex(field => !nextFieldIds.includes(field.id));
    nextKey = keys.slice(0, missingFieldPosition).join(KEY_SEPARATOR);
  } else if (current.fields.length < next.fields.length) {
    const previousFieldIds = current.fields.map(field => field.id);
    const newFieldPosition = next.fields
      .findIndex(field => !previousFieldIds.includes(field.id));
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

class PivotGrid extends PureComponent {
  constructor(props) {
    super(props);
    this.rowStartIndex = 0;
    this.columnStartIndex = 0;
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
      nextColumnStartIndex = keyToIndex(nextProps.columnHeaders, nextFirstHeaderKey);
    }
    // If keyToIndex does not find the key in the headers, it returns -1
    // In this case, do nothing
    if (nextRowStartIndex >= 0) this.rowStartIndex = nextRowStartIndex;
    if (nextColumnStartIndex >= 0) this.columnStartIndex = nextColumnStartIndex;
  }

  handleSectionRendered(onSectionRendered) {
    return (indexes) => {
      const { rowStartIndex, columnStartIndex } = indexes;
      // When the data cells grid is re rendered, it resets row and column
      // start indexes, losing the information about the previous position,
      // this prevents that.
      // I's a hack until I better understand this behaviour.
      if (rowStartIndex) this.rowStartIndex = rowStartIndex;
      if (columnStartIndex) this.columnStartIndex = columnStartIndex;
      onSectionRendered(indexes);
    };
  }


  render() {
    const { connectDropTarget, width, layout, customFunctions } = this.props;
    const {
      columnHorizontalCount,
      rowVerticalCount,
    } = layout;

    return connectDropTarget(
      // Width has to be set in order to render correctly in a resizable box
      <div style={{ width }}>
        <DragLayer />
        <ArrowKeyStepper
          columnCount={columnHorizontalCount}
          mode="edges"
          rowCount={rowVerticalCount}
          scrollToRow={this.rowStartIndex}
          scrollToColumn={this.columnStartIndex}
        >
          {({ onSectionRendered, scrollToColumn, scrollToRow }) => (
            <ScrollSync>
              {({ onScroll, scrollLeft, scrollTop }) => (
                <div className="orb-pivotgrid">
                  <div style={{ display: 'flex' }}>
                    <DimensionHeaders />
                    <ColumnHeaders
                      scrollLeft={scrollLeft}
                      ref={(ref) => { this.columnHeaders = ref; }}
                    />
                  </div>
                  <div style={{ display: 'flex' }}>
                    <RowHeaders
                      scrollTop={scrollTop}
                      ref={(ref) => { this.rowHeaders = ref; }}
                    />
                    <DataCells
                      customFunctions={customFunctions}
                      onSectionRendered={this.handleSectionRendered(onSectionRendered)}
                      scrollToColumn={scrollToColumn}
                      scrollToRow={scrollToRow}
                      onScroll={onScroll}
                    />
                  </div>
                </div>
              )
             }
            </ScrollSync>
          )}
        </ArrowKeyStepper>
      </div>);
  }
 }

const gridSpec = {
  drop(props, monitor, component) {
    const handle = monitor.getItem();
    const initialOffset = monitor.getInitialClientOffset();
    const offset = monitor.getClientOffset();
    component.props.updateCellSize({ handle, offset, initialOffset });
  },
};

const collect = connect => ({
  connectDropTarget: connect.dropTarget(),
});

export default DropTarget('cell-resize-handle', gridSpec, collect)(PivotGrid);
