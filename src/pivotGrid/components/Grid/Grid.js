import React, { PropTypes, Component } from 'react';
import { ScrollSync } from 'react-virtualized/dist/commonjs/ScrollSync';
import { DropTarget } from 'react-dnd';

import ArrowKeyStepper from '../ArrowKeyStepper/ArrowKeyStepper';
import DataCells from '../../containers/DataCells';
import DimensionHeaders from '../../containers/DimensionHeaders';
import ColumnHeaders from '../../containers/ColumnHeaders';
import RowHeaders from '../../containers/RowHeaders';
import DragLayer from './DragLayer';
import { Header, DataHeader } from '../../Cells';
import { keyToIndex } from '../../AxisUi';
import { KEY_SEPARATOR } from '../../constants';

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
  constructor(props) {
    super(props);
    this.rowStartIndex = 0;
    this.columnStartIndex = 0;
    // this.getGridRect = this.getGridRect.bind(this);
    // this.state = {};
  }

  componentDidMount() {
    // this.gridRect = this.getGridRect();
    // this.setState({ gridRect: this.getGridRect() });
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
      // this.setState({ gridRect: this.getGridRect() });
    }
  }

  handleSectionRendered(onSectionRendered) {
    return indexes => {
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
    const {
      connectDropTarget,
      width,
      layout,
      customFunctions,
      drilldown
    } = this.props;

    const {
      columnHorizontalCount,
      rowVerticalCount
    } = layout;
    const gridId = this.props.id || 0;

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
                <div className="orb-pivotgrid">
                  <div style={{ display: 'flex' }}>
                    <DimensionHeaders gridId={gridId} />
                    <ColumnHeaders
                      gridId={gridId}
                      scrollLeft={scrollLeft}
                      ref={ref => {
                        this.columnHeaders = ref;
                      }}
                    />
                  </div>
                  <div style={{ display: 'flex' }}>
                    <RowHeaders
                      scrollTop={scrollTop}
                      gridId={gridId}
                      ref={ref => {
                        this.rowHeaders = ref;
                      }}
                    />
                    <DataCells
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
  // height: PropTypes.number.isRequired,
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

// Add grid id to the type to ensure only correct drop target is used
export default DropTarget(
  props => `cell-resize-handle--${props.id}`,
  gridSpec,
  collect
)(PivotGrid);
