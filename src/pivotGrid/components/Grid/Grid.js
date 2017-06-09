import PropTypes from 'prop-types';
import React, { Component } from 'react';
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
import { getNextKey, getCellInfosKey } from '../../utils/keys';

class PivotGrid extends Component {
  constructor(props) {
    super(props);
    this.rowStartIndex = 0;
    this.columnStartIndex = 0;
    this.focusCellKeys = [];
  }

  componentWillReceiveProps(nextProps) {
    let nextRowStartIndex;
    let nextColumnStartIndex;
    const current = {};
    const next = {};
    if (this.props.focusCells !== nextProps.focusCells) {
      this.focusCellKeys = nextProps.focusCells.map(cell =>
        getCellInfosKey(cell)
      );
      if (this.focusCellKeys.length > 0) {
        nextColumnStartIndex = keyToIndex(
          nextProps.columnHeaders,
          this.focusCellKeys[0].columns
        );
        nextRowStartIndex = keyToIndex(
          nextProps.rowHeaders,
          this.focusCellKeys[0].rows
        );
      }
    } else {
      current.dataFieldsCount = this.props.dataFieldsCount;
      next.dataFieldsCount = nextProps.dataFieldsCount;
      if (this.props.rowHeaders.length !== nextProps.rowHeaders.length) {
        current.fields = this.props.rowFields;
        next.fields = nextProps.rowFields;
        current.firstHeaderRow = this.props.rowHeaders[this.rowStartIndex];
        const nextFirstHeaderKey = getNextKey(current, next);
        nextRowStartIndex = keyToIndex(
          nextProps.rowHeaders,
          nextFirstHeaderKey
        );
      }
      if (this.props.columnHeaders.length !== nextProps.columnHeaders.length) {
        current.fields = this.props.columnFields;
        next.fields = nextProps.columnFields;
        current.firstHeaderRow = this.props.columnHeaders[
          this.columnStartIndex
        ];
        const nextFirstHeaderKey = getNextKey(current, next);
        nextColumnStartIndex = keyToIndex(
          nextProps.columnHeaders,
          nextFirstHeaderKey
        );
      }
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
      width,
      layout,
      customFunctions,
      drilldown,
      id: gridId
    } = this.props;

    const { columnHorizontalCount, rowVerticalCount } = layout;

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
                    <DimensionHeaders gridId={gridId} />
                    <ColumnHeaders
                      gridId={gridId}
                      scrollLeft={scrollLeft}
                      scrollToColumn={scrollToColumn}
                      ref={ref => {
                        this.columnHeaders = ref;
                      }}
                    />
                  </div>
                  <div style={{ display: 'flex' }}>
                    <RowHeaders
                      scrollTop={scrollTop}
                      // scrollToRow={scrollToRow}
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
                      focusCellKeys={this.focusCellKeys}
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
