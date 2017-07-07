import PropTypes from 'prop-types';
import React, { Component } from 'react';
import { ScrollSync } from 'react-virtualized/dist/commonjs/ScrollSync';
import { ArrowKeyStepper } from 'react-virtualized/dist/commonjs/ArrowKeyStepper';
import { DropTarget } from 'react-dnd';

// import ArrowKeyStepper from '../../containers/ArrowKeyStepper';
import DataCells from '../../containers/DataCells';
import DimensionHeaders from '../../containers/DimensionHeaders';
import ColumnHeaders from '../../containers/ColumnHeaders';
import RowHeaders from '../../containers/RowHeaders';
import DragLayer from './DragLayer';
import { Header, DataHeader } from '../../Cells';
import { AxisType } from '../../Axis';
import { keyToIndex } from '../../AxisUi';
import { getNextKey, getCellInfosSelectorKey } from '../../utils/keys';
import {
  isNull,
  isInRange,
  isNullOrUndefined,
  isUndefined
} from '../../utils/generic';

class PivotGrid extends Component {
  constructor(props) {
    super(props);
    this.scrollToRow = 0;
    this.scrollToColumn = 0;
    this.focusCellKeys = [];
    this.handleScrollToChange = this.handleScrollToChange.bind(this);
    this.handleKeyDown = this.handleKeyDown.bind(this);
  }

  componentDidMount() {
    document.addEventListener('copy', this.handleCopy);
  }

  componentDidUnMount() {
    document.removeEventListener('copy', this.handleCopy);
  }

  componentWillReceiveProps(nextProps) {
    let nextScrollToRow;
    let nextScrollColumn;
    const current = {};
    const next = {};
    // if (this.props.focusCells !== nextProps.focusCells) {
    //   this.focusCellKeys = nextProps.focusCells.map(cell =>
    //     getCellInfosSelectorKey(cell)
    //   );
    //   if (this.focusCellKeys.length > 0) {
    //     nextScrollColumn = keyToIndex(
    //       nextProps.columnHeaders,
    //       this.focusCellKeys[0].columns
    //     );
    //     nextScrollToRow = keyToIndex(
    //       nextProps.rowHeaders,
    //       this.focusCellKeys[0].rows
    //     );
    //   }
    // } else {

    // // Update row and column indexes to make sure the same header stay in view when toggling measure or filtering
    // current.dataDimensionsCount = this.props.dataDimensionsCount;
    // next.dataDimensionsCount = nextProps.dataDimensionsCount;
    // if (this.props.layout.rowVerticalCount !== nextProps.layout.rowVerticalCount) {
    //   console.log('updating row start index');
    //   current.dimensions = this.props.rowDimensions;
    //   next.dimensions = nextProps.rowDimensions;
    //   current.firstHeaderRow = this.props.rowHeaders[this.scrollToRow];
    //   const nextFirstHeaderKey = getNextKey(current, next);
    //   nextScrollToRow = keyToIndex(nextProps.rowHeaders, nextFirstHeaderKey);
    // }
    // if (this.props.columnHeaders.length !== nextProps.columnHeaders.length) {
    //   console.log('updating column start index');
    //   current.dimensions = this.props.columnDimensions;
    //   next.dimensions = nextProps.columnDimensions;
    //   current.firstHeaderRow = this.props.columnHeaders[this.columnStartIndex];
    //   const nextFirstHeaderKey = getNextKey(current, next);
    //   nextScrollColumn = keyToIndex(
    //     nextProps.columnHeaders,
    //     nextFirstHeaderKey
    //   );
    // }
    // }
    // // If keyToIndex does not find the key in the headers, it returns -1
    // // In this case, do nothing
    // if (nextScrollToRow >= 0) this.scrollToRow = nextScrollToRow;
    // if (nextScrollColumn >= 0) this.columnStartIndex = nextScrollColumn;
    if (!isNull(nextProps.selectedRange.selectedCellEnd)) {
      this.scrollToRow = nextProps.selectedRange.selectedCellEnd.rowIndex;
      this.scrollToColumn = nextProps.selectedRange.selectedCellEnd.columnIndex;
    }
  }

  componentDidUpdate(prevProps) {
    const { height, width, setSizes } = this.props;
    if (height !== prevProps.height || width !== prevProps.width) {
      setSizes({ height, width });
    }
  }

  handleCopy = () => {
    if (
      // Works only if the grid is focused
      this.modifierKeyIsPressed
    ) {
      // const { selectedCellStart, selectedCellEnd } = this.state;
      this.props.copy(this.props.selectedRange);
    }
  };

  // handleMouseDown = (columnIndex, rowIndex) => {
  // this.props.handleMouseDown(e, {
  //   columnIndex: this.props.columnIndex,
  //   rowIndex: this.props.rowIndex
  // });
  // console.log([columnIndex, rowIndex]);
  // this.scrollToRow = rowIndex;
  // this.scrollToColumn = columnIndex;
  // };

  // handleMouseOver = () => {
  // this.props.handleMouseOver({
  //   columnIndex: this.props.columnIndex,
  //   rowIndex: this.props.rowIndex
  // });
  // };

  handleKeyDown = e => {
    const { columnHorizontalCount, rowVerticalCount } = this.props.layout;
    this.modifierKeyIsPressed = e.ctrlKey || e.metaKey;
    this.shiftKeyIsPressed = e.shiftKey;
    // console.log([this.shiftKeyIsPressed, e.shiftKey]);
    // ctrl+A
    if (e.which === 65 && (e.metaKey || e.ctrlKey)) {
      this.props.selectRange({
        selectedCellStart: { columnIndex: 0, rowIndex: 0 },
        selectedCellEnd: {
          columnIndex: columnHorizontalCount,
          rowIndex: rowVerticalCount
        }
      });
      e.preventDefault();
    }
  };
  handleKeyUp = e => {
    if (e.which === 17) {
      this.modifierKeyIsPressed = false;
    }
    if (e.which === 16) {
      this.shiftKeyIsPressed = false;
      // console.log(this.shiftKeyKeyIsPressed);
    }
  };
  handleSectionRendered(onSectionRendered) {
    return indexes => {
      // const { rowStartIndex, columnStartIndex } = indexes;
      // this.scrollToRow = rowStartIndex;
      // this.scrollToColumn = columnStartIndex;

      onSectionRendered(indexes);
    };
  }

  handleScrollToChange({ scrollToColumn, scrollToRow }) {
    console.log([scrollToColumn, scrollToRow, this.shiftKeyIsPressed]);
    if (this.shiftKeyIsPressed) {
      this.props.selectRange({
        selectedCellStart: this.props.selectedRange.selectedCellStart,
        selectedCellEnd: { columnIndex: scrollToColumn, rowIndex: scrollToRow }
      });
    } else {
      this.props.selectCell({
        columnIndex: scrollToColumn,
        rowIndex: scrollToRow
      });
    }
    this.scrollToRow = scrollToRow;
    this.scrollToColumn = scrollToColumn;
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
      <div
        style={{ width, position: 'relative' }}
        onKeyDown={this.handleKeyDown}
        onKeyUp={this.handleKeyUp}
      >
        <DragLayer gridId={gridId} />
        <ArrowKeyStepper
          columnCount={columnHorizontalCount}
          mode="cells"
          rowCount={rowVerticalCount}
          scrollToRow={this.scrollToRow}
          scrollToColumn={this.scrollToColumn}
          onScrollToChange={this.handleScrollToChange}
          isControlled={true}
        >
          {({ onSectionRendered, scrollToColumn, scrollToRow }) =>
            <ScrollSync>
              {({ onScroll, scrollLeft, scrollTop }) =>
                <div className="pivotgrid-pivotgrid">
                  <div style={{ display: 'flex' }}>
                    <DimensionHeaders gridId={gridId} />
                    <ColumnHeaders
                      gridId={gridId}
                      scrollLeft={scrollLeft}
                      scrollTop={0}
                    />
                  </div>
                  <div style={{ display: 'flex' }}>
                    <RowHeaders
                      scrollTop={scrollTop}
                      scrollLeft={0}
                      gridId={gridId}
                    />
                    <DataCells
                      customFunctions={customFunctions}
                      onSectionRendered={this.handleSectionRendered(
                        onSectionRendered
                      )}
                      scrollToColumn={scrollToColumn}
                      scrollToRow={scrollToRow}
                      // focusCellKeys={this.focusCellKeys}
                      onScroll={onScroll}
                      drilldown={drilldown}
                      // handleMouseDown={this.handleMouseDown}
                      // handleMouseOver={this.handleMouseOver}
                    />
                  </div>
                </div>}
            </ScrollSync>}
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
  columnDimensions: PropTypes.arrayOf(PropTypes.object).isRequired,
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
  dataDimensionsCount: PropTypes.number.isRequired,
  drilldown: PropTypes.func.isRequired,
  id: PropTypes.oneOfType([PropTypes.number, PropTypes.string]),
  layout: PropTypes.shape({
    columnHorizontalCount: PropTypes.number,
    columnVerticalCount: PropTypes.number,
    rowHorizontalCount: PropTypes.number,
    rowVerticalCount: PropTypes.number
  }).isRequired,
  rowDimensions: PropTypes.arrayOf(PropTypes.object).isRequired,
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
