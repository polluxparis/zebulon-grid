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
import { isEmpty } from '../../utils/generic';

// ------------------------------------------

class PivotGrid extends Component {
  constructor(props) {
    super(props);
    this.scrollToRow = 0;
    this.scrollToColumn = 0;
    this.focusCellKeys = [];
    this.handleScrollToChange = this.handleScrollToChange.bind(this);
    this.handleKeyDown = this.handleKeyDown.bind(this);
    // this.DynamicMenu = this.DynamicMenu.bind(this);
    // this.ConnectedMenu = this.ConnectedMenu.bind(this);
  }

  componentDidMount() {
    document.addEventListener('copy', this.handleCopy);
  }

  componentDidUnMount() {
    document.removeEventListener('copy', this.handleCopy);
  }

  componentWillReceiveProps(nextProps) {
    // let nextScrollToRow;
    // let nextScrollColumn;
    // const current = {};
    // const next = {};
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
    if (!isEmpty(nextProps.selectedRange.focusedCell)) {
      this.scrollToRow = nextProps.selectedRange.focusedCell.rowIndex;
      this.scrollToColumn = nextProps.selectedRange.focusedCell.columnIndex;
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
      this.props.copy(this.props.selectedRange);
    }
  };

  handleKeyDown = e => {
    const { columnHorizontalCount, rowVerticalCount } = this.props.layout;
    this.modifierKeyIsPressed = e.ctrlKey || e.metaKey;
    this.shiftKeyIsPressed = e.shiftKey;
    if (e.metaKey || e.ctrlKey) {
      // ctrl A -> select all
      if (e.which === 65) {
        this.props.selectRange({
          selectedCellStart: { columnIndex: 0, rowIndex: 0 },
          selectedCellEnd: {
            columnIndex: columnHorizontalCount,
            rowIndex: rowVerticalCount
          }
        });
        e.preventDefault();
      }
      // ctrl + -> zoom in
      if (e.key === '+') {
        this.props.selectRange({
          selectedCellStart: { columnIndex: 0, rowIndex: 0 },
          selectedCellEnd: {
            columnIndex: columnHorizontalCount,
            rowIndex: rowVerticalCount
          }
        });
        e.preventDefault();
      }
      // ctrl - -> zoom out
      if (e.key === '+') {
        this.props.selectRange({
          selectedCellStart: { columnIndex: 0, rowIndex: 0 },
          selectedCellEnd: {
            columnIndex: columnHorizontalCount,
            rowIndex: rowVerticalCount
          }
        });
        e.preventDefault();
      }
    }
  };
  handleKeyUp = e => {
    if (e.which === 17) {
      this.modifierKeyIsPressed = false;
    }
    if (e.which === 16) {
      this.shiftKeyIsPressed = false;
    }
  };

  handleScrollToChange = ({ scrollToColumn, scrollToRow }) => {
    if (this.shiftKeyIsPressed) {
      this.props.selectRange({
        selectedCellEnd: { columnIndex: scrollToColumn, rowIndex: scrollToRow },
        focusedCell: { columnIndex: scrollToColumn, rowIndex: scrollToRow }
      });
    } else {
      this.props.selectCell({
        columnIndex: scrollToColumn,
        rowIndex: scrollToRow
      });
    }
    this.scrollToRow = scrollToRow;
    this.scrollToColumn = scrollToColumn;
  };

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
              {({
                clientHeight,
                clientWidth,
                onScroll,
                scrollLeft,
                scrollTop
              }) =>
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
                      onSectionRendered={onSectionRendered}
                      scrollToColumn={scrollToColumn}
                      scrollToRow={scrollToRow}
                      onScroll={onScroll}
                      drilldown={drilldown}
                      clientHeight={clientHeight}
                      clientWidth={clientWidth}
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

PivotGrid.defaultProps = { id: 0 };

// Add grid id to the type to ensure only correct drop target is used
export default DropTarget(
  props => `cell-resize-handle--${props.id || 0}`,
  gridSpec,
  collect
)(PivotGrid);
