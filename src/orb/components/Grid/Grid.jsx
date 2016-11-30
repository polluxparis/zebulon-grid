import React, { PureComponent } from 'react';
// import { findDOMNode } from 'react-dom';
import { ScrollSync, ArrowKeyStepper } from 'react-virtualized';
import { DropTarget } from 'react-dnd';

import DataCells from '../../containers/DataCells';
import DimensionHeaders from '../../containers/DimensionHeaders';
import ColumnHeaders from '../../containers/ColumnHeaders';
import RowHeaders from '../../containers/RowHeaders';
import DragLayer from './DragLayer';

class PivotGrid extends PureComponent {
  constructor(props) {
    super(props);
    const { layout } = props;

    this.state = {
      rowVerticalCount: layout.rowVerticalCount,
      rowHorizontalCount: layout.rowHorizontalCount,
      columnVerticalCount: layout.columnVerticalCount,
      columnHorizontalCount: layout.columnHorizontalCount,
      cellsCache: { },
    };

    // this.scrollLeft = 0;
    // this.scrollTop = 0;

    this.isMouseDown = false;

    // store.getColumnWidth = store.getColumnWidth.bind(store);
    // store.getRowHeight = store.getRowHeight.bind(store);
    // store.getLastChildSize = store.getLastChildSize.bind(store);
  }

  componentWillReceiveProps(nextProps) {
    // Change scroll values to stay at the same position when modifying the layout
    // The current implementation only works when all cells have the same size
    // A better implementation would be to find which cells are at the beginning
    // upon receiving props and jumping there after
    // this.scrollLeft = this.dataCellsRef.grid.state.scrollLeft
    //   * (this.props.store.layout.columnHorizontalCount / this.state.columnHorizontalCount);
    // this.scrollTop = this.dataCellsRef.grid.state.scrollTop
    //   * (this.props.store.layout.rowVerticalCount / this.state.rowVerticalCount);

    // this.scrollToColumn = this.dataCellsRef.grid.props.scrollToColumn;
    // this.scrollToRow = this.dataCellsRef.grid.props.scrollToRow;
    this.setState({
      rowVerticalCount: nextProps.layout.rowVerticalCount,
      rowHorizontalCount: nextProps.layout.rowHorizontalCount,
      columnVerticalCount: nextProps.layout.columnVerticalCount,
      columnHorizontalCount: nextProps.layout.columnHorizontalCount,
    });
  }

  // componentDidUpdate() {
  //   // Clean cache for cell sizes
  //   // Call forceUpdate on the grid, so cannot be done in render
  //   // this.columnHeadersRef.grid.recomputeGridSize();
  //   // this.rowHeadersRef.grid.recomputeGridSize();
  //   // this.dataCellsRef.grid.recomputeGridSize();
  // }


  render() {
    const { connectDropTarget, width } = this.props;
    const {
      columnHorizontalCount,
      rowVerticalCount,
    } = this.state;

    return connectDropTarget(
      // Width has to be set in order to render correctly in a resizable box
      <div style={{ width }}>
        <DragLayer />
        <ArrowKeyStepper
          columnCount={columnHorizontalCount}
          mode="cells"
          rowCount={rowVerticalCount}
        >
          {({ onSectionRendered, scrollToColumn, scrollToRow }) => (
            <ScrollSync>
              {({ onScroll, scrollLeft, scrollTop }) => {
                this.datacellsCache = {};
                return (
                  <div>
                    <div style={{ display: 'flex' }}>
                      <DimensionHeaders />
                      <ColumnHeaders
                        scrollLeft={scrollLeft}
                      />
                    </div>
                    <div style={{ display: 'flex' }}>
                      <RowHeaders
                        scrollTop={scrollTop}
                      />
                      <DataCells
                        onSectionRendered={onSectionRendered}
                        scrollToColumn={scrollToColumn}
                        scrollToRow={scrollToRow}
                        // columnCount={columnHorizontalCount}
                        // height={Math.min(height - columnHeadersHeight,
                        //   rowHeadersHeight + scrollbarSize())}
                        onScroll={onScroll}
                        // ref={(ref) => { this.dataCellsRef = ref; }}
                        // rowCount={rowVerticalCount}
                        // width={Math.min(width - rowHeadersWidth,
                          // columnHeadersWidth + scrollbarSize())}
                      />
                    </div>
                  </div>
                );
              }
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
