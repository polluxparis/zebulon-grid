import React, { Component } from 'react';
// import { findDOMNode } from 'react-dom';
import { ScrollSync } from 'react-virtualized';
import { DropTarget } from 'react-dnd';

import DataCells from '../DataCells';
import DimensionHeaders from '../DimensionHeaders';
import ColumnHeaders from '../ColumnHeaders';
import RowHeaders from '../RowHeaders';
import DragLayer from './DragLayer';
import { scrollbarSize } from '../../utils/domHelpers';

export class Grid extends Component {
  constructor(props) {
    super(props);
    const { store } = props;

    this.state = {
      rowVerticalCount: store.layout.rowVerticalCount,
      rowHorizontalCount: store.layout.rowHorizontalCount,
      columnVerticalCount: store.layout.columnVerticalCount,
      columnHorizontalCount: store.layout.columnHorizontalCount,
      cellsCache: { },
    };

    this.scrollLeft = 0;
    this.scrollTop = 0;

    this.isMouseDown = false;

    store.getColumnWidth = store.getColumnWidth.bind(store);
    store.getRowHeight = store.getRowHeight.bind(store);
    store.getLastChildSize = store.getLastChildSize.bind(store);
  }

  componentWillReceiveProps(nextProps) {
    // // Change scroll values to stay at the same position when modifying the layout
    // // The current implementation only works when all cells have the same size
    // // A better implementation would be to find which cells are at the beginning
    // // upon receiving props and jumping there after
    // this.scrollLeft = this.dataCellsRef.state.scrollLeft
    //   * (this.props.store.layout.columnHorizontalCount / this.state.columnHorizontalCount);
    // this.scrollTop = this.dataCellsRef.state.scrollTop
    //   * (this.props.store.layout.rowVerticalCount / this.state.rowVerticalCount);

    this.setState({
      rowVerticalCount: nextProps.store.layout.rowVerticalCount,
      rowHorizontalCount: nextProps.store.layout.rowHorizontalCount,
      columnVerticalCount: nextProps.store.layout.columnVerticalCount,
      columnHorizontalCount: nextProps.store.layout.columnHorizontalCount,
    });
  }

  componentDidUpdate() {
    // Clean cache for cell sizes
    // Call forceUpdate on the grid, so cannot be done in render
    this.columnHeadersRef.grid.recomputeGridSize();
    this.rowHeadersRef.grid.recomputeGridSize();
    this.dataCellsRef.grid.recomputeGridSize();
  }


  render() {
    const { connectDropTarget, store } = this.props;
    const {
      columnHeadersHeight,
      columnHeadersWidth,
      rowHeadersHeight,
      rowHeadersWidth,
     } = store.sizes;
    const {
      columnHorizontalCount,
      columnVerticalCount,
      rowHorizontalCount,
      rowVerticalCount,
    } = this.state;
    const height = this.props.height !== undefined ? this.props.height : store.config.height;
    const width = this.props.width !== undefined ? this.props.width : store.config.width;
    const hasScrollbarAtBottom = width
    < columnHeadersWidth + rowHeadersWidth + scrollbarSize();
    const hasScrollbarAtRight = height
    < columnHeadersHeight + rowHeadersHeight + scrollbarSize();
    const rowHeadersVisibleHeight = Math.min(
      height - columnHeadersHeight - (hasScrollbarAtBottom ? scrollbarSize() : 0),
      rowHeadersHeight);
    const columnHeadersVisibleWidth = Math.min(
      width - rowHeadersWidth - (hasScrollbarAtRight ? scrollbarSize() : 0),
      columnHeadersWidth);
    const previewSizes = {};
    previewSizes.height = Math.min(height - (hasScrollbarAtBottom ? scrollbarSize() : 0),
      rowHeadersHeight + columnHeadersHeight);
    previewSizes.width = Math.min(width - (hasScrollbarAtRight ? scrollbarSize() : 0),
      columnHeadersWidth + rowHeadersWidth);

    return connectDropTarget(
      // Width has to be set in order to render correctly in a resizable box
      <div style={{ width }}>
        <DragLayer />
        <ScrollSync>
          {({ onScroll, scrollLeft, scrollTop }) => {
            this.datacellsCache = {};
            return (
              <div>
                <div style={{ display: 'flex' }}>
                  <DimensionHeaders
                    store={store}
                    previewSizes={previewSizes}
                    height={columnHeadersHeight}
                    width={rowHeadersWidth}
                  />
                  <ColumnHeaders
                    columnCount={columnHorizontalCount}
                    height={columnHeadersHeight}
                    previewSizes={previewSizes}
                    ref={(ref) => { this.columnHeadersRef = ref; }}
                    rowCount={columnVerticalCount}
                    scrollLeft={scrollLeft}
                    store={store}
                    width={columnHeadersVisibleWidth}
                  />
                </div>
                <div style={{ display: 'flex' }}>
                  <RowHeaders
                    columnCount={rowHorizontalCount}
                    height={rowHeadersVisibleHeight}
                    previewSizes={previewSizes}
                    ref={(ref) => { this.rowHeadersRef = ref; }}
                    rowCount={rowVerticalCount}
                    scrollTop={scrollTop}
                    store={store}
                    width={rowHeadersWidth}
                  />
                  <DataCells
                    onScroll={onScroll}
                    store={store}
                    columnCount={columnHorizontalCount}
                    rowCount={rowVerticalCount}
                    ref={(ref) => { this.dataCellsRef = ref; }}
                    height={Math.min(height - columnHeadersHeight,
                      rowHeadersHeight + scrollbarSize())}
                    width={Math.min(width - rowHeadersWidth,
                      columnHeadersWidth + scrollbarSize())}
                  />
                </div>
              </div>
            );
          }
         }
        </ScrollSync>
      </div>);
  }
 }

const gridSpec = {
  drop(props, monitor, component) {
    const handle = monitor.getItem();
    const initialOffset = monitor.getInitialClientOffset();
    const offset = monitor.getClientOffset();
    component.props.store.updateCellSizes(handle, offset, initialOffset);
  },
};

const collect = connect => ({
  connectDropTarget: connect.dropTarget(),
});

export default DropTarget('cell-resize-handle', gridSpec, collect)(Grid);
