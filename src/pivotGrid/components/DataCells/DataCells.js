import React, { PureComponent } from 'react';
import { Grid as ReactVirtualizedGrid } from 'react-virtualized/dist/commonjs/Grid';

import { isInRange, isUndefined } from '../../utils/generic';
import DataCellComponent from '../DataCell/DataCell';
import { AXIS_SEPARATOR, HeaderType, AxisType } from '../../constants';
import { connectMenu } from 'react-contextmenu';
import ContextMenu from '../ContextMenu/ContextMenu';
class DataCells extends PureComponent {
  state = {
    valuesCache: {}
  };

  componentWillReceiveProps() {
    this.setState({ valuesCache: this.valuesCache });
  }
  componentShouldUpdate(nextProps) {
    // if (
    //   nextProps.zoom !== this.props.zoom ||
    //   nextProps.sizes !== this.props.sizes
    // ) {
    //   this.grid.recomputeGridSize();
    // }
    return (
      nextProps.zoom !== this.props.zoom ||
      nextProps.sizes !== this.props.sizes ||
      nextProps.rowLeaves !== this.props.rowLeaves ||
      nextProps.columnLeaves !== this.props.columnLeaves ||
      nextProps.selectedRange !== this.props.selectedRange
    );
  }

  componentDidUpdate(prevProps) {
    if (
      prevProps.sizes !== this.props.sizes ||
      prevProps.dimensions !== this.props.dimensions ||
      prevProps.zoom !== this.props.zoom
    ) {
      this.grid.recomputeGridSize();
    }
    this.isUpdating = false;
  }

  handleKeyDown = e => {
    // Page down
    if (e.key === 'PageDown') {
      if (e.shiftKey) {
      } else {
      }
      e.preventDefault();
    }

    // Page up
    if (e.key === 'PageUp') {
      if (e.shiftKey) {
      } else {
      }
      e.preventDefault();
    }
  };

  handleMouseDown = (e, { columnIndex, rowIndex }) => {
    if (e.button === 0) {
      this.isMouseDown = true;
      if (e.shiftKey) {
        if (
          !(
            this.props.selectedRange.selectedCellEnd.columnIndex ===
              columnIndex &&
            this.props.selectedRange.selectedCellEnd.rowIndex === rowIndex
          )
        ) {
          this.props.selectRange({
            selectedCellEnd: { columnIndex, rowIndex },
            focusedCell: { columnIndex, rowIndex }
          });
        }
      } else {
        this.props.selectCell({ columnIndex, rowIndex });
      }
    }
  };

  handleMouseUp = () => {
    this.isMouseDown = false;
  };

  handleMouseOver = (e, { columnIndex, rowIndex }) => {
    // buttons = 1 to help when mouse is up outside of the grid
    if (this.isMouseDown && e.buttons === 1) {
      if (
        !(
          this.props.selectedRange.selectedCellEnd.columnIndex ===
            columnIndex &&
          this.props.selectedRange.selectedCellEnd.rowIndex === rowIndex
        )
      ) {
        this.props.selectRange({
          selectedCellEnd: { columnIndex, rowIndex },
          focusedCell: { columnIndex, rowIndex }
        });
      }
    }
  };

  handleDrilldown = cell => {
    return this.props.drilldown(this.props.getCellInfosSelector(cell));
  };
  collectMenu = props => {
    return {
      ...props,
      dimensions: this.props.dimensions
    };
  };
  cellRenderer = ({ columnIndex, key, rowIndex, style }) => {
    const {
      getCellValue,
      // customFunctions,
      rowLeaves,
      columnLeaves,
      measures,
      selectedRange,
      gridId
    } = this.props;
    const rowHeader = rowLeaves[rowIndex];
    const columnHeader = columnLeaves[columnIndex];

    let measure;
    if (rowHeader.type === HeaderType.MEASURE) {
      measure = measures[rowHeader.id];
    } else {
      measure = measures[columnHeader.id];
    }
    // empty measure
    if (isUndefined(measure)) {
      measure = { format: v => v, valueAccessor: () => null };
    }

    let selected = false;
    if (selectedRange.selectedCellStart && selectedRange.selectedCellEnd) {
      selected = isInRange(
        { columnIndex, rowIndex },
        selectedRange.selectedCellStart,
        selectedRange.selectedCellEnd
      );
    }
    let focused = false;
    if (selectedRange.focusedCell) {
      focused =
        columnIndex === selectedRange.focusedCell.columnIndex &&
        rowIndex === selectedRange.focusedCell.rowIndex;
    }
    // This causes all the data cells to be rendered when new cells are selected via mouse actions
    // It is not optimal, we could implement a memoizer so that cells are not recalculated but it would
    // bring complexity and this is good enough at the time.
    const value = getCellValue(
      measure.valueAccessor,
      rowHeader.dataIndexes,
      columnHeader.dataIndexes,
      measure.aggregation
      // customFunctions.aggregation[measure.id]
    );
    const cellKey = `${rowHeader.key}${AXIS_SEPARATOR}${columnHeader.key}`;
    this.valuesCache[cellKey] = value;
    let valueHasChanged = false;
    if (this.isUpdating) {
      const oldValue = this.state.valuesCache[cellKey];
      // NaN is not equal to NaN... hence the last condition
      if (oldValue !== undefined && value !== oldValue && !isNaN(oldValue)) {
        valueHasChanged = true;
      }
    }
    const caption = measure.format(value);
    return (
      <DataCellComponent
        key={key}
        valueHasChanged={valueHasChanged}
        style={style}
        rowIndex={rowIndex}
        columnIndex={columnIndex}
        caption={caption}
        drilldown={this.handleDrilldown}
        handleMouseDown={this.handleMouseDown}
        handleMouseOver={this.handleMouseOver}
        handleMouseUp={this.handleMouseUp}
        selected={selected}
        focused={focused}
        collectMenu={this.collectMenu}
        gridId={gridId}
      />
    );
  };

  render() {
    const {
      getColumnWidth,
      getRowHeight,
      onScroll,
      height,
      width,
      scrollToColumn,
      scrollToRow,
      onSectionRendered,
      zoom,
      columnLeaves,
      rowLeaves,
      gridId
    } = this.props;
    this.valuesCache = {};
    const ConnectedMenu = connectMenu(`context-menu-data-cell-${gridId}`)(
      ContextMenu
    );

    return (
      <div>
        <ReactVirtualizedGrid
          cellRenderer={this.cellRenderer}
          className="pivotgrid-data-cells"
          columnCount={columnLeaves.length}
          columnWidth={getColumnWidth}
          height={height}
          onScroll={onScroll}
          ref={ref => {
            this.grid = ref;
          }}
          rowCount={rowLeaves.length}
          rowHeight={getRowHeight}
          onSectionRendered={onSectionRendered}
          scrollToColumn={scrollToColumn}
          scrollToRow={scrollToRow}
          style={{ fontSize: `${zoom * 100}%` }}
          width={width}
        />
        <ConnectedMenu />
      </div>
    );
  }
}

export default DataCells;
