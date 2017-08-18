import React, { PureComponent } from 'react';
import { Grid as ReactVirtualizedGrid } from 'react-virtualized/dist/commonjs/Grid';

import { isInRange, isUndefined } from '../../utils/generic';
import DataCell from '../DataCell/DataCell';
import { AXIS_SEPARATOR, HeaderType } from '../../constants';

class DataCells extends PureComponent {
  state = {
    valuesCache: {}
  };

  componentWillReceiveProps() {
    this.setState({ valuesCache: this.valuesCache });
  }
  shouldComponentUpdate(nextProps) {
    return (
      nextProps.zoom !== this.props.zoom ||
      nextProps.sizes !== this.props.sizes ||
      nextProps.rowLeaves !== this.props.rowLeaves ||
      nextProps.columnLeaves !== this.props.columnLeaves ||
      nextProps.selectedRange !== this.props.selectedRange ||
      nextProps.height !== this.props.height ||
      nextProps.width !== this.props.width
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
    return this.props.drilldown(this.props.getCellInfos(cell));
  };
  collectMenu = props => {
    return {
      ...props,
      dimensions: this.props.dimensions,
      menuFunctions: this.props.menuFunctions,
      filters: this.props.filters,
      zoom: this.props.zoom
    };
  };
  handleClickMenu = (e, data, target) => {
    if (e.button === 0) {
      if (data.action === 'drilldown') {
        this.handleDrilldown({
          columnIndex: data.columnIndex,
          rowIndex: data.rowIndex
        });
      } else if (data.functionType === 'cell') {
        this.props.menuFunctions.dataCellFunctions[data.action].function(
          this.props.getCellInfos({
            columnIndex: data.columnIndex,
            rowIndex: data.rowIndex
          })
        );
      } else if (data.functionType === 'range') {
        this.props.menuFunctions.rangeFunctions[data.action].function(
          this.props.getRangeInfos(this.props.selectedRange)
        );
      } else if (data.functionType === 'function') {
        this.props.menuFunctions.functions[data.action].function();
      }
    }
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
      <DataCell
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
        handleClickMenu={this.handleClickMenu}
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
      columnLeaves,
      rowLeaves
    } = this.props;
    this.valuesCache = {};

    return (
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
        width={width}
      />
    );
  }
}

export default DataCells;
