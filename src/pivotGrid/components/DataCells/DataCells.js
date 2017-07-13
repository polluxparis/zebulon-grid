import React, { PureComponent } from 'react';
import { Grid as ReactVirtualizedGrid } from 'react-virtualized/dist/commonjs/Grid';

import { isInRange, isNullOrUndefined, isUndefined } from '../../utils/generic';
import { DataCell, HeaderType } from '../../Cells';
import DataCellComponent from '../DataCell';
import { AXIS_SEPARATOR } from '../../constants';

class DataCells extends PureComponent {
  constructor(props) {
    super(props);
    // this.cellRenderer = this.cellRenderer.bind(this);

    this.state = {
      valuesCache: {}
      // selectedCellStart: null,
      // selectedCellEnd: null
    };
  }

  componentWillReceiveProps() {
    this.setState({ valuesCache: this.valuesCache });
  }

  componentDidUpdate(prevProps) {
    this.isUpdating = false;
    if (
      prevProps.zoom !== this.props.zoom ||
      prevProps.sizes.heights !== this.props.sizes.heights ||
      prevProps.sizes.widths !== this.props.sizes.widths
    ) {
      this.grid.recomputeGridSize();
    }
  }

  handleMouseDown = (e, { columnIndex, rowIndex }) => {
    if (e.button === 0) {
      let range;

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
            // selectedCellStart: this.props.selectedRange.selectedCellStart,
            selectedCellEnd: { columnIndex, rowIndex },
            focusedCell: { columnIndex, rowIndex }
          });
        }
      } else {
        this.props.selectCell({ columnIndex, rowIndex });
      }
    }
    // this.props.handleMouseDown(columnIndex, rowIndex);
  };

  handleMouseUp = () => {
    this.isMouseDown = false;
  };

  handleMouseOver = (e, { columnIndex, rowIndex }) => {
    if (this.isMouseDown && e.buttons === 1) {
      if (
        !(
          this.props.selectedRange.selectedCellEnd.columnIndex ===
            columnIndex &&
          this.props.selectedRange.selectedCellEnd.rowIndex === rowIndex
        )
      ) {
        this.props.selectRange({
          // selectedCellStart: this.props.selectedRange.selectedCellStart,
          selectedCellEnd: { columnIndex, rowIndex },
          focusedCell: { columnIndex, rowIndex }
        });
      }
      // this.props.handleMouseDown(columnIndex, rowIndex);
    }
  };

  // handleDocumentMouseDown = e => {
  //   if (e.button === 0 && this.state.selectedCellStart) {
  //     if (!this.isMouseDown) {
  //       this.props.selectRange({
  //         selectedCellStart: null,
  //         selectedCellEnd: null
  //       });
  //     }
  //   }
  // };

  handleDrilldown = cell => {
    return this.props.drilldown(this.props.getCellInfosSelector(cell));
  };
  cellRenderer = ({ columnIndex, key, rowIndex, style }) => {
    // const { selectedCellStart, selectedCellEnd } = this.state;

    const {
      getCellValue,
      customFunctions,
      selectedCellEnd,
      rowLeaves,
      columnLeaves,
      measures,
      selectedRange
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
    // const cell = new DataCell(
    //   getCellValue,
    //   rowHeader,
    //   columnHeader,
    //   customFunctions
    // );
    const value = getCellValue(
      measure.valueAccessor,
      rowHeader.dataIndexes,
      columnHeader.dataIndexes,
      customFunctions.aggregation[measure.id]
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
      />
    );
  };
  // focused={focused}
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
        style={{ fontSize: `${zoom * 100}%` }}
        width={width}
      />
    );
  }
}

export default DataCells;
