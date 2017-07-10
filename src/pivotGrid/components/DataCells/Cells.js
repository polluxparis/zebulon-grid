import React, { PureComponent } from 'react';
import { findDOMNode } from 'react-dom';
import { Grid as ReactVirtualizedGrid } from 'react-virtualized/dist/commonjs/Grid';

import { isInRange, isUndefined, isNullOrUndefined } from '../../utils/generic';
import { DataCell, HeaderType } from '../../Cells';
import DataCellComponent from '../DataCell';
import { AXIS_SEPARATOR } from '../../constants';
class Cells extends PureComponent {
  constructor(props) {
    super(props);
    //     this.cellRenderer = this.cellRenderer.bind(this);
    //     this.handleCopy = this.handleCopy.bind(this);
    //     this.handleMouseDown = this.handleMouseDown.bind(this);
    //     this.handleMouseUp = this.handleMouseUp.bind(this);
    //     this.handleMouseOver = this.handleMouseOver.bind(this);
    //     this.handleKeyDown = this.handleKeyDown.bind(this);
    //     this.handleDocumentMouseDown = this.handleDocumentMouseDown.bind(this);
    //     this.handleDrilldown = this.handleDrilldown.bind(this);

    //     this.state = {
    //       valuesCache: {}
    //       // selectedCellStart: null,
    //       // selectedCellEnd: null
    //     };
  }

  componentDidMount() {
    document.addEventListener('mouseup', this.handleMouseUp);
    document.addEventListener('mousedown', this.handleDocumentMouseDown);
    document.addEventListener('keydown', this.handleKeyDown);
    document.addEventListener('copy', this.handleCopy);
  }

  componentWillReceiveProps() {
    this.setState({ valuesCache: this.valuesCache });
  }

  componentWillUpdate() {
    this.isUpdating = true;
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

  componentDidUnMount() {
    document.removeEventListener('mouseup', this.handleMouseUp);
    document.removeEventListener('mousedown', this.handleDocumentMouseDown);
    document.removeEventListener('keydown', this.handleKeyDown);
    document.removeEventListener('copy', this.handleCopy);
  }
  // mouse down
  handleMouseDown = (e, { columnIndex, rowIndex }) => {
    if (e.button === 0) {
      this.isMouseDown = true;
      this.props.selectRange({
        selectedCellStart: { columnIndex, rowIndex },
        selectedCellEnd: { columnIndex, rowIndex }
      });
      // this.setState({ selectedCellStart: { columnIndex, rowIndex } });
      // this.setState({ selectedCellEnd: { columnIndex, rowIndex } });
    }
  };
  // mouse up
  handleMouseUp = () => {
    this.isMouseDown = false;
  };
  // mouse over
  handleMouseOver = ({ columnIndex, rowIndex }) => {
    if (this.isMouseDown) {
      this.props.selectRange({
        selectedCellEnd: { columnIndex, rowIndex }
      });
    }
  };

  handleDocumentMouseDown = e => {
    if (e.button === 0 && this.state.selectedCellStart) {
      if (!this.isMouseDown) {
        this.props.selectRange({
          selectedCellStart: null,
          selectedCellEnd: null
        });
      }
    }
  };
  // key down
  handleKeyDown = e => {
    const { columnLeaves, rowLeaves } = this.props;
    if (e.which === 69) {
      if (!this.perf) {
        window.Perf.start();
        this.perf = true;
      } else {
        this.perf = false;
        window.Perf.stop();
      }
    }
    // ctrl+A
    if (e.which === 65 && (e.metaKey || e.ctrlKey)) {
      if (
        // Works only if the data cells are focused
        // Later we could make it work if any part of the grid
        // (row and columns headers...) are focused
        findDOMNode(this.grid) === e.target
      ) {
        this.props.selectRange({
          selectedCellStart: { columnIndex: 0, rowIndex: 0 },
          selectedCellEnd: {
            columnIndex: columnLeaves.length,
            rowIndex: rowLeaves.length
          }
        });
      }
      e.preventDefault();
    }
  };
  // copy
  handleCopy = () => {
    if (
      // Works only if the data cells are focused
      // Later we could make it work if any part of the grid
      // (row and columns headers...) are focused
      findDOMNode(this.grid) === document.activeElement
    ) {
      const { selectedCellStart, selectedCellEnd } = this.state;
      this.props.copy({
        selectedCellStart,
        selectedCellEnd
      });
    }
  };
  // drill down
  handleDrilldown = cell => {
    return this.props.drilldown(this.props.getCellInfosSelector(cell));
  };

  // returns if the cell must be selected
  isSelected = (columnIndex, rowIndex) => {
    const { selectedRange } = this.props;
    if (
      !isNullOrUndefined(selectedRange) &&
      selectedRange.selectedCellStart &&
      selectedRange.selectedCellEnd
    ) {
      return isInRange(
        { columnIndex, rowIndex },
        selectedRange.selectedCellStart,
        selectedRange.selectedCellEnd
      );
    } else {
      return false;
    }
  };

  getCellInfo = (key, parameters) => {
    const {
      isCached,
      areChangesManaged,
      getValue,
      getCaption,
      getStyle
    } = this.props;
    let cellInfo = this.state.cellCache[key] || {};
    const isEmpty = isEmptyObj(cellInfo);
    if (!isEmpty && areChangesManaged) {
      cellInfo.oldValue = cellInfo.value;
    }
    if (!isCached || isEmpty) {
      if (!isNullOrUndefined(getValue)) {
        cellInfo.value = getCellValue(parameters);
      }
      if (!isNullOrUndefined(getCellCaption)) {
        cellInfo.caption = getCellCaption(parameters);
      }
      if (!isNullOrUndefined(getCellStyle)) {
        cellInfo.style = getCellStyle(parameters);
      }
      if (!isNullOrUndefined(getColumnWidth)) {
        cellInfo.sizes.width = getColumnWidth(parameters);
      }
      if (!isNullOrUndefined(getRowHeight)) {
        cellInfo.sizes.height = getRowHeight(parameters);
      }
      if (isCached) {
        this.state.cellCache = cellInfo;
      }
    }

    return cellInfo;
  };
  // getCellValue=()=>{};

  cellRenderer = ({ columnIndex, key, rowIndex, style }) => {
    // const { selectedCellStart, selectedCellEnd } = this.state;

    const {
      getCellValue,
      customFunctions,
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

    // This causes all the data cells to be rendered when new cells are selected via mouse actions
    // It is not optimal, we could implement a memoizer so that cells are not recalculated but it would
    // bring complexity and this is good enough at the time.
    // const cell = new DataCell(
    //   getCellValue,
    //   rowHeader,
    //   columnHeader,
    //   customFunctions
    // );
    const parameters = {
      accessor: measure.valueAccessor,
      rowDataindexes: rowHeader.dataIndexes,
      columnDataIndexes: columnHeader.dataIndexes,
      aggregation: customFunctions.aggregation[measure.id],
      format: measure.format(value)
    };
    // const cellKey = `${rowHeader.key}${AXIS_SEPARATOR}${columnHeader.key}`;
    // this.valuesCache[cellKey] = value;
    // let valueHasChanged = false;
    // if (this.isUpdating) {
    //   const oldValue = this.state.valuesCache[cellKey];
    //   // NaN is not equal to NaN... hence the last condition
    //   if (oldValue !== undefined && value !== oldValue && !isNaN(oldValue)) {
    //     valueHasChanged = true;
    //   }
    // }
    // const caption = measure.format(value);
    return (
      <DataCellComponent
        key={key}
        value={cellInfo.value}
        oldValue={cellInfo.OldValue}
        caption={cellInfo.caption}
        style={cellInfo.style}
        width={cellInfo.width}
        height={cellInfo.height}
        rowIndex={rowIndex}
        columnIndex={columnIndex}
        drilldown={this.handleDrilldown}
        handleMouseDown={this.handleMouseDown}
        handleMouseOver={this.handleMouseOver}
        selected={isSelected(columnIndex, rowIndex)}
      />
    );
  };
  // // focused={focused}
  // render() {
  //   const {
  //     getColumnWidth,
  //     getRowHeight,
  //     onScroll,
  //     height,
  //     width,
  //     scrollToColumn,
  //     scrollToRow,
  //     onSectionRendered,
  //     zoom,
  //     columnLeaves,
  //     rowLeaves
  //   } = this.props;
  //   this.valuesCache = {};
  //   return (
  //     <ReactVirtualizedGrid
  //       className="pivotgrid-data-cells"
  //       cellRenderer={this.cellRenderer}
  //       height={height}
  //       width={width}
  //       ref={ref => {
  //         this.grid = ref;
  //       }}
  //       style={{ fontSize: `${zoom * 100}%` }}
  //       columnCount={columnLeaves.length}
  //       columnWidth={getColumnWidth}
  //       rowCount={rowLeaves.length}
  //       rowHeight={getRowHeight}
  //       scrollToAlignment="start"
  //       scrollToColumn={scrollToColumn}
  //       scrollToRow={scrollToRow}
  //       onScroll={onScroll}
  //       onSectionRendered={onSectionRendered}
  //     />
  //   );
  // }
}

export default Cells;
