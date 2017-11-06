import React, { Component } from "react";
import { findDOMNode } from "react-dom";
import { DropTarget } from "react-dnd";
import { connectMenu } from "react-contextmenu";
import ContextMenu from "../ContextMenu/ContextMenu";

// import ArrowKeyStepper from '../../containers/ArrowKeyStepper';
import DataCells from "../../containers/DataCells";
import DimensionHeaders from "../../containers/DimensionHeaders";
import ColumnHeaders from "../../containers/ColumnHeaders";
import RowHeaders from "../../containers/RowHeaders";
import DragLayer from "./DragLayer";
// import { isEmpty } from "../../utils/generic";
import { ZOOM_IN, ZOOM_OUT, AxisType, ScrollbarSize } from "../../constants";
// import * as actions from '../../actions';
// ------------------------------------------
// CONCEPTS
// ------------------------------------------
// The pivot grid is build using a dataset (as an array of data rows) and a meta description(configuration) of the data set
// most of the configuration can be determined from the dataset, nevertheless
// - MEASURE:
// measures are the descriptions for available computed (aggregated) values displayed into the grid
// measures properties are
//    - id
//    - label
//    - aggregation function
//    - data accessor
//    - format
//  measures together can be restituted in rows or columns
// - DIMENSION:
//  dimensions are the descriptions of the axes used to determine the perimeter of the calculation of the measures (group by)
// dimension properties are:
//    - id
//    - label
//    - id accessor
//    - label accessor
//    - sort accessor
//    - format
//    - parent dimension for 'attributes'
//      attributes are additional properties of a dimension with (0,1) cardinality (person -> gender, age, nationality...) ,g
//      it will be possible to expand/collapse those attributes
//  any dimension can be restituted either in row nor in column
//  filters can be defined on the dimension and will be applied to the dataset
// in a 'SQL like' expression you may represent the result of your configuration as something like:
// select
//        measure1.aggregation_function(measure1.data_accessor),
//        measure2.aggregation_function(measure2.data_accessor)
// from   dataset
// where  dimension3.filter_function()
// group by
//        dimension1.id_accessor,dimension1.label_accessor in row,
//        dimension2.id_accessor,dimension2.label_accessor in row,
//        dimension3.id_accessor,dimension3.label_accessor in column
// order by
//        dimension1.sort_accessor,dimension2.sort_accessor
// ------------------------------------------
// PIVOT GRID
// ------------------------------------------
// pivot grid is the main component is the main component
// it is decomposed in 4 different parts
// - the data cells grid where the measures aggregation calculations are displayed
// this is a virtualised grid in rows and columns with standards navigation, selection, copy... functionalities
// - the row headers grid
// this is a virtualised grid in rows, synchronized in rows with the the datacells grid that display,
// as a tree the successive dimensions values (for in row dimensions)
// this is a virtualised grid in columns, synchronized in columns with the the datacells grid that display,
// as a tree the successive dimensions values (for in column dimensions)
// - the dimension headers grid that will display the captions of dimensions in rows and columns
// most of changes of the pivot grid configuration are available there (dimension  moves, sort...)
// const Checkbox = ({ onChange, checked, key, label, index, style }) =>
//   <div style={{ ...style, textAlign: 'left' }}>
//     <label>
//       <input
//         type="checkbox"
//         value={label}
//         onChange={onChange}
//         checked={checked || false}
//       />
//       {label}
//     </label>
//   </div>;

class PivotGrid extends Component {
  componentDidMount() {
    // this.element = document.getElementById(this.props.id);
    // if (this.element) {
    document.addEventListener("copy", this.handleCopy);
    document.addEventListener("keydown", this.handleKeyDown);
    // }
  }
  componentDidUnMount() {
    // if (this.element) {
    document.removeEventListener("copy", this.handleCopy);
    document.removeEventListener("keydown", this.handleKeyDown);
    // }
  }
  componentDidUpdate(prevProps) {
    const { height, width, setSizes } = this.props;
    if (height !== prevProps.height || width !== prevProps.width) {
      setSizes({ height, width });
    }
    // if (!this.element) {
    //   this.element = document.getElementById(this.props.id);
    //   if (this.element) {
    //     this.element.addEventListener("copy", this.handleCopy);
    //     this.element.addEventListener("keydown", this.handleKeyDown);
    //   }
    // }
  }
  //  componentDidMount() {
  //   this.element = document.getElementById(this.props.id);
  //   // this.element.addEventListener("copy", this.handleCopy);
  //   // this.element.addEventListener("keydown", this.handleKeyDown);
  // }

  componentWillReceiveProps(nextProps) {
    this.isPushing = this.props.pushedData !== nextProps.pushedData;
  }
  nextVisible = (leaves, index, direction, offset) => {
    let ix = 0,
      n = 0;
    if (offset === 0) {
      ix = -direction;
      n = -1;
    }
    while (
      n < offset &&
      (direction === 1 ? index + ix < leaves.length - 1 : index + ix > 0)
    ) {
      ix += direction;
      n += leaves[index + ix].isVisible || 0;
    }
    return index + ix;
  };
  handleKeyDown = e => {
    if (
      !e.defaultPrevented && this.props.isActive === undefined
        ? true
        : this.props.isActive
    ) {
      this.modifierKeyIsPressed = e.ctrlKey || e.metaKey;
      const { selectedRange, selectRange, selectCell, headers } = this.props;
      const { rows, columns } = headers;
      if (e.metaKey || e.ctrlKey) {
        // To be consistent with browser behaviour, we also accept = which is on the same keyboard touch as +
        if (e.key === "+" || e.key === "=") {
          this.props.zoom(ZOOM_IN);
          e.preventDefault();
        }
        // ctrl - -> zoom out
        // To be consistent with browser behaviour, we also accept _ which is on the same keyboard touch as -
        if (e.key === "-" || e.key === "_") {
          this.props.zoom(ZOOM_OUT);
          e.preventDefault();
        } else if (e.which === 65) {
          // ctrl A -> select all
          this.props.selectRange({
            selectedCellStart: {
              columnIndex: this.nextVisible(columns.leaves, 0, -1, 0),
              rowIndex: this.nextVisible(rows.leaves, 0, -1, 0)
            },
            selectedCellEnd: {
              columnIndex: this.nextVisible(
                columns.leaves,
                columns.length - 1,
                1,
                0
              ),
              rowIndex: this.nextVisible(rows.leaves, rows.length - 1, 1, 0)
            }
          });
          e.preventDefault();
        }
        // arrow keys
      } else if (e.which > 32 && e.which < 41) {
        let direction, cell, axis;
        if (e.key === "ArrowDown" || e.key === "ArrowUp") {
          direction = e.key === "ArrowDown" ? 1 : -1;
          axis = AxisType.ROWS;
          cell = {
            columnIndex: selectedRange.selectedCellEnd.columnIndex,
            rowIndex: this.nextVisible(
              rows.leaves,
              selectedRange.selectedCellEnd.rowIndex,
              direction,
              1
            )
          };
        } else if (e.key === "ArrowRight" || e.key === "ArrowLeft") {
          direction = e.key === "ArrowRight" ? 1 : -1;
          axis = AxisType.COLUMNS;
          cell = {
            columnIndex: this.nextVisible(
              columns.leaves,
              selectedRange.selectedCellEnd.columnIndex,
              direction,
              1
            ),
            rowIndex: selectedRange.selectedCellEnd.rowIndex
          };
        } else if (e.key === "PageUp" || e.key === "PageDown") {
          direction = e.key === "PageDown" ? 1 : -1;
          if (e.altKey) {
            axis = AxisType.COLUMNS;
            cell = {
              columnIndex: this.nextVisible(
                columns.leaves,
                selectedRange.selectedCellEnd.columnIndex,
                direction,
                columns.cells.length - 1
              ),
              rowIndex: selectedRange.selectedCellEnd.rowIndex
            };
          } else {
            axis = AxisType.ROWS;
            cell = {
              columnIndex: selectedRange.selectedCellEnd.columnIndex,
              rowIndex: this.nextVisible(
                rows.leaves,
                selectedRange.selectedCellEnd.rowIndex,
                direction,
                rows.cells.length - 1
              )
            };
          }
        } else if (e.key === "Home" || e.key === "End") {
          direction = e.key === "End" ? 1 : -1;
          if (e.altKey) {
            axis = AxisType.COLUMNS;
            cell = {
              columnIndex: this.nextVisible(
                columns.leaves,
                direction === 1 ? columns.length - 1 : 0,
                -direction,
                0
              ),
              rowIndex: selectedRange.selectedCellEnd.rowIndex
            };
          } else {
            axis = AxisType.ROWS;
            cell = {
              columnIndex: selectedRange.selectedCellEnd.columnIndex,
              rowIndex: this.nextVisible(
                rows.leaves,
                direction === 1 ? rows.length - 1 : 0,
                -direction,
                0
              )
            };
          }
        }
        // selection
        if (e.shiftKey) {
          selectRange({
            ...selectedRange,
            selectedCellEnd: cell
          });
        } else {
          selectCell(cell);
        }
        // shift display at end
        const scroll = { rows: rows.scroll, columns: columns.scroll };
        if (
          axis === AxisType.ROWS &&
          rows.hasScrollbar &&
          (cell.rowIndex >= rows.stopIndex || cell.rowIndex <= rows.startIndex)
        ) {
          scroll.rows = { index: cell.rowIndex, direction: -direction };
          this.onScroll(scroll.rows, scroll.columns);
        } else if (
          axis === AxisType.COLUMNS &&
          columns.hasScrollbar &&
          (cell.columnIndex >= columns.stopIndex ||
            cell.columnIndex <= columns.startIndex)
        ) {
          scroll.columns = { index: cell.columnIndex, direction: -direction };
          this.onScroll(scroll.rows, scroll.columns);
        }
        e.preventDefault();
      }
    }
  };
  handleWheel = e => {
    if (!e.defaultPrevented) {
      e.preventDefault();
      const { rows, columns } = this.props.headers;
      const sense = e.altKey || e.deltaX !== 0 ? "columns" : "rows";
      const leaves = sense === "columns" ? columns : rows;
      const direction = Math.sign(sense === "columns" ? e.deltaX : e.deltaY);

      const prevIndex = direction === 1 ? leaves.stopIndex : leaves.startIndex;
      const offset = leaves.direction === direction && leaves.offset > 2;
      const index = this.nextVisible(
        leaves.leaves,
        prevIndex - offset,
        direction,
        1
      );
      const scroll = { rows: rows.scroll, columns: columns.scroll };
      if (sense === "columns") {
        scroll.columns = { index, direction: -direction };
        this.onScroll(scroll.rows, scroll.columns);
      } else {
        scroll.rows = { index, direction: -direction };
        this.onScroll(scroll.rows, scroll.columns);
      }
    }
  };
  // componentDidUpdate(prevProps) {
  //   const { height, width, setSizes } = this.props;
  //   if (height !== prevProps.height || width !== prevProps.width) {
  //     setSizes({ height, width });
  //   }
  // }
  handleCopy = e => {
    if (
      // Works only if the grid is focused
      this.modifierKeyIsPressed && this.props.isActive === undefined
        ? true
        : this.props.isActive
    ) {
      this.props.copy(this.props.selectedRange);
      // e.preventDefault();
    }
  };
  handleExport = () => {
    if (
      // Works only if the grid is focused
      this.modifierKeyIsPressed
    ) {
      this.props.copy(this.props.selectedRange);
    }
  };

  onScroll = (scrollToRow, scrollToColumn) => {
    let { rows, columns } = this.props.headers;
    if (
      scrollToRow !== null &&
      (scrollToRow.index !== rows.index ||
        scrollToRow.direction !== rows.direction)
    ) {
      this.props.scrollToRow(scrollToRow);
    }
    if (
      scrollToColumn !== null &&
      (scrollToColumn.index !== columns.index ||
        scrollToColumn.direction !== columns.direction)
    ) {
      this.props.scrollToColumn(scrollToColumn);
    }
  };

  render() {
    const {
      connectDropTarget,
      height,
      width,
      drilldown,
      zoomValue,
      headers,
      gridId
    } = this.props;
    const { rows, columns } = headers;
    let grid;
    if (this.props.status.loading || this.props.status.loadingConfig) {
      grid = <div>Loading data...</div>;
    } else if (this.props.status.error) {
      if (this.props.status.error.message === "No rows retrieved") {
        grid = <div style={{ width: "max-content" }}>No rows retrieved</div>;
      } else {
        grid = (
          <div style={{ color: "red", width: "max-content" }}>
            <p>{this.props.status.error.type}</p>
            <p>{this.props.status.error.message}</p>
          </div>
        );
      }
    } else if (rows !== undefined && columns !== undefined) {
      // this.computeGrid(rows, columns);
      const scrollbarsWidth = {
        horizontal: ScrollbarSize * (columns.hasScrollbar || 0),
        vertical: ScrollbarSize * (rows.hasScrollbar || 0)
      };
      const dataCellsSizes = {
        height: Math.min(
          height - rows.crossSize - scrollbarsWidth.horizontal,
          rows.size
        ),
        width: Math.min(
          width - columns.crossSize - scrollbarsWidth.vertical,
          columns.size
        )
      };
      const ConnectedMenu = connectMenu(`context-menu-${gridId}`)(ContextMenu);
      grid = connectDropTarget(
        // Width has to be set in order to render correctly in a resizable box
        // Position must be relative so that the absolutely positioned DragLayer behaves correctly
        <div
          style={{
            width,
            height,
            position: "relative"
            // border: "solid 0.05em"
          }}
          // tabIndex={0}
          id={gridId}
          // onCopy={this.handleCopy}
          // onKeyDown={this.handleKeyDown}
          // onKeyUp={this.handleKeyUp}
          onWheel={this.handleWheel}
        >
          <DragLayer gridId={gridId} />
          <div
            className="zebulon-grid-zebulon-grid"
            style={{ fontSize: `${zoomValue * 100}%` }}
          >
            <div style={{ display: "flex" }}>
              <DimensionHeaders gridId={gridId} />
              <ColumnHeaders
                gridId={gridId}
                rows={null}
                columns={columns}
                height={rows.crossSize}
                width={dataCellsSizes.width}
              />
            </div>
            <div style={{ display: "flex" }}>
              <RowHeaders
                gridId={gridId}
                rows={rows}
                columns={null}
                height={dataCellsSizes.height}
                width={columns.crossSize}
              />
              <DataCells
                drilldown={drilldown}
                menuFunctions={this.props.menuFunctions}
                rows={rows}
                columns={columns}
                gridId={gridId}
                height={height - rows.crossSize}
                width={width - columns.crossSize}
                scrollbarsWidth={scrollbarsWidth}
                onScroll={this.onScroll}
                isPushing={this.isPushing}
              />
            </div>
            <ConnectedMenu />
          </div>
        </div>
      );
    } else {
      grid = <div>Loading...</div>;
    }
    return grid;
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
