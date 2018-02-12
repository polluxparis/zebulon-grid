import React, { Component } from "react";
// import { findDOMNode } from "react-dom";
import { DropTarget } from "react-dnd";
// import { connectMenu } from "react-contextmenu";
import { getMenu } from "../ContextMenu/ContextMenu";

// import ArrowKeyStepper from '../../containers/ArrowKeyStepper';
import DataCells from "../../containers/DataCells";
import DimensionHeaders from "../../containers/DimensionHeaders";
import ColumnHeaders from "../../containers/ColumnHeaders";
import RowHeaders from "../../containers/RowHeaders";
import DragLayer from "./DragLayer";
// import { utils } from "zebulon-controls";
// import { isEmpty } from "../../utils/generic";
import { constants, utils, ContextualMenu } from "zebulon-controls";
// import {  } from "zebulon-controls";
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
  constructor(props) {
    super(props);
    this.state = {
      toolTip: { style: { opacity: 0, height: 0 }, modal: false }
    };
  }
  componentDidUpdate(prevProps) {
    const { height, width, setSizes } = this.props;
    if (height !== prevProps.height || width !== prevProps.width) {
      setSizes({ height, width });
    }
  }
  componentDidMount() {
    this.props.getRef(this);
  }

  componentWillReceiveProps(nextProps) {
    this.isPushing = this.props.pushedData !== nextProps.pushedData;
    if (
      this.contextualMenu &&
      this.contextualMenu.state.menu &&
      this.contextualMenu.state.menu.visible
    ) {
      // Object.keys(nextProps).map(key=>({key,equal:nextProps[key]===this.props[key]}));
      this.closeOpenedWindows();
    }
  }
  // ------------------------------------
  // Navigation
  // ------------------------------------
  closeOpenedWindows = () => {
    this.contextualMenu.close();
    if (this.state.toolTip.comment !== undefined) {
      this.setState({
        toolTip: { toolTip: { style: { opacity: 0, height: 0 }, modal: false } }
      });
    }
  };

  hasParent(element, id) {
    if (!element.parentElement) {
      return false;
    } else if (element.parentElement.id === id) {
      return true;
    } else {
      return this.hasParent(element.parentElement, id);
    }
  }
  handleKeyDown = e => {
    // a voir
    const isFilter = this.hasParent(document.activeElement, "filter");
    if (e.key === "Escape") {
      this.closeOpenedWindows();
    }
    if (isFilter && e.key === "Tab") {
      return false;
    }
    if (
      !isFilter &&
      this.dataCells &&
      this.dataCells.handleNavigationKeys &&
      utils.isNavigationKey(e)
    ) {
      // if (this.state.openedFilter) {
      //   this.setState({ openedFilter: undefined });
      // }
      // if (e.type === "copy") return this.handleCopy(e);
      // else if (e.type === "paste") return this.handlePaste(e);
      // else if (e.type === "keydown")
      return this.dataCells.handleNavigationKeys(e);
    }
  };
  // if (e.metaKey || e.ctrlKey) {
  //       // To be consistent with browser behaviour, we also accept = which is on the same keyboard touch as +
  //       if (e.key === "+" || e.key === "=") {
  //         this.props.zoom(ZOOM_IN);
  //         e.preventDefault();
  //       }
  //       // ctrl - -> zoom out
  //       // To be consistent with browser behaviour, we also accept _ which is on the same keyboard touch as -
  //       if (e.key === "-" || e.key === "_") {
  //         this.props.zoom(ZOOM_OUT);
  //         e.preventDefault();
  //       } else if (e.which === 65) {
  //         // ctrl A -> select all
  //         this.props.selectRange({
  //           selectedCellStart: {
  //             columnIndex: this.nextVisible(columns.leaves, 0, -1, 0),
  //             rowIndex: this.nextVisible(rows.leaves, 0, -1, 0)
  //           },
  //           selectedCellEnd: {
  //             columnIndex: this.nextVisible(
  //               columns.leaves,
  //               columns.length - 1,
  //               1,
  //               0
  //             ),
  //             rowIndex: this.nextVisible(rows.leaves, rows.length - 1, 1, 0)
  //           }
  //         });
  //         e.preventDefault();
  //       }
  //       // arrow keys
  //     }
  //-----------------------------
  handleCopy = e => {
    this.closeOpenedWindows();
    this.props.copy(this.props.selectedRange);
  };
  handlePaste = e => {
    this.closeOpenedWindows();
    if (this.props.editable) {
      const data = this.props.paste(
        e.clipboardData.getData("text"),
        this.props.selectedRange.end
      );
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
      this.closeOpenedWindows();
      this.props.scrollToRow(scrollToRow);
    }
    if (
      scrollToColumn !== null &&
      (scrollToColumn.index !== columns.index ||
        scrollToColumn.direction !== columns.direction)
    ) {
      this.closeOpenedWindows();
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
        horizontal: constants.ScrollbarSize * (columns.hasScrollbar || 0),
        vertical: constants.ScrollbarSize * (rows.hasScrollbar || 0)
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
      // const ConnectedMenu = connectMenu(`context-menu-${gridId}`)(ContextMenu);
      grid = connectDropTarget(
        // Width has to be set in order to render correctly in a resizable box
        // Position must be relative so that the absolutely positioned DragLayer behaves correctly
        // position of Datacells must be absolute to avoid columns size distorsion
        <div
          style={{
            width,
            height,
            position: "relative"
          }}
          id={gridId}
          onWheel={e => this.dataCells.onWheel(e)}
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
                setToolTip={toolTip => this.setState({ toolTip })}
                style={{
                  // position: "absolute",
                  top: rows.crossSize,
                  left: columns.crossSize
                }}
                getRef={ref => (this.dataCells = ref)}
              />
            </div>
            <ContextualMenu
              key="contextual-menu"
              getMenu={getMenu}
              componentId={gridId}
              ref={ref => (this.contextualMenu = ref)}
            />
            <div
              key={"tool-tip"}
              className="zebulon-tool-tip"
              style={this.state.toolTip.style}
            >
              {this.state.toolTip.comment}
            </div>
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
  props => `cell-resize-handle--${props.gridId || 0}`,
  gridSpec,
  collect
)(PivotGrid);
