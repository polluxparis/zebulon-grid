import React, { Component } from 'react';
import { ScrollSync } from 'react-virtualized/dist/commonjs/ScrollSync';
import { ArrowKeyStepper } from 'react-virtualized/dist/commonjs/ArrowKeyStepper';
import { DropTarget } from 'react-dnd';
import { connectMenu } from 'react-contextmenu';
import ContextMenu from '../ContextMenu/ContextMenu';

// import ArrowKeyStepper from '../../containers/ArrowKeyStepper';
import DataCells from '../../containers/DataCells';
import DimensionHeaders from '../../containers/DimensionHeaders';
import ColumnHeaders from '../../containers/ColumnHeaders';
import RowHeaders from '../../containers/RowHeaders';
import DragLayer from './DragLayer';
import { isEmpty, isNull } from '../../utils/generic';
import { ZOOM_IN, ZOOM_OUT } from '../../constants';
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
    this.scrollToRow = 0;
    this.scrollToColumn = 0;
    this.focusCellKeys = [];
    // this.handleScrollToChange = this.handleScrollToChange.bind(this);
    // this.handleKeyDown = this.handleKeyDown.bind(this);
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
      this.scrollToRow =
        nextProps.selectedRange.focusedCell.rowIndex || this.scrollToRow;
      this.scrollToColumn =
        nextProps.selectedRange.focusedCell.columnIndex || this.scrollToColumn;
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
      // To be consistent with browser behaviour, we also accept = which is on the same keyboard touch as +
      if (e.key === '+' || e.key === '=') {
        this.props.zoom(ZOOM_IN);

        e.preventDefault();
      }
      // ctrl - -> zoom out
      // To be consistent with browser behaviour, we also accept _ which is on the same keyboard touch as -
      if (e.key === '-' || e.key === '_') {
        this.props.zoom(ZOOM_OUT);
        e.preventDefault();
      }
      // Page down
      //   if (e.key === 'PageDown') {
      //     handleScrollToChange({});

      //     e.preventDefault();
      // }
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
    const selectedRange = this.props.selectedRange;

    if (this.shiftKeyIsPressed) {
      // after column selection
      if (isNull(selectedRange.focusedCell.rowIndex)) {
        const columnIndex =
          selectedRange.selectedCellEnd.columnIndex +
          (scrollToColumn > this.scrollToColumn ? 1 : -1);
        this.props.selectRange({
          selectedCellEnd: {
            columnIndex,
            rowIndex: selectedRange.selectedCellEnd.rowIndex
          },
          focusedCell: { columnIndex, rowIndex: null }
        });
      } else if (isNull(selectedRange.focusedCell.columnIndex)) {
        // after row  selection
        const rowIndex =
          selectedRange.selectedCellEnd.rowIndex +
          (scrollToRow > this.scrollToRow ? 1 : -1);
        this.props.selectRange({
          selectedCellEnd: {
            rowIndex,
            columnIndex: selectedRange.selectedCellEnd.columnIndex
          },
          focusedCell: { rowIndex, columnIndex: null }
        });
      } else {
        this.props.selectRange({
          selectedCellEnd: {
            columnIndex: scrollToColumn,
            rowIndex: scrollToRow
          },
          focusedCell: { columnIndex: scrollToColumn, rowIndex: scrollToRow }
        });
      }
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
      // customFunctions,
      drilldown,
      zoomValue,
      id: gridId
    } = this.props;

    const { columnHorizontalCount, rowVerticalCount } = layout;
    let grid;
    if (this.props.status.loading) {
      grid = <div>Loading data...</div>;
    } else if (this.props.status.error) {
      grid = (
        <div style={{ color: 'red' }}>
          <p>Error loading data</p>
          {this.props.status.error.message}
        </div>
      );
    } else {
      const ConnectedMenu = connectMenu(`context-menu-${gridId}`)(ContextMenu);
      grid = connectDropTarget(
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
                  <div
                    className="pivotgrid-pivotgrid"
                    style={{ fontSize: `${zoomValue * 100}%` }}
                  >
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
                        onSectionRendered={onSectionRendered}
                        scrollToColumn={scrollToColumn}
                        scrollToRow={scrollToRow}
                        onScroll={onScroll}
                        drilldown={drilldown}
                        menuFunctions={this.props.menuFunctions}
                        clientHeight={clientHeight}
                        clientWidth={clientWidth}
                        gridId={gridId}
                      />
                    </div>
                    <ConnectedMenu />
                  </div>}
              </ScrollSync>}
          </ArrowKeyStepper>
        </div>
      );
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
// expose all actions
// Object.keys(actions).forEach(action => {
//   /* eslint-disable func-names */
//   PivotGrid.prototype[action] = function(...args) {
//     this.state.store.dispatch(actions[action](...args));
//   };
//   /* eslint-enable */
// });
// Add grid id to the type to ensure only correct drop target is used
export default DropTarget(
  props => `cell-resize-handle--${props.id || 0}`,
  gridSpec,
  collect
)(PivotGrid);
