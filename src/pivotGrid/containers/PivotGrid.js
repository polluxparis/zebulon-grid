import { connect } from "react-redux";
import { DragDropContext } from "react-dnd";
import HTML5Backend from "react-dnd-html5-backend";

import PivotGrid from "../components/Grid/Grid";
import {
  getLayout,
  getCellSizes,
  getRowAxis,
  getColumnUiAxis,
  getRowUiAxis,
  getColumnAxis,
  getActivatedMeasures
} from "../selectors";
import { updateCellSize, setConfigProperty } from "../actions";
import { AxisType } from "../Axis";

const mapStateToProps = state => ({
  width: state.config.width,
  layout: getLayout(state),
  defaultCellSizes: getCellSizes(state),
  sizes: state.sizes,
  columnHeaders: getColumnUiAxis(state).headers,
  rowHeaders: getRowUiAxis(state).headers,
  rowDimensions: getRowAxis(state).dimensions,
  columnDimensions: getColumnAxis(state).dimensions,
  dataDimensionsCount: getActivatedMeasures(state).length
});

const mapDispatchToProps = dispatch => ({
  updateCellSize: ({
    handle,
    offset,
    initialOffset,
    sizes,
    defaultCellSizes
  }) => {
    if (
      handle.leafSubheaders &&
      handle.leafSubheaders.length &&
      ((handle.axis === AxisType.COLUMNS && handle.position === "right") ||
        (handle.axis === AxisType.ROWS && handle.position === "bottom"))
    ) {
      const fractionalOffset = {
        x: (offset.x - initialOffset.x) / handle.leafSubheaders.length,
        y: (offset.y - initialOffset.y) / handle.leafSubheaders.length
      };
      handle.leafSubheaders.forEach(subheader => {
        dispatch(
          updateCellSize({
            handle: { ...handle, leafSubheaders: [], id: subheader.key },
            offset: fractionalOffset,
            initialOffset: { x: 0, y: 0 },
            defaultCellSizes,
            sizes
          })
        );
      });
    } else {
      dispatch(
        updateCellSize({
          handle,
          offset,
          initialOffset,
          sizes,
          defaultCellSizes
        })
      );
    }
  },
  setSizes: ({ height, width }) => {
    if (height) dispatch(setConfigProperty({ height, width }, "height"));
    if (width) dispatch(setConfigProperty({ height, width }, "width"));
  }
});

const mergeProps = (
  {
    width,
    layout,
    headerSizes,
    sizes,
    defaultCellSizes,
    columnHeaders,
    rowHeaders,
    rowDimensions,
    columnDimensions,
    dataDimensionsCount
  },
  { updateCellSize, setSizes },
  ownProps
) => ({
  rowDimensions,
  columnDimensions,
  columnHeaders,
  rowHeaders,
  width,
  layout,
  dataDimensionsCount,
  updateCellSize: ({ handle, offset, initialOffset }) =>
    updateCellSize({ handle, offset, initialOffset, sizes, defaultCellSizes }),
  setSizes,
  ...ownProps
});

export const PivotGridWithoutDndContext = connect(
  mapStateToProps,
  mapDispatchToProps,
  mergeProps
)(PivotGrid);

export default DragDropContext(HTML5Backend)(PivotGridWithoutDndContext);
