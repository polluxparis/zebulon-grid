import { connect } from 'react-redux';

import PivotGrid from '../components/Grid/Grid';
import {
  getLayout,
  getHeaderSizes,
  getCellSizes,
  getRowAxis,
  getColumnUiAxis,
  getRowUiAxis,
  getColumnAxis,
  getActivatedDataFields,
 } from '../selectors';
import { updateCellSize, setConfigProperty } from '../actions';
import { AxisType } from '../Axis';

const mapStateToProps = state => ({
  width: state.config.width,
  layout: getLayout(state),
  defaultCellSizes: getCellSizes(state),
  headerSizes: getHeaderSizes(state),
  sizes: state.sizes,
  columnHeaders: getColumnUiAxis(state).headers,
  rowHeaders: getRowUiAxis(state).headers,
  rowFields: getRowAxis(state).fields,
  columnFields: getColumnAxis(state).fields,
  dataFieldsCount: getActivatedDataFields(state).length,
});

const mapDispatchToProps = dispatch => ({
  updateCellSize: ({ handle, offset, initialOffset, sizes, defaultCellSizes }) => {
    if (handle.leafSubheaders && handle.leafSubheaders.length &&
      (
        (handle.axis === AxisType.COLUMNS && handle.position === 'right')
        || (handle.axis === AxisType.ROWS && handle.position === 'bottom')
      )
    ) {
      const fractionalOffset = {
        x: (offset.x - initialOffset.x) / handle.leafSubheaders.length,
        y: (offset.y - initialOffset.y) / handle.leafSubheaders.length,
      };
      handle.leafSubheaders.forEach((subheader) => {
        dispatch(
          updateCellSize({
            handle: { ...handle, leafSubheaders: [], id: subheader.key },
            offset: fractionalOffset,
            initialOffset: { x: 0, y: 0 },
            defaultCellSizes,
            sizes,
          }),
      );
      });
    } else {
      dispatch(updateCellSize({ handle, offset, initialOffset, sizes, defaultCellSizes }));
    }
  },
  setSizes: ({ height, width }) => {
    if (height) dispatch(setConfigProperty({ height, width }, 'height'));
    if (width) dispatch(setConfigProperty({ height, width }, 'width'));
  },
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
    rowFields,
    columnFields,
    dataFieldsCount,
   },
  { updateCellSize, setSizes },
  ownProps,
) => ({
  rowFields,
  columnFields,
  columnHeaders,
  rowHeaders,
  width,
  layout,
  dataFieldsCount,
  headerSizes,
  updateCellSize: ({ handle, offset, initialOffset }) =>
    updateCellSize({ handle, offset, initialOffset, sizes, defaultCellSizes }),
  setSizes,
  ...ownProps,
});

export default connect(mapStateToProps, mapDispatchToProps, mergeProps)(PivotGrid);
